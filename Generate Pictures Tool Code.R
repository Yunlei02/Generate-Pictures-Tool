library(shiny)
library(httr)
library(jsonlite)
library(shinyjs)
library(shinybusy)
library(shinydashboard)
library(shinythemes)
library(waiter)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "OpenAI Assistant"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("Get API Key", tabName = "get_api", icon = icon("key")),
      menuItem("Key Plots Extraction", tabName = "keyplot", icon = icon("book")),
      menuItem("Image Generation", tabName = "image", icon = icon("image"))
    )
  ),
  dashboardBody(
    add_busy_spinner(spin = "fading-circle"),
    tabItems(
      # Tab for Get API Key
      tabItem(tabName = "get_api",
              fluidPage(
                titlePanel("Get Your OpenAI API Key"),
                mainPanel(
                  h3("Follow these steps to get your OpenAI API Key:"),
                  tags$ol(
                    tags$li("Visit the ", a("OpenAI platform", href = "https://platform.openai.com/", target = "_blank"), " website and sign up or log in."),
                    tags$li("If you are a new user, find 'Billing' on the left menu bar under 'Settings' and add the amount. Visit the ", a("OpenAI Pricing", href = "https://platform.openai.com/", target = "_blank"), " to know more about price charging."),
                    tags$li("If you are an old user or have already finished recharging, please find 'API keys' in the left menu and click 'Create a new secret key' to generate API keys."),
                    tags$li("Copy the API key when you use 'Key Plots Extraction' and 'Image Generation'.")
                  ),
                  br(),
                  actionButton("keyPlotsButton", "Go to Key Plots Extraction"),
                  actionButton("imageGenButton", "Go to Image Generation")
                )
              )
      ),
      # Tab for Key Plots Extraction
      tabItem(tabName = "keyplot",
              fluidPage(
                useShinyjs(),  # Initialize shinyjs
                titlePanel("Key Plots Extraction with OpenAI"),
                sidebarLayout(
                  sidebarPanel(
                    passwordInput("api_key", "Enter your OpenAI API key:"),
                    p("Get your own OpenAI API key at", a(href = "https://openai.com/api/", "OpenAI.com")),
                    textAreaInput("text_input", "Enter the text:", value="", rows=10),
                    htmlOutput("word_count"),
                    tags$br(),
                    actionButton("submit_btn", "Extract Key Plots")
                  ),
                  mainPanel(
                    box(title = "Extracted Key Plots", HTML("<div id='keyplots_output'></div>"), width = 8, collapsible = TRUE, status = "primary")
                  )
                )
              )
      ),
      # Tab for Image Generation
      tabItem(tabName = "image",
              fluidPage(
                useWaiter(),
                titlePanel("OpenAI DALL·E Image Generation App"),
                theme = shinytheme("flatly"),
                fluidRow(
                  column(4, 
                         passwordInput("api_key_image", "Enter your OpenAI API key:"),
                         p("Get your own OpenAI API key at", a(href = "https://openai.com/api/", "OpenAI.com")),
                         sliderInput("num_images", "Number of Images", min = 1, max = 4, value = 1),
                         selectInput("category", "Category (optional)", choices = c("","Line Drawing", "Black and White", "Color"), selected = NULL),
                         selectInput("style", "Style (optional)", choices = c("","Realistic", "Cartoon", "Abstract"), selected = NULL)
                  ),
                  column(8,
                         textAreaInput(
                           "description", "Enter your image description:",
                           value = "",
                           placeholder = "e.g., 'A beautiful sunset over a mountain'",
                           height = "100px"
                         ),
                         actionButton("submit_image", "Generate Image"),
                         uiOutput("dynamicImage")
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Navigate to Key Plots Extraction Tab
  observeEvent(input$keyPlotsButton, {
    updateTabItems(session, "sidebar", "keyplot")
  })
  
  # Navigate to Image Generation Tab
  observeEvent(input$imageGenButton, {
    updateTabItems(session, "sidebar", "image")
  })
  
  # Word count for text input
  output$word_count <- renderUI({
    word_count <- length(unlist(strsplit(input$text_input, "\\W+")))
    if(word_count > 1000) {
      color <- "red"
    } else {
      color <- "black"
    }
    HTML(sprintf("<span style='color: %s'>%d/1000</span>", color, word_count))
  })
  
  # For Key Plots Extraction
  observeEvent(input$submit_btn, {
    if (input$api_key == "") {
      showModal(modalDialog(title = "Error", "Please enter your API key for key plots extraction."))
      return()
    }
    if (input$text_input == "") {
      showModal(modalDialog(title = "Error", "Please enter the text to extract key plots."))
      return()
    }
    
    # Call OpenAI API
    messages <- list(
      list(role="system", content="You are a helpful assistant. Extract key plot points from the text."),
      list(role="user", content=input$text_input)
    )
    url <- "https://api.openai.com/v1/chat/completions"
    response <- POST(
      url,
      config = add_headers(
        Authorization = paste("Bearer", input$api_key),
        "Content-Type" = "application/json"
      ),
      body = toJSON(list(
        model = "gpt-4",  # Specify the model here
        messages = messages  # Messages for the chat-based API
      ), auto_unbox = TRUE)
    )
    
    # Check and log the response
    result <- content(response, "parsed")
    
    if (http_status(response)$category != "Success") {
      showModal(modalDialog(title = "Error", "Failed to extract key plots. Please try again."))
    } else {
      # Update the output with extracted key plots and handle line breaks
      extracted_text <- result$choices[[1]]$message$content
      formatted_text <- gsub("\\n", "<br/>", extracted_text)
      shinyjs::html("keyplots_output", formatted_text)
    }
  })
  
  # For DALL·E Image Generation
  imageURLs <- reactiveVal(list())
  
  observeEvent(input$submit_image, {
    if (input$api_key_image == "") {
      showModal(modalDialog(title = "Error", "Please enter your API key for image generation."))
      return()
    }
    
    # Show busy spinner
    shinybusy::add_busy_spinner(spin = "fading-circle")
    
    # Set up the API request
    request_data <- list(
      prompt = paste(input$description, 
                     if (!is.null(input$category)) input$category, 
                     if (!is.null(input$style)) input$style, 
                     sep = ", "),
      n = input$num_images  # Specify the number of images
    )
    
    response <- POST(
      url = "https://api.openai.com/v1/images/generations",
      body = toJSON(request_data, auto_unbox = TRUE),
      config = add_headers(
        Authorization = paste("Bearer", input$api_key_image),
        "Content-Type" = "application/json"
      )
    )
    
    result <- content(response, "parsed")
    
    # Extracting image URLs from the API response
    if (!is.null(result$data)) {
      imageURLs(lapply(result$data, function(x) x$url))
    } else {
      showModal(modalDialog(title = "Error", "API did not return valid image URLs."))
    }
  })
  
  output$dynamicImage <- renderUI({
    img_tags <- lapply(imageURLs(), function(url) {
      img(src = url, alt = "Generated Image", width = "250px")
    })
    do.call(tagList, img_tags)
  })
}

shinyApp(ui = ui, server = server)

