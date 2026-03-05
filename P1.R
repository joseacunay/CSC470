library(shiny)
library(httr)
library(jsonlite)

call_claude <- function(user_text){
  
  response <- POST(
    url = "https://api.anthropic.com/v1/messages",
    add_headers(
      "x-api-key" = Sys.getenv("ANTHROPIC_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ),
    body = toJSON(list(
      model = "claude-3-haiku-20240307",
      max_tokens = 600,
      messages = list(
        list(
          role = "user",
          content = paste(
            "You are an international scholarship advisor specialized in NCAA and NAIA recruiting.",
            user_text
          )
        )
      )
    ), auto_unbox = TRUE)
  )
  
  result <- content(response, as="parsed", type="application/json")
  
  return(result$content[[1]]$text)
}

ui <- fluidPage(
  titlePanel("ScholarBot Assistant " ),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("user_input",
                    "Tell me about yourself:",
                    rows = 5),
      actionButton("send", "Ask Pepe")
    ),
    
    mainPanel(
      h3("Pepe's Response"),
      verbatimTextOutput("response")
    )
  )
)

server <- function(input, output){
  
  observeEvent(input$send, {
    
    output$response <- renderText({
      call_claude(input$user_input)
    })
    
  })
}

shinyApp(ui, server)