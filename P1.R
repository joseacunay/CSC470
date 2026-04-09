library(shiny)
library(httr)
library(jsonlite)
library(commonmark)

call_claude <- function(user_text){
  
  system_prompt <- paste(
    "You are ScholarBot, an international scholarship advisor specialized in NCAA and NAIA recruiting.",
    "Formatting rules:",
    "- When explaining steps, requirements or recommendations use numbered lists automatically.",
    "- When comparing universities, leagues, scholarships, costs or options use markdown tables automatically.",
    "- Do NOT wait for the user to ask for lists or tables.",
    "- Keep answers clear, structured and professional.",
    "- Use markdown formatting."
  )
  
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
        list(role="user", content=paste(system_prompt, user_text))
      )
    ), auto_unbox = TRUE)
  )
  
  result <- content(response, as="parsed", type="application/json")
  result$content[[1]]$text
}

ui <- fluidPage(
  titlePanel("ScholarBot Assistant"),
  
  tags$style(HTML("
    .chat-container {height:400px;overflow-y:auto;border:1px solid #ddd;
    padding:15px;background:#f9f9f9;border-radius:10px;display:flex;flex-direction:column;}
    .msg-user {align-self:flex-end;background:#007bff;color:white;padding:8px 15px;
    border-radius:15px 15px 2px 15px;margin:5px;max-width:75%;}
    .msg-bot {align-self:flex-start;background:#e9e9eb;color:black;padding:8px 15px;
    border-radius:15px 15px 15px 2px;margin:5px;max-width:75%;}
  ")),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("user_input","Tell me about yourself:",rows=5),
      actionButton("send","Ask Pepe")
    ),
    mainPanel(
      h3("Pepe's Response"),
      div(class="chat-container", uiOutput("chat_ui"))
    )
  )
)

server <- function(input, output, session){
  
  chat_history <- reactiveVal(data.frame(sender=character(),message=character()))
  
  observeEvent(input$send,{
    req(input$user_input)
    
    hist <- rbind(chat_history(),
                  data.frame(sender="You",message=input$user_input))
    
    bot <- call_claude(input$user_input)
    
    hist <- rbind(hist,
                  data.frame(sender="Bot",message=bot))
    
    chat_history(hist)
    updateTextAreaInput(session,"user_input",value="")
  })
  
  output$chat_ui <- renderUI({
    msgs <- chat_history()
    
    lapply(1:nrow(msgs), function(i){
      
      div(
        class = ifelse(msgs$sender[i]=="You","msg-user","msg-bot"),
        HTML(commonmark::markdown_html(msgs$message[i]))
      )
      
    })
  })
}


shinyApp(ui, server)