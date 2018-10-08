library(shiny)
library(data.table)

# source("/georgoff.github.io/forest_malaria/TaR-malaria.R")

ui <- fluidPage(
  titlePanel("Forest Malaria Simulator"),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput(inputId = "a",
                   label = "a: Human Blood Feeding Rate",
                   value = 0.88,
                   min = 0, max = 1, step = 0.01),
      numericInput(inputId = "b",
                   label = "b: Proportion of Bites by Infectious Mosquitoes That Cause an Infection",
                   value = 0.55,
                   min = 0, max = 1, step = 0.01),
      numericInput(inputId = "c",
                   label = "c: Proportion of Mosquitoes Infected After Biting Infectious Human",
                   value = 0.15,
                   min = 0, max = 1, step = 0.01),
      numericInput(inputId = "g",
                   label = "g: Per Capita Death Rate of Mosquitoes",
                   value = 0.1,
                   min = 0, max = 1, step = 0.01),
      numericInput(inputId = "n",
                   label = "n: Time for Sporogonic Cycle",
                   value = 12,
                   min = 0, step = 0.5),
      numericInput(inputId = "r",
                   label = "r: Rate that Humans Recover from an Infection",
                   value = 1/200,
                   min = 0),
      
      fileInput(inputId = "params_csv",
                label = "Upload params.csv",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      fileInput(inputId = "psi_csv",
                label = "Upload psi.csv",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      
      # submitButton(text = "GO")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Constants",
                 textOutput(outputId = "a_text"),
                 textOutput(outputId = "b_text")),
        tabPanel("Parameters",
                 tableOutput(outputId = "params_table"),
                 tableOutput(outputId = "psi_table"))
      )
    )
  )
  
)

server <- function(input, output) {
  output$a_text <- renderText({
    as.character(input$a)
  })
  
  output$b_text <- renderText({
    as.character(input$b)
  })
  
  output$params_table <- renderTable({
    inFile <- input$params_csv
    
    if (is.null(inFile)) {
      return("Please upload params.csv")
    }
  
    read.csv(inFile$datapath)
  })
  
  output$psi_table <- renderTable({
    inFile <- input$psi_csv
    
    if (is.null(inFile)) {
      return("Please upload psi.csv")
    }
    
    read.csv(inFile$datapath)
  })
}

shinyApp(ui = ui, server = server)