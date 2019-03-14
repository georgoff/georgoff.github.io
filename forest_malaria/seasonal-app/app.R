library(shiny)
library(RColorBrewer)

source("/homes/georgoff/georgoff.github.io/forest_malaria/seasonal_R/Seasonal-Reproductive-Numbers-Functions.R")

ui <- fluidPage(
  titlePanel("Seasonality of Reproductive Numbers"),
  
  tabsetPanel(
    tabPanel("VCrel",
             plotOutput("seasonality_plot"),
             
             hr(),
             
             fluidRow(
               column(2,
                      h4("Select Shape"),
                      radioButtons(inputId = "pattern",
                                   label = "Shape",
                                   choices = c("sin", "sin1", "block"))),
               
               column(2,
                      h4("Enter Number of Years"),
                      numericInput(inputId = "Nyr",
                                   label = "Number of Years",
                                   value = 5,
                                   min = 1, max = NA, step = 1))
             )),
    
    tabPanel("VCtime",
             plotOutput("VCtime_plot"),
             
             hr(),
             
             fluidRow(
               column(2,
                      numericInput(inputId = "EIP",
                                   label = "EIP",
                                   value = 12,
                                   min = 0, max = NA, step = 1)),
               
               column(2,
                      numericInput(inputId = "p",
                                   label = "p",
                                   value = 0.9,
                                   min = 0, max = 1, step = 0.01)),
               
               column(2,
                      numericInput(inputId = "a",
                                   label = "a",
                                   value = 0.3,
                                   min = 0, max = 1, step = 0.01)),
               
               column(2,
                      numericInput(inputId = "tol",
                                   label = "tol",
                                   value = 0.001,
                                   min = 0, max = NA, step = 0.001))
             )),
    
    tabPanel("Dtime",
             plotOutput("Dtime_plot"),
             
             hr(),
             
             fluidRow(
               column(2,
                      numericInput(inputId = "D",
                                   label = "D",
                                   value = 38,
                                   min = 0, max = NA, step = 1))
             )),
    
    tabPanel("Rtime",
             plotOutput("Rtime_plot"),
             
             hr(),
             
             fluidRow(
               column(2,
                      numericInput(inputId = "Re",
                                   label = "Re",
                                   value = 1,
                                   min = 0, max = NA, step = 0.1))
             )),
    
    tabPanel("Generations Graph",
             plotOutput("generations_graph"),
             
             hr(),
             
             fluidRow(
               column(2,
                      numericInput(inputId = "num_gen",
                                   label = "Number of Generations",
                                   value = 40,
                                   min = 1, max = NA, step = 1))
             )),
    
    tabPanel("PlotEigen",
             sidebarLayout(
               sidebarPanel(numericInput(inputId = "NGEN",
                                         label = "Number of Generations",
                                         value = 15,
                                         min = 1, max = NA, step = 1),
                            width = 1),
               
               mainPanel(plotOutput("plot_eigen",
                                    width = "100%"))
             )),
    
    tabPanel("Eigenvalues",
             plotOutput("eigenvalues"),
             
             hr(),
             
             fluidRow(
               column(2,
                      textOutput("gen_message"))
             ))

  
  )
)


server <- function(input, output) {
  output$seasonality_plot <- renderPlot({
    makeVCrel(pattern = input$pattern, Nyr = input$Nyr, showit=TRUE)
  })
  
  output$VCtime_plot <- renderPlot({
    makeVCtime(EIP = input$EIP, p = input$p, a = input$a, tol = input$tol, plot=TRUE)
  })
  
  output$Dtime_plot <- renderPlot({
    makeDtime(D = input$D, plot=TRUE)
  })
  
  output$Rtime_plot <- renderPlot({
    makeRtime(input$Re, plot = TRUE)
  })
  
  output$generations_graph <- renderPlot({
    generations_graph(Re = input$Re,
                      num_gen = input$num_gen,
                      Rtime = makeRtime(input$Re, plot = FALSE),
                      VCrel = makeVCrel(pattern = input$pattern,
                                        Nyr = input$Nyr,
                                        showit = FALSE))
  })
  
  output$plot_eigen <- renderPlot({
    plotEigen(NGEN = input$NGEN,
              Rtime = makeRtime(input$Re, plot = FALSE),
              VCrel = makeVCrel(pattern = input$pattern,
                                Nyr = input$Nyr,
                                showit = FALSE))
  })
  
  output$eigenvalues <- renderPlot({
    plot(plotEigen(NGEN = input$NGEN,
                   Rtime = makeRtime(input$Re, plot = FALSE),
                   VCrel = makeVCrel(pattern = input$pattern,
                                     Nyr = input$Nyr,
                                     showit = FALSE)),
         type = "l")
  })
  
  output$gen_message <- renderText({
    paste0("Number of Generations = ", input$NGEN)
  })
  
}


shinyApp(ui = ui, server = server)