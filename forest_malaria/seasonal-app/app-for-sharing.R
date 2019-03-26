rm(list = ls())

list.of.packages <- c("shiny", "devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(devtools)
source_url("georgoff.github.io/forest_malaria/seasonal_R/Seasonal-Reproductive-Numbers-Functions.R")

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
                                   choices = c("sin", "sin1", "block", "Upload Seasonal Signal File"))),
               
               column(2,
                      fileInput(inputId = "pattern_file",
                                label = "Input Seasonal Signal File"),
                      
                      hr(),
                      helpText("Seasonal Signal File must be a .csv of daily values over the course of a year (365 days)")),
               
               column(2,
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
                                    width = "100%",
                                    height = "1000px"))
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
    if(input$pattern != "Upload Seasonal Signal File") {
      makeVCrel(pattern = input$pattern, Nyr = input$Nyr, showit=TRUE)
    }
    if(input$pattern == "Upload Seasonal Signal File") {
      # create function to sparse file input and create seasonal signal
      user_pattern <- read.csv(input$pattern_file$datapath)
      makeVCrel(pattern = user_pattern[,1],
                Nyr = input$Nyr,
                showit = TRUE)
    }
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
    if(input$pattern != "Upload Seasonal Signal File") {
      this_VCrel <- makeVCrel(pattern = input$pattern, Nyr = input$Nyr, showit=TRUE)
    }
    if(input$pattern == "Upload Seasonal Signal File") {
      # create function to sparse file input and create seasonal signal
      user_pattern <- read.csv(input$pattern_file$datapath)
      this_VCrel <- makeVCrel(pattern = user_pattern[,1],
                              Nyr = input$Nyr,
                              showit = TRUE)
    }
    generations_graph(Re = input$Re,
                      num_gen = input$num_gen,
                      Rtime = makeRtime(input$Re, plot = FALSE),
                      VCrel = this_VCrel)
  })
  
  output$plot_eigen <- renderPlot({
    if(input$pattern != "Upload Seasonal Signal File") {
      this_VCrel <- makeVCrel(pattern = input$pattern, Nyr = input$Nyr, showit=TRUE)
    }
    if(input$pattern == "Upload Seasonal Signal File") {
      # create function to sparse file input and create seasonal signal
      user_pattern <- read.csv(input$pattern_file$datapath)
      this_VCrel <- makeVCrel(pattern = user_pattern[,1],
                              Nyr = input$Nyr,
                              showit = TRUE)
    }
    plotEigen(NGEN = input$NGEN,
              Rtime = makeRtime(input$Re, plot = FALSE),
              VCrel = this_VCrel)
  })
  
  output$eigenvalues <- renderPlot({
    if(input$pattern != "Upload Seasonal Signal File") {
      this_VCrel <- makeVCrel(pattern = input$pattern, Nyr = input$Nyr, showit=TRUE)
    }
    if(input$pattern == "Upload Seasonal Signal File") {
      # create function to sparse file input and create seasonal signal
      user_pattern <- read.csv(input$pattern_file$datapath)
      this_VCrel <- makeVCrel(pattern = user_pattern[,1],
                              Nyr = input$Nyr,
                              showit = TRUE)
    }
    plot(plotEigen(NGEN = input$NGEN,
                   Rtime = makeRtime(input$Re, plot = FALSE),
                   VCrel = this_VCrel),
         type = "l",
         ylab = "Eigenvalue")
  })
  
  output$gen_message <- renderText({
    paste0("Number of Generations = ", input$NGEN)
  })
  
}


shinyApp(ui = ui, server = server)