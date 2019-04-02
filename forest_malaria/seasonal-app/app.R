library(shiny)
library(RColorBrewer)

source("H:/georgoff.github.io/forest_malaria/seasonal_R/Seasonal-Reproductive-Numbers-Functions.R")

ui <- fluidPage(
  titlePanel("Seasonality of Reproductive Numbers"),
  
  tabsetPanel(
    tabPanel("Tutorial",
             tabsetPanel(
               tabPanel("Introduction",
                        
                        h2("Welcome to the Seasonality Simulator 5000"),
                        
                        hr(),
                        
                        h4("You're about to embark on the journey of a lifetime...")),
               
               tabPanel("VCrel",
                        plotOutput("seasonality_plot_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(4,
                                 helpText("Welcome to the tutorial. Let the games begin.")),
                          
                          column(2,
                                 h4("Select Shape"),
                                 radioButtons(inputId = "pattern_tutorial",
                                              label = "Shape",
                                              choices = c("sin", "sin1", "block"))),
                          
                          column(2,
                                 numericInput(inputId = "Nyr_tutorial",
                                              label = "Number of Years",
                                              value = 5,
                                              min = 1, max = NA, step = 1)))
                        ),
               
               tabPanel("VCtime",
                        plotOutput("VCtime_plot_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(4,
                                 helpText("Banging your head against a wall for one hour burns 150 calories."))
                        )
                        ),
               
               tabPanel("Dtime",
                        plotOutput("Dtime_plot_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(4,
                                 helpText("In Switzerland it is illegal to own just one guinea pig."))
                        )
                        ),
               
               tabPanel("Rtime",
                        plotOutput("Rtime_plot_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(4,
                                 helpText("7% of American adults believe that chocolate milk comes from brown cows."))
                        )
                        ),
               
               tabPanel("Generations Graph",
                        plotOutput("generations_graph_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(4,
                                 helpText("Bananas are curved because they grow towards the sun."))
                        )
                        ),
               
               tabPanel("PlotEigen",
                        fluidRow(
                          column(3,
                                 helpText("Most toilets flush in E flat.")),
                          column(9,
                                 plotOutput("plot_eigen_tutorial",
                                            height = "1000px"))
                        )
                        ),
               
               tabPanel("Eigenvalues",
                        plotOutput("eigenvalues_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(4,
                                 helpText("All swans in England belong to the queen."))
                        )
                        ),
               
               tabPanel("Jawbreaker Plot",
                        plotOutput("jawbreaker_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(4,
                                 helpText("This will ruin your teeth."))
                        ))
               
               
             )),
    
    tabPanel("Interactive",
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
               
               
             ))
  )
  

)


server <- function(input, output) {
  output$seasonality_plot_tutorial <- renderPlot({
      makeVCrel(pattern = input$pattern_tutorial,
                Nyr = input$Nyr_tutorial,
                showit = TRUE)
    })
  
  output$VCtime_plot_tutorial <- renderPlot({
    makeVCtime(plot=TRUE)
  })

  output$Dtime_plot_tutorial <- renderPlot({
    makeDtime(plot=TRUE)
  })

  output$Rtime_plot_tutorial <- renderPlot({
    makeRtime(plot = TRUE)
  })

  output$generations_graph_tutorial <- renderPlot({
    generations_graph(Re = 3,
                      num_gen = 40,
                      Rtime = makeRtime(Re = 3, plot = FALSE),
                      VCrel = makeVCrel(pattern = "sin",
                                        Nyr = 10,
                                        showit = TRUE))
  })

  output$plot_eigen_tutorial <- renderPlot({
    plotEigen(NGEN = 15,
              Rtime = makeRtime(Re = 3, plot = FALSE),
              VCrel = makeVCrel(pattern = "sin",
                                Nyr = 10,
                                showit = TRUE))
  })

  output$eigenvalues_tutorial <- renderPlot({
    plot(plotEigen(NGEN = 15,
                   Rtime = makeRtime(Re = 3, plot = FALSE),
                   VCrel = makeVCrel(pattern = "sin",
                                     Nyr = 10,
                                     showit = TRUE)),
         type = "l",
         ylab = "Eigenvalue")
  })
  
  output$jawbreaker_tutorial <- renderPlot({
    VCrel = makeVCrel(pattern="sin", Nyr=10)
    this_Rtime = makeRtime(Re = 3)
    this_Rtime = 2.6*this_Rtime/sum(this_Rtime)
    genN = c(0,1, rep(0,3648))
    gen = nextGenSeasMat(gen = c(0,1, rep(0,3648)),
                         Rtime = this_Rtime,
                         VCrel = makeVCrel(pattern = "sin", Nyr = 10),
                         Nyr = 10,
                         NN = 48, showit=TRUE)
  })
  
  # output$seasonality_plot <- renderPlot({
  #   if(input$pattern != "Upload Seasonal Signal File") {
  #     makeVCrel(pattern = input$pattern, Nyr = input$Nyr, showit=TRUE)
  #   }
  #   if(input$pattern == "Upload Seasonal Signal File") {
  #     # create function to sparse file input and create seasonal signal
  #     user_pattern <- read.csv(input$pattern_file$datapath)
  #     makeVCrel(pattern = user_pattern[,1],
  #               Nyr = input$Nyr,
  #               showit = TRUE)
  #   }
  # })
  # 
  # output$VCtime_plot <- renderPlot({
  #   makeVCtime(EIP = input$EIP, p = input$p, a = input$a, tol = input$tol, plot=TRUE)
  # })
  # 
  # output$Dtime_plot <- renderPlot({
  #   makeDtime(D = input$D, plot=TRUE)
  # })
  # 
  # output$Rtime_plot <- renderPlot({
  #   makeRtime(input$Re, plot = TRUE)
  # })
  # 
  # output$generations_graph <- renderPlot({
  #   if(input$pattern != "Upload Seasonal Signal File") {
  #     this_VCrel <- makeVCrel(pattern = input$pattern, Nyr = input$Nyr, showit=TRUE)
  #   }
  #   if(input$pattern == "Upload Seasonal Signal File") {
  #     # create function to sparse file input and create seasonal signal
  #     user_pattern <- read.csv(input$pattern_file$datapath)
  #     this_VCrel <- makeVCrel(pattern = user_pattern[,1],
  #               Nyr = input$Nyr,
  #               showit = TRUE)
  #   }
  #   generations_graph(Re = input$Re,
  #                     num_gen = input$num_gen,
  #                     Rtime = makeRtime(input$Re, plot = FALSE),
  #                     VCrel = this_VCrel)
  # })
  # 
  # output$plot_eigen <- renderPlot({
  #   if(input$pattern != "Upload Seasonal Signal File") {
  #     this_VCrel <- makeVCrel(pattern = input$pattern, Nyr = input$Nyr, showit=TRUE)
  #   }
  #   if(input$pattern == "Upload Seasonal Signal File") {
  #     # create function to sparse file input and create seasonal signal
  #     user_pattern <- read.csv(input$pattern_file$datapath)
  #     this_VCrel <- makeVCrel(pattern = user_pattern[,1],
  #                             Nyr = input$Nyr,
  #                             showit = TRUE)
  #   }
  #   plotEigen(NGEN = input$NGEN,
  #             Rtime = makeRtime(input$Re, plot = FALSE),
  #             VCrel = this_VCrel)
  # })
  # 
  # output$eigenvalues <- renderPlot({
  #   if(input$pattern != "Upload Seasonal Signal File") {
  #     this_VCrel <- makeVCrel(pattern = input$pattern, Nyr = input$Nyr, showit=TRUE)
  #   }
  #   if(input$pattern == "Upload Seasonal Signal File") {
  #     # create function to sparse file input and create seasonal signal
  #     user_pattern <- read.csv(input$pattern_file$datapath)
  #     this_VCrel <- makeVCrel(pattern = user_pattern[,1],
  #                             Nyr = input$Nyr,
  #                             showit = TRUE)
  #   }
  #   plot(plotEigen(NGEN = input$NGEN,
  #                  Rtime = makeRtime(input$Re, plot = FALSE),
  #                  VCrel = this_VCrel),
  #        type = "l",
  #        ylab = "Eigenvalue")
  # })
  # 
  # output$gen_message <- renderText({
  #   paste0("Number of Generations = ", input$NGEN)
  # })
  
}


shinyApp(ui = ui, server = server)