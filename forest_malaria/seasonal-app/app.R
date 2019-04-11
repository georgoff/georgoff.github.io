library(shiny)
library(RColorBrewer)

source("~/Desktop/georgoff.github.io/forest_malaria/seasonal_R/Seasonal-Reproductive-Numbers-Functions.R")

ui <- fluidPage(
  titlePanel("Seasonality of Reproductive Numbers"),
  
  tabsetPanel(
    tabPanel("Tutorial",
             tabsetPanel(
               tabPanel("Introduction",
                        
                        h2("Welcome to the Seasonality Simulator 5000"),
                        
                        hr(),
                        
                        h4("You're about to embark on the journey of a lifetime...")),
               
               tabPanel("Seasonality Signal",
                        # VCrel
                        plotOutput("seasonality_plot_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(8,
                                 h4("Defining the Seasonal Signal"),
                                 h6("First we must decide upon a seasonal signal for relative vectorial capacity.
                                    This signal defines how strong malaria transmission is during every part of a given year.
                                    There are a few example options available via the radio buttons to the right; feel free to select each one and examine how the signal changes.
                                    For this tutorial, we will be using the \"sin\" signal.")),
                          
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
               
               tabPanel("Temporal Vectorial Capacity",
                        # VCtime
                        plotOutput("VCtime_plot_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(8,
                                 h4("Temporal Dispersion of Vectorial Capacity"),
                                 h6("This graph depicts how vectorial capacity (VC) changes over time.
                                    After the Entomological Innoculation Period (EIP), which is 12 days in this example, VC starts at its highest value then decreases over time.
                                    This waiting period is due to the fact that the parasite takes time to mature in the host; therefore, the mosquito doesn't become infectious until after the EIP."))
                        )
                        ),
               
               tabPanel("Temporal Human Infectiousness",
                        # Dtime
                        plotOutput("Dtime_plot_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(8,
                                 h4("Temporal Dispersion of Human Infectiousness"),
                                 h6("Similar to vectorial capacity, there is a lag period for infectiousness in humans.
                                    For this tutorial, we assume a 17 day dormant period before the infected human becomes infectious to mosquitoes that bite them.
                                    Following this dormant period, the infectiousness curve follows a gamma distribution."))
                        )
                        ),
               
               tabPanel("Temporal Basic Reproductive Rate",
                        # Rtime
                        plotOutput("Rtime_plot_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(8,
                                 h4("Temporal Dispersion of Basic Reproductive Rate"),
                                 h6("By combining the temporal effects of vectorial capacity and human infectiousness, we arrive at the temporal dispersion of the basic reproductive rate.
                                    This distribution tells us how strong the transmission capacity is in our system at a given time."))
                        )
                        ),
               
               tabPanel("Generations Graph",
                        plotOutput("generations_graph_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(8,
                                 h4("Next-Generation Graph"),
                                 h6("Gonna have Dave write this part, don't know what's going on here to be honest..."))
                        )
                        ),
               
               tabPanel("Long-Term Generations Graph",
                        # PlotEigen
                        fluidRow(
                          column(3,
                                 h4("Next-Generation Graphs Over Time"),
                                 h6("Also gonna have Dave write this...")),
                          column(9,
                                 plotOutput("plot_eigen_tutorial",
                                            height = "1000px"))
                        )
                        ),
               
               tabPanel("Eigenvalues",
                        plotOutput("eigenvalues_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(8,
                                 h4("Eigenvalues Over Time"),
                                 h6("This is all Dave."))
                        )
                        ),
               
               tabPanel("Jawbreaker Plot",
                        plotOutput("jawbreaker_tutorial"),
                        
                        hr(),
                        
                        fluidRow(
                          column(8,
                                 h4("Couldn't even begin to tell you what's going on here"),
                                 h6("It looks pretty though!"))
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
    # VCrel = makeVCrel(pattern="sin", Nyr=10)
    this_Rtime = makeRtime(Re = 2)
    this_Rtime = 2.6*this_Rtime/sum(this_Rtime)
    genN = c(0,1, rep(0,3648))
    # gen = nextGenSeasMat(genN, Rtime, VCrel, Nyr = 10, 48, showit=TRUE)
    gen = nextGenSeasMat(genN,
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