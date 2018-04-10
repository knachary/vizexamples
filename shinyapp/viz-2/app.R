# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(ggplot2)
library(shinydashboard)

# Read CSV into R
#dsim <- read.csv(file="data/alldata2.csv", header=TRUE, sep=",")
dsim <- read.csv('data/alldata2.csv', stringsAsFactors = FALSE, header=TRUE)
#recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)
head(dsim)

ui <- dashboardPage(
  dashboardHeader(title="Multi-Robot Simulation Dashboard", titleWidth = 350),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Main", tabName = "main", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("bar-chart-o"))
    ),
    textOutput("res")
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "main",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              ),
              fluidRow(
                # Dynamic infoBoxes
                infoBoxOutput("progressBox")
              ),
              fluidRow(
                # Clicking this will increment the progress amount
                box(width = 4, actionButton("count", "Increment progress"))
              ),
              fluidRow(
                box (
                  # Input: Select a file ----
                  fileInput("file1", "Choose CSV File",
                          multiple = TRUE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                  # Horizontal line ----
                  tags$hr(),
                
                  # Input: Checkbox if file has header ----
                  checkboxInput("header", "Header", TRUE),
                
                  # Input: Select separator ----
                  radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                
                  # Input: Select quotes ----
                  radioButtons("quote", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"'),
                
                  # Horizontal line ----
                  tags$hr(),
                
                  # Input: Select number of rows to display ----
                  radioButtons("disp", "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head")
                  ),
                box(
                  tableOutput("contents")
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              box(
                selectInput('xcol', 'X Variable', names(dsim)),
                selectInput('ycol', 'Y Variable', names(dsim),
                            selected = names(dsim)[[12]]),
                numericInput('clusters', 'Cluster count', 3,
                             min = 1, max = 9)
              ),
              box(
                plotOutput('plot2',
                           click = "plot_click",
                           dblclick = "plot_dblclick",
                           hover = "plot_hover",
                           brush = "plot_brush"
                ),
                verbatimTextOutput("info")
              )
      )
    )
  )
)
  
  server <- function(input, output, session) {
    
    ## output table for file contents (Main)
    output$contents <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      
      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
      
    })
    
    ## output progress box increment (Main)
    output$progressBox <- renderInfoBox({
      infoBox(
        "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
        color = "purple"
      )
    })
    
    ## output text message for sidebar selection
    output$res <- renderText({
      paste("You've selected:", input$tabs)
    })
    
    ## output plot of histogram (Main)
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
      data <- histdata[seq_len(input$slider)]
      hist(data)
    })
    
    ## output plot for multi-robot simulation data (Widgets)
    ## data for k-means cluster - dsim
    selectedData <- reactive({
      dsim[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
      kmeans(selectedData(), input$clusters)
    })
    
    output$plot2 <- renderPlot({
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      text(selectedData(), pos=1, labels = dsim$score)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    ## output text info box for plot click, hover etc. (Widgets)
    output$info <- renderText({
      xy_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
      }
      xy_range_str <- function(e) {
        if(is.null(e)) return("NULL\n")
        paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
               " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
      }
      
      paste0(
        "click: ", xy_str(input$plot_click),
        "dblclick: ", xy_str(input$plot_dblclick),
        "hover: ", xy_str(input$plot_hover),
        "brush: ", xy_range_str(input$plot_brush)
      )
    })
  }
  
  shinyApp(ui = ui, server = server)