# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(ggplot2)
# Read CSV into R
##dsim <- read.csv(file="data/alldata2.csv", header=TRUE, sep=",")
dsim <- read.csv("data/alldata2.csv")
head(dsim)

ui <- fluidPage(
  headerPanel('Uploading csv files'),
  # Sidebar panel for inputs ----
  sidebarPanel(
    
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
  mainPanel(
    # Output: Data file ----
    tableOutput("contents")
  ),
  
  headerPanel('Using k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(dsim)),
    selectInput('ycol', 'Y Variable', names(dsim),
      selected = names(dsim)[[12]]),
    numericInput('clusters', 'Cluster count', 3,
      min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1',
    click = "plot_click",
    dblclick = "plot_dblclick",
    hover = "plot_hover",
    brush = "plot_brush"
  ),
  verbatimTextOutput("info"),
  plotOutput('plot2')
  )
)

server <- function(input, output) {

  # Read CSV into R
  ##dsim <- read.csv(file="data/alldata2.csv", header=TRUE, sep=",")
  #dsim <- read.csv("data/alldata2.csv")
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
  
  selectedData <- reactive({
    dsim[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    text(selectedData(), pos=1, labels = dsim$score)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
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
  
  ## placeholder for linear regression plot
  output$plot2 <- renderPlot({
    # Separate regressions of mpg on weight for each number of cylinders
    qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"),
          method="lm", formula=y~x, color=cyl,
          main="Regression of MPG on Weight",
          xlab="Weight", ylab="Miles per Gallon")
  })

}

shinyApp(ui = ui, server = server)
