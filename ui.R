library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bienenstand Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      
      dateRangeInput("daterange", 
                      "Date range:",
                      start  = "2018/01/01",
                      end    = "2019/07/01",
                      min    = "2016/01/01",
                      max    = "2024/12/31",
                      format = "yy/mm/dd",
                      separator = " bis "),
       
      
      downloadButton('downloadData', 'Download Data'),
      br(),
      # adding the new div tag to the sidebar            
      tags$div(
        br(),
        tags$p("Javan Rasokat, Daniel Böhm (76477) - Master ISM")),
               
      img(src="bee_small.jpg")
      
    ),
    mainPanel(tabsetPanel(
      tabPanel("Data", dataTableOutput("tabelle")),
      tabPanel("Histogramm Gewicht", plotOutput("distPlot")),
      tabPanel("Summary", dataTableOutput("summary")),
      tabPanel("Corr", textOutput("cor")),
      tabPanel("Exmaple", plotOutput("firsttry")),
      tabPanel("About", uiOutput("about"))
    ))
    
    
    
  )
))
