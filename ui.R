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
                      separator = " bis ")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Tabelle der Daten", dataTableOutput("tabelle")),
      tabPanel("Histogramm Gewicht", plotOutput("distPlot")),
      tabPanel("Summary", dataTableOutput("summary")),
      tabPanel("Cor", textOutput("cor"))
    ))
  )
))
