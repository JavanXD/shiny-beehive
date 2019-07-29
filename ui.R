library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bienenstand Shiny App"),

  sidebarLayout(
    sidebarPanel(
       sliderInput("x_min",
                   "Smallest Number in graphic",
                   min = 0,
                   max = 50,
                   value = 5),
       sliderInput("x_max",
                   "Largest Number in graphic",
                   min = 0,
                   max = 50,
                   value = 45),
       sliderInput("num_cyl",
                   "Choose Number of Cylinders",
                   min=4,
                   max=8,
                   step=2,
                   value=c(4,8)),
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
