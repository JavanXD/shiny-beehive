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
       
      downloadButton('downloadData', 'Download'),
      
      
      # adding the new div tag to the sidebar            
      tags$div(tags$p("Los geht's mit der Bienen Shiny AppLorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, 
                      no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet 
                      clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet")),
               
      img(src="bee_small.jpg")
      
    ),
    mainPanel(tabsetPanel(
      tabPanel("Tabelle der Daten", dataTableOutput("tabelle")),
      tabPanel("Histogramm Gewicht", plotOutput("distPlot")),
      tabPanel("Summary", dataTableOutput("summary")),
      tabPanel("Cor", textOutput("cor"))
    ))
    
    
    
  )
))
