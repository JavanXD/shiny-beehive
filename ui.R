library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bienenstand Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      p(h4("Eigenes File hochladen")),
      fileInput("fileuploadFile", "CSV-Datei auswählen:",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Input: Checkbox if file has header ----
      checkboxInput("fileuploadHeader", "Header vorhanden?", TRUE),
      # Input: Select separator ----
      radioButtons("fileuploadSep", "Separator auswählen:",
                   choices = c(Semicolon = ";",
                               Comma = ",",
                               Tab = "\t"),
                   selected = ","),
      # Horizontal line ----
      tags$hr(),
      p(h4("Daten eingrenzen")),
      # DateRange
      dateRangeInput("daterange", 
                      "Zeitspanne auswählen:",
                      start  = "2018/01/01",
                      end    = "2019/07/01",
                      min    = "2016/01/01",
                      max    = "2024/12/31",
                      format = "yy/mm/dd",
                      separator = " bis "),
      # Checkbox
      checkboxInput("filterhours", "Auf stündliche Werte begrenzen? (Speed-Up)", FALSE),
      verbatimTextOutput("filterhours"),
      # Horizontal line ----
      tags$hr(),
      downloadButton('downloadData', 'Daten herunterladen'),
      # Horizontal line ----
      tags$hr(),
      # adding the new div tag to the sidebar            
      tags$div(
        tags$p("Javan Rasokat (79133)"),
        tags$p("Daniel Böhm (76477)"),       
        img(src="bee_small.jpg", width="200px"))
      
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
