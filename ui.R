library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(readr)
library(corrplot)
library(dplyr)
library(lubridate)
library(plotly)
library(forecast)

shinyUI(
  # Application title
  navbarPage("Bienenstand Shiny App",
    tabPanel("Start",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            useShinyjs(),
            actionButton(inputId = "toggleButton", label = "Fileupload ein/ausblenden", icon = icon("caret-square-up")),
            box(id = "myBox", width = '800px',
              p(h4("Eigenes File hochladen")),
              fileInput("fileuploadFile", "CSV-Datei auswählen:",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv"))
              # # Input: Checkbox if file has header ----
              # checkboxInput("fileuploadHeader", "Header vorhanden?", TRUE),
              # # Input: Select separator ----
              # radioButtons("fileuploadSep", "Separator auswählen:",
              #              choices = c(Semicolon = ";",
              #                          Comma = ",",
              #                          Tab = "\t"),
              #              selected = ",")
            ),
            # Horizontal line ----
            tags$hr(),
            p(h4("Daten eingrenzen")),
            # DateRange
            dateRangeInput("daterange", 
                            "Zeitspanne auswählen:",
                            start  = "2018-01-01",
                            end    = "2019-07-01",
                            min    = "2016-01-01",
                            max    = "2024-12-31",
                            format = "yy-mm-dd",
                            separator = " bis "),
            actionButton(inputId = "resetFilter", label = "Filter zurücksetzen", icon = icon("refresh")),
            # Checkbox
            checkboxInput("filterhours", "Auf stündliche Werte begrenzen? (Speed-Up)", FALSE),
            verbatimTextOutput("filterhours"),
            # Horizontal line ----
            tags$hr(),
            downloadButton('downloadData', 'Daten herunterladen'),
            # Horizontal line ----
            tags$hr(),
            # Div mit Copyrights         
            tags$div(
              tags$p("Javan Rasokat (79133)"),
              tags$p("Daniel Böhm (76477)"),       
              img(src="bee_small.jpg", width="200px"))
            
          ),
          mainPanel(tabsetPanel(id = "inTabset",
            tabPanel("Messdatentabelle", icon = icon("table"), dataTableOutput("tabelle")),
            tabPanel("Wertebereich bestimmen", icon = icon("database"), uiOutput("summaryUI")),
            tabPanel("Histogramm", icon = icon("chart-bar"), uiOutput("histogramUI")),
            tabPanel("Korrelation", plotOutput("cor")),
            tabPanel("Beispiel Daniel", plotOutput("firsttry")),
            tabPanel("Jahresverlauf Gewicht (Boxplots)", plotlyOutput("monthlyBoxplot"), p("Die Ausreißer im Jun 2018, April 2019, Mai 2019 und Juni 2019 in den niedrigen Gewichtsbereich zeigen das Honigernten.")),
            tabPanel("Tagesverlauf Gewicht (Boxplots)", uiOutput("dailyBoxplotUI")),
            tabPanel("Zeitstrahl", uiOutput("zeitstrahlUI")),
            tabPanel("Gewichtsanalyse", uiOutput("gewichtsDeltasUI")),
            tabPanel("Zeitreihenanalyse und Prognose", uiOutput("zeitreihenanalyseUI")),
            tabPanel("About", uiOutput("about"))
          ))
        )
      )
    )
  )
)
