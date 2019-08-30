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
library(DT)

########################################
# Wichtige UI Komponenen
########################################
oldbody <- tabsetPanel(id = "inTabset",
             #tabPanel("Messdatentabelle", icon = icon("table"), dataTableOutput("tabelle")),
             tabPanel("Wertebereich bestimmen", icon = icon("database"), uiOutput("summaryUI")),
             tabPanel("Histogramm", icon = icon("chart-bar"), uiOutput("histogramUI")),
             tabPanel("Korrelation", plotOutput("cor")),
             tabPanel("Beispiel Daniel", plotOutput("firsttry")),
             tabPanel("Jahresverlauf Gewicht (Boxplots)", plotlyOutput("monthlyBoxplot"), p("Die Ausreißer im Jun 2018, April 2019, Mai 2019 und Juni 2019 in den niedrigen Gewichtsbereich zeigen das Honigernten.")),
             tabPanel("Tagesverlauf Gewicht (Boxplots)", uiOutput("dailyBoxplotUI")),
             tabPanel("Zeitstrahl", uiOutput("zeitstrahlUI")),
             tabPanel("Gewichtsanalyse", uiOutput("gewichtsDeltasUI")),
             tabPanel("Zeitreihenanalyse und Prognose", uiOutput("zeitreihenanalyseUI"))
             #tabPanel("About", uiOutput("about"))
)
examplebody <- fluidRow(
  column(width = 4,
         box(
           title = "Box title", width = NULL, status = "primary",
           "Box content"
         ),
         box(
           title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
           "Box content"
         ),
         box(
           width = NULL, background = "black",
           "A box with a solid black background"
         )
  ),
  column(width = 4,
         box(
           status = "warning", width = NULL,
           "Box content"
         ),
         box(
           title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
           "Box content"
         ),
         box(
           title = "Title 5", width = NULL, background = "light-blue",
           "A box with a solid light-blue background"
         )
  ),
  column(width = 4,
         box(
           title = "Title 2", width = NULL, solidHeader = TRUE,
           "Box content"
         ),
         box(
           title = "Title 6", width = NULL, background = "maroon",
           "A box with a solid maroon background"
         )
  )
)

upload <- tags$div(useShinyjs(),
                   #actionButton(inputId = "toggleButton", label = "Fileupload ein/ausblenden", icon = icon("caret-square-up")),
                   #box(id = "myBox", width = "400px",
                       #p(h4("Eigenes File hochladen")),
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
                   )
download <- downloadButton('downloadData', 'Daten herunterladen')
controlls <- tags$div(
        # DateRange
        dateRangeInput("daterange", 
                       "Zeitspanne auswählen:",
                       start  = "2018-01-01",
                       end    = "2019-07-01",
                       min    = "2016-01-01",
                       max    = "2024-12-31",
                       format = "yy-mm-dd",
                       separator = " bis "),
        # Checkbox
        checkboxInput("filterhours", "Auf stündliche Werte begrenzen? (Speed-Up)", FALSE),
        verbatimTextOutput("filterhours"),
        actionButton(inputId = "resetFilter", label = "Filter zurücksetzen", icon = icon("refresh"))
        
        
)
url_javan <- tags$a("Javan Rasokat", href = "https://javan.de")
url_twitter <- "https://twitter.com/intent/tweet?text=%23HoneyPi &url=https://www.honey-pi.de"
url_repo <- "https://github.com/JavanXD/shiny-beehive"
url_shiny <- "https://honeypi.shinyapps.io/shiny-beehive/"
info <- tags$div(
          tags$hr(style="color:#a1afb6;background-color:#a1afb6;border-color:#a1afb6"),
          # Div mit Copyrights         
          tags$div(style="padding:10px;color:#a1afb6",
            HTML(paste("Die HoneyPi Shiny App dient zur Visualisierung von CSV-Dateien von Bienenstand-Monitoring-Systemen. Der Quellcode ist OpenSource auf Github unter der MIT Lizenz verfügbar. Die App wurde als Hochschulprojekt mit Hilfe von RStudio und Shiny von ", url_javan, " und Daniel Böhm entwickelt."))
            #tags$p("Javan Rasokat (79133)"),
            #tags$p("Daniel Böhm (76477)"),       
            #img(src="bee_small.jpg", width="200px"),
            )
)
about <- box(
            title = "Beehive Shiny App", width = NULL, solidHeader = TRUE, status = "primary",
            tags$h3("Bescheibung der Felder"),
            "hum1 - Luftfeuchte, hum2, weight, temp1, temp2",
            tags$h3("Notwendige Felder"),
            "hum1, hum2, weight, temp1, temp2"
          )

########################################
# Neues Shiny Dashboard Layout
########################################
body <- dashboardBody(tabItems(
  tabItem(tabName = "info",
          fluidRow(
            column(12,
               #uiOutput("about")
               about
            )
          )
  ),
  tabItem(tabName = "dashboard",
    oldbody
  ),
  tabItem(tabName = "widgets",
    examplebody
  ),
  tabItem(tabName = "rawdata",
          fluidRow(
            column(4,
                   box(
                     title = "Upload", width = NULL, solidHeader = TRUE, status = "warning",
                     upload
                   )
             ), 
            column(4,
                   box(
                     title = "Eingrenzen", width = NULL, solidHeader = TRUE, status = "warning",
                     controlls
                   )
             ), 
            column(4,
                   box(
                     title = "Download", width = NULL, solidHeader = TRUE, status = "warning",
                     download
                   )
            ),
            column(12,
                 box(
                   title = "Messdatentabelle", width = NULL, solidHeader = TRUE, status = "warning",
                   dataTableOutput('tabelle')
                 )
            ),
            column(12,
                   box(
                     title = "Hinweis", width = NULL, solidHeader = TRUE, status = "primary",
                     "Bitte zuerst die Datei hier hochladen, bevor die Graphen aufgerufen werden."
                   )
            )
           )
)))

header <- dashboardHeader(title = "HoneyPi Shiny App")
sidebar <- dashboardSidebar(collapsed = FALSE, 
                            sidebarMenu(id = "tabs",
                              menuItem("Info", tabName = "info", icon = icon("info")),
                              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                              menuItem("Widgets", tabName = "widgets", icon = icon("th")),
                              menuItem("Messdaten", tabName = "rawdata", icon = icon("table"))
                            ), 
                            #controlls, 
                            info
)
footer <- tags$footer(tags$div("", 
                               actionButton("twitter_share",
                                            label = "Share",
                                            icon = icon("twitter"),
                                            onclick = sprintf("window.open('%s')", url_twitter)),
                               actionButton("github_share",
                                            label = "GitHub",
                                            icon = icon("github"),
                                            onclick = sprintf("window.open('%s')", url_repo)),
                               actionButton("shiny_share",
                                            label = "Shiny",
                                            icon = icon("cloud"),
                                            onclick = sprintf("window.open('%s')", url_shiny))), 
                      align = "center", 
                      style = "
    position: relative; 
    text-align:right;
    width: 100%;
    bottom: 0;
    color: white;
    background-color: #222d32;
    height:50px;   /* Height of the footer */
    color: white;
    padding: 10px;
    z-index: 1000;")
ui <- tagList(dashboardPage(skin = "yellow", title = "Beehive CSV Viewer",
  header,
  sidebar,
  body
  ),
  footer
)
########################################
# Altes Shiny Layout [einfach unten shinyUI(oldui) ausführen]
########################################
oldui <- navbarPage("Bienenstand Shiny App",
           tabPanel("Start",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    upload,
                    controlls, 
                    info
                  ),
                  mainPanel(oldbody)
                )
              )
            )
)

########################################
# Main
########################################               
shinyUI(ui)