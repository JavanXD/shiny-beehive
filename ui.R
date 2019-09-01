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
dashboard <- tabBox(title = NULL, width = 12, 
             tabPanel("Wertebereich bestimmen", icon = icon("database"), uiOutput("summaryUI")),
             tabPanel("Merkmal Visualisierung", icon = icon("chart-bar"), uiOutput("histogramUI")),
             tabPanel("Korrelation", icon = icon("project-diagram"), plotOutput("cor")),
             tabPanel("Beispiel Daniel", plotOutput("firsttry")),
             tabPanel("Jahresverlauf Gewicht (Boxplots)", icon = icon("calendar"), plotlyOutput("monthlyBoxplot"), p("Die Ausreißer im Jun 2018, April 2019, Mai 2019 und Juni 2019 in den niedrigen Gewichtsbereich zeigen das Honigernten.")),
             tabPanel("Tagesverlauf Gewicht (Boxplots)", icon = icon("calendar"), uiOutput("dailyBoxplotUI")),
             tabPanel("Zeitstrahl", icon = icon("chart-line"), uiOutput("zeitstrahlUI")),
             tabPanel("Gewichtsanalyse", icon = icon("balance-scale"), uiOutput("gewichtsDeltasUI")),
             tabPanel("Zeitreihenanalyse und Prognose", icon = icon("eye"), uiOutput("zeitreihenanalyseUI"))
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
url_honeypi <- "https://www.honey-pi.de"
url_twitter <- "https://twitter.com/intent/tweet?text=%23HoneyPi&url=https://www.honey-pi.de"
url_repo <- "https://github.com/JavanXD/shiny-beehive"
url_shiny <- "https://honeypi.shinyapps.io/shiny-beehive/"
info <- tags$div(
          tags$hr(style="color:#a1afb6;background-color:#a1afb6;border-color:#a1afb6"),
          # Div mit Copyrights         
          tags$div(style="padding:10px;color:#a1afb6",
            HTML(paste("Die HoneyPi Shiny App dient zur Visualisierung von Messdaten von Bienenstand-Monitoring-Systemen. Der Quellcode ist OpenSource auf GitHub unter der MIT Lizenz verfügbar. Die App wurde als Hochschulprojekt mit Hilfe von RStudio und Shiny von Daniel Böhm und ", url_javan, " entwickelt."))
            #tags$p("Javan Rasokat (79133)"),
            #tags$p("Daniel Böhm (76477)"),       
            #img(src="bee_small.jpg", width="200px"),
            )
)
start <- fluidRow(
  column(4,
         box(
           title = "Datenerhebung", width = NULL, solidHeader = FALSE, status = "warning",
           "Die bereits vorliegenden Messdaten stammen von einem Bienenstand-Monitoring-System. Dabei messen Sensoren im und am Bienenstand Umwelteinflüsse, wie die Temperatur, die Luftfeuchtigkeit und das Gewicht des Bienenstands. Dieses Monitoring System ist modular, sodass weitere Sensorenarten wie Luftdruck, Regenmenge und Luftqualität ergänzt werden können. Eine weitere Ergänzung wäre beispielsweise das hinzufügen weiterer Temperatursensoren innerhalb jeder Wabengasse um die Bewegung der Wintertraube im Bienenstand zu erfassen. Die Messungen finden kontinuierlich, beispielsweise in einem Zyklus von alle 30 Minuten statt."
         )
  ),
  column(4,
         box(
           title = "Statistische Einheit und Merkmale", width = NULL, solidHeader = FALSE, status = "warning",
           HTML(
"<strong>Beschreibung der Merkmale</strong>
<ul>
<li><strong>timestamp</strong>: <i>nicht häufbar, diskret, nicht-klassiert, quantitaves Merkmal [Zeitstempel] </i> <br>- Uhrzeit zum Zeitpunkt der Messung</li>
<li><strong>weight</strong>: <i>metrisch, stetig, quantiatives Merkmal [kg] </i> <br>- gemessenes Gewicht des Bienenstandes</li>
<li><strong>temp_hive</strong>: <i>metrisch, stetig, quantiatives Merkmal [°C] </i> <br>- gemessene Stocktemperatur im Bienenstock</li>
<li><strong>temp_out</strong>: <i>metrisch, stetig, quantiatives Merkmal [°C] </i> <br>- gemessene Außentemperatur am Bienenstand</li>
<li><strong>hum_hive</strong>: <i>metrisch, stetig, quantiatives Merkmal [%] </i> <br>- gemessene Luftfeuchtigkeit im Bienenstock</li>
<li><strong>hum_out</strong>: <i>metrisch, stetig, quantiatives Merkmal [%] </i> <br>- gemessene Luftfeuchtigkeit am Bienenstande</li>
<li><strong>delta_weight</strong>: <i>metrisch, quantiatives Merkmal [g] </i> <br>- Gewichtsdifferenz zum vorangegangenen Messergebnis</li>
</ul>"
           )
         )
  ),
  column(4,
         box(
           title = "Bemerkung", width = NULL, solidHeader = FALSE, status = "warning",
           "Der Quellcode der Shiny App ist bisher noch relativ statisch, sodass der importierte Datensatz in einem ganz bestimmten Format vorliegen muss. Daher müssen auch die Header-Felder der CSV-Datei noch mindestens die beschriebenen Merkmale enthalten."
         )
  ),
  column(4,
         box(
           title = "Changelog", width = NULL, solidHeader = FALSE, status = "warning",
           HTML(
"
<ul>
<li><strong>2019/09/09</strong> Erste MVP Version</li>
</ul>")
         )
  ),
  column(4,
         box(
           title = "To-Do's (Roadmap)", width = NULL, solidHeader = FALSE, status = "warning",
           HTML(
             '
<ul>
<li>Das Unterstützen weiterer Uploadmöglichkeiten der Messdaten. Dadurch kann eine Livevisualisierung ermöglicht werden.
  <ul>
      <li><a href="https://www.honey-pi.de/messdaten-visualisieren-und-auswerten/">HoneyPi und ThingSpeak Anbindung</a></li>
      <li>Online CSV Import per URL (ähnlich wie in der <a href="https://www.honey-pi.de/honeypi-ios-app/">PWA</a>)</li>
  </ul>
</li>
</ul>')
         )
  )
)

########################################
# Neues Shiny Dashboard Layout
########################################
body <- dashboardBody(tabItems(
  tabItem(tabName = "start",
    start
  ),
  tabItem(tabName = "dashboard",
          fluidRow( 
            dashboard
          )
  ),
  tabItem(tabName = "zeitreihe",
          fluidRow(
            column(12,
                   box(
                     title = "Zeitreihe", width = NULL, solidHeader = FALSE, status = "warning",
                     uiOutput("zeitreiheUI")
                   )
            )
          )
  ),
  tabItem(tabName = "rawdata",
          fluidRow(
            column(4,
                   box(
                     title = "Upload", width = NULL, solidHeader = FALSE, status = "warning",
                     upload
                   )
             ), 
            column(4,
                   box(
                     title = "Eingrenzen", width = NULL, solidHeader = FALSE, status = "warning",
                     controlls
                   )
             ), 
            column(4,
                   box(
                     title = "Download", width = NULL, solidHeader = FALSE, status = "warning",
                     download
                   )
            ),
            column(12,
                 box(
                   title = "Messdatentabelle", width = NULL, solidHeader = FALSE, status = "warning",
                   dataTableOutput('tabelle')
                 )
            ),
            column(12,
                   box(
                     title = "Hinweis", width = NULL, solidHeader = FALSE, status = "primary",
                     "Bitte zuerst die Datei hier hochladen, bevor die Graphen aufgerufen werden."
                   )
            )
           )
)))
logo <- tags$li(a(href = 'https://www.honey-pi.de',
                tags$img(src='honeypi-brand.svg', height="20px"),
                title = "Open HoneyPi.de"),
                class = "dropdown",
                style = "margin-right:15px")
header <- dashboardHeader(title = "HoneyPi Shiny App", logo)
sidebar <- dashboardSidebar(collapsed = FALSE, 
                            sidebarMenu(id = "tabs",
                              menuItem("Startseite", tabName = "start", icon = icon("info-circle")),
                              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                              menuItem("Zeitreihe", tabName = "zeitreihe", icon = icon("chart-line")),
                              menuItem("Messdaten", tabName = "rawdata", icon = icon("table"))
                            ), 
                            #controlls, 
                            info
)
footer <- tags$footer(tags$div("", 
                               actionButton("honeypi_share",
                                            label = "HoneyPi",
                                            icon = icon("globe"),
                                            onclick = sprintf("window.open('%s')", url_honeypi)),
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
    height:55px;
    color: white;
    padding: 10px;
    z-index: 1000;")
ui <- tagList(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  dashboardPage(skin = "yellow", title = "HoneyPi Shiny App",
  header,
  sidebar,
  body
  ),
  footer
)

########################################
# Main
########################################               
shinyUI(ui)