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
library(pryr)
library(ggplotify)

# Highchart
library(highcharter)
library(xts)

server <- function(input, output, session) {
  
  ########################################
  # Daten aufbereiten 
  ########################################
  format <- "%Y/%m/%d %H:%M:%S"
  # import csv and skip unnecessary columns, set timeformat
  beehive_df <- reactiveValues()
  beehive_df <- read_csv("beehive.csv", col_types = cols(ticks = col_number(), 
                                                      timestamp = col_datetime(format = format), 
                                                      x5 = col_skip(),
                                                      x7 = col_skip(),
                                                      x8 = col_skip()))
  beehive_df_unfiltered <- beehive_df
  
  treat_df <- function(df) {
    # TODO: Zeile in Produktion entfernen. 
    df <- subset(df, weight != 100) # Demo: Im Datensatz sind viele Test/Kalibrierungsmessungen mit 100kg. Die müssen rausgefiltert werden.
    df <- subset(df, !is.na(hum2))
    df <- subset(df, !is.na(weight))
    
    # berechne differenzen zwischen reihen
    delta_weight <- diff(df$weight[order(df$ticks)])
    delta_temp1 <- diff(df$temp1[order(df$ticks)])
    delta_temp2 <- diff(df$temp2[order(df$ticks)])
    delta_hum1 <- diff(df$hum1[order(df$ticks)])
    delta_hum2 <- diff(df$hum2[order(df$ticks)])
    # delete first row
    df <- df[-1,] 
    df$delta_weight <- round(delta_weight, digits=4)
    df$delta_temp1 <- round(delta_temp1, digits=4)
    df$delta_temp2 <- round(delta_temp2, digits=4)
    df$delta_hum1 <- round(delta_hum1, digits=4)
    df$delta_hum2 <- round(delta_hum2, digits=4)
    
    return(df)
  }
  
  obsDf <- observe({
      beehive_df <- treat_df(beehive_df)
    }, quoted = TRUE)
  
  ########################################
  # Skripte Testen
  ########################################
  # Gruppiere Daten nach Tag und füge Min Max pro Tag hinzu ----
  beehive_df_daily <- beehive_df %>%
    mutate(Date = ymd_hms(timestamp), dt = as_date(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
    group_by(dt, month, year) %>% 
    summarise(temp1_max = max(temp1), temp1_min = min(temp1), weight_mean = mean(weight))
  
  # Gruppiere nach Monate für Boxplot
  beehive_df_monthly <- beehive_df %>%
    mutate(Date = ymd_hms(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
    group_by(month, year) %>% 
    summarise(weight_max = max(weight), weight_min = min(weight), weight_mean = mean(weight), weight = sum(weight))
  #View(beehive_df)

  ########################################
  # Graphen zeichnen
  ########################################
  # Daniel erster Versuch. Zusammenhang nicht gut
  firsttry = ggplot(beehive_df, aes(x = temp1, y = delta_weight)) + geom_point() + geom_smooth(method='lm') +
    labs(title = "Vorlage GEOM Daniel", 
         subtitle = "Erster Versuch", 
         x = "Außentemperatur in Celsius", y = "Gewichtsdifferenz zum Vortag"
    )


  ########################################
  # Funktionen definieren, die für Graphen verwendet werden.
  ########################################
  plotCor <- reactive({
    # Korrelation berechnen
    correlation <- cor(beehive_df[,c("weight", "temp1", "temp2", "hum1", "hum2", "delta_weight", "delta_temp1", "delta_temp2", "delta_hum1", "delta_hum2")])
    # Korrelation visualisieren
    corrplot(correlation, method = "ellipse", type = "upper", tl.srt = 45)
  })
  
  change_dataframe <- reactive({
    # Read file ----
    if (!is.null(input$fileuploadFile)) {
      # Wenn ein File ausgewählt wurde, dann nehme das File
      # beehive_df <- read_csv(input$fileuploadFile$datapath,
      #                        header = input$fileuploadHeader,
      #                        sep = input$fileuploadSep)
      beehive_df_unfiltered <<- read_csv(input$fileuploadFile$datapath, col_types = cols(ticks = col_number(), 
                                                                             timestamp = col_datetime(format = format), 
                                                                             x5 = col_skip(),
                                                                             x7 = col_skip(),
                                                                             x8 = col_skip()))
      beehive_df <- beehive_df_unfiltered
                             
    } else {
      # Per default behalte den dataframe
      beehive_df <- beehive_df
    }
    
    # Filter Daterange ----
    date_start_date <- as.Date(input$daterange[1], origin = "1970-01-01")
    date_end_date <- as.Date(input$daterange[2], origin = "1970-01-01")
    beehive_df <- subset(beehive_df, timestamp >= date_start_date & timestamp <= date_end_date)
    
    # Filter Datenmengen ----
    if (input$filterhours == TRUE) {
      beehive_df <- beehive_df %>% 
               mutate(Date = ymd_hms(timestamp), dt = as_date(timestamp), hr = hour(timestamp)) %>% 
               group_by(dt, hr) %>% 
               filter(Date == min(Date)) %>% 
               ungroup()
    } else {
      beehive_df <- beehive_df
    }
    
    # change it global
    beehive_df <<- treat_df(beehive_df)
    
  })
  
  forecast_model <- function(){
    
    # Filter Zeitraum
    # Abhängig von Konfiguration Zeitraum eingrenzen
    date_end_date <- as.Date(input$selectedDayZeitreihenanlalyse)
    count_days = input$selectedCountZeitreihenanlalyse
    date_start_date <- ymd(date_end_date) - days(count_days)
    
    beehive_df <- subset(beehive_df, timestamp >= date_start_date & timestamp <= date_end_date)
    
    # Zeitreihenanalyse ist es hilfreich, den Datensatz in einer R-Variable abzuspeichern
    time_series <- beehive_df$weight
    
    # plot the raw data
    raw_plot <- as.ggplot(function()  plot(time_series) +
                            abline(reg=lm(time_series~time(time_series)))) # fit a trend line
    
    # Grafische Analyse der Zeitreihe
    TS <- ts(time_series, frequency = count_days)
    
    # In R gibt es eine Funktion, mit deren Hilfe man Zeitreihen in die drei Komponenten Trend, Saisonalität und zufällige Fluktuationen aufteilen kann:
    time_series_components <- decompose(TS)
    
    # Der Rückgabewert time_series_components dieser Funktion enthält eine Liste, welcher verschiedene Komponenten enthält. Ein Plot dieser Liste zeigt Folgendes:
    components_plot <- as.ggplot(function()  plot(time_series_components))
    
    # Fehler: Zeitreihe hat keine oder weniger als 2 Perioden
    # => zurück zum exponentiellen Glätten unter Verwendung der Holt-Winters-Funktion.
    # Die Idee, die hinter der exponentiellen Glättung steht, ist besonders für ökonomische Zeitreihen einsichtig: Ist es  sinnvoll, allen Beobachtungen der Zeitreihe das gleiche Gewicht einzuräumen, oder ist es sinnvoller jüngeren  Beobachtungen mehr Gewicht als älteren Bobachtungen einzuräumen? Wenn Sie für Ihre Zeitreihe diesem  Gedanken zustimmen können, ist die Methodik des exponentiellem Glättens wahrscheinlich die Richtige für Sie!
    time_series_vorhersage <- HoltWinters(time_series, alpha = 0.5, beta = 0.5, gamma = F)
    #plot(time_series_vorhersage)
    hw_plot <- as.ggplot(function() plot(time_series_vorhersage, main = "Holt-Winters-Glättung", sub = "Exponetielles Glätten: alpha = 0.5 beta = 0.5"))
    
    
    m <- stats::HoltWinters(time_series, alpha = 0.5, beta = 0.5, gamma = F)
    forecast_data <- forecast(m, h=14)
    forecast_plot <- as.ggplot(function()  plot(forecast_data))
    
    
    #  Für weitere EDA untersuchen wir Zyklen über Tage hinweg:
    #cycle(TS)
    
    # return all object as a list
    return(list(raw_plot,hw_plot,components_plot,forecast_plot))
  }
  
  ########################################
  # Bedienelemente definieren
  ########################################
  selectDay <- function (id="selectedDay", val="2019-06-7", text="Datum auswählen") { dateInput(id, text, value = val,
                         format = "yyyy-mm-dd", startview = "month", width = "100px") }
  
  selectDay2 <- selectDay(id="selectedDay2",val="2019-07-1")
  
  selectDaysCount <- function (id="selectedDaysCount", val=30, text="Anzahl Tage auswählen") {sliderInput(id, text, 1, 180, val, step = 1, round = FALSE,
                                 ticks = TRUE, animate = TRUE,
                                 width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL,
                                 timezone = NULL, dragRange = TRUE)}
  selectableFields <- c("weight", "temp1", "temp2", "hum1", "hum2", "delta_weight", "delta_temp1", "delta_hum1")
  selectField <- function (id="selectedField", val=NULL) {
                selectInput(id,label="Merkmal auswählen",choice=selectableFields, selected=val, selectize=FALSE) }
  
  ########################################
  # UI-Komponenten ausgeben
  ########################################
  output$tabelle <- renderDataTable({
    change_dataframe()
  })
  output$sum <- renderPrint({
    summary(beehive_df[input$selectedField])
  })
  output$box <- renderPlot({
    boxplot(beehive_df[input$selectedField],col="sky blue",border="purple", main=names(beehive_df[input$selectedField]))
  })
  output$summaryUI <- renderUI({
    tags$div(
      br(),
      selectField(),
      verbatimTextOutput("sum"),
      tags$h3("Boxplot"),
      tags$p("Ein Boxplot zeigt für jedes Merkmal Median, Quartilsabstand, Normalbereich und Ausreißer an."),
      plotOutput("box"),
      p("Mit Hilfe dieser Übersicht kann die Art des Sensors ausgemacht werden und ggf. die erste Header-Zeile in der CSV Datei an den Typ (weight, temp, hum etc.) angepasst werden. Außerdem kann die Anzahl der Fehlmessungen (NAs) für bestimmte Merkmale abgelesen werden."))
  })
  output$histogramPlot <- renderPlot({
    do_plot <- function (columnName) {
      ggplot(data=beehive_df, aes_string(columnName)) + 
      geom_histogram(aes(y =..density..), 
                     col="red", 
                     fill="green", 
                     alpha = .2) + 
      geom_density(col=2) + 
      labs(title=paste("Histogram for", toString(columnName))) +
      labs(x=toString(columnName), y="Count")
    }
    field <- input$selectedFieldSummary
    do_plot(field)
  })
  output$qqPlot <- renderPlot({
    field <- input$selectedFieldSummary
    y <- beehive_df[[field]]
    qqnorm(y)
    qqline(y) 
  })
  output$scatterPlot <- renderPlot({
    field <- input$selectedFieldSummary
    y <- beehive_df[[field]]
    x <- beehive_df$weight  
    plot(x, y,       
            ylab=paste("Ausgewähltes Merkmal: ", toString(field)),
            xlab="Gewicht [kg]")
  })
  output$histogramUI <- renderUI({
    tags$div(
      br(),
      selectField(id="selectedFieldSummary", val="temp1"),
      tags$h3("Histogramm"),
      tags$p("Das Histogramm macht die absolute oder relative Häufigkeitsverteilung und Häufigkeitsdichte sichtbar."),
      plotOutput("histogramPlot"),
      tags$h3("QQ-Plot"),
      tags$p("Das Q-Q-Diagramm (bzw. Q-Q-Plot) ist eine Grafik, mir der eine quantiative Variable auf das Vorliegen einer Normalverteilung überprüft werden kann."),
      plotOutput("qqPlot"),
      tags$h3("Scatter Plot"),
      tags$p("Das Scatterplot trägt zwei quantiative Merkmale im Koordinatensystem gegeneinander ab und lässt Zusammenhänge vermuten."),
      plotOutput("scatterPlot")
    )
  })
  output$cor <- renderPlot({
    plotCor()
  })
  output$firsttry <- renderPlot({
    firsttry
  })
  
  output$monthlyBoxplot <- renderPlotly({
    # add month to df
    beehive_df$month <- format(beehive_df$timestamp, "%B %y")
   
    # draw boxplot
    p <- plot_ly(beehive_df,
                 y = ~weight, 
                 x = reorder(format(beehive_df$timestamp,'%B %y'), beehive_df$timestamp),
                 color = ~month, 
                 type = "box",
                 boxpoints = "suspectedoutliers") %>% layout(yaxis = list(title = "Gewicht [kg]"))
    
    # create multiple boxplots (but "Overall" is not used at the moment)
    p1 <- p %>% add_boxplot(x = "Overall")
    p2 <- p %>% add_boxplot(x = reorder(format(beehive_df$timestamp,'%B %y'), beehive_df$timestamp))
    subplot(
      p2, shareY = TRUE,
      widths = c(1), margin = 0
    ) %>% hide_legend()
  })
  
  output$dailyBoxplot <- renderPlotly({
    
    # Abhängig von Konfiguration Zeitraum eingrenzen
    date_end_date <- as.Date(input$selectedDay)
    date_start_date <- ymd(date_end_date) - days(input$selectedDaysCount)
    
    beehive_df <- subset(beehive_df, timestamp >= date_start_date & timestamp <= date_end_date)
    
    # draw boxplot
    plot_ly(beehive_df,
             y = ~weight, 
             x = reorder(format(beehive_df$timestamp,'%d %B %y'), beehive_df$timestamp),
             type = "box",
             boxpoints = "suspectedoutliers") %>% layout(yaxis = list(title = "Gewicht [kg]")) %>% hide_legend()
  })
  
  output$dailyBoxplotUI <- renderUI({
    tags$div(
      br(),
      selectDay(),
      selectDaysCount(), 
      plotlyOutput("dailyBoxplot"), 
      p("Am 22. Mai muss der Imker arbeiten am Bienenvolk vorgenommen haben und den Honigraum heruntergenommen haben. Eventuell Schwarmkontrolle.")
    )
  })
  
  output$zeitstrahlPlotly <- renderPlotly({
    
    # Filter Zeitraum
    date_start_date <- as.Date(input$selectedDayZeitstrahl2)
    date_end_date <- as.Date(input$selectedDayZeitstrahl)
    beehive_df <- subset(beehive_df, timestamp >= date_start_date & timestamp <= date_end_date)
    
    # Draw Zeitstrahl
    plot_ly(beehive_df, x = ~timestamp, y = ~weight, name = 'Gewicht [kg]', type = 'scatter', mode = 'lines+markers') %>%
      add_trace(y = ~temp1, name = 'Temperatur Brutraum [°C]', mode = 'lines+markers') %>%
      add_trace(y = ~temp2, name = 'Temperatur Außen [°C]', mode = 'lines+markers') %>%
      add_trace(y = ~hum1, name = 'Luftfeuchte [%]', mode = 'lines+markers')  %>%
      filter(timestamp >= as.Date("2019-01-05")) %>% layout(xaxis = list(title = "Datum"), yaxis = list(title = ""))
  })
  
  output$zeitstrahlUI <- renderUI({
    tags$div(
      br(),
      selectDay(id="selectedDayZeitstrahl",text="Datum von:", val="2019-06-15"),
      selectDay(id="selectedDayZeitstrahl2",text="Datum bis:",val="2019-06-1"),
      plotlyOutput("zeitstrahlPlotly")
    )
  })
  
  output$gewichtsDeltas <- renderPlotly({
    
    # Abhängig von Konfiguration Zeitraum eingrenzen
    date_end_date1 <- as.Date(input$selectedDayGewichtsDeltas)
    date_start_date1 <- ymd(date_end_date1) - days(input$selectedDaysCount)
    date_end_date2 <- as.Date(input$selectedDayGewichtsDeltas2)
    date_start_date2 <- ymd(date_end_date2) - days(input$selectedDaysCount)
    
    beehive_df_weight1 <- subset(beehive_df, timestamp >= date_start_date1 & timestamp <= date_end_date2)
    beehive_df_weight2 <- subset(beehive_df, timestamp >= date_start_date2 & timestamp <= date_end_date2)
    
    
    # draw boxplot
    p <- plot_ly(alpha = 0.4) %>%
      add_histogram(x = ~beehive_df_weight1$delta_weight, name= "Zeitraum #1") %>%
      add_histogram(x = ~beehive_df_weight2$delta_weight, name=  "Zeitraum #2") %>%
      layout(barmode = "overlay") %>% layout(xaxis = list(title = ""), yaxis = list(title = "Anzahl"))
    
  })
  
  output$gewichtsDeltasUI <- renderUI({
    tags$div(
      br(),
      selectDay(id="selectedDayGewichtsDeltas", val="2019-06-1", text= "Datum für Zeitraum 1"),
      selectDay(id="selectedDayGewichtsDeltas2", val="2019-07-1", text= "Datum für Zeitraum 2"),
      selectDaysCount(val=14), 
      plotlyOutput("gewichtsDeltas"),
      p("Anhand der Häufigkeitsverteilung können Gewichtszu-  und Abnahmen als Links- und Rechtssteil oder Symmetrisch festgestellt werden. Außerdem ist ersichtlich wie ein Zeitraum gegenüber einem anderen Zeitraum performt (Honigertrag) hat.")
    )
  })

  output$plotgraph1 <- renderPlot({forecast_model()[1]})
  output$plotgraph2 <- renderPlot({forecast_model()[2]})
  output$plotgraph3 <- renderPlot({forecast_model()[3]})
  output$plotgraph4 <- renderPlot({forecast_model()[4]})

  output$zeitreihenanalyseUI <- renderUI({
    tags$div(
      br(),
      selectDay(id="selectedDayZeitreihenanlalyse", val="2019-04-01"),
      selectDaysCount(id="selectedCountZeitreihenanlalyse", val=4),
      p(""),
      plotOutput("plotgraph1"),
      plotOutput("plotgraph2"),
      plotOutput("plotgraph3"),
      plotOutput("plotgraph4")
    )
  })

  output$highchartPlot <- renderHighchart({
    
    #beehive_df[1] <- NULL # remove first clumn
    #beehive_df <- beehive_df[, c(7,1,2,3,4,5,6)] # reorder columns
    # create unix timestamp from date
    #beehive_df$Date <- as.POSIXct(strptime(beehive_df$timestamp,format),tz="UTC")

    # Test Dataframe
    df2 <- data_frame(
      #date = c(1460648544, 1460574864, 1460665314),
      date = c(as.Date("2018-04-05 08:00:45"), as.Date("2018-05-05 08:00:45"), as.Date("2018-06-05 08:00:45")),
      value = c(4, 4, 4.2),
      variable = c(1, 2, 3)
    )
    # convert unix to date
    #df2 <- mutate(df2, date=as.POSIXct(as.numeric(as.character(date)),origin="1970-01-01"))
    # convert data frame to ts
    #xts2 <- xts(df2[,-1], order.by=df2$date)
    xts <- xts(beehive_df[,-1], order.by=beehive_df$timestamp)

    # draw chart
    hc <- highchart(type = "stock") %>% 
      # create axis :)
      hc_yAxis_multiples(
        create_yaxis(1, height = c(2, 1), turnopposite = FALSE)
      ) %>% 
      # series :D
      hc_add_series(data=xts$temp1, yAxis = 0, name = "Temperatur Brutraum [°C]", type = "line") %>% 
      hc_add_series(data=xts$temp2, yAxis = 0, name = "Temperatur Außen [°C]") %>% 
      hc_add_series(data=xts$weight, yAxis = 0, name = "Gewicht [kg]") %>% 
      hc_add_series(data=xts$hum1, color = "gray", yAxis = 0, name = "Luftfeuchte Innen [%]", type = "line") %>% 
      hc_add_series(data=xts$hum2, yAxis = 0, name = "Luftfeuchte Außen [%]", color = hex_to_rgba("green", 0.7))
      #hc_add_series(SPY.RSI.SellLevel, color = hex_to_rgba("red", 0.7),
      #              yAxis = 2, name = "Sell level") %>% 
      #hc_add_series(SPY.RSI.BuyLevel, color = hex_to_rgba("blue", 0.7),
      #              yAxis = 2, name = "Buy level") 
    # add theme
    hc <- hc %>% hc_add_theme(hc_theme_darkunica())
    # add plot options
    hc <- hc %>% hc_plotOptions(series = list(
                                   compare = "percent",
                                   showInNavigator = TRUE
                                 ))
    # set rangeSelector
    hc <- hc %>% hc_rangeSelector(
        verticalAlign = "bottom",
        selected = 0 # select month by default
      )
    # set tooltip
    hc <- hc %>% hc_tooltip(
               pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}</b> ({point.change}%)<br/>',
               valueDecimals = 2,
               split =  TRUE,
               crosshairs = TRUE)
    
    # return
    hc
    
  })
  output$zeitreiheUI <- renderUI({
    tags$div(
      highchartOutput("highchartPlot"),
      br(),
      HTML('<p>Die Visualisierung erfolgt mit <a href="https://www.highcharts.com/stock/demo/compare/dark-unica" target="_blank" rel="noopener">Highcharts (Highstock)</a>.')
    )
  })
  
  output$about <- renderUI({
    tags$div(
      br(),
      tags$h1("About"),
      "Fragestellungen die beantwortet werden sollen:",
      br(),
      "Zusammenhang zwischen Temperatur und Honigproduktion / Ertrag",
      br(),
      "Darstellen an welchen Tagen der Imker da war und Honig entnommen hat?",
      br(),
      "Darstellen an welchen Tagen es geregnet hat und kein Honig gesammelt wurde?",
      tags$h2("Data"),
      "Beschreibung woher die Daten kommen",
      tags$h2("Summary"),
      "Zeigt die Summary Funktion",
      tags$h2("Corr"),
      "Zeigt die Korrelation"
      )
  })
    
  ########################################
  # Weitere UI Utilities
  ########################################
  
  ## observe the toggleButton being pressed
  observeEvent(input$toggleButton, {
    if(input$toggleButton %% 2 == 1){
      shinyjs::hide(id = "myBox")
    }else{
      shinyjs::show(id = "myBox")
    }
  })
  
  ## observe the resetFilter-Button being pressed
  observeEvent(input$resetFilter, {
    updateCheckboxInput(session, "filterhours", value = F)
    updateDateRangeInput(session, "daterange", start = "2018-01-01", end = "2019-07-01")
    beehive_df <<- beehive_df_unfiltered
    updateTabsetPanel(session, "inTabset")
  })
  
  # Download Funktionalität 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv2(beehive_df, con, sep = ",", quote = FALSE)
    },
    contentType = "text/csv"
  )
  
}

########################################
# Main
########################################      
shinyServer(server)