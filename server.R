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

# Kruskal-Wallis
library(ggpubr)

server <- function(input, output, session) {
  
  ########################################
  # Daten aufbereiten 
  ########################################
  format <- "%Y/%m/%d %H:%M:%S"
  # import csv and skip unnecessary columns, set timeformat
  beehive_df <- reactiveValues()
  beehive_df <- read_csv("beehive.csv", col_types = cols(unixtime = col_number(), 
                                                      timestamp = col_datetime(format = format), 
                                                      x1 = col_skip()))
  beehive_df_unfiltered <- beehive_df
  
  treat_df <- function(df) {
    # TODO: Zeile in Produktion entfernen. 
    df <- subset(df, hum_hive != 100)
    df <- subset(df, !is.na(hum_hive))
    df <- subset(df, !is.na(hum_out))
    df <- subset(df, !is.na(weight))
    
    # berechne differenzen zwischen reihen
    delta_weight <- diff(df$weight[order(df$timestamp)]) * 1000 # convert kg to g
    delta_temp_hive <- diff(df$temp_hive[order(df$timestamp)])
    delta_temp_out <- diff(df$temp_out[order(df$timestamp)])
    delta_hum_hive <- diff(df$hum_hive[order(df$timestamp)])
    delta_hum_out <- diff(df$hum_out[order(df$timestamp)])
    # delete first row
    df <- df[-1,] 
    df$delta_weight <- round(delta_weight, digits=4)
    df$delta_temp_hive <- round(delta_temp_hive, digits=4)
    df$delta_temp_out <- round(delta_temp_out, digits=4)
    df$delta_hum_hive <- round(delta_hum_hive, digits=4)
    df$delta_hum_out <- round(delta_hum_out, digits=4)
    
    return(df)
  }
  
  obsDf <- observe({
      beehive_df <- treat_df(beehive_df)
    }, quoted = TRUE)
  
  ########################################
  # Skripte Testen
  ########################################
  # Gruppiere Daten nach Tag und füge Min Max pro Tag hinzu
  beehive_df_daily <- beehive_df %>%
    mutate(Date = ymd_hms(timestamp), dt = as_date(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
    group_by(dt, month, year) %>% 
    summarise(temp_hive_max = max(temp_hive), temp_hive_min = min(temp_hive), weight_mean = mean(weight))
  
  # Gruppiere nach Monate für Boxplot
  beehive_df_monthly <- beehive_df %>%
    mutate(Date = ymd_hms(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
    group_by(month, year) %>% 
    summarise(weight_max = max(weight), weight_min = min(weight), weight_mean = mean(weight), weight = sum(weight))
  #View(beehive_df)

  ########################################
  # Tab Korrelation
  ########################################
  
  # Korrelation mit Selectfeldern nach Spearman + Textoutput Pearson und Spearman
  output$spearmanPlot <- renderPlot({
    x <- beehive_df[[input$selectedFieldSpearmanX]]
    y <- beehive_df[[input$selectedFieldSpearmanY]]

  output$calccor <- renderPrint({
    cor.test(x,y,method="spearman")
  })
    
  output$calcpearson <- renderPrint({
    cor.test(x,y,method="pearson")
  })
    
  # draw plot
    ggplot(beehive_df, aes(x = x, y = y)) + geom_point() + geom_smooth(se = FALSE, method='lm') +
    labs(x = toString(input$selectedFieldSpearmanX), y = toString(input$selectedFieldSpearmanY))
  })
  
  #Korrelationsmatrix berechnen
  output$corPlot <- renderPlot({
    # Korrelation berechnen
    corrdataframe <- cor(beehive_df[,c("weight", "temp_hive", "temp_out", "hum_hive", "hum_out", "delta_weight", "delta_temp_hive", "delta_temp_out", "delta_hum_hive", "delta_hum_out")])
    # Korrelation visualisieren
    corspearman <- cor(corrdataframe, use="complete.obs", method="spearman") 
    # draw plot
    corrplot(corspearman, method = "ellipse", type = "upper", tl.srt = 45)
  })
  
  output$spearmanUI <- renderUI({
    tags$div(
      br(),
      tags$h3("Korrelationsmatrix"),
      plotOutput("corPlot"),
      br(),
      p("Um eine Übersicht der Korrelationen zu bekommen erstellen wir eine Korrelationsmatrix. Diese gibt uns einen Überblick, ob zwischen zwei Merkmalen eine positive (blau bzw. Wert gegen 1), keine (weiß bzw. Wert gegen 0) oder eine negative (rot bzw. Wert gegen -1) Korrelation vorliegt. Der genaue Korrelationskoeffizient kann untenstehend, durch die Auswahl in den Select-Boxen, berechnet werden."),
      br(),
      tags$h3("Spearman Korrelationskoeffizient"),
      p("Für den untenstehenden Plot wurde die Visualisierung des Spearman Korrelationskoeffizient gewählt. Grund dafür war, die Auswertung nicht anfällig für (extreme) Ausreißer in den verwendeten Basis-Daten zu machen. Die Ausreißer können vorkommen, wenn beispielsweise die Waage am Bienenstock nicht richtig funktioniert oder der Imker Arbeiten am Bienenstock durchführt und es zu extremen Messungen kommt. Alternativ hätten auch systematisch die höchsten und niedriegsten Werte aus dem Datensatz entfernt werden können (via Winsorizing). Zusätzlich wurde der Pearson Korrelationskoeffizient mitausgegeben um die Unterschiede beider Korrelationskoeffizienten aufzuzeigen. Bei einem Wert von + 1  (bzw. − 1) besteht ein vollständig positiver (bzw. negativer) linearer Zusammenhang zwischen den betrachteten Merkmalen. Wenn der Korrelationskoeffizient den Wert 0 aufweist, hängen die beiden Merkmale überhaupt nicht linear voneinander ab. Es kann sich dabei jedoch um Scheinkorrelationen handeln!"),
      selectField(id="selectedFieldSpearmanX", val="temp_out", text="Merkmal für x-Achse"),
      selectField(id="selectedFieldSpearmanY", val="weight", text="Merkmal für y-Achse"),
      verbatimTextOutput("calccor"),
      verbatimTextOutput("calcpearson"),
      plotOutput("spearmanPlot")
      )
  })
  
 
  ########################################
  # Tab Kruskal-Wallis
  ########################################
  calc_group <- function (col) {
    col <- as.numeric(col)
    sapply(col, function(col){
      if(col >= 40){
        return("heiß") 
      }else if(col >= 25){
        return("warm") 
      }else if(col >= 10){
        return("kalt") 
      }else{
        return("eiskalt") 
      }
    })
  }
  
  tempLevels <- c("heiß", "warm", "kalt", "eiskalt")
  
  kruskal_wallis <- reactive({
    # Bilde Gruppen
    beehive_df_kruskal <- beehive_df %>%
      mutate(group = calc_group(temp_out), temp_out = temp_out)
    
    # Reorder
    beehive_df_kruskal$group <- ordered(beehive_df_kruskal$group,
                                        levels = tempLevels)
    
    # Gruppiere
    beehive_df_kruskal_visualized <- group_by(beehive_df_kruskal, group) %>% 
      summarise(
        count = n(),
        mean = mean(weight, na.rm = TRUE),
        sd = sd(weight, na.rm = TRUE),
        median = median(weight, na.rm = TRUE),
        IQR = IQR(weight, na.rm = TRUE)
      )
    
    output$kruskalDf <- renderPrint({
      beehive_df_kruskal_visualized
    })
    
    # Box plots
    # ++++++++++++++++++++
    # Plot weight by group and color by group
    output$kruskalBoxplot <- renderPlot({
      ggboxplot(beehive_df_kruskal, x = "group", y = "weight", 
                color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#313131"),
                order = tempLevels,
                ylab = "Gewicht [kg]", xlab = "Temperaturempfinden")
    })

    
    # Mean plots
    # ++++++++++++++++++++
    # Plot weight by group
    # Add error bars: mean_se
    # (other values include: mean_sd, mean_ci, median_iqr, ....)
    output$kruskalMean <- renderPlot({
      ggline(beehive_df_kruskal, x = "group", y = "weight", 
             add = c("mean_se", "jitter"), 
             order = tempLevels,
             ylab = "Gewicht [kg]", xlab = "Temperaturempfinden")
    })

    
    # We want to know if there is any significant difference between the average weights of plants in the 3 experimental conditions.
    output$kruskalTest <- renderPrint({
      kruskal.test(weight ~ group, data = beehive_df_kruskal)
    })
    
    # From the output of the Kruskal-Wallis test, we know that there is a significant difference between groups, but we don’t know which pairs of groups are different.
    output$kruskalComparison <- renderPrint({
      pairwise.wilcox.test(beehive_df_kruskal$weight, beehive_df_kruskal$group,
                           p.adjust.method = "BH")
    })
    
  })
  
  output$corUI <- renderUI({
    kruskal_wallis()
    tags$div(
      br(),
      tags$h3("Kruskal-Wallis"),
      verbatimTextOutput("kruskalDf"),
      plotOutput("kruskalBoxplot"),
      plotOutput("kruskalMean"),
      verbatimTextOutput("kruskalTest"),
      verbatimTextOutput("kruskalComparison"),
      p("test")
    )
  })
  
  ########################################
  # Funktionen definieren, die für Graphen verwendet werden.
  ########################################
  
  change_dataframe <- reactive({
    # Read file ----
    if (!is.null(input$fileuploadFile)) {
      # Wenn ein File ausgewählt wurde, dann nehme das File
      # beehive_df <- read_csv(input$fileuploadFile$datapath,
      #                        header = input$fileuploadHeader,
      #                        sep = input$fileuploadSep)
      beehive_df_unfiltered <<- read_csv(input$fileuploadFile$datapath, col_types = cols(unixtime = col_number(), 
                                                                             timestamp = col_datetime(format = format)))
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
    
    # In Trend, Saisonalität und zufällige Fluktuationen aufteilen
    time_series_components <- decompose(TS)
    
    # Der Rückgabewert time_series_components dieser Funktion enthält eine Liste, welcher verschiedene Komponenten enthält. Ein Plot dieser Liste zeigt Folgendes:
    components_plot <- as.ggplot(function()  plot(time_series_components))
    
    # Expotentielles Glätten
    hw_plot <- as.ggplot(function() {
      time_series_vorhersage <- HoltWinters(time_series, alpha = 0.5, beta = 0.5, gamma = F)
      plot(time_series_vorhersage, main = "Holt-Winters-Glättung", sub = "Exponetielles Glätten: alpha = 0.5 beta = 0.5")
    })
    
    # HoltWInters forecast
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
  selectableFields <- c("weight", "temp_hive", "temp_out", "hum_hive", "hum_out", "delta_weight", "delta_temp_hive", "delta_hum_hive")
  selectField <- function (id="selectedField", val=NULL, text="Merkmal auswählen") {
                selectInput(id,label=text,choice=selectableFields, selected=val, selectize=FALSE) }
  
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
    field2 <- input$selectedFieldSummary2
    y <- beehive_df[[field]]
    x <- beehive_df[[field2]]  
    plot(x, y,       
            ylab=paste("Ausgewähltes Merkmal: ", toString(field)),
            xlab=paste("Ausgewähltes Merkmal: ", toString(field2)))
  })
  output$histogramUI <- renderUI({
    tags$div(
      br(),
      selectField(id="selectedFieldSummary", val="temp_hive"),
      tags$h3("Histogramm"),
      tags$p("Das Histogramm macht die absolute oder relative Häufigkeitsverteilung und Häufigkeitsdichte sichtbar."),
      plotOutput("histogramPlot"),
      tags$h3("QQ-Plot"),
      tags$p("Das Q-Q-Diagramm (bzw. Q-Q-Plot) ist eine Grafik, mit der eine quantitative Variable auf das Vorliegen einer Normalverteilung überprüft werden kann."),
      plotOutput("qqPlot"),
      tags$h3("Scatter Plot"),
      tags$p("Das Scatterplot trägt zwei quantitative Merkmale im Koordinatensystem gegeneinander ab und lässt Zusammenhänge vermuten."),
      selectField(id="selectedFieldSummary2", val="hum_hive", text ="2. Merkmal auswählen"),
      plotOutput("scatterPlot")
    )
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
  
  output$monthlyBoxplotUI <- renderUI({
    tags$div(
      br(),
      plotlyOutput("monthlyBoxplot"), 
      br(),
      p("Die einzelnen Ausreißer in den niedrigen Gewichtsbereichen im April 2018, Mai 2018 und Juni 2018 zeigen deutlich die Arbeiten am Bienenstand. Dabei hat der Imker vermutlich einzelne Zargen abgenommen oder Honig geschleudert. Diese Ansicht zeigt beispielhaft den Gewichtsverlauf am Bienenstock, in welchen Sommermonaten ein Bienenvolk den meisten Honigertrag verzeichnet und wie stark das Gewicht innerhalb eines Monats variiert.")
    )
  })
  
  
  output$dailyBoxplot <- renderPlotly({
    
    # Abhängig von Konfiguration Zeitraum eingrenzen
    date_end_date <- as.Date(input$selectedDayDailyBoxplot)
    date_start_date <- ymd(date_end_date) - days(input$selectedDaysCountDailyBoxplot)
    
    beehive_df <- subset(beehive_df, timestamp >= date_start_date & timestamp <= date_end_date)
    
    # draw boxplot
    plot_ly(beehive_df,
             y = ~weight, 
             x = reorder(format(beehive_df$timestamp,'%d %B %y'), beehive_df$timestamp),
             type = "box",
             boxpoints = "suspectedoutliers") %>% 
      layout(yaxis = list(title = "Gewicht [kg]")) %>% 
      hide_legend()
  })
  
  output$dailyBoxplotUI <- renderUI({
    tags$div(
      br(),
      selectDay(id="selectedDayDailyBoxplot"),
      selectDaysCount(id="selectedDaysCountDailyBoxplot", val=54), 
      plotlyOutput("dailyBoxplot"),
      br(),
      p("In der Woche vom 15. April bis 25. April hat es stark 'gehonigt'. Die Bienen haben fleißig über 10kg Honig eingebracht. Der Imker wartet den richtigen Zeitpunkt ab. Am 12. Mai muss der Imker Arbeiten am Bienenvolk vorgenommen haben und den vollen Honigraum heruntergenommen haben. Dabei muss es sich um die Honigernte gehandelt haben. Daraufhin stellt er am 14. Mai die ausgeschleuderten Waben mit Zarge wieder auf den Bienenkasten. In den darauffolgenden Tagen nimmt das Gewicht am Bienenstand leicht ab. Es lässt sich vermuten, dass die Bienen zunächst den Resthonig aus den ausgeschleuderten Waben ausschlecken und verzehren. Möglicherweise war in der Woche auch schlechtes Wetter. Denn ändert man nun das Datum auf den 1. Juli 2019 kann man schnell erkennen, dass ab dem 8. Juni wieder einen Gewichtszuwachs verzeichnet wurde. Dies ist zu dieser Jahreszeit zu erwarten.")
    )
  })
  
  output$zeitstrahlPlotly <- renderPlotly({
    
    # Filter Zeitraum
    date_start_date <- as.Date(input$selectedDayZeitstrahl2)
    date_end_date <- as.Date(input$selectedDayZeitstrahl)
    beehive_df <- subset(beehive_df, timestamp >= date_start_date & timestamp <= date_end_date)
    
    # Draw Zeitstrahl
    plot_ly(beehive_df, x = ~timestamp, y = ~weight, name = 'Gewicht [kg]', type = 'scatter', mode = 'lines+markers') %>%
      add_trace(y = ~temp_hive, name = 'Stocktemperatur [°C]', mode = 'lines+markers') %>%
      add_trace(y = ~temp_out, name = 'Temperatur Außen [°C]', mode = 'lines+markers') %>%
      add_trace(y = ~hum_hive, name = 'Luftfeuchte Innen [%]', mode = 'lines+markers')  %>%
      add_trace(y = ~hum_out, name = 'Luftfeuchte Außen [%]', mode = 'lines+markers')  %>%
      layout(xaxis = list(title = "Datum"), yaxis = list(title = ""))
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
    
    beehive_df_weight1 <- subset(beehive_df, timestamp >= date_start_date1 & timestamp <= date_end_date1)
    beehive_df_weight2 <- subset(beehive_df, timestamp >= date_start_date2 & timestamp <= date_end_date2)
    
    
    # draw boxplot
    p <- plot_ly(alpha = 0.4, 
                 type = "histogram", 
                 nbinsx = 50,
                 nbinsy = 50,
                 autobinx = FALSE,
                 xbins = list(size=20), 
                 autobiny = FALSE, 
                 ybins = list(size=20)) %>%
      add_histogram(x = ~beehive_df_weight1$delta_weight, name= "Zeitraum #1") %>%
      add_histogram(x = ~beehive_df_weight2$delta_weight, name=  "Zeitraum #2") %>%
      layout(barmode = "overlay",
             xaxis = list(title = "Gewichtsdifferenz [g]",
                          range = c(-150, 150)), # zoom in
             yaxis = list(title = "Anzahl"))

  })
  
  output$gewichtsDeltasDailyPlot <- renderPlotly({
    
    # Abhängig von Konfiguration Zeitraum eingrenzen
    date_end_date <- as.Date(input$selectedDayGewichtsDeltas)
    date_start_date <- ymd(date_end_date) - days(input$selectedDaysCount)
    
    beehive_df <- subset(beehive_df, timestamp >= date_start_date & timestamp <= date_end_date)

    beehive_df_daily <- beehive_df %>%
      mutate(Date = ymd_hms(timestamp), dt = as_date(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
      group_by(dt, month, year) %>% 
      summarise(weight_sum = sum(delta_weight), weight_mean = mean(weight))
    #View(beehive_df_daily)
    
    x <- beehive_df_daily$dt
    y <- beehive_df_daily$weight_sum
    p <- plot_ly(beehive_df_daily, x = ~x, y = ~y, type = 'bar', color = I("orange")) %>%
      layout(title = "",
             xaxis = list(title = "", 
                          type="date", 
                          #tickmode="linear", # show every date
                          tickangle=45),
             yaxis = list(title = "Gewichtsveränderung [g]",
                          range = c(-3000, 3000))
             )
  })
  
  output$gewichtsVerlaufDailyPlot <- renderPlotly({
    
    # Abhängig von Konfiguration Zeitraum eingrenzen
    date_end <- as.Date(input$selectedDayGewichtsDeltas)
    date_stop1 <- ymd(date_end) - days(1)
    date_stop2 <- ymd(date_stop1) - days(1)
    date_stop3 <- ymd(date_stop2) - days(1)
    date_stop4 <- ymd(date_stop3) - days(1)
    date_stop5 <- ymd(date_stop4) - days(1)

    
    beehive_df1 <- subset(beehive_df, timestamp >= date_stop1 & timestamp <= date_end)
    beehive_df2 <- subset(beehive_df, timestamp >= date_stop2 & timestamp <= date_stop1)
    beehive_df3 <- subset(beehive_df, timestamp >= date_stop3 & timestamp <= date_stop2)
    beehive_df4 <- subset(beehive_df, timestamp >= date_stop4 & timestamp <= date_stop3)
    beehive_df5 <- subset(beehive_df, timestamp >= date_stop5 & timestamp <= date_stop4)
    
    beehive_df_hourly1 <- beehive_df1 %>%
      mutate(Date = ymd_hms(timestamp), hour = hour(timestamp), dt = as_date(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
      group_by(hour, dt, month, year) %>% 
      summarise(weight1 = round(mean(weight), 4))
    beehive_df_hourly2 <- beehive_df2 %>%
      mutate(Date = ymd_hms(timestamp), hour = hour(timestamp), dt = as_date(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
      group_by(hour, dt, month, year) %>% 
      summarise(weight2 = round(mean(weight), 4))
    beehive_df_hourly3 <- beehive_df3 %>%
      mutate(Date = ymd_hms(timestamp), hour = hour(timestamp), dt = as_date(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
      group_by(hour, dt, month, year) %>% 
      summarise(weight3 = round(mean(weight), 4))
    beehive_df_hourly4 <- beehive_df4 %>%
      mutate(Date = ymd_hms(timestamp), hour = hour(timestamp), dt = as_date(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
      group_by(hour, dt, month, year) %>% 
      summarise(weight4 = round(mean(weight), 4))
    beehive_df_hourly5 <- beehive_df5 %>%
      mutate(Date = ymd_hms(timestamp), hour = hour(timestamp), dt = as_date(timestamp), month = format(timestamp, "%m"), year = format(timestamp, "%Y")) %>% 
      group_by(hour, dt, month, year) %>% 
      summarise(weight5 = round(mean(weight), 4))
    #View(beehive_df_hourly1)
    #View(beehive_df_hourly2)
    
    # merge dataframes into one
    final_df <- merge(beehive_df_hourly1[0:24,c("hour", "weight1")],beehive_df_hourly2[0:24,c("hour", "weight2")], all.x = TRUE,all.y = TRUE, by.y="hour")
    final_df <- merge(final_df,beehive_df_hourly3[0:24,c("hour", "weight3")],all.x = TRUE,all.y = TRUE, by.y="hour")
    final_df <- merge(final_df,beehive_df_hourly4[0:24,c("hour", "weight4")],all.x = TRUE,all.y = TRUE, by.y="hour")
    final_df <- merge(final_df,beehive_df_hourly5[0:24,c("hour", "weight5")],all.x = TRUE,all.y = TRUE, by.y="hour")
    #View(final_df)
    
    pal <- c("#4B0082", "#800080", "darkorchid", "blueviolet", "mediumorchid", "magenta")
    plot_ly(final_df, x = ~hour, y = ~weight1, name = 'vor 1 Tag', type = 'scatter', mode = 'lines+markers', colors = pal) %>%
      add_trace(x = ~hour, y = ~weight2, name = 'vor 2 Tagen', mode = 'lines+markers') %>%
      add_trace(x = ~hour, y = ~weight3, name = 'vor 3 Tagen', mode = 'lines+markers') %>%
      add_trace(x = ~hour, y = ~weight4, name = 'vor 4 Tagen', mode = 'lines+markers')  %>%
      add_trace(x = ~hour, y = ~weight5, name = 'vor 5 Tagen', mode = 'lines+markers')  %>%
      layout(xaxis = list(title = "Uhrzeit", tickmode="linear", tickformat="Uhr", range = c(0,23)), 
             yaxis = list(title = "Gewicht [kg]"))
  })
  
  output$gewichtsDeltasUI <- renderUI({
    tags$div(
      br(),
      selectDay(id="selectedDayGewichtsDeltas", val="2019-06-1", text= "Datum für Zeitraum 1"),
      selectDay(id="selectedDayGewichtsDeltas2", val="2019-07-1", text= "Datum für Zeitraum 2"),
      selectDaysCount(val=14),
      h3("Gewichtsveränderungen zweier Zeiträume vergleichen"),
      plotlyOutput("gewichtsDeltas"),
      br(),
      p("Anhand der Häufigkeitsverteilung können Gewichtszu-  und -abnahmen als Links- und Rechtssteil oder Symmetrisch festgestellt werden. Außerdem ist ersichtlich wie ein Zeitraum gegenüber einem anderen Zeitraum performt (Honigertrag)."),
      br(),
      h3("Tägliche Gewichtsveränderungen"),
      plotlyOutput("gewichtsDeltasDailyPlot"),
      br(),
      h3("Gewicht der letzten 5 Tage"),
      plotlyOutput("gewichtsVerlaufDailyPlot")
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
      p("Die Gewichtsdaten werden wie sie sind dargestellt. Zusätzlich ist eine Regressionsgerade eingezeichnet."),
      plotOutput("plotgraph1"),
      p("Es kann zum Fehler kommen, dass die Zeitreihe keine oder weniger als 2 Perioden hat.
      Dann kann das exponentiellen Glätten unter Verwendung der Holt-Winters-Funktion angewandt werden.
      Die Idee, die hinter der exponentiellen Glättung steht, ist besonders für ökonomische Zeitreihen einsichtig: 
      Ist es  sinnvoll, allen Beobachtungen der Zeitreihe das gleiche Gewicht einzuräumen, oder ist es sinnvoller jüngeren Beobachtungen mehr Gewicht als älteren Bobachtungen einzuräumen? 
      Wir wollen für unsere Zeitreihe diese Methodik des exponentiellen Glätten anschauen.
      "),
      plotOutput("plotgraph2"),
      p("In R gibt es eine Funktion, mit deren Hilfe man Zeitreihen in die drei Komponenten Trend, Saisonalität und zufällige Fluktuationen aufteilen kann."),
      plotOutput("plotgraph3"),
      p("Im letzten Graphen ist mit blau die Prognose eingezeichnet. Die Prognose erfolgte mit dem Vorhersagemodell HoltWinters."),
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
      # create y-Axis
      # hc_yAxis_multiples(
      #   list(title = list(text = "Messdaten"), lineWidth = 3),
      #   list(title = list(text = "Gewichtsdifferenz"), showLastLabel = FALSE, opposite = TRUE)
      # ) %>% 
      #hc_yAxis_multiples(
      #  create_yaxis(1, height = c(2, 1), turnopposite = TRUE)
      #) %>% 
      hc_yAxis(max = 120, min = -15, opposite = TRUE) %>% 
      # add series for Axis 0
      hc_add_series(data=xts$temp_hive, name = "Stocktemperatur [°C]", type = "line") %>% 
      hc_add_series(data=xts$temp_out, name = "Temperatur Außen [°C]", type = "line") %>% 
      hc_add_series(data=xts$weight, name = "Gewicht [kg]", type = "line", color = hex_to_rgba("black", 0.7)) %>% 
      hc_add_series(data=xts$hum_hive, name = "Luftfeuchte Innen [%]", type = "line") %>% 
      hc_add_series(data=xts$hum_out, name = "Luftfeuchte Außen [%]", type = "line")
      # add series for Axis 2  
      #hc_add_series(data=xts$delta_weight, yAxis = 1, name = "Zuwachs/Abnahme [g]", type = "line", color = hex_to_rgba("grey", 0.7))
      # add theme
      hc <- hc %>% hc_add_theme(hc_theme_google())
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
      paste('beehive-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      fixdateformat <- transform(beehive_df, timestamp = format(timestamp, format)) # turn 2018-04-05 08:15:45 into 2018/04/05 08:15:45
      write.table(fixdateformat, con, sep = ",", dec=".", na="", quote = FALSE,row.names=F, col.names=T)
    },
    contentType = "text/csv"
  )
  
}

########################################
# Main
########################################      
shinyServer(server)