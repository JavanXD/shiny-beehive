library(shiny)
library(ggplot2)
library(readr)
library(corrplot)
library(dplyr)
library(lubridate)

shinyServer(function(input, output) {
  
  # Daten aufbereiten ----
  # import csv and skip unnecessary columns, set timeformat
  beehive_df <- read_csv("beehive.csv", col_types = cols(ticks = col_number(), 
                                                      timestamp = col_datetime(format = "%Y/%m/%d %H:%M:%S"), 
                                                      x5 = col_skip(),
                                                      x7 = col_skip(),
                                                      x8 = col_skip()))
  
  # berechne differenzen zwischen reihen
  delta_weight <- diff(beehive_df$weight[order(beehive_df$ticks)])
  delta_temp1 <- diff(beehive_df$temp1[order(beehive_df$ticks)])
  delta_temp2 <- diff(beehive_df$temp2[order(beehive_df$ticks)])
  delta_hum1 <- diff(beehive_df$hum1[order(beehive_df$ticks)])
  delta_hum2 <- diff(beehive_df$hum2[order(beehive_df$ticks)])
  # delete first row
  beehive_df = beehive_df[-1,] 
  beehive_df$delta_weight <- delta_weight
  beehive_df$delta_temp1 <- delta_temp1
  beehive_df$delta_temp2 <- delta_temp2
  beehive_df$delta_hum1 <- delta_hum1
  beehive_df$delta_hum2 <- delta_hum2
  
  # Test Zeug ----
  #View(beehive_df)
  #summary(beehive_df)
  
  # Plots ----
  # Korrelation berechnen
  correlation <- cor(beehive_df[,c("weight", "temp1", "temp2", "hum1", "hum2", "delta_weight", "delta_temp1", "delta_temp2", "delta_hum1", "delta_hum2")])
  # Korrelation visualisieren
  #correlationvisual <- corrplot(correlation)
  # Daniel erster Versuch. Zusammenhang nicht gut
  firsttry = ggplot(beehive_df, aes(x = temp1, y = delta_weight)) + geom_point() + geom_smooth(method='lm') +
    labs(title = "Vorlage GEOM Daniel", 
         subtitle = "Erster Versuch", 
         x = "Außentemperatur in Celsius", y = "Gewichtsdifferenz zum Vortag"
    )
  
  # Reactive function ----
  zu_plotten <- reactive({
    
    # Read file ----
    if (!is.null(input$fileuploadFile)) {
      # Wenn ein File ausgewählt wurde, dann nehme das File
      beehive_df <- read.csv(input$fileuploadFile$datapath,
                             header = input$fileuploadHeader,
                             sep = input$fileuploadSep)
    } else {
      # Per default behalte den dataframe
      beehive_df <- beehive_df
    }
    
    # Filter Daterange ----
    date_start_date <- as.Date(input$daterange[1], origin = "1970-01-01")
    date_end_date <- as.Date(input$daterange[2], origin = "1970-01-01")
    beehive_df <- subset(beehive_df, timestamp > date_start_date & timestamp < date_end_date)
    
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
    
  })
  
  ####### Tabs definieren (Outputs)
  output$distPlot <- renderPlot({
    ggplot(beehive_df)+geom_histogram(aes(weight))
    #ggplot(beehive_df)+geom_point(aes(x=weight,y=temp1))+xlim(input$x_min,input$x_max)
  })
  output$tabelle <- renderDataTable({
    zu_plotten()
  })
  output$summary <- renderDataTable({
    summary(beehive_df)
  })
  output$cor <- renderPlot({
    corrplot(correlation, type="lower")
  })
  output$firsttry <- renderPlot({
    firsttry
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
  # Download Funktionalität 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv2(beehive_df, con)
    }
  )
  
})
