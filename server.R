library(shiny)
library(ggplot2)
library(readr)
library(corrplot)

shinyServer(function(input, output) {
  
  ##### Daten aufbereiten
  # import csv and skip unnecessary columns, set timeformat
  beehive_data <- read_csv("beehive.csv", col_types = cols(ticks = col_number(), 
                                                      timestamp = col_datetime(format = "%Y/%m/%d %H:%M:%S"), 
                                                      x5 = col_skip(),
                                                      x7 = col_skip(),
                                                      x8 = col_skip()))
  # berechne differenzen zwischen reihen
  delta_weight <- diff(beehive_data$weight[order(beehive_data$ticks)])
  delta_temp1 <- diff(beehive_data$temp1[order(beehive_data$ticks)])
  delta_temp2 <- diff(beehive_data$temp2[order(beehive_data$ticks)])
  delta_hum1 <- diff(beehive_data$hum1[order(beehive_data$ticks)])
  delta_hum2 <- diff(beehive_data$hum2[order(beehive_data$ticks)])
  # delete first row
  beehive_data = beehive_data[-1,] 
  beehive_data$delta_weight <- delta_weight
  beehive_data$delta_temp1 <- delta_temp1
  beehive_data$delta_temp2 <- delta_temp2
  beehive_data$delta_hum1 <- delta_hum1
  beehive_data$delta_hum2 <- delta_hum2
  
  ######## Test
  View(beehive_data)
  summary(beehive_data)
  # Korrelation berechnen
  correlation <- cor(beehive_data[,c("weight", "temp1", "temp2", "hum1", "hum2", "delta_weight", "delta_temp1", "delta_temp2", "delta_hum1", "delta_hum2")])
  # Korrelation visualisieren
  correlationvisual <- corrplot(correlation)
  
  # function for daterange
  zu_plotten <- reactive({
    date_start_date <- as.Date(input$daterange[1], origin = "1970/01/01")
    date_end_date <- as.Date(input$daterange[2], origin = "1970/01/01")
    subset(beehive_data)
  })
  
  ####### Tabs definieren
  output$distPlot <- renderPlot({
    ggplot(beehive_data)+geom_histogram(aes(weight))
    #ggplot(beehive_data)+geom_point(aes(x=weight,y=temp1))+xlim(input$x_min,input$x_max)
  })
  output$tabelle <- renderDataTable({
    zu_plotten()
  })
  output$summary <- renderDataTable({
    summary(beehive_data)
  })
  output$cor <- renderText({
    correlation
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
    write.csv2(beehive_data, con)
    }
  )
  
  # Daniel erster Versuch. Zusammenhang nicht gut
  firsttry = ggplot(beehive_data, aes(x = temp1, y = delta_weight)) + geom_point() + geom_smooth(method='lm') +
    labs(title = "Vorlage GEOM Daniel", 
         subtitle = "Erster Versuch", 
         x = "Außentemperatur in Celsius", y = "Gewichtsdifferenz zum Vortag"
    )
  
})
