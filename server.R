#install.packages('weatherData')
#install.packages("geonames")
library(shiny)
library(ggplot2)
library(weatherData)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  wdate<-reactive({
    #GNfindNearbyPlaceName(input$lat, input$lon, radius = "", maxRows = "1", style = "MEDIUM")  
    getWeatherForDate(input$select, input$date,opt_all_columns = TRUE)
  
  })
  cDate<-reactive({
    getCurrentTemperature(input$select, input$date,opt_all_columns = TRUE)
    
  })
  output$plot1<- renderPlot({
    wdata<-getWeatherForYear(input$select, input$year, opt_detailed = FALSE, opt_write_to_file = FALSE) 
    a<-ggplot(wdata, aes(x=wdata$Date,y=wdata$Mean_TemperatureF)) +ggtitle(paste("Mean Temperature over",input$year)) +
      labs(x="Year",y="Mean. Temperature") +  geom_bar(stat='identity',fill="green")
    print(a)
  })
  output$plot2<- renderPlot({
    wdata<-getWeatherForYear(input$select, input$year, opt_detailed = FALSE, opt_write_to_file = FALSE) 
    a<-ggplot(wdata, aes(x=wdata$Date,y=wdata$Min_TemperatureF)) + ggtitle(paste("Minimum Temperature over",input$year)) +
      labs(x="Year",y="Min. Temperature") + geom_bar(stat='identity', fill="steelblue")
    print(a)
  })
  output$plot3<- renderPlot({
    wdata<-getWeatherForYear(input$select, input$year, opt_detailed = FALSE, opt_write_to_file = FALSE) 
    a<-ggplot(wdata, aes(x=wdata$Date,y=wdata$Max_TemperatureF))+ggtitle(paste("Maximum Temperature over",input$year)) +
      labs(x="Year",y="Max. Temperature") +  geom_bar(stat='identity',fill="orange")
    print(a)
  })
  output$p1 <- renderPlot({ 
    wc_df<-getDetailedWeather(input$select, input$date, opt_all_columns = TRUE)
    b<-ggplot(wc_df, aes(x=wc_df$Time,y=wc_df$TemperatureF, group=1))+geom_point()+geom_line(color="red4",size=2)+ggtitle(paste("Weather in ",input$select," on",input$date))+labs(x="Time",y="Temperature (F)")+ coord_cartesian(ylim = c(-20, 80))
    print(b)
  })
  output$p2<- renderPlot({ 
    wc_df<-getDetailedWeather(input$select, input$date, opt_all_columns = TRUE)
    b<-ggplot(wc_df, aes(x=wc_df$Time,y=wc_df$Humidity, group=1))+geom_point()+geom_line(color="royalblue2",size=2)+ggtitle(paste("Weather in ",input$select," on",input$date))+labs(x="Time",y="Humidity")+ coord_cartesian(ylim = c(0, 100))
    print(b)
  })
  output$p3 <- renderPlot({ 
    wc_df<-getDetailedWeather(input$select, input$date, opt_all_columns = TRUE)
    b<-ggplot(wc_df, aes(x=wc_df$Time,y=wc_df$Wind_SpeedMPH, group=1))+geom_point()+geom_line(color="springgreen",size=2)+ggtitle(paste("Weather in ",input$select," on",input$date))+labs(x="Time",y="Wind Speed (MPH)")
    print(b)
  })
  
  
  

})