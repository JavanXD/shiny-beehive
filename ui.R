#install.packages("shiny")
#install.packages("weatherData")
library(shiny)
library(weatherData)

shinyUI(fluidPage(
  
  # Application title
  title="Weather App!",
  fluidRow(
    
    column(2,
           h4("World Wide Weather"),
           wellPanel(
             dateInput(inputId = 'date',
                       label = 'Select Date',
                       value = Sys.Date()
             ),
             selectInput("select", label = h3("Select Location"), 
                         choices = list("New York" = "NYC", "San Francisco" = "SFO", "London" = "LON")),
             selectInput("year", label = h3("Select Year"), 
                         choices = list("2016" = 2016, "2015" = 2015, "2014" = 2014), 
                         selected = 1)
           )
           ),
    column(10,
           mainPanel("Daily Weather Details over Selected Date",
                     tabsetPanel(
                       tabPanel("Temperature", plotOutput("p1")), 
                       tabPanel("Humidity", plotOutput("p2")), 
                       tabPanel("Wind Speed", plotOutput("p3"))
                     )
           )
    ),
    column(10,offset=2,
             tabsetPanel("Annual Weather Details",
             tabPanel("Mean Temperature", plotOutput("plot1")), 
             tabPanel("Minimum Temperature", plotOutput("plot2")), 
             tabPanel("Maximum Temperature", plotOutput("plot3"))
           )   ) 
  )
          
    
  )
  
  
)