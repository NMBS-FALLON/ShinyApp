library("ggplot2")
#library("stringr")
#library("repmis")
library("ggthemes")
#library("quantmod")
#library("foreach")
library("scales")
library("shiny")
library("leaflet")


shinyUI(
  navbarPage("My Application",
             tabPanel("Ton Time-Series",
               sidebarLayout(
                 sidebarPanel(
                   dateInput("startDate", label = h5("Start Date:"), value = "2016-01-01"),
                   dateInput("endDate", label = h5("End Date:"), value = "2017-01-01"),
                   selectizeInput('graphs', label = h5("Graph(s):"), choices = list(
                     Sales = c(`Joist Sold` = 'Joist Sold', `Deck Sold` = 'Deck Sold',`Joist Quoted` = 'Joist Quoted',`Deck Quoted` = 'Deck Quoted'),
                     Engineering = c(`Joist Released` = 'Joist Released', `Deck Released` = 'Deck Released')
                   ), multiple = TRUE),
                   selectInput("interval", label = h5("Interval:"), choices=list(
                     Interval = c(`DAILY` = 'daily', `WEEKLY` = 'weekly',`MONTHLY` = 'monthly', `QUARTERLY` = 'quarterly', `YEARLY` = 'yearly')))
                 ),
                 
                 mainPanel(
                   plotOutput("myPlot"),
                   plotOutput("myBarPlot")
                 )
               )
             ),
             tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          selectInput("whatToMap", label = h5("MAP:"), choices=list(
                            whatToMap = c(`Joist Sold` = 'joistSold', `Joist Quoted` = 'joistQuoted'))),
                          uiOutput("ui"),
                          dateInput("startDate_Map", label = h5("Start Date:"), value = "2016-01-01"),
                          dateInput("endDate_Map", label = h5("End Date:"), value = "2017-01-01")
                        ),
                      
                      mainPanel(width = 9,
                        leafletOutput("map", height = "600pt")
                      )
                      )
             )
             #tabPanel("Component 3")
  )
)