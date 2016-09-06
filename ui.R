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
             tabPanel("Component 1",
               sidebarLayout(
                 sidebarPanel(
                   dateInput("startDate", label = h3("Start Date:"), value = "2016-01-01"),
                   dateInput("endDate", label = h3("End Date:"), value = "2017-01-01"),
                   selectizeInput('graphs', 'GRAPH(S):', choices = list(
                     Sales = c(`Joist Sold` = 'Joist Sold', `Deck Sold` = 'Deck Sold',`Joist Quoted` = 'Joist Quoted',`Deck Quoted` = 'Deck Quoted'),
                     Engineering = c(`Joist Released` = 'Joist Released', `Deck Released` = 'Deck Released')
                   ), multiple = TRUE),
                   selectInput("interval", label = h3("Interval:"), choices=list(
                     Interval = c(`DAILY` = 'daily', `WEEKLY` = 'weekly',`MONTHLY` = 'monthly', `QUARTERLY` = 'quarterly', `YEARLY` = 'yearly')))
                 ),
                 
                 mainPanel(
                   plotOutput("myPlot"),
                   plotOutput("myBarPlot")
                 )
               )
             ),
             tabPanel("Component 2",
                      bootstrapPage(
                        selectInput("whatToMap", label = h3("MAP:"), choices=list(
                          whatToMap = c(`Joist Sold` = 'joistSold', `Joist Quoted` = 'joistQuoted'))),
                        leafletOutput("map", height = "600pt")
                      )
                      ),
             tabPanel("Component 3")
  )
)