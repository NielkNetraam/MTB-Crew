#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(leaflet)

shinyUI(navbarPage(theme = "cerulean", "MTB UH 2016",
    tabPanel("Documentation",
             fluidRow(column(8, offset = 2,
                h1("MTB Utrechtse Heuvelrug 2016")
             )),
             br(),
             fluidRow(column(8, offset = 2,
                p("This application shows the number off rides on the MTB tracks of the Utrechtse Heuvelrug
                in 2016 and gives a forecast of the expected rides in the coming days")
             )),
             br(),
             fluidRow(column(8, offset = 2,
                p(strong("Ride Count"),": Gives an overview of the number of rides. ",
                strong("Select one or more tracks"), " if you don't want to see all tracks you have to deselect 'all'.")
             )),
             fluidRow(column(8, offset = 3,    
                p(strong("Tab Month"),": shows the number of rides per month of the selected tracks"),
                p(strong("Tab Day"),": shows the number of rides per day of the selected tracks, you can select the ", 
                strong("range months")," for which you want to see the selection"),
                p(strong("Tab Hour"),": shows the number of rides per hour of the selected tracks for one month, you can select the ", 
                strong("month")," for which you want to see the selection")
             )),
             fluidRow(column(8, offset = 2,
                             p(strong("Forecast"),": gives a forecast of the number of rides per hour in the upcoming days. 
                             Select a ", strong("track"), " and ",strong("the number of days to forecast"),".
                             The forecast is based on a simple regression tree using count, track, weekday and hour")
             )),
             fluidRow(column(8, offset = 2,
                             p(strong("Tracks"),": gives an overview of the MTB track on the Utrechtse Heuvelrug (build with Leaflet)")
             ))
    ),
    tabPanel("Ride Count",
             sidebarLayout(
                 sidebarPanel(
                     selectizeInput("selection",
                                    "Choose a track:",
                                    choices = tracks, 
                                    multiple = TRUE,
                                    selected = 0,
                                    options = list(
                                         placeholder = 'Please select an option below'
                                    #     onInitialize = I('function() { this.setValue(""); }')
                                         )
                                    )
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     tabsetPanel(type = "tabs", 
                                 tabPanel("Month", 
                                          plotlyOutput("plotMonth")
                                          ), 
                                 tabPanel("Day", 
                                          plotlyOutput("plotDay"),
                                          hr(),
                                          sliderInput("months", "Months:", min = 1, max = 12, value = c(1,12))
                                          ), 
                                 tabPanel("Hour", 
                                          plotlyOutput("plotHour"),
                                          hr(),
                                          sliderInput("month", "Month:", min = 1, max = 12, value = 1)
                                          )
                     )
                 )
             )
           ),
    tabPanel("Forecast",
             sidebarLayout(
                 sidebarPanel(
                     selectizeInput("track",
                                    "Choose a track:",
                                    choices = tracks2, 
                                    multiple = FALSE,
                                    selected = 12
                     ),
                     sliderInput("days", "Number of days to forecast:", min = 7, max = 28, step = 7,value = 7)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotlyOutput("plotForecast")
                 )
             )
    ),
    tabPanel("Tracks", leafletOutput("plotTrack"))
))
