library(shiny)
library(plotly)
library(leaflet)
library(DT)

nodename <-Sys.info()["nodename"]

shinyUI(
    navbarPage(
        theme = "cerulean", 
        "MTBCrew",
        tabPanel("Utrechtse Heuvelrug",
                 plotlyOutput("walhallaPlot"),
                 h4("Total numbers since 2016-01-01"),
                 tableOutput("walhallaView"),
                 br()
        ),
        tabPanel("Info",
                 fluidRow(column(3, uiOutput("chooseTrack")),
                          column(3, radioButtons("infolevel", 
                                                 label = "Info level:", choices = c("Tracks", "Sectors"), 
                                                 selected = "Tracks", inline = TRUE)),
                          column(3, checkboxInput("completeEfforts", "Tracks on complete Efforts", FALSE))
                          ),
                 tabsetPanel(type = "tabs",
                             tabPanel("Tracks", 
                                      leafletOutput("plotTrack", height=600)
                                      ),
                             tabPanel("Numbers",  
                                      dataTableOutput('infoNumbersDetail')
                                      ),
                             tabPanel("Charts",  
                                      plotlyOutput("infoDetailPlot"),
                                      plotlyOutput("infoRoundCountsPlot")
                             ),
                             tabPanel("Monthy Chart",  
                                      plotlyOutput("plotTrackMonth")
                             ),
                             tabPanel("Daily Chart",  
                                      plotlyOutput("plotTrackDay"),
                                      hr(),
                                      sliderInput("dateRange", "Date range", 
                                                  min = as.Date("2016-01-01"),
                                                  max = as.Date("2017-01-01"),
                                                  value = c(as.Date("2016-01-01"),Sys.Date()),
                                                  timeFormat="%b %Y")
                             ),
                             tabPanel("Hour Chart",
                                      plotlyOutput("plotTrackHour"),
                                      hr(),
                                      fluidRow(column(3, sliderInput("dateSelect", "Month",
                                                  min = as.Date("2016-01-01"),
                                                  max = Sys.Date(),
                                                  value = as.Date("2017-01-01"),
                                                  timeFormat="%F")),
                                               column(3, sliderInput("daysSelect", "Number of days",
                                                  min = 1, max = 30, value=7)))
                             ),
                             tabPanel("Track Efforts",
                                      plotlyOutput("plotTrackEfforts"),
                                      checkboxInput("trackEffortsCb", "All Efforts", FALSE)
                             )
                 ),
                 br()
                     
        ),
        tabPanel("Forecast",
                 fluidRow(column(3, uiOutput("chooseTrackForecast"))),
                 fluidRow(column(3, htmlOutput("weatherFrame")),
                          column(9, plotlyOutput("plotForecast"))),
                  br()
        ),
        navbarMenu("More",
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
                            )),
                            br()
                   ),
                   tabPanel("Maintenance", 
                            fluidRow(column(8, offset = 2,
                                            h1("Maintenance")
                            )),
                            uiOutput("maintenance"),
                            br()
                            )
        ),
        footer = verbatimTextOutput("exportTimestamp"),
        windowTitle = "MTBCrew"
    )
)
