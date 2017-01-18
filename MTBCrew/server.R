#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library("shiny")
library("plotly")
library("plyr")
library("dplyr")
library("lubridate")
library("leaflet")
library("rjson")
library("googleway")
library("rpart")
library(RCurl)

source("loadData.R")

#load tracks
tracks <- loadTracks()
tracksdf <- do.call("rbind.fill", lapply(tracks$data, as.data.frame))
tracksdf <- tracksdf %>% mutate(name = as.character(name), color = as.character(color), polyline = as.character(polyline)) %>% filter(id != 8, type == "permanent") 

#load track data
names <-c("Rhenen", "2", "3", "4", "5", "6" ,"Kwintelooijen", "8", "9", "10", "11", "Amerongen", "13","14","15","Hoge Ginkel","17","18","19","20","21","22","Leersum","Zeist","25","26", "27","28","29","Austerlitz")
sectorDaily <- loadSectorAggregate()
sectorDaily <- sectorDaily %>% 
    filter(dmy(date) <= ymd("2016-12-31")) %>%
    mutate(date=dmy(date),
           time=ymd_h(paste(date,hour)),
           name = names[id],
           weekday = weekdays(date))

#simple regression tree model to predict the number of ride on track.
#fit <- rpart(count ~ id+weekday+hour, method="anova", data=sectorDaily)

#loads the model "model" stored on google drive
reloadModel(0)

#shiny server
shinyServer(function(input, output) {
    options(warn = -1) 
    
    sectorFilter <- reactive({
        if ("0" %in% input$selection)
            sectorDaily
        else
            sectorDaily %>% filter(id %in% input$selection)
    })
    
    output$plotMonth <- renderPlotly({
        sectorMonth <- sectorFilter() %>% group_by(name, month) %>% summarise(total = sum(count)) 
        plot_ly(data=sectorMonth,  x = ~month, y=~total, type="scatter", mode="lines", color=~name)
    })
    
    output$plotDay <- renderPlotly({
        sectorDay <- sectorFilter() %>% filter(month >= input$months[1], month <= input$months[2]) %>% group_by(name, date) %>% summarise(total = sum(count)) 
        plot_ly(data=sectorDay,  x = ~date, y=~total, type="scatter", mode="lines", color=~name)
    })
    
    output$plotHour <- renderPlotly({
        sectorHour <- sectorFilter() %>% arrange(time, time) %>% filter(month==input$month) 
        plot_ly(data=sectorHour,  x = ~time, y=~count, type="scatter", mode="lines", color=~name)
    })
    
    output$plotTrack <- renderLeaflet({
        muh <- leaflet() %>% setView(lng = 5.40759, lat = 52.01522, zoom = 11) %>% addTiles()
        for (i in 1:nrow(tracksdf)) {
            decode_track <- decode_pl(tracksdf[i,]$polyline)
            muh <- muh %>% addPolylines(lng=decode_track$lon, lat=decode_track$lat, color = tracksdf[i,]$color, popup = tracksdf[i,]$name)
        }
        muh <- muh %>% addLegend(position = "bottomleft", labels=tracksdf$name, colors=tracksdf$color)
        muh
    })

    output$plotForecast <- renderPlotly({
        reloadModel(10)
        
        forecastData <- data.frame(date=rep(seq(as.Date(now()), by=1, length.out = input$days),1,each=24), id = as.numeric(input$track), hour = rep(0:23,input$days))
        forecastData <- forecastData %>% mutate(weekday = weekdays(date), month=month(date) ,time=ymd_h(paste(date,hour)))
        forecastData$count <- predict(model, newdata = forecastData)
        plot_ly(data=forecastData,  x = ~time, y=~count, type="scatter", mode="lines") %>%
            layout(                yaxis = list(rangemode = "tozero"))
    })
    
})
