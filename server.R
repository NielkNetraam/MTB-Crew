2016*12#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(plyr)
library(dplyr)
library(lubridate)
library(leaflet)
library(rjson)
library(googleway)
library(rpart)
library(RCurl)
library(reshape)
library(sp)
library(maptools)

source("util.R")
source("model.R")
source("loadData.R")
source("patrolData.R")

#simple regression tree model to predict the number of ride on track.
#fit <- rpart(count ~ id+weekday+hour, method="anova", data=sectorDaily)
exportTimestamp <- loadExportTimestamp()
#shiny server
shinyServer(function(input, output) {
    options(warn = -1) 

    trackDayReactive <- reactiveValues()
    trackHourReactive <- reactiveValues()
    
    observe({ trackDayReactive$range <- c(yearMonth(input$dateRange[1]), yearMonth(input$dateRange[2])) })
    observe({ trackHourReactive$date <- as.Date(input$dateSelect) })
    observe({ trackHourReactive$days <- input$daysSelect })
    
    overallCount <- reactive({ loadOverallCount() }) 
    trackDetail <- reactive({ loadTrackDetail() })
    trackActivityEfforts <- reactive({ loadTrackActivityEfforts() })
    trackRoundCounts <- reactive({ loadTrackRoundCounts() })
    trackHourCounts <- reactive({ loadTrackHourCounts() })
    trackSector <- reactive({ loadTrackSector() })    
    sectorDetail <- reactive({ loadSectorDetail() })
    sectorRoundCounts <- reactive({ loadSectorRoundCounts() })
    sectorHourCounts <- reactive({ loadSectorHourCounts() })

    output$exportTimestamp <- renderText({ paste("Updated till: ", exportTimestamp) })
    
    trackOnSectors <- reactive({
        trackDetail <- trackDetail() %>% select(-athleteCount, -activityCount, -effortCount)
        sectorDetail <- sectorDetail() %>% select(id, athleteCount, activityCount, effortCount)
        trackSector <- trackSector() %>% select(track_id, sector_id, model)
        
        sts <- merge(trackSector, sectorDetail, by.x="sector_id", by.y = "id")  %>% 
            filter(model==1) %>%
            group_by(track_id) %>%
            summarize(athleteCount=as.integer(mean(athleteCount)),
                      activityCount=as.integer(mean(activityCount)),
                      effortCount=as.integer(mean(effortCount)))
        
        merge(trackDetail, sts, by.x = "id", by.y="track_id")
    })
       
    trackFiltered <- reactive({
        if (input$completeEfforts) 
            trackDetail <- trackDetail()
        else
            trackDetail <- trackOnSectors()
        
        if (length(input$trackSelection) == 0) {
            trackDetail
        } else if (input$trackSelection == "Overall") {
            trackDetail
        } else {
            trackId <- trackDetail[trackDetail$name==input$trackSelection, "id"]
            if (length(trackId) == 0) trackId = 1
            trackDetail %>% filter(id==trackId)
        }
    })

    sectorFiltered <- reactive({
        sectorDetail <- sectorDetail()
        
        if (length(input$trackSelection) == 0) {
            sectorDetail
        } else if (input$trackSelection == "Overall") {
            sectorDetail
        } else {
            trackDetail <- trackDetail()
            trackId <- trackDetail[trackDetail$name==input$trackSelection, "id"]
            if (length(trackId) == 0) trackId = 1
            merge(sectorDetail, select(trackSector(), track_id, sector_id), by.x = "id", by.y="sector_id") %>% 
                filter(track_id == trackId) %>% select(-track_id)
        }    
    })
   
    trackOnSectorsHourCounts <- reactive({ 
        ts <- trackSector() %>% filter(model==1)%>% select(track_id, sector_id)
        shc <- sectorHourCounts() 
        merge(shc, ts, by.x="id", by.y="sector_id") %>% 
            group_by(track_id, timestamp) %>% 
            summarize(count=as.integer(mean(count))) %>% 
            rename(c(track_id="id")) 
    })
    
    trackHourCountsT <- reactive({ 
        trackDetail <- trackDetail()

        if (input$completeEfforts) 
            thc <- trackHourCounts() 
        else
            thc <- trackOnSectorsHourCounts() 

        head(thc)
        fd <- as.POSIXlt(min(thc$timestamp)); fd$hour <- 0
        ld <- as.POSIXlt(max(thc$timestamp)); ld$hour <- 23
        
        thcsq <- data.frame(timestamp = seq.POSIXt(fd,ld,by="hour"))
        thcsq <- data.frame(id=rep(trackDetail[trackDetail$trackType==0,]$id, each=nrow(thcsq)), timestamp=thcsq)
        thcsq <- merge(thcsq, thc, by=c("id","timestamp"), all.x=TRUE)
        thcsq$count <- ifelse(is.na(thcsq$count), 0, thcsq$count)
        thcsq
    })
    
    sectorHourCountsT <- reactive({ 
        sectorDetail <- sectorDetail()
        thc <- sectorHourCounts() 
        fd <- as.POSIXlt(min(thc$timestamp)); fd$hour <- 0
        ld <- as.POSIXlt(max(thc$timestamp)); ld$hour <- 23
        
        thcsq <- data.frame(timestamp = seq.POSIXt(fd,ld,by="hour"))
        thcsq <- data.frame(id=rep(sectorDetail$id, each=nrow(thcsq)), timestamp=thcsq)
        thcsq <- merge(thcsq, thc, by=c("id","timestamp"), all.x=TRUE)
        thcsq$count <- ifelse(is.na(thcsq$count), 0, thcsq$count)
        thcsq
    })
    
    trackHourFiltered <- reactive({
        merge(trackHourCountsT(), trackFiltered(), by = "id") %>% select(name, timestamp, count) 
    })
    
    sectorHourFiltered <- reactive({
        merge(sectorHourCountsT(), sectorFiltered(), by = "id") %>%
            select(short_name, timestamp, count) %>% rename(c(short_name="name")) 
    })
    
    observeEvent(input$makeModel, {
        createModel(drives)
    })
    
    observeEvent(input$makeExport, {
        exportTrack(drives)
        exportSector(drives)
        exportOverallCount(drives)
        exportTimestamp(drives)
    })

    # Drop-down selection box for which data set
    output$choose_track <- renderUI({
        trackDetail <- trackDetail()
        selectizeInput("trackSelection", "Choose a track:", c("Overall", trackDetail[trackDetail$trackType==0,]$name))
    })
    
    output$maintenance <- renderUI({
        if (isLocal())
            mainPanel(
                actionButton("makeModel", "Make Model"),
                actionButton("makeExport", "Export Data"))
        else 
            p("Only possible with local version")
    })
    
    output$walhallaView <- renderTable({
        overallCount() %>% 
            filter(class=="total") %>% 
            select(athletes, activities)
    })
    
    output$walhallaPlot <- renderPlotly({
        a <- overallCount()

        plotColors <- allColors[1:length(unique(a[a$year>0,]$year))]

        ay <- a %>% filter(class=="year") %>% mutate(year=as.factor(year))
        pyat <- plot_ly(data = ay , x=~year, y=~athletes,
                        color=~year, colors = plotColors, type = 'bar', showlegend = FALSE) %>%
            layout(xaxis = list(title = ""))
        pyac <- plot_ly(data = ay , x=~year, y=~activities,
                        color=~year, colors = plotColors, type = 'bar') 

        am <- a %>% filter(class=="month") %>% mutate(month=as.factor(month),year=as.factor(year))
        pmat <- plot_ly(data = am , x=~month, y=~athletes,
                        color=~year, colors = plotColors, type = 'bar', showlegend = FALSE) %>%
            layout(xaxis = list(title = ""), yaxis = list(title = ""))
        pmac <- plot_ly(data = am , x=~month, y=~activities,
                        color=~year, colors = plotColors,
                        type = 'bar', showlegend = FALSE) %>%
            layout(yaxis = list(title = ""))


        subplot(nrows=2,pyat,pmat,pyac,pmac, titleX = TRUE, titleY = TRUE, shareX = TRUE) %>%
            layout(title="Number of athletes and activities / period", margin = list(t=35))
    })
    
    output$infoNumbersDetail <- renderDataTable({
        if (input$infolevel == "Tracks") {
            datatable(trackFiltered() %>% 
                          filter(trackType==0) %>% 
                          select(name, athleteCount, activityCount, effortCount) %>%
                          arrange(name),
                      options = list(paging = FALSE, searching = FALSE)
                      )
        } else {
            datatable(sectorFiltered() %>% 
                          select(short_name, name, athleteCount, activityCount, effortCount) %>%
                          arrange(short_name),
                      options = list(paging = FALSE, searching = FALSE)
            )
        }
    })                   

    output$infoDetailPlot <- renderPlotly({
        if (input$infolevel == "Tracks") {
            title <- "Total Numbers per Track"
            trd <- trackFiltered() %>% 
                filter(trackType==0) %>% 
                select(name, activityCount, effortCount, athleteCount) %>% 
                rename(c(activityCount="activities", effortCount="efforts", athleteCount="athletes")) %>%
                melt(id = "name") %>%
                arrange(name)
        } else {
            title <- "Total Numbers per Sector"
            trd <- sectorFiltered() %>% 
                select(short_name, activityCount, effortCount, athleteCount) %>% 
                rename(c(short_name="name", activityCount="activities", effortCount="efforts", athleteCount="athletes")) %>%
                melt(id = "name") %>%
                arrange(name)
        }
        
        plot_ly(data = trd , x=~name, y=~value, color=~variable, type = 'bar') %>%
            layout(title = title, 
                   xaxis = list(title = input$infolevel),
                   yaxis = list(title = "number of"), 
                   margin = list(t=35, b=70))
    })
    
    output$infoRoundCountsPlot <- renderPlotly({
        if (input$infolevel == "Tracks") {
            title <- "Number of Efforts per activity / Track"
            trc <- merge(trackRoundCounts(), trackFiltered(), by="id") %>% 
                filter(trackType == 0) %>%
                select(name, round, count) %>%
                mutate(round = as.factor(ifelse(round >= 5, "5 and more", round))) %>%
                group_by(name, round) %>% summarize(count = sum(count))
        } else {
            title <- "Number of Efforts per activity / Sector"
            trc <- merge(sectorRoundCounts(), sectorFiltered(), by="id") %>% 
                select(short_name, round, count) %>%
                rename(c(short_name="name")) %>%
                mutate(round = as.factor(ifelse(round >= 5, "5 and more", round))) %>%
                group_by(name, round) %>% summarize(count = sum(count))
        }
        
        plot_ly(data = trc , x=~name, y=~count, color=~round, type = 'bar') %>%
            layout(title = title, 
                   xaxis = list(title = input$infolevel),
                   yaxis = list(title = "number of activities", type = "log"), 
                   margin = list(t=35, b=70))
    })
    
    output$plotTrackEfforts <- renderPlotly({
        trackDetail <- trackDetail()
        trackId <- trackDetail[trackDetail$name==input$trackSelection, "id"]
        if (length(trackId) == 0) trackId = 1
           
        title <- paste0("Efforts in one activity - ", trackDetail[trackDetail$id==trackId,"name"])
        
        t <- trackActivityEfforts() %>% filter(id == trackId) %>% 
            mutate(name = factor(paste(firstname, lastname))) %>% 
            select(name, count, startDateLocal) %>% 
            arrange(desc(count), startDateLocal) %>% 
            mutate(position = 1) %>%
            mutate(position = cumsum(position), rank = as.factor(rank(-count)))
        
        if (!input$trackEffortsCb) {
            t <- t %>% group_by(name) %>%
                summarize(count = max(count), startDateLocal = min(startDateLocal)) %>%
                arrange(desc(count), startDateLocal) %>% 
                mutate(position = 1) %>%
                mutate(position = cumsum(position), rank = as.factor(rank(-count)))
        }
        
        xpos <- min(t$count)/2
        
        plot_ly(data = t, x=~count, y = ~reorder(position, -position),type = 'bar', 
                orientation = 'h', color = ~rank) %>%
            add_annotations(text = ~name, showarrow = FALSE, x = xpos,
                            font = list(family = 'Arial', size = 14, color = 'rgba(245, 246, 249, 1)')) %>%
            add_annotations(text = ~startDateLocal, showarrow = FALSE, x = ~count+1,
                            font = list(family = 'Arial', size = 12)) %>%
            layout(title = title, xaxis = list(title = "efforts"),
                   yaxis = list(title = ""), showlegend = FALSE, margin = list(t=35))
    })
    
    output$plotTrackMonth <- renderPlotly({
        withProgress(message = 'Making plot', value = 0, {
            incProgress(1/3, detail = "Loading Data")
            
            if (input$infolevel == "Tracks") {
                title <- "Efforts per track per month"
                trackMonth <- trackHourFiltered() 
            } else {
                title <- "Efforts per sector per month"
                trackMonth <- sectorHourFiltered() 
            }
            
            trackMonth <- trackMonth %>%
                mutate(month = paste(year(timestamp), substr(100+month(timestamp),2,3), sep="-")) %>%
                group_by(name, month) %>% 
                summarise(count = sum(count))
            
            incProgress(2/3, detail = "Creating plot")
            plot <- plot_ly(data=trackMonth,  x = ~month, y=~count, type="scatter", mode="lines", color=~name) %>%
                layout(title = title, xaxis = list(title = ""),
                yaxis = list(title = "efforts"), showlegend = FALSE, margin = list(t=35,r=35,b=70))
            incProgress(3/3, detail = "plot ready")
        })
        plot
    })

    output$plotTrackDay <- renderPlotly({
        withProgress(message = 'Making plot', value = 0, {
            dataRange <- trackDayReactive$range

            incProgress(1/3, detail = "Loading Data")
            if (input$infolevel == "Tracks") {
                title <- "Efforts per track per day"
                trackDay <- trackHourFiltered()  %>%  
                    mutate(date = date(timestamp), month = year(timestamp)*100+month(timestamp)) %>%
                    filter(month >= dataRange[1], month <= dataRange[2]) %>%
                    group_by(name, date) %>%
                    summarise(count = sum(count))
            } else {
                title <- "Efforts per sector per day"
                trackDay <- sectorHourFiltered()  %>%  
                    mutate(date = date(timestamp), month = year(timestamp)*100+month(timestamp)) %>%
                    filter(month >= dataRange[1], month <= dataRange[2]) %>%
                    group_by(name, date) %>%
                    summarise(count = sum(count))
            }
            
            incProgress(2/3, detail = "Creating plot")
            plot <- plot_ly(data=trackDay,  x = ~date, y=~count, type="scatter", mode="lines", color=~name) %>%
                layout(title = title, xaxis = list(title = ""),
                yaxis = list(title = "efforts"), showlegend = FALSE, margin = list(t=35,r=35,b=70))
            incProgress(3/3, detail = "plot ready")
        })
        
        plot
    })

    output$plotTrackHour <- renderPlotly({
        withProgress(message = 'Making plot', value = 0, {
            dateSelected <- trackHourReactive$date
            daysSelect <- trackHourReactive$days
            dateSelectedEnd <- dateSelected + days(daysSelect)

            incProgress(1/3, detail = "Loading Data")
            if (input$infolevel == "Tracks") {
                title <- "Efforts per track per hour"
                trackDay <- trackHourFiltered()  %>%
                    filter(timestamp >= dateSelected, timestamp < dateSelectedEnd) %>%
                    group_by(name, timestamp) %>%
                    summarise(count = sum(count))
            } else {
                title <- "Efforts per sector per hour"
                trackDay <- sectorHourFiltered()  %>%
                    filter(timestamp >= dateSelected, timestamp < dateSelectedEnd) %>%
                    group_by(name, timestamp) %>%
                    summarise(count = sum(count))
            }

            incProgress(2/3, detail = "Creating plot")
            plot <- plot_ly(data=trackDay,  x = ~timestamp, y=~count, type="scatter", mode="lines", color=~name) %>%
                layout(title = title, xaxis = list(title = ""),
                       yaxis = list(title = "efforts"), showlegend = FALSE, margin = list(t=35,r=35,b=70))
            incProgress(3/3, detail = "plot ready")
        })

        plot
    })
    
    output$plotTrack <- renderLeaflet({
        td <- trackFiltered()
        lng <- 5.40759
        lat <- 52.01522
        lng_min <- 10
        lat_min <- 100
        lng_max <- 0
        lat_max <- 0
        
        muh <- leaflet() %>% addTiles()
        
        for (i in 1:nrow(td)) {
            decode_track <- decode_pl(td[i,]$polyline)

            lng_min <- ifelse(lng_min <= min(decode_track$lon), lng_min, min(decode_track$lon))
            lat_min <- ifelse(lat_min <= min(decode_track$lat), lat_min, min(decode_track$lat))
            lng_max <- ifelse(lng_max >= max(decode_track$lon), lng_max, max(decode_track$lon))
            lat_max <- ifelse(lat_max >= max(decode_track$lat), lat_max, max(decode_track$lat))
            
            muh <- muh %>% addPolylines(lng=decode_track$lon, lat=decode_track$lat, weight = 5-td[i,]$trackType,
                                        color = td[i,]$color, popup = td[i,]$name)
        }
        
        if (nrow(td) == 1) {
            muh <- muh %>% setView(lng = (lng_max+lng_min)/2, lat = (lat_max+lat_min)/2, zoom = 14) 
        } else {
            muh <- muh %>% setView(lng = (lng_max+lng_min)/2, lat = (lat_max+lat_min)/2, zoom = 12) 
        }
        
        if (input$infolevel == "Sectors") {
            sd <- sectorFiltered() 
            start <- coordinates2Lines(sd$short_name,
                                       sd$start_p1_lat, sd$start_p1_lng, 
                                       sd$start_p2_lat, sd$start_p2_lng)
            
            end <- coordinates2Lines(sd$short_name,
                                       sd$end_p1_lat, sd$end_p1_lng, 
                                       sd$end_p2_lat, sd$end_p2_lng)
            
            muh <- muh %>% 
                addPolylines(data = start, popup = ~id, color = "green", weight = 2, opacity = 0.7) %>%
                addPolylines(data = end, popup = ~id, color = "red", weight = 2, opacity = 0.7)
        }

        muh <- muh %>% addLegend(position = "bottomleft", labels=td$name, colors=td$color)
        muh
    })
    
    autoInvalidate <- reactiveTimer(3600000)

    modelLocal <- reactive({
        autoInvalidate()
        reloadModel(0)
        model
    })
    
    output$plotForecast <- renderPlotly({
        model <- modelLocal()
        forecastData <- data.frame(date=rep(seq(as.Date(now()), by=1, length.out = input$days),1,each=24), id = as.numeric(input$track), hour = rep(0:23,input$days))
        forecastData <- forecastData %>% mutate(weekday = weekdays(date), month=month(date) ,time=ymd_h(paste(date,hour)))
        forecastData$count <- predict(model, newdata = forecastData)
        plot_ly(data=forecastData,  x = ~time, y=~count, type="scatter", mode="lines") %>%
            layout(                yaxis = list(rangemode = "tozero"))
    })
})

