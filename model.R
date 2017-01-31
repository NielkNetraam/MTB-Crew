require(plyr)
require(dplyr)
require(caret)
require(randomForest)
require(RWeka)
require(RWekajars)
require(lubridate)

features <- function(df, holiday1, holidays1, weather) {
    f <- df %>% mutate(id = as.factor(id),
                       month=month(timestamp),
                       monthShift = shiftMonth(month(timestamp)),
                       hour = hour(timestamp), 
                       weekday = as.integer(as.factor(weekdays(timestamp))),
                       weekdayShift = shiftWeekday(as.integer(as.factor(weekdays(timestamp)))),
                       yearDay = yday(timestamp),
                       yearDayShift = shiftYearday(day(timestamp)),
                       summertime = isSummerTime(timestamp),
                       holiday = isHoliday(timestamp, holiday1, holidays1, 0),
                       holidaysNorth = isHoliday(timestamp, holiday1, holidays1, 1),
                       holidaysMiddle = isHoliday(timestamp, holiday1, holidays1, 2),
                       holidaysSouth = isHoliday(timestamp, holiday1, holidays1, 3),
                       holidaysCount = holidaysNorth+holidaysMiddle+holidaysSouth
    )
    
    dayWeather <- weather %>% 
        mutate(date = date(timestamp)) %>% 
        group_by(date) %>% 
        summarise(dayPrecipitation = sum(precipitation))
    
    weather <- weather %>% mutate(date = date(timestamp)) 
    weather <- merge(weather, dayWeather, by="date") %>% select(-date) 
    
    merge(f, weather, by="timestamp") %>% select(-sunshine) 
}

createModelData <- function(trackDetail, trackSector, sectorHourCounts, holiday, holidays, weatherHourly) {
    trackDetail <- trackDetail %>% filter(trackType==0) %>% select(id)
    
    thc <- getTrackOnSectorsHourCounts(trackSector, sectorHourCounts) 
    thcsq <- getTimeFrame(min(thc$timestamp), max(thc$timestamp))
    thcsq <- data.frame(id=rep(trackDetail$id, each=nrow(thcsq)), timestamp=thcsq)
    thcsq <- merge(thcsq, thc, by=c("id","timestamp"), all.x=TRUE)
    thcsq[is.na(thcsq)] <- 0
    thcsq <- features(thcsq, holiday, holidays, weatherHourly)
    
    thcsq
}

createForecastData <- function(trackId, date, days, holiday, holidays, weatherForecast) {
    forecastData <- getTimeFrame(as.Date(date), as.Date(date)+days-1) 
    forecastData <- data.frame(id=trackId, timestamp=forecastData)
    forecastData <- features(forecastData, holiday, holidays, weatherForecast)
    forecastData
}

createModel <- function(data, trackId, method, mtry=NULL, regression=FALSE) {
    dataTrack <- data %>% filter(id==trackId,hour>=6 & hour <=21)
    
    if (trackId == 2) { # track was opened on 2016-03-20
        dataTrack <- dataTrack %>% filter(timestamp >= ymd("2016-03-20"))
    }
 
    predictors <- dataTrack %>% select(-id, -timestamp)
    
    ptm <- proc.time()
    if (method == "rf") {
        predictors <- predictors %>% select(-count)
        if (length(mtry)==0) {
            cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
            registerDoParallel(cluster)
            fitControl <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
            finalModel <- train(predictors,dataTrack$count, method="rf", trControl = fitControl, nodesize = 5, ntree = 200)
        } else
            finalModel <- randomForest(predictors, dataTrack$count, mtry=mtry, nodesize = 5, ntree = 200)
    } else
        if (method=="M5P") {
            finalModel <- M5P(count~., data=predictors, control = Weka_control(R = regression))
        }

    print(proc.time() - ptm)
    
    finalModel
}


forecastTrack <- function(model, forecastData, round=TRUE) {
    predictors <- forecastData %>% select(-timestamp)
    predicted <- predict(model, predictors)
    if (round) {
        ifelse(predicted < 0, 0, round(predicted,1))
    } else 
        predicted
    
        
}

forecast <- function(model, trackName, forecastData)  {
    forecastTrack(model[[trackName]], forecastData, TRUE)
}

createAndExportModel <- function(path) {
    print("createAndExportModel")
    trackDetail <- loadTrackDetail()
    trackDetail <- trackDetail %>% filter(trackType==0)

    holiday <- loadHoliday()
    holidays <- loadHolidays()
    weatherHourly <- loadWeatherHourly()
    trackSector <- loadTrackSector()
    sectorHourCounts <- loadSectorHourCounts()

    data <- createModelData(trackDetail, trackSector, sectorHourCounts, holiday, holidays, weatherHourly)

    model <- list()
    for (trackId in trackDetail$id) {
        print(trackDetail[trackDetail$id==trackId,]$name)
        model[[trackDetail[trackDetail$id==trackId,]$name]] <- 
            createModel(dataFinal2, trackId, method = "M5P", regression = TRUE)
        .jcache(model[[trackDetail[trackDetail$id==trackId,]$name]]$classifier)
    }
    format(object.size(mds2), units = "Kb")
    
    modelTimestamp = now()
    
    lapply(path, function(x) {
        saveRDS(model,          file =  file.path(x,"model.rds"))
        saveRDS(modelTimestamp, file =  file.path(x,"modelTimestamp.rds"))
    })
    print("createAndExportModel ready")
}

createModelOld <- function(path) {
    trackDetail <- loadTrackDetail() %>% filter(trackType==0) %>% select(id)
    sectorHourCounts <- loadSectorHourCounts()
    trackSector <- loadTrackSector()
    
    thc <- getTrackOnSectorsHourCounts(trackSector, sectorHourCounts) 
    thcsq <- getTimeFrame(min(thc$timestamp), max(thc$timestamp))
    thcsq <- data.frame(id=rep(trackDetail$id, each=nrow(thcsq)), timestamp=thcsq)
    thcsq <- merge(thcsq, thc, by=c("id","timestamp"), all.x=TRUE)
    thcsq[is.na(thcsq)] <- 0
    thcsq <- thcsq %>% mutate(month=month(timestamp), 
                              hour = hour(timestamp), 
                              weekday = as.factor(weekdays(timestamp)))
    
    #simple regression tree model to predict the number of ride on track.
    model <- rpart(count ~ id+weekday+hour+month, method="anova", data=thcsq)

    trimmedModel <- model
    trimmedModel$data <- NULL
    trimmedModel$y <- NULL
    trimmedModel$linear.predictors <- NULL
    trimmedModel$weights <- NULL
    trimmedModel$fitted.values <- NULL
    trimmedModel$model <- NULL
    trimmedModel$prior.weights <- NULL
    trimmedModel$residuals <- NULL
    trimmedModel$effects <- NULL
    trimmedModel$where <- NULL
    
    modelTimestamp = now()

    lapply(path, function(x) {
        saveRDS(model,          file =  file.path(x,"model.rds"))
        saveRDS(trimmedModel,   file =  file.path(x,"trimmedModel.rds"))
        saveRDS(modelTimestamp, file =  file.path(x,"modelTimestamp.rds"))
    })
}

forecastOld <- function(model, trackId, days) {
    forecastData <- getTimeFrame(as.Date(now()), as.Date(now())+days-1) 
    forecastData <- data.frame(id=trackId, timestamp=forecastData)
    forecastData <- forecastData %>% mutate(month=month(timestamp), 
                                            hour = hour(timestamp), 
                                            weekday = as.factor(weekdays(timestamp)))
    forecastData$count <- predict(model, newdata = forecastData)
    forecastData
}




#createAndExportModel(c("/Users/mklein/Documents/R/MTBCrew/data"))
#createModel(c("/Users/mklein/Documents/R/MTBCrew/data", "/Users/mklein/Google Drive/MTBCrew"))
#createModel(c("/Users/mklein/Google Drive/MTBCrew"))