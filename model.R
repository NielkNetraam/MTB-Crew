require(plyr)
require(dplyr)
require(rpart)
require(lubridate)


createModel <- function(path) {
    #load track data
    names <-c("Rhenen", "2", "3", "4", "5", "6" ,"Kwintelooijen", "8", "9", "10", "11", "Amerongen", "13","14","15","Hoge Ginkel","17","18","19","20","21","22","Leersum","Zeist","25","26", "27","28","29","Austerlitz")
    sectorDaily <- read.csv(file.path(path, "data/SectorAggregate.csv"), sep=";")
    sectorDaily <- sectorDaily %>% 
        filter(dmy(date) <= ymd("2016-12-31")) %>%
        mutate(date=dmy(date),
               time=ymd_h(paste(date,hour)),
               name = names[id],
               weekday = weekdays(date))
    
    #simple regression tree model to predict the number of ride on track.
    model <- rpart(count ~ id+weekday+hour, method="anova", data=sectorDaily)
    
    modelTimestamp = now()

    lapply(path, function(x) {
        modelFile <- file(file.path(x,"model.RData"), "wb")
        save(model, file =  modelFile)
        close(modelFile)

        modelTimestampFile <- file(file.path(x,"modelTimestamp.RData"), "wb")
        save(modelTimestamp, file =  modelTimestampFile)
        close(modelTimestampFile)
    })
}

#createModel(c("/Users/mklein/Documents/R/MTB-Crew/MTBCrewModeler/data", "/Users/mklein/Google Drive/MTBCrew"))