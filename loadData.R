require(lubridate)
require("rjson")

modelTimestamp <- ymd("1970-1-1")
modelLastCheck <- ymd("1970-1-1")

#loads the timestamp of the model (modelTimestamp) stored on google drive
loadModelTimestamp <- function() {
    x = getBinaryURL("https://docs.google.com/uc?export=download&id=0BxO4UBF1t4EdQWo2VTFwZk1pcFk", followlocation = TRUE, ssl.verifypeer = FALSE)
    con <- rawConnection(x, open='rb')
    load(con)
    close(con)
    rm(x)
    modelTimestamp
} 

loadModel <- function() {
    x = getBinaryURL("https://docs.google.com/uc?export=download&id=0BxO4UBF1t4EdUzZJXzZ6Smk4N0E", followlocation = TRUE, ssl.verifypeer = FALSE)
    con <- rawConnection(x, open='rb')
    load(con)
    close(con)
    rm(x)
    model
} 

#loads the model "model" stored on google drive
reloadModel <- function(model=NULL) {
    result <- model
    lModelTimestamp <- loadFile("modelTimestamp")  #loadModelTimestamp()
    
    reload <- FALSE
    if (length(model) == 0)
        reload <- TRUE
    else if (length(model$timestamp) == 0) 
        reload <- TRUE
    else if (lModelTimestamp > model$timestamp) 
        reload <- TRUE
    
    if (reload) {
        print(paste("(re)load model: version", lModelTimestamp))
#        result <- loadFile("trimmedModel")  # loadModel() 
        result <- loadFile("model")  # loadModel() 
        result$timestamp <- lModelTimestamp
    }

    result
}    

loadSectorAggregate <- function() {
    print("load sectorAggregate")
    destfile <- tempfile()
    x = getBinaryURL("https://docs.google.com/uc?export=download&id=0BxO4UBF1t4EdNE5iUE5TZXM0a2s", followlocation = TRUE, ssl.verifypeer = FALSE)
    writeBin(x, destfile, useBytes = TRUE)
    rm(x)
    sectorAggregate <- read.csv(destfile, sep=";")
    sectorAggregate
}


