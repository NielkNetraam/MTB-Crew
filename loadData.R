require(lubridate)
require("rjson")

model <- NULL
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
reloadModel <- function(seconds) {
    if (difftime(now(), modelLastCheck,units="secs") >= seconds) {
        lModelTimestamp <- loadModelTimestamp()
        if (lModelTimestamp > modelTimestamp) {
            print(paste("(re)load model: version", lModelTimestamp))
            model <<- loadModel() 
            modelTimestamp <<- lModelTimestamp
        }
        modelLastCheck <<- now()
    }
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


