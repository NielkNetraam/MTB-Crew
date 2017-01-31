require(sp)
require(maptools)

allColors <- c("indianred1","steelblue1", "palegreen1")

localDrivePath <- "data"
googleDrivePath <- "/Users/mklein/Google Drive/MTBCrew"
drives <- c(localDrivePath, googleDrivePath)

gdFile <- c("trackDetail", 
            "trackActivityEfforts",
            "trackRoundCounts",
            "trackHourCounts",
            "sectorDetail",
            "sectorRoundCounts",
            "sectorHourCounts",
            "trackSector",
            "overallCount",
            "exportTimestamp",
            "model",
            "trimmedModel",
            "modelTimestamp")
gdFilename <- c("trackDetail.csv", 
             "trackActivityEfforts.csv",
             "trackRoundCounts.csv",
             "trackHourCounts.csv",
             "sectorDetail.csv",
             "sectorRoundCounts.csv",
             "sectorHourCounts.csv",
             "trackSector.csv",
             "overallCount.csv",
             "exportTimestamp.csv",
             "model.rds",
             "trimmedModel.rds",
             "modelTimestamp.rds")
gdFiletype <- c("csv", 
                "csv", 
                "csv", 
                "csv", 
                "csv", 
                "csv", 
                "csv", 
                "csv",
                "csv",
                "csv",
                "rds",
                "rds",
                "rds")
gdLink <- c("0BxO4UBF1t4EdVE52TU1VOVFNSms",
            "0BxO4UBF1t4EdaHVKNC1YeGFhRGc",
            "0BxO4UBF1t4EdMzdVTGpLRklSOEk", 
            "0BxO4UBF1t4EdQzM3dW9tbU1zZjA", 
            "0BxO4UBF1t4EddWQybFBqTjR1d2M", 
            "0BxO4UBF1t4EdUEI3Wmlna0lPVUU", 
            "0BxO4UBF1t4EdeUV2TkhYckQwV2M", 
            "0BxO4UBF1t4EdbXIzZW90ZUc1S1k",
            "0BxO4UBF1t4EdbHo0b0dEd3NTbDg",
            "0BxO4UBF1t4EdVXFuQVd4UGw4UVU",
            "0BxO4UBF1t4EdUzZJXzZ6Smk4N0E",
            "0BxO4UBF1t4EdNncwQXR2Q19jTFk",
            "0BxO4UBF1t4EdQWo2VTFwZk1pcFk")

googleDrive <- data.frame(file=gdFile, filename=gdFilename, filetype=as.factor(gdFiletype), link=gdLink)

isLocal <- function() {
    Sys.info()["nodename"] == "M-Kleins-MacBook-Pro.local" 
}

loadFile <- function(file) {
    print(paste("load", file))
    fileResult <- NULL
    
    fileDf <- googleDrive[googleDrive$file==file,]
    if (fileDf$filetype == "csv") {
        destfile <- tempfile()
    
        x = getBinaryURL(paste0("https://docs.google.com/uc?export=download&id=",fileDf$link), followlocation = TRUE, ssl.verifypeer = FALSE)
        writeBin(x, destfile, useBytes = TRUE)
        rm(x)
        fileResult <- read.csv(destfile, sep=",")
    } else {
        if (fileDf$filetype == "rds") {
            destfile <- tempfile()
            
            x = getBinaryURL(paste0("https://docs.google.com/uc?export=download&id=",fileDf$link), followlocation = TRUE, ssl.verifypeer = FALSE)
            writeBin(x, destfile, useBytes = TRUE)
            rm(x)
            fileResult <- readRDS(destfile)
        }
    }

    fileResult
}

monthRange <- function(fy,fm, ty, tm) {
    range <- NULL
    for (y in fy:ty) {
        lfm <- ifelse(y == fy, fm, 1)
        ltm <- ifelse(y == ty, tm, 12)
        for (m in lfm:ltm) {
            r <- paste(y,substr(m+100,2,3),sep="-")
            range <- c(range,r)
        }
    }
    range
}

monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
}

yearMonth <- function(x) {
    x <- as.POSIXlt(x)
    year(x)*100+month(x)
}

coordinates2Lines <- function(id, lat1, lng1, lat2, lng2) {
    df <- data.frame(id)
    rownames(df) <- df$id

    sd2 <- data.frame(id=rep(id,2), lat=c(lat1, lat2), lng=c(lng1, lng2))
    coordinates(sd2) <- c("lng","lat")
    paths <- sp::split(sd2, sd2[["id"]])
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), paths[[1]]$id[1])))
    
    if (length(paths) > 1) {
        for (p in 2:length(paths)) {
            l <- SpatialLines(list(Lines(list(Line(paths[[p]])), paths[[p]]$id[1])))
            sp_lines <- spRbind(sp_lines, l)
        }
    }
    SpatialLinesDataFrame(sp_lines, df)
}

getTimeFrame <- function(timestamp_min, timestamp_max) {
    firstDay <- as.POSIXlt(as.Date(timestamp_min)); firstDay$hour <- 0
    lastDay  <- as.POSIXlt(as.Date(timestamp_max)); lastDay$hour  <- 23
    data.frame(timestamp = seq.POSIXt(firstDay,lastDay,by="hour"))
}

shiftMonth <- function(month) {
    ifelse(month <= 6, month + 6, month-6)
}

shiftWeekday <- function(weekDay) {
    ifelse(weekDay <= 4, weekDay + 3, weekDay-4)
}

shiftYearday <- function(yearDay) {
    ifelse(yearDay <= 183, yearDay + 183, yearDay-183)
}

startSummerTime <- function(year) {
    date = as.Date(ymd(paste(year, "3-31",sep="-")))
    sunday <- weekdays(ymd("2017-1-2"))
    while(weekdays(date) != sunday) 
        date <- date -1

    date;	
}

endSummerTime <- function(year) {
    date = as.Date(ymd(paste(year, "10-31",sep="-")))
    sunday <- weekdays(ymd("2017-1-2"))
    while(weekdays(date) != sunday) 
        date <- date -1
    
    date;	
}

isSummerTime <- function(date) {
    df <- data.frame(date, 
                     isSummerTime = sapply(month(date), 
                                           function(x) { ifelse(x[1]<3 | x[1]>10, FALSE, ifelse(x[1]>3 & x[1]<10, TRUE, NA)) }
                                           )
    )

    if (sum((is.na(df$isSummerTime))) > 0) {
        y <- unique(year(df[is.na(df$isSummerTime), "date"]))
        stse <- data.frame(y, sts=sapply(y, startSummerTime), ste = sapply(y, endSummerTime))
        df[is.na(df$isSummerTime), "isSummerTime"] <-
            sapply(df[is.na(df$isSummerTime),"date"], 
                   function(x) ifelse(month(x) == 3, x >= stse[stse$y==year(x),"sts"], x < stse[stse$y==year(x),"ste"]))
    }
    
    df$isSummerTime
}

isHoliday <- function(date, holiday, holidays, region) {
    ifelse(region == 0, r <- c(0,1,2,3), r <- c(0, region))
    hs <- holidays %>% filter(region %in% r)
    h <- merge(as.data.frame(date), hs) %>% 
        filter(date>=start, date<= end) %>% 
        select(date) %>% 
        unique()
    date %in% holiday$holiday | date %in% h$date
}
