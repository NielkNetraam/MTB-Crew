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
            "exportTimestamp")
gdFilename <- c("trackDetail.csv", 
             "trackActivityEfforts.csv",
             "trackRoundCounts.csv",
             "trackHourCounts.csv",
             "sectorDetail.csv",
             "sectorRoundCounts.csv",
             "sectorHourCounts.csv",
             "trackSector.csv",
             "overallCount.csv",
             "exportTimestamp.csv")
gdFiletype <- c("csv", 
                "csv", 
                "csv", 
                "csv", 
                "csv", 
                "csv", 
                "csv", 
                "csv",
                "csv",
                "csv")
gdLink <- c("0BxO4UBF1t4EdVE52TU1VOVFNSms",
            "0BxO4UBF1t4EdaHVKNC1YeGFhRGc",
            "0BxO4UBF1t4EdMzdVTGpLRklSOEk", 
            "0BxO4UBF1t4EdQzM3dW9tbU1zZjA", 
            "0BxO4UBF1t4EddWQybFBqTjR1d2M", 
            "0BxO4UBF1t4EdUEI3Wmlna0lPVUU", 
            "0BxO4UBF1t4EdeUV2TkhYckQwV2M", 
            "0BxO4UBF1t4EdbXIzZW90ZUc1S1k",
            "0BxO4UBF1t4EdbHo0b0dEd3NTbDg",
            "0BxO4UBF1t4EdVXFuQVd4UGw4UVU")

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