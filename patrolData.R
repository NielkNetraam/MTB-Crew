require(plyr)
require(dplyr)
require(tibble)
require(rjson)
require(RMySQL)
require(reshape)
require(lubridate)

getConnection <- function() {
    dbCon <- dbConnect(MySQL(),user="tester", password="tester", db="patrol_test")
}

getTable <- function(table) {
    dbCon <- getConnection()
    sectors <- dbReadTable(dbCon, table)
    dbDisconnect(dbCon)
    sectors
}

getActivity <- function(id) {
    query <- paste("select id, athlete_id, startDateLocal from activity where id = ", id, sep="")
    activity <- dbGetQuery(dbCon<-getConnection(), query)
    dbDisconnect(dbCon)
    activity
}

getAthlete <- function(id) {
    query <- paste("select id, firstname, lastname, profile from athlete where id = ", id, sep="")
    activity <- dbGetQuery(dbCon<-getConnection(), query)
    dbDisconnect(dbCon)
    activity
}

getCount <- function(table, id) {
    info <-fromJSON(table$info)
    infoDf <- data.frame(id = table[,id],
                         activityCount = info$activityCount,
                         effortCount = info$effortCount,
                         athleteCount = info$athleteCount)
    infoDf
}

getRoundCount <- function(table, id) {
    info <-fromJSON(table$info)
    if (length(info$roundCount) > 0) {
        roundCount<-melt(info$roundCount)
        names(roundCount) <- c("count","round")
        roundCount %>% mutate(id = table[,id]) %>% select(id, round, count)
    } else {
        data.frame()
    }
}

getHourCount <- function(table, id) {
    info <-fromJSON(table$info)
    if (length(info$hourCount) > 0) {
        hourCount<-melt(info$hourCount)
        names(hourCount) <- c("count","timestamp")
        hourCount %>% mutate(id = table[,id], timestamp = ymd_hm(timestamp)) %>% select(id, timestamp, count)
    } else {
        data.frame()
    }
}

getActivityEfforts <- function(table, id) {
    info <-fromJSON(table$info)
    if (length(info$hourCount) > 0) {
        activityEfforts<-melt(info$activityEfforts)
        names(activityEfforts) <- c("count","activity")
        activityEfforts %>% mutate(id = table[,id]) %>% select(id, activity, count) %>% arrange(desc(count))
    } else {
        data.frame()
    }
}

getInfoCounts <- function(table, id, func) {
    df <- data.frame()
    for (i in 1:nrow(table)) {
        df <- rbind(df, func(table[i,], id))
    }
    df
}

readTrackDetail <- function() {
    track <- getTable("track")
    trackInfo <- getTable("track_info")
    trackCounts <- getInfoCounts(table = trackInfo, id = "track_id", func = getCount)
    merge(track, trackCounts, by="id") 
}

readTrackRoundCounts <- function() {
    trackInfo <- getTable("track_info")
    getInfoCounts(table = trackInfo, id = "track_id", func = getRoundCount) %>% 
        mutate(round = as.integer(round), count = as.integer(count))
}

readTrackHourCounts <- function() {
    trackInfo <- getTable("track_info")
    getInfoCounts(table = trackInfo, id = "track_id", func = getHourCount) %>% 
        filter(year(timestamp)>=2016) %>%
        mutate(count = as.integer(count))
}

readTrackActivityEfforts <- function() {
    trackInfo <- getTable("track_info")
    trackActivityEfforts  <- getInfoCounts(table = trackInfo, id = "track_id", func = getActivityEfforts) 
    trackActivityEfforts <- 
        merge(trackActivityEfforts, 
              ldply(trackActivityEfforts$activity, getActivity), by.x="activity", by.y="id")
    trackActivityEfforts <- 
        merge(trackActivityEfforts, 
              ldply(unique(trackActivityEfforts$athlete_id), getAthlete), by.x="athlete_id", by.y="id")
    trackActivityEfforts %>% mutate(startDateLocal = ymd_hms(startDateLocal))
}

exportTrack <- function(path) {
    print("export Track")
    trackDetail <- readTrackDetail()
    trackRoundCounts <- readTrackRoundCounts()
    trackHourCounts  <- readTrackHourCounts()
    trackActivityEfforts  <- readTrackActivityEfforts()
    trackSector  <- readTrackSector()
    
    lapply(path, function(x) write.csv(trackDetail, file.path(x, "trackDetail.csv"), row.names=FALSE))
    lapply(path, function(x) write.csv(trackActivityEfforts, file.path(x, "trackActivityEfforts.csv"), row.names=FALSE))
    lapply(path, function(x) write.csv(trackRoundCounts, file.path(x, "trackRoundCounts.csv"), row.names=FALSE))
    lapply(path, function(x) write.csv(trackHourCounts, file.path(x, "trackHourCounts.csv"), row.names=FALSE))
    lapply(path, function(x) write.csv(trackSector, file.path(x, "trackSector.csv"), row.names=FALSE))
    
    print("Track ready")
}

loadTrackDetail <- function() {
    if (isLocal()) {
        readTrackDetail()
    } else {
        loadFile("trackDetail") %>% 
            mutate(name = as.character(name), color = as.character(color), polyline = as.character(polyline))
    }
}

loadTrackActivityEfforts <- function() {
    if (isLocal()) {
        readTrackActivityEfforts()
    } else {
        loadFile("trackActivityEfforts") %>% 
            mutate(startDateLocal = as.POSIXct(startDateLocal), firstname = as.character(firstname),
                   lastname = as.character(lastname), profile = as.character(profile))       
    }
}

loadTrackRoundCounts <- function() {
    if (isLocal()) {
        readTrackRoundCounts()
    } else {
        loadFile("trackRoundCounts")        
    }
}

loadTrackSector <- function() {
    if (isLocal()) {
        readTrackSector()
    } else {
        loadFile("trackSector")        
    }
}

loadTrackHourCounts <- function() {
    if (isLocal()) {
        readTrackHourCounts()
    } else {
        loadFile("trackHourCounts") %>%
            mutate(timestamp = as.POSIXct(timestamp))
    }
}

readSectorDetail <- function() {
    sector <- getTable("sector")
    sectorInfo <- getTable("sector_info")
    sectorCounts <- getInfoCounts(table = sectorInfo, id = "sector_id", func = getCount)
    sectorDetail <- merge(sector, sectorCounts, by="id") 
}

readSectorRoundCounts <- function() {
    sectorInfo <- getTable("sector_info")
    getInfoCounts(table = sectorInfo, id = "sector_id", func = getRoundCount) %>% 
        mutate(round = as.integer(round), count = as.integer(count))
}

readSectorHourCounts <- function() {
    sectorInfo <- getTable("sector_info")
    getInfoCounts(table = sectorInfo, id = "sector_id", func = getHourCount) %>% 
        mutate(count = as.integer(count)) %>% filter(timestamp >= "2016-01-01")
}

readSectorActivityEfforts <- function() {
    sectorInfo <- getTable("sector_info")
    sectorActivityEfforts  <- getInfoCounts(table = sectorInfo, id = "sector_id", func = getActivityEfforts)
    sectorActivityEfforts <- 
        merge(sectorActivityEfforts, 
              ldply(sectorActivityEfforts$activity, getActivity), by.x="activity", by.y="id")
    sectorActivityEfforts <- 
        merge(sectorActivityEfforts, 
              ldply(unique(sectorActivityEfforts$athlete_id), getAthlete), by.x="athlete_id", by.y="id")
    sectorActivityEfforts %>% mutate(startDateLocal = ymd_hms(startDateLocal))
}

readTrackSector <- function() {
    query <- "select track_id, sector_id, IF(start,TRUE,FALSE) as start, IF(model,TRUE,FALSE) as model from track_sector"
    result <- dbGetQuery(dbCon<-getConnection(), query)
    dbDisconnect(dbCon)
    result
}


exportSector <- function(path) {
    print("export Sector")
    sectorDetail <- readSectorDetail()
    sectorRoundCounts <- readSectorRoundCounts()
    sectorHourCounts  <- readSectorHourCounts()
    sectorActivityEfforts  <- readSectorActivityEfforts()
    
    lapply(path, function(x) write.csv(sectorDetail, file.path(x, "sectorDetail.csv"), row.names=FALSE))
    lapply(path, function(x) write.csv(sectorRoundCounts, file.path(x, "sectorRoundCounts.csv"), row.names=FALSE))
    lapply(path, function(x) write.csv(sectorHourCounts, file.path(x, "sectorHourCounts.csv"), row.names=FALSE))
    print("Sector ready")
}

loadSectorDetail <- function() {
    if (isLocal()) {
        readSectorDetail()
    } else {
        loadFile("sectorDetail")%>% 
            mutate(name = as.character(name), short_name = as.character(short_name))
    }
}

# loadSectorActivityEfforts <- function() {
#     if (isLocal()) {
#         readSectorActivityEfforts()
#     } else {
#         loadFile("sectorActivityEfforts")        
#     }
# }

loadSectorRoundCounts <- function() {
    if (isLocal()) {
        readSectorRoundCounts()
    } else {
        loadFile("sectorRoundCounts")        
    }
}

loadSectorHourCounts <- function() {
    if (isLocal()) {
        readSectorHourCounts()
    } else {
        loadFile("sectorHourCounts") %>%
            mutate(timestamp = as.POSIXct(timestamp))      
    }
}

exportTimestamp <- function(path) {
    print("export ExportTimestamp")
    exportTimestamp = now()
    lapply(path, function(x) write.csv(exportTimestamp, file.path(x, "exportTimestamp.csv"), row.names=FALSE))
    print("ExportTimestamp ready")
}

getOverallCount <- function(id) {
    query <- "SELECT 'total' class, 0 year, 0 month, count(1) activities, count(distinct athlete_id) athletes
                    FROM patrol_test.activity_status a
                    inner join patrol_test.activity s 
                    on a.id = s.id 
                    where rideType = 1
                    and year(startDateLocal) >= 2016
                   union
                   SELECT 'year' class, year(startDateLocal) year, 0 month, count(1) activities, count(distinct athlete_id) athletes
                   FROM patrol_test.activity_status a
                   inner join patrol_test.activity s
                   on a.id = s.id
                   where rideType = 1
                   and year(startDateLocal) >= 2016
                   group by year(startDateLocal)
                   union
                   SELECT 'month' class, year(startDateLocal) year, month(startDateLocal) month, count(1) activities,  count(distinct athlete_id) athletes
                   FROM patrol_test.activity_status a
                   inner join patrol_test.activity s
                   on a.id = s.id
                   where rideType = 1
                   and year(startDateLocal) >= 2016
                   group by year(startDateLocal), month(startDateLocal)
                 "
    activity <- dbGetQuery(dbCon<-getConnection(), query)
    dbDisconnect(dbCon)
    activity
}

exportOverallCount <- function(path) {
    print("export OverallCount")
    overallCount <- getOverallCount()
    
    lapply(path, function(x) write.csv(overallCount, file.path(x, "overallCount.csv"), row.names=FALSE))
    
    print("OverallCount ready")
}

loadOverallCount <- function() {
    if (isLocal()) {
        a <- getOverallCount()
    } else {
        a<- loadFile("overallCount")        
    }
    
    a %>% mutate(activities = as.integer(activities), athletes = as.integer(athletes))
}

loadExportTimestamp <- function() {
    if (isLocal()) {
        now()
    } else {
        a<- loadFile("exportTimestamp")    
        as.POSIXct(a[1,1])
    }
}

#exportOverallCount(c("/Users/mklein/Documents/R/MTB-Crew/MTBCrewModeler/data","/Users/mklein/Google Drive/MTBCrew"))
#exportTrack(c("/Users/mklein/Documents/R/MTB-Crew/MTBCrewModeler/data","/Users/mklein/Google Drive/MTBCrew"))
#exportSector(c("/Users/mklein/Documents/R/MTB-Crew/MTBCrewModeler/data","/Users/mklein/Google Drive/MTBCrew"))
#exportTrack("/Users/mklein/Documents/R/MTB-Crew/MTBCrewModeler")
#exportTimestamp(c("/Users/mklein/Documents/R/MTB-Crew/MTBCrewModeler/data","/Users/mklein/Google Drive/MTBCrew"))