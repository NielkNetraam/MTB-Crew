require(RCurl)
require(rjson)
require(lubridate)

source("util.R")


hours2df <- function(hours) {
    h  <-  as.data.frame(matrix(unlist(hours), nrow=length(hours), byrow = TRUE) , stringsAsFactors=FALSE)
    names(h) <- names(hours[[1]])
    h[,"datetime"] <- ymd_hms(h[,"datetime"])
    h[,"hour"] <- as.numeric(h[,"hour"])
    h[,"temperature"] <- as.numeric(h[,"temperature"])
    h[,"precipitationmm"] <- as.numeric(h[,"precipitationmm"])
    h[,"precipitation"] <- as.numeric(h[,"precipitation"])
    h[,"sunshinepower"] <- as.numeric(h[,"sunshinepower"])
    h %>% select(datetime, hour, temperature, precipitationmm, precipitation, sunshinepower)
}

getWeatherForecast <- function() {
    w <- getURL(paste0("http://api.buienradar.nl/data/forecast/1.1/all/2757783?btc=",format(now(), format="%Y%m%d%H"),"00"))
    wj <- fromJSON(w)   
    
    sunrise <- NULL;
    sunset <- NULL;
    uh <- matrix(c(5.40759, 52.01522), nrow=1)
    sunrise <- sunriset(uh, now(), direction="sunrise", POSIXct.out=TRUE)
    sunset  <- sunriset(uh, now(), direction="sunset",  POSIXct.out=TRUE)
    
    diffInSeconds <- as.numeric(difftime(sunset$time, sunrise$time, units = "secs"))   
    diffInMins <- as.numeric(difftime(sunset$time, sunrise$time, units = "mins"))    
    
    wfh <- data.frame()
    
    for (day in wj$days) {
        if (length(day$temperature) == 0) next
        if (day$temperature == -999) next
        
        date <- ymd_hms(day$date)
        
        if (length(day$sunrise) > 0) sunrise <- ymd_hms(day$sunrise)
        if (length(day$sunset) > 0) sunset <- ymd_hms(day$sunset)
        
        if (length(day$hours) != 24) {
            wfhd <- data.frame(date = date,
                               hour = 0:23,
                               temperature = -1,
                               precipitationmm = -1,
                               sunshinepower = -1,
                               rain = FALSE,
                               snow = FALSE,
                               td = day$temperature,
                               tdmin = day$mintemperature,
                               tsmax = day$maxtemperature,
                               sunrise = sunrise,
                               sunset = sunset,
                               p = -1,
                               pp = -1,
                               pd = day$precipitationmm
            )
            
            if (length(day$hours) > 0) {
                hours <- hours2df(day$hours)
                pt <- sum(hours[hours$precipitationmm == 0,]$precipitation)
                rt <- day$precipitationmm - sum(hours$precipitationmm)
                precipitationmm <- rt*(24-length(day$hours))/24/12
                rt <- rt*length(day$hours)/24
                rt <- ifelse(rt<0,0,rt)
            } else {
                rt <- 0
                precipitationmm <- round(day$precipitationmm/12,1)
            }
            
            deltaMinutes <- as.numeric(difftime(sunset, sunrise, units = "mins"))  - 120
            deltaMinutes2 <- (24*60) - deltaMinutes
            
            deltaTemp <- NULL
            deltaTemp2 <- NULL
            
            if (length(day$maxtemperature) == 0) {
                deltaTemp <- ((day$maxtemp-day$mintemp)/deltaMinutes) * 60
                deltaTemp2 <- ((day$maxtemp-day$mintemp)/deltaMinutes2) * 60
            } else {
                deltaTemp <- ((day$maxtemperature-day$mintemperature)/deltaMinutes) * 60
                deltaTemp2 <- ((day$maxtemperature-day$mintemperature)/deltaMinutes2) * 60
            }
            
            sunriseHour <- hour(sunrise)
            sunsetHourMin2 <- hour(sunset) - 2
            
            sunshinepower <- 0
            
            for (hour in 0:23) {
                temperature <-0
                
                if (hour <= sunriseHour) {
                    temperature <- day$mintemperature+((sunriseHour-hour)*deltaTemp2)
                } else {
                    if (hour <= sunsetHourMin2) {
                        temperature <- day$mintemperature+((hour-sunriseHour)*deltaTemp)
                    } else {
                        temperature <- day$maxtemperature-((hour-sunsetHourMin2)*deltaTemp2)
                    }
                }
                
                feeltemperature <- temperature+day$feeltemperature-day$temperature
                
                wfhd[wfhd$hour==hour, "temperature"] = temperature
                wfhd[wfhd$hour==hour, "precipitationmm"] = precipitationmm
                wfhd[wfhd$hour==hour, "sunshinepower"] = sunshinepower
            }  
            
            if (length(day$hours) > 0) {
                pt <- sum(hours[hours$precipitationmm == 0,]$precipitation)
                rt <- day$precipitationmm - sum(hours$precipitationmm)
                
                for(hour in hours$hour) {
                    wfhd[wfhd$hour==hour, "temperature"]     = hours[hours$hour==hour,]$temperature
                    wfhd[wfhd$hour==hour, "precipitationmm"] = hours[hours$hour==hour,]$precipitationmm
                    wfhd[wfhd$hour==hour, "sunshinepower"]   = hours[hours$hour==hour,]$sunshinepower
                    wfhd[wfhd$hour==hour, "p"]               = hours[hours$hour==hour,]$precipitation
                    wfhd[wfhd$hour==hour, "pp"]              = ifelse(pt==0,0,round(rt*hours[hours$hour==hour,]$precipitation/pt,1))
                }
            }
        } else {
            hours <- hours2df(day$hours)
            pt <- sum(hours[hours$precipitationmm == 0,]$precipitation)
            rt <- day$precipitationmm - sum(hours$precipitationmm)
            rt <- ifelse(rt<0,0,rt)
            
            wfhd <- data.frame(date = date,
                               hour = hours$hour,
                               temperature = hours$temperature,
                               precipitationmm = hours$precipitationmm,
                               sunshinepower = hours$sunshinepower,
                               rain = FALSE,
                               snow = FALSE,
                               td = day$temperature,
                               tdmin = day$mintemperature,
                               tsmax = day$maxtemperature,
                               sunrise = sunrise,
                               sunset = sunset,
                               p = hours$precipitation,
                               pp = ifelse(pt==0,0,round(rt*hours$precipitation/pt,1)),
                               pd = day$precipitationmm
            )
        }
        wfh <- rbind(wfh, wfhd)    
    }
    
    wfh <- wfh %>% 
        mutate(precipitation = ifelse(precipitationmm>0,precipitationmm,pp),
                          rain = ifelse(precipitationmm > 0 & temperature >= 0, TRUE, FALSE),
                          snow = ifelse(precipitationmm > 0 & temperature <  0, TRUE, FALSE),
               timestamp = ymd_h(paste0(date,hour))) %>%
        dplyr::rename(sunshine=sunshinepower) %>%
        select(timestamp, temperature, precipitation, sunshine, rain, snow)
}




