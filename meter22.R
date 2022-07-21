library(lubridate)
library(ggplot2)
library(dplyr)



DirMeter <- ("/Users/emilybenson/Desktop/campus_weather/METER/weatherjuly22.csv")

meterTable <- read.csv(paste0(DirMeter), skip=3,header=FALSE)




colnames(meterTable) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                          "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                          "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp")


#set up day of year
dateForm <-  ymd_hms(meterTable$Date, tz="America/New_York")

meterTable$year <- year(dateForm) 
meterTable$doy <- yday(dateForm)
meterTable$hour <- hour(dateForm)
meterTable$minute <- minute(dateForm)
meterTable$time <- hour(dateForm)+(minute(dateForm)/60)
meterTable$DD <- meterTable$doy + (meterTable$time/24) 
meterTable$DY <- round(meterTable$year+((meterTable$DD-1)/ifelse(leap_year(meterTable$year),366,365)),6)



MeterMeta <- data.frame(name = c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                                 "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                                 "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp"),
                        units = c("MM/DD/YYYY HH:MM",
                                  "W/m^2","mm","NA","km","degree","m/s","m/s","C",
                                  "kPa","kPa","degree","degree","mm/h","C","kPa","%","mV","kPa","C"))

meterTable2 <- meterTable[meterTable$doy >= 160 & meterTable$year == 2022,]

meterTable2 <- na.omit(meterTable2)

#metertable2se <- meterTable2 %>%
group_by(DD, AirTemp, SolRad, VPD, Precip) %>%
  summarise(VPDse = sd(VPD)/sqrt(length(VPD)))
