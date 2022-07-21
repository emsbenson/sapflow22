#### libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)

#### data directory ----
#sapflow and sensor data parent directory
source("/Users/emilybenson/Documents/GitHub/sapflow22/sapflow_process.r")


#### read in data ----

weather <- read.csv("/Users/emilybenson/Desktop/campus_weather/METER/weather22/weatherjuly22.csv")

basswoodmeas <- read.csv("/Users/emilybenson/Downloads/basswoodmeas.csv")
hemlockmeas <- read.csv("/Users/emilybenson/Downloads/hemlockmeas.csv")


#### initial plots ----

sensors <- read.csv("/Users/emilybenson/Downloads/sensordata_061522.csv")

#add sapwood to sensors
#hemlock allometry from "Water use by eastern hemlock (Tsuga canadensis) 
#and black birch (Betula lenta): implications of effects of the hemlock woolly adelgid" by Daley et Al
#basswood allometry by Ewers et. Al

sensors$sd.cm <- ifelse(sensors$Tree.Type == "Hemlock", #if sensors is hemlock, 
                        -0.0133 + (0.1252*sensors$DBH..cm.),
                        -0.7783 + (0.24546*sensors$DBH..cm.))
                        
                
#allometry from basswood and hemlock
basswood
hemlock
#organize data for easier calculations
tabledt <- sapflowR


dtAll <- data.frame(date= rep(tabledt$date, times = 16), 
                    doy = rep(tabledt$doy, times = 16),
                    hour = rep(tabledt$hour, times = 16),
                    DD = rep(tabledt$DD, times = 16),
                    sensor = rep(seq(1,16), each = nrow(tabledt)), 
                    dT = c(tabledt[,3],
                           tabledt[,4],
                           tabledt[,5],
                           tabledt[,6],
                           tabledt[,7],
                           tabledt[,8],
                           tabledt[,9],
                           tabledt[,10],
                           tabledt[,11],
                           tabledt[,12],
                           tabledt[,13],
                           tabledt[,14],
                           tabledt[,15],
                           tabledt[,16],
                           tabledt[,17],
                           tabledt[,18]))



#################
#check for dt outliers
quantile(dtAll$dT, prob=seq(0,1,by=0.001))
#definitely few outliers. 99.5% and above are unusually high
dtAll <- dtAll[dtAll$dT <= quantile(dtAll$dT, prob=0.995),]

#join sensor info into table dt
#make a doy that contains the same night
#so new day actually starts at 5 am not midnight
dtAll$doy5 <- ifelse(dtAll$hour < 5, dtAll$doy-1,dtAll$doy)

night <- dtAll[dtAll$hour < 5|dtAll$hour >= 22,]

#filter night so maximum in day and sensor is provided
maxnight1 <- night %>% 
  group_by(sensor, doy5) %>%
  filter(dT == max(dT),na.rm=TRUE)
#remove duplicate maximums that occur for longer than 15 min
#just take earliest measurement
maxnight <- maxnight1  %>% 
  group_by(sensor, doy5) %>%
  filter(hour == min(hour),na.rm=TRUE)

ggplot(maxnight, aes(doy5,dT, color=sensor))+
  geom_point()
#isolate max and join back into table
maxJoin <- data.frame(sensor=maxnight$sensor, 
                      doy5=maxnight$doy5,
                      maxDT = maxnight$dT)

#join backinto tabledt
dtCalct1 <- left_join(dtAll, maxJoin, by=c("sensor","doy5"))
#join sensor info
dtCalc <- left_join(dtCalct1 , sensors, by=c("sensor"="Sensor.Number"))

#from clearwater

#sap velocity m s-1 (v)
#v = 0.000119*k^1.231
#flow is F (L s-1) = v* A (m2, sapwood area)

#K= (dTmax - dT)/dT if sensor is fully within sapwood

#otherwise correction is:
#dt sap = (dT - b* Dtmax)/a

#a = proportion of probe in sapwood and b=1-a

dtCalc$a <- ifelse(dtCalc$sd.cm >= 3,1,
                   dtCalc$sd.cm/3)

dtCalc$b <- 1 - dtCalc$a 

dtCalc$dTCor <- (dtCalc$dT - (dtCalc$b * dtCalc$maxDT))/dtCalc$a
dtCalc$K <- (dtCalc$maxDT - dtCalc$dTCor)/dtCalc$dTCor
dtCalc$velo <- 0.000119*(dtCalc$K^1.231)


#seperate types
hemlock <- dtCalc[dtCalc$Tree.Type == "Hemlock",]
basswood <- dtCalc[dtCalc$Tree.Type == "Basswood",]


#############
#compare N & S sensors for hemlock
sens5 <- data.frame(date = hemlock$date[hemlock$sensor == 5],
                    veloN = hemlock$velo[hemlock$sensor == 5])

sens6 <- data.frame(date = hemlock$date[hemlock$sensor == 6],
                    veloS = hemlock$velo[hemlock$sensor == 6])

treeD1 <- inner_join(sens5,sens6, by="date")

#compare N & S sensors for hemlock
sens12 <- data.frame(date = hemlock$date[hemlock$sensor == 12],
                     veloN = hemlock$velo[hemlock$sensor == 12])

sens11 <- data.frame(date = hemlock$date[hemlock$sensor == 11],
                     veloS = hemlock$velo[hemlock$sensor == 11])

treeD2 <- inner_join(sens12,sens11, by="date")

sens15 <- data.frame(date = hemlock$date[hemlock$sensor == 15],
                     veloN = hemlock$velo[hemlock$sensor == 15])

sens14 <- data.frame(date = hemlock$date[hemlock$sensor == 14],
                     veloS = hemlock$velo[hemlock$sensor == 14])

treeD3 <- inner_join(sens15,sens14, by="date")

treeDirHem <- rbind(treeD1,treeD2,treeD3)
#check relationship
azim.rel <- lm(treeDirHem$veloS ~ treeDirHem$veloN)
summary(azim.rel)

ggplot(treeDirHem, aes(veloN,veloS))+
  geom_point()+
  geom_abline()


#regression does not differ significantly from S=0 + 1*N

# check basswood


sens1 <- data.frame(date = basswood$date[basswood$sensor == 1],
                    veloN = basswood$velo[basswood$sensor == 1])

sens2 <- data.frame(date = basswood$date[basswood$sensor == 2],
                    veloS = basswood$velo[basswood$sensor == 2])

treeB1 <- inner_join(sens1,sens2, by="date")

sens3 <- data.frame(date = basswood$date[basswood$sensor == 3],
                    veloN = basswood$velo[basswood$sensor == 3])

sens4 <- data.frame(date = basswood$date[basswood$sensor == 4],
                     veloS = basswood$velo[basswood$sensor == 4])

treeB2 <- inner_join(sens3,sens4, by="date")

sens8 <- data.frame(date = basswood$date[basswood$sensor == 8],
                    veloN = basswood$velo[basswood$sensor == 8])

sens9 <- data.frame(date = basswood$date[basswood$sensor == 9],
                    veloS = basswood$velo[basswood$sensor == 9])

treeB3 <- inner_join(sens8,sens9, by="date")

treeBDir <- rbind(treeB1,treeB2, treeB3)

azimB.rel <- lm(treeBDir$veloS ~ treeBDir$veloN)
# summary(azimB.rel)

#ggplot(treeBDir, aes(veloN,veloS))+
 # geom_point()+
  #geom_abline()
#regression does not differ significantly from S=0 + 1*N

#use N for final data

hemlock.tree <- hemlock[hemlock$Direction == "N", ]

buckthorn.tree <- buckthorn[buckthorn$Direction == "N", ]


##############################
#### canopy leaf allometry   ----


#Ash allometry from literature

hemlockmeas$treeArea <- ((hemlockmeas$DBH.cm /2)^2)*pi

#plot(greenwood$dbh.cm,greenwood$sap.area.cm)

#saparea.reg <- lm(greenwood$sap.area.cm ~ greenwood$dbh.cm)
#summary(saparea.reg)


#sap cm2 = -9.6 + 8.854*DBH cm


##############################
#### Canopy calculations   ----

## sapwood area

# ash tree
hemlock.tree$sap.areacm2 <- hemlockmeas$SapwoodArea.cm2

#convert sap area to m2
hemlock.tree$sap.aream2 <- 0.0001*hemlock.tree$sap.areacm2

# buckthorn

#calculate heartwood
buckthorn.tree$Htwd <- buckthorn.tree$DBH.cm - (bbark.calc*2) - (bsap.calc*2)



#calculate sapwood area

buckthorn.tree$sap.areacm2 <- (pi*(((bsap.calc/2)+(buckthorn.tree$Htwd/2))^2))-(pi*((buckthorn.tree$Htwd/2)^2))
buckthorn.tree$sap.aream2 <-  0.0001*buckthorn.tree$sap.areacm2
## tree leaf area
#meadows paper
#LA (m2) = -66.185 +  6.579*DBH in cm
hemlock.tree$LA.m2 <- -66.185 +  6.579*hemlock.tree$DBH.cm

# buckthorn

#Leaf Area/ leaf Mass (cm2 / g)
buckthorn.SLA <- mean(buckthornSLA$area.cm2/buckthornSLA$weight.g)

buckthornLA$Leaf.area <- buckthorn.SLA * buckthornLA$Dry.Leaf.g
buckthornLA$LA.m2 <-  buckthornLA$Leaf.area*0.0001



#check relationship
# lm.log<- lm(log(buckthornLA$LA.m2) ~ log(buckthornLA$DBH.cm))
# summary(lm.log)
# plot(buckthornLA$DBH.cm, buckthornLA$LA.m2)
# plot(log(buckthornLA$DBH.cm), log(buckthornLA$LA.m2))
#regression log(LA (m2)) = -1.058 + 1.828 * log(dbh.cm)

#estimate leaf area in m2
buckthorn.tree$LA.m2 <- exp(-1.058 + (1.828*log(buckthorn.tree$DBH.cm)))

##############################
#### Flow calculations   ----

#flow rate according to clearwater
#F(L s-1) =  v(m s-1)* A (m2)

hemlock.tree$Flow.m3.s <- hemlock.tree$velo * hemlock.tree$sap.aream2

buckthorn.tree$Flow.m3.s <- buckthorn.tree$velo * buckthorn.tree$sap.aream2

#convert to L per secton

hemlock.tree$Flow.L.s <- hemlock.tree$Flow.m3.s * 1000

buckthorn.tree$Flow.L.s <- buckthorn.tree$Flow.m3.s * 1000

#normalize by canopy leaf area
hemlock.tree$Flow.L.m2.s <- hemlock.tree$Flow.L.s /hemlock.tree$LA.m2 

buckthorn.tree$Flow.L.m2.s <- buckthorn.tree$Flow.L.s /buckthorn.tree$LA.m2 

#summarize total per day for each tree
#remove NA
hemlock.treeNN <- hemlock.tree[is.na(hemlock.tree$Flow.L.s)==FALSE,]
#calculate total water use by each tree in a day
#total liters used in 15 min period
hemlock.treeNN$L.p <- hemlock.treeNN$Flow.L.s* 60 *15
#per canopy area
hemlock.treeNN$L.p.m2  <- hemlock.treeNN$L.p/hemlock.treeNN$LA.m2 

#summarize total per day for each tree
#remove NA
buckthorn.treeNN <- buckthorn.tree[is.na(buckthorn.tree$Flow.L.s)==FALSE,]
#calculate total water use by each tree in a day
#total liters used in 15 min period
buckthorn.treeNN$L.p <- buckthorn.treeNN$Flow.L.s* 60 *15
# per canopy area
buckthorn.treeNN$L.p.m2  <- buckthorn.treeNN$L.p/buckthorn.treeNN$LA.m2 


##############################
#### Summary tables    ----

#summary table
#flow L s every 15 min by treatment
ash.Flow <- hemlock.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.s),sd=sd(Flow.L.s), n=length(Flow.L.s))
#flow L m-2 leaf s-1 by 15min
ash.Flow.m2 <- hemlock.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.m2.s),
            sd=sd(Flow.L.m2.s), 
            n=length(Flow.L.m2.s))
#only use time points with at least 3 trees
ash.Flow <- ash.Flow[ ash.Flow$n >=3,]
ash.Flow.m2 <- ash.Flow.m2[ ash.Flow.m2$n >=3,]
ash.Flow$se <- ash.Flow$sd/sqrt(ash.Flow$n)
ash.Flow.m2$se <- ash.Flow.m2$sd/sqrt(ash.Flow.m2$n)

buckthorn.Flow <- buckthorn.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.s),sd=sd(Flow.L.s), n=length(Flow.L.s))

#flow L m-2 leaf s-1 by 15min
buckthorn.Flow.m2 <- buckthorn.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.m2.s),sd=sd(Flow.L.m2.s), n=length(Flow.L.m2.s))

#only use time points with at least 3 trees
buckthorn.Flow <- buckthorn.Flow[ buckthorn.Flow$n >=3,]
buckthorn.Flow.m2 <- buckthorn.Flow.m2[ buckthorn.Flow.m2$n >=3,]
buckthorn.Flow$se <- buckthorn.Flow$sd/sqrt(buckthorn.Flow$n)
buckthorn.Flow.m2$se <- buckthorn.Flow.m2$sd/sqrt(buckthorn.Flow.m2$n)



#total liters per day used by the tree 
ash.L.sens <- hemlock.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p ), n=length(L.p))

ash.L.sens <- ash.L.sens[ ash.L.sens$n == 96,]
#calculate average daily transpiration in each experimental plot

ash.L.day <- ash.L.sens %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
ash.L.day <-ash.L.day [ash.L.day$n >= 3,]
ash.L.day$se <- ash.L.day$sd/sqrt(ash.L.day$n)

buckthorn.L.sens <- buckthorn.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p ), n=length(L.p))

buckthorn.L.sens <- buckthorn.L.sens[ buckthorn.L.sens$n == 96,]
#calculate average daily transpiration in each experimental plot

buckthorn.L.day <- buckthorn.L.sens %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
buckthorn.L.day <-buckthorn.L.day [buckthorn.L.day$n >= 3,]
buckthorn.L.day$se <- buckthorn.L.day$sd/sqrt(buckthorn.L.day$n)

#total liters per day used by the tree normalized per m2 of leaf area
ash.L.m2.sens <- hemlock.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p.m2 ), n=length(L.p.m2))

ash.L.m2.sens <- ash.L.m2.sens[ ash.L.m2.sens$n == 96,]
#calculate average daily transpiration in each experimental plot

ash.L.m2.day <- ash.L.m2.sens %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
ash.L.m2.day <-ash.L.m2.day [ash.L.m2.day$n >= 3,]
ash.L.m2.day$se <- ash.L.m2.day$sd/sqrt(ash.L.m2.day$n)

buckthorn.L.m2.sens <- buckthorn.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p.m2 ), n=length(L.p.m2))

buckthorn.L.m2.sens <- buckthorn.L.m2.sens[ buckthorn.L.m2.sens$n == 96,]
#calculate average daily transpiration in each experimental plot

buckthorn.L.m2.day <- buckthorn.L.m2.sens %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
buckthorn.L.m2.day <-buckthorn.L.m2.day [buckthorn.L.m2.day$n >= 3,]
buckthorn.L.m2.day$se <- buckthorn.L.m2.day$sd/sqrt(buckthorn.L.m2.day$n)


rm(list=setdiff(ls(), c("hemlock.tree","buckthorn.tree",
                        "ash.Flow","buckthorn.Flow",
                        "ash.Flow.m2","buckthorn.Flow.m2",
                        "buckthorn.L.m2.day", "ash.L.m2.day", 
                        "buckthorn.L.day", "ash.L.day",  
                        "weather")))

