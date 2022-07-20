library(dplyr)

# user # 1 = kroppheather, 2 = embenson
userID <- 1

# directory for all sapflow data
sapDir <- c("e:/Google Drive/research/students/Ecohydro-22/sapflux/Sapflux data",
            "/User/emilybenson/Desktop/sapflux/campbell/07_20_22")

fileNames <- c(
  "Sapflow_Status3.csv.backup",
  "Sapflow_Status.dat.5.backup",
  "Sapflow_Status.dat.7.backup",
  "Sapflow_Status.dat.9.backup",
  "Sapflow_Status.dat.11.backup",
  "Sapflow_Status.dat")


# get all files
filesIn <- list()

for(i in 1:6){
  filesIn[[i]] <- read.csv(paste0(sapDir[userID],"/",fileNames[i]),
                           header=FALSE,skip=4,na.strings=c("NAN"))
}
# bind all data frames
sapflowRaw <- do.call( "rbind", filesIn)