library(ggplot2)
library(dplyr)
library(lubridate)


source("/Users/emilybenson/Documents/GitHub/sapflow22/sapflow_process.r")
source("/Users/emilybenson/Documents/GitHub/sapflow22/meter22.r")
#source("/Users/hkropp/Documents/GitHub/buckthorn/spatial_process.r")
##############################
#### output directory ### ----

outDir <- "/Users/emilybenson/Desktop"



##############################
#### Standard plot argument ----

#plot colors

pt.cols <- c(rgb(0,114,178,155,maxColorValue=255), #hemlock
             rgb(213,94,0,155,maxColorValue=255)) #basswood

pt.cols2 <- c(rgb(0,114,178,50,maxColorValue=255), #hemlock
              rgb(213,94,0,50,maxColorValue=255)) #basswood

pt.cols3 <- c(rgb(0,114,178,maxColorValue=255), #hemlock
              rgb(213,94,0,maxColorValue=255)) #basswood

pt.cols4 <- c(rgb(0,114,178,190,maxColorValue=255), #hemlock
              rgb(213,94,0,190,maxColorValue=255)) #basswood

wd <- 16*2.54
hd <- 4*2.54

#point cex
pt.c <- 4
#line thickness
ln.w <- 3

#line thickness for line only
lln.w <- 4
#tick lwd
tlw <- 3
#axis tick label size
alc <- 2.5
#  axis label size
llc <- 2.5
#legend size
lg.c <- 2
#axis size
ax.c <- 2
#text size
tcx <- 2



##############################
#### daily T per unit leaf ----

hemlock.L.m2.dayS <- hemlock.L.m2.day[hemlock.L.m2.day$doy >= 165, ]
basswood.L.m2.dayS <- basswood.L.m2.day[basswood.L.m2.day$doy >= 165, ]


png(paste0(outDir,"test1.png"), width = 20, height = 7, units = "in", res=300)
par(mai=c(1.5,3,0.5,0.5))
plot(c(0,0), c(0,0), ylim=c(0,0.5),
     xlim=c(190,270),
     axes=FALSE, xlab=" ",
     ylab= " ", xaxs = "i", yaxs="i")
points(hemlock.L.m2.dayS$doy,
       hemlock.L.m2.dayS$mean,
       pch=19, col=pt.cols[1],
       type="b", cex=pt.c, lwd=ln.w)
arrows(hemlock.L.m2.dayS$doy,
       hemlock.L.m2.dayS$mean-hemlock.L.m2.dayS$se,
       hemlock.L.m2.dayS$doy,
       hemlock.L.m2.dayS$mean+hemlock.L.m2.dayS$se,
       code=0, lwd=ln.w, 
       col=pt.cols[1])

points(basswood.L.m2.dayS$doy,
       basswood.L.m2.dayS$mean,
       pch=19, col=pt.cols[2],
       type="b", cex=pt.c, lwd=ln.w)
arrows(basswood.L.m2.dayS$doy,
       basswood.L.m2.dayS$mean-basswood.L.m2.dayS$se,
       basswood.L.m2.dayS$doy,
       basswood.L.m2.dayS$mean+basswood.L.m2.dayS$se,
       code=0, lwd=ln.w, 
       col=pt.cols[2])


legend("topright",
       c("Hemlock",
         "Basswood"),
       col=pt.cols,
       pch=19, lwd=ln.w,
       cex=lg.c, pt.cex=pt.c, bty="n", horiz=TRUE)

axis(1, seq(190,270, by=10), rep(" ", length(seq(190,270, by=10))), cex.axis=ax.c, lwd.ticks=tlw)
axis(2, seq(0,0.5, by=0.1), rep(" ", length(seq(0,0.5, by=0.1))), las=2, cex.axis=ax.c, lwd.ticks=tlw)
mtext( seq(190,270, by=10), at= seq(190,270, by=10), side=1, line=2, cex=alc)
mtext( seq(0,0.5, by=0.1), at= seq(0,0.5, by=0.1), side=2, line=2, cex=alc, las=2)
mtext(expression(paste("Canopy transpiration ")), side=2, line=9, cex=llc)
mtext(expression(paste("(L m"^"-2","day"^"-1",")")), side=2, line=6, cex=llc)
mtext("Day of year", side=1, line=4, cex=llc)
dev.off()

