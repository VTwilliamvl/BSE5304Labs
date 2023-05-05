if (!require("pacman")) install.packages("pacman")
pacman::p_load(meteoForecast)

gfsvars = grepVar('precip', service = 'gfs', complete = TRUE)
gfsvarstemp = grepVar('temp', service = 'gfs', complete = TRUE)
gfsvarsflow = grepVar('snow', service = 'gfs', complete = TRUE)
gfsvarsP = grepVar('', service = 'gfs', complete = TRUE)

testDay <- Sys.Date() - 1
today = Sys.Date()

library(lattice)

myflowgage_id="01077400"
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = today)

myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3
source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max=today,targElev=myflowgage$elev,
                  method = "IDW",alfa=2)

## Multiple variables
Precipvars <- getPoint(c(myflowgage$declon, myflowgage$declat), 
                 vars = c("Total_precipitation_surface_Mixed_intervals_Accumulation"), service = "gfs", day = today)
 
GFSPrecip = aggregate(vars, as.Date(time(vars)), sum)

Tempvars <- getPoint(c(myflowgage$declon, myflowgage$declat), 
                     vars = c("Temperature_surface"), service = "gfs", day = today)

GFSMaxTemp = aggregate(Tempvars-273, as.Date(time(Tempvars)), max)
GFSMinTemp = aggregate(Tempvars-273, as.Date(time(Tempvars)), min)

Flowvars <- getPoint(c(myflowgage$declon, myflowgage$declat), 
                     vars = c("Temperature_surface"), service = "gfs", day = today)

plot(GFSMinTemp, ylim = c(276,300))
lines(GFSMaxTemp)



BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")

dir.create("~/pngs")
setwd("~/pngs")
graphdir="~/pngs"
png(paste0(graphdir,"/TempForecast.png"))
plot(GFSMinTemp, ylim = c(0,25), col="blue", 
     xlab = "Day", ylab = "Temperature (C)")
lines(GFSMaxTemp, col="red")
legend("topleft", legend=c("Minimum Temp", "Maximum Temp"),
       col = c("blue", "red"), lty=1:2, cex = 0.8)
dev.off()

png(paste0(graphdir,"/Precip.png"))
plot(GFSPrecip, xlab = "Day", ylab = "Precipitation (mm)")
dev.off()

xyplot(vars)

