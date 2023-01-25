if (!require("pacman")) install.packages("pacman")
if (!require("rnoaa")) install.packages("rnoaa")
if (!require("dylpr")) install.packages("dplyr")
library(rnoaa)
library(dplyr)
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
print("hello world version 2")

system("git config --global user.email 'williamvl@vt.edu' ") 
system("git config --global user.name 'William Le' ")

# data <- data.frame(
# day = as.Date("2019-01-01") + 0:99,
#  tmin = runif(100) + seq(1,100)^2.5 / 10000,
#  price = runif(100) + seq(100,1)^1.5 / 10
#)

#Homework 1:

stns=meteo_distance(
  station_data=ghcnd_stations(),
  lat=38.9001911,
  long=-77.2805236,
  units = "deg",
  radius = 20,
  limit = NULL
)

WXData=meteo_pull_monitors(
  monitors= "USC00448737",
  keep_flags = FALSE,
  date_min = "2016-01-01",
  date_max = NULL,
  var = c("TMAX","TMIN","PRCP") 
)

data <- data.frame(
  day = as.Date("2016-01-01") + 0:2587,
  tmin = WXData$tmin / 10,
  precipitation = WXData$prcp/10
)

data$tmax = WXData$tmax / 10
# coeff <- 6
# A few constants
tminColor <- "#0000ff"
tmaxColor <- "#ff0000"
precipColor <- rgb(0.2, 0.7, 0.4, 1)
dir.create("pdfs")
basestr=format(Sys.time(),"./pdfs/%Y%m%d%H%M")
p1 = ggplot(data, aes(x=day)) +
  geom_line( aes(y=tmin), linewidth=2, color=tminColor) + 
  geom_line( aes(y=tmax), linewidth=2, color=tmaxColor) + 
  geom_bar( aes(y=precipitation), stat="identity", size=.5, fill=precipColor, color= rgb(0.2, 0.7, 0.4, 1), alpha=.4) +
    scale_y_continuous(
      # Features of the first axis
      name = "Temp(°C)",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coeff, name= "Precipitation (mm)")
    ) + 
    # theme_ipsum() +
    theme(
      axis.title.y = element_text(color = "black", size=12),
      axis.title.y.right = element_text(color = precipColor, size=12)
    ) +
    ggtitle("Low and High Temperatures (°C) Compared to Precipitation (mm)")

#Homework 2:
source("https://goo.gl/Cb8zGn")
myflowgage_id="01645762"
myflowgage = get_usgs_gage(myflowgage_id,
                           begin_date="2016-01-01",end_date="2022-31-12")
class(myflowgage)
View(myflowgage$flowdata)
plot(myflowgage$flowdata$mdate,myflowgage$flowdata$flow,
     main=myflowgage$gagename,xlab = "Date",
     ylab="Flow m^3/day",type="l")

WXData <- WXData %>%  
  mutate(date = as.character(date))
combined = inner_join(WXData, myflowgage$flowdata, by= "date")

dataC <- data.frame(
  day = as.Date("2016-01-01") + 0:2567,
  tmin = combined$tmin / 10,
  precipitation = combined$prcp/10
)
dataC$tmax = combined$tmax / 10
dataC$flow = combined$flow

library(ggplot2)
library(gtable)
p2 = ggplot(dataC, aes(x=day)) +
  geom_line( aes(y=tmin), linewidth=2, color=tminColor) + 
  geom_line( aes(y=tmax), linewidth=2, color=tmaxColor) + 
  geom_bar( aes(y=precipitation), stat="identity", size= 0.5, fill=precipColor, color= rgb(0.2, 0.7, 0.4, 1), alpha=.4) + 
  scale_y_continuous(
    # Features of the first axis
    name = "Temp(°C)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name= "Precipitation (mm)"), 
  ) + 
  # theme_ipsum() +
  theme(
    axis.title.y = element_text(color = "black", size=12),
    axis.title.y.right = element_text(color = precipColor, size=12)
  ) +
  ggtitle("Temp and Precip over Days")
plot(p2)

p3 = p2 + ggplot(dataC, aes(x=day)) +
  geom_line(aes(y=flow), linewidth=2, color= "Orange") + theme(
    axis.title.y = element_text(color = "black", size=12),
    axis.title.y.right = element_text(color = precipColor, size=12) 
  ) + scale_y_continuous(name = "Flow (m^3/day)") + ggtitle("Flow vs Days")

filename=paste0(basestr,"graph01.pdf")
pdf(filename) 
plot(p1)
plot(p3)
dev.off()
print("file size")
print(file.size(filename))
print("I finished!")
