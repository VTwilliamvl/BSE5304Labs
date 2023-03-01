# 
# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
objects()  # This will list the objects you have.
rm(list=objects()) # Removes ALL the objects… so be careful here.
#
# What is going to change from use case to use case 
LabNo="/Lab04a"
myflowgage_id="01077400"  # Old Friendly Gage: 0205551460 
#
# What needs to be loaded
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
#
# Getting our organization on for where we want to put
# Data, external programs, and our project files.
# Things are going to get messy if we don't start issolating
# our data files by Lab
#
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)
# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# WOOO HOOO... took me a few hours to find this function!
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'williamvl@vt.edu' ") 
system("git config --global user.name 'William Le' ")
system("git config pull.rebase false")
#
# This week, we discovered some "features" that make removing and 
# re-installing the EcoHydrology Library necessary.
#
setwd(srcdir)
#detach("package:EcoHydRology", unload = TRUE)
# remove.packages("EcoHydRology", lib="~/R/x86_64-pc-linux-gnu-library/4.2")
system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)

setwd(datadir)
#
# Should we do a gage that is easy, or deal with some reality?
#
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",
                         end_date = "2022-03-01")

#
# This is where some folks had issues... they forgot to check their 
# watershed areas per the homework... though there were ways to fix
# it later with lower resolution DEM pull
#
print(paste0("reported Area ",myflowgage$area))
# For most watershed modelling purposes we normalize Q in mm/day for basins
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3

# In the Lab02, we introduced you to a way to quickly get your WX Data 
# for any location in the world way easier than traditional download and
# parsing methods most old people use.
#
source("https://raw.githubusercontent.com/Rojakaveh/FillMissWX/main/FillMissWX.R")
# Remove this if it syncs with the EcoHydrology Version.
WXData=FillMissWX(declat=myflowgage$declat, declon=myflowgage$declon,
                  StnRadius=30,minstns=10,date_min="2010-01-01",
                  date_max="2023-02-01",targElev=myflowgage$elev,
                  method = "IDW",alfa=2)

BasinData=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
TMWB=BasinData

# url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TMWBFuncs.R"
# # This will grab the solution for last weeks Lab03 Homework
# download.file(url,"TMWBFuncs.R")
# file.edit("TMWBFuncs.R")
# source(url)
source("https://raw.githubusercontent.com/VTwilliamvl/BSE5304Labs/main/R/TMWBFuncs.R")

# url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TISnow.R"
# # This will grab the solution for last weeks Lab03 Homework
# source(url)
source("https://raw.githubusercontent.com/VTwilliamvl/BSE5304Labs/main/R/TISnow.R")

#CN Model
source("https://raw.githubusercontent.com/VTwilliamvl/BSE5304Labs/main/R/CNModel.R")

outTMWB = TMWBmodel(TMWBdf = TMWB)
NSE(outTMWB$Qmm, outTMWB$Qpred)

TMWBoptFunc <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  outTMWB=TMWBmodel(TMWBdf = TMWB, fcres = x1, Z = x2, SFTmp = x3, bmlt6 = x4)
  return (1-NSE(outTMWB$Qmm, outTMWB$Qpred))
  
}
#If it does not work, remember to detach! detach(WBData) OR detach(TMWBdf)
lower <- c(.01,300, 1, .1)
upper <- c(.95,3000, 6, 5)
outDEoptm = DEoptim(TMWBoptFunc, lower, upper, 
                    DEoptim.control(NP=80,itermax = 10, F= 1.2, CR = 0.7))

CNoptFunc <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  outCN=CNmodel(CNmodeldf = TMWB, CNavg = x1, IaFrac = x2)
  return (1-NSE(outCN$Qmm, outCN$Qpred))
}
lower <- c(35,.01)
upper <- c(99,.25)
outCEoptm = DEoptim(CNoptFunc, lower, upper, 
                    DEoptim.control(NP=80,itermax = 10, F= 1.2, CR = 0.7))

setwd("~/src/")
install.packages(c("ecohydrology/pkg/SWATmodel/"),repos = NULL)
pacman::p_load(SWATmodel)
setwd(datadir)

AllDays=data.frame(date=seq(min(myflowgage$flowdata$mdate), by = "day", 
                              length.out = max(myflowgage$flowdata$mdate)-min(myflowgage$flowdata$mdate)))
WXData=merge(AllDays,WXData,all=T)
WXData$PRECIP=WXData$P
WXData$PRECIP[is.na(WXData$PRECIP)]=-99
WXData$TMX=WXData$MaxTemp
WXData$TMX[is.na(WXData$TMX)]=-99
WXData$TMN=WXData$MinTemp
WXData$TMN[is.na(WXData$TMN)]=-99
WXData$DATE=WXData$date
#making swat init in the directory with the same name as usgs gagename
build_swat_basic(dirname= myflowgage$gagename, iyr=min(year(WXData$DATE),na.rm=T),
                   nbyr=(max(year(WXData$DATE),na.rm=T)-min(year(WXData$DATE),na.rm=T) +1),
                   wsarea=myflowgage$area, elev=myflowgage$elev, declat=myflowgage$declat,
                   declon=myflowgage$declon, hist_wx=WXData)
# 
# Wait for Dan!
#

build_wgn_file() #wgn func
runSWAT2012() #run swat 

#
# Next steps would be to Delineate and Initialize the basin
# TauDEMBasinInit might be in the github folder!
#