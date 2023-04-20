if(!exists("LabNo")){
  LabNum <- as.numeric(readline(prompt="What Week is this? "))
  LabNo=paste0("/Lab",LabNum)
}

# Since everything depends on the libraries you install
# it is worthwhile loading them at the beginning
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa)
pacman::p_load(operators,topmodel,DEoptim,soilDB,sp,curl,httr,
               rnoaa,raster,shapefiles,rgdal,elevatr,terra,progress,lubridate)
#
# Getting our organization on for where we want to put
# Data, external programs, and our project files.
# Things are going to get messy if we don't start isolating
# our data files by Lab
#
user=Sys.getenv("USER")
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
#setwd(srcdir)

#detach("package:EcoHydRology", unload = TRUE)
#remove.packages("EcoHydRology", lib="~/R/x86_64-pc-linux-gnu-library/4.2")
#system("svn checkout svn://scm.r-forge.r-project.org/svnroot/ecohydrology/"); 
#install.packages(c("ecohydrology/pkg/EcoHydRology/"),repos = NULL)
pacman::p_load(EcoHydRology)
setwd(datadir)

#
url="https://cran.r-project.org/web/packages/multisensi/vignettes/multisensi-vignette.pdf"
browseURL(url)
pacman::p_load(data.table,multisensi)
# KEEP OPEN AS YOU WILL BE WALKING THROUGH IT FOR LAB	
vignette("multisensi-vignette")
#
# Let’s get started as normal. 

verhulst <- function(K, Y0, a, t) {
  output <- K/(1 + (K/Y0 - 1) * exp(-a * t))
  return(output)
}

T <- seq(from = 5, to = 365, by = 5)
verhulst2 <- function(X, t = T) {
  out <- matrix(nrow = nrow(X), ncol = length(t), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- verhulst(X$K[i], X$Y0[i], X$a[i], t)
  }
  out <- as.data.frame(out)
  names(out) <- paste("t", t, sep = "")
  return(out)
}

n <- 10
set.seed(1234)
X <- data.frame(K = runif(n, min = 100, max = 1000), Y0 = runif(n, min = 1,
                                                                max = 40), a = runif(n, min = 0.05, max = 0.2))
Y <- verhulst2(X)
par(cex.axis = 0.7, cex.lab = 0.8)
plot(T, Y[1, ], type = "l", xlab = "Time", ylab = "Population size",
     ylim = c(0, 1000))
for (i in 2:n) {
  lines(T, Y[i, ], type = "l", col = i)
}

library(multisensi)
verhulst.seq <- multisensi(model=verhulst2, reduction=NULL, center=FALSE,
                           design.args = list( K=c(100,400,1000), Y0=c(1,20,40), a=c(0.05,0.1,0.2)))

print(verhulst.seq, digits = 2)

# color palettes: rainbow, heat.colors, terrain.colors, topo.colors,
# cm.colors
plot(verhulst.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Time in half-decades.")
plot(verhulst.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Time in half-decades.")

X <- expand.grid(K=c(100,400,1000), Y0=c(1,20,40), a=c(0.05,0.1,0.2))
Y <- verhulst2(X) ## this part can be performed outside R if necessary
verhulst.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE)
## [*] Analysis + Sensitivity Indices

## Note that this old syntax of multisensi still works:
## verhulst.gsi <- gsi(formula=2, Y, X)
verhulst.pca <- multisensi(design=X, model=Y, reduction=basis.ACP, scale=FALSE)

summary(verhulst.pca, digits = 2)

plot(verhulst.pca, graph = 1)

plot(verhulst.pca, graph = 2)     

plot(verhulst.pca, graph = 3)

verhulst.poly <- multisensi(design = X, model = Y, reduction = basis.poly,
                            dimension = 0.99, center = FALSE, scale = FALSE, cumul = FALSE,
                            basis.args = list(degree=6, x.coord=T), analysis = analysis.anoasg,
                            analysis.args = list(formula=2, keep.outputs=FALSE))

summary(verhulst.poly, digits=2)

verhulst.bspl <- multisensi(design=X, model=Y, reduction=basis.bsplines,
                            dimension=NULL, center=FALSE, scale=FALSE,
                            basis.args=list(knots=10, mdegree=3), cumul=FALSE,
                            analysis=analysis.anoasg,
                            analysis.args=list(formula=2, keep.outputs=FALSE))

library(sensitivity)

m <- 10000
Xb <- data.frame(K = runif(m, min = 100, max = 1000), Y0 = runif(m, min = 1,
                                                                 max = 40), a = runif(m, min = 0.05, max = 0.2))
verhulst.seq.sobol <- multisensi(design = sobol2007, model = verhulst2,
                                 reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                 design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                 analysis.args = list(keep.outputs = FALSE))

print(verhulst.seq.sobol, digits = 2)

verhulst.seq.fast <- multisensi(design = fast99, model = verhulst2,
                                center = FALSE, reduction = NULL, analysis = analysis.sensitivity,
                                design.args=list( factors=c("K","Y0","a"), n=1000, q = "qunif",
                                                  q.arg = list(list(min=100, max=1000), list(min=1, max=40),
                                                               list(min = 0.05, max = 0.2))),
                                analysis.args=list(keep.outputs=FALSE))

print(verhulst.seq.fast,digits=2)

EcoHydRology::Solar

#
J <- seq(from = 1, to = 365, by = 5)
# Solar(lat, Jday, Tx, Tn, albedo=0.2, forest=0, slope=0, aspect = 0,
#      units="kJm2d")
# Note that the EcoHydRology::Solar() function is for specific days, 
# as such, we will want to create a function to loop through our period
# of interest:
Solar_Looped <- function(X, Jday = J) {
  out <- matrix(nrow = nrow(X), ncol = length(Jday), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- Solar(lat=X$lat[i],
                      Jday=Jday, Tx=X$Tx[i], 
                      Tn=(X$Tx[i]-X$Trange[i]), 
                      X$slope[i],X$aspect[i],units="Wm2")
  }
  out <- as.data.frame(out)
  names(out) <- paste("Jday", Jday, sep = "")
  return(out)
}

# A sample of with graph, from the vignette, we continue to build a 
# dataframe for our specific case with random uniform numbers for the 
# Tx, Tn (Tx - Trange), slope, and aspect.
# 
n <- 10
set.seed(1234)
X <- data.frame(Tx = runif(n, min = 5, max = 30), Trange = runif(n, min = 2,
                                                                   max = 16), slope = runif(n, min = 0.0, max = 0.2),
                  aspect = runif(n, min = 0.0, max = 0.2),
                  lat=runif(n, min = 0.0, max = 1.1))  # 1.1 radians lat is where?

View(X)
#
Y <- Solar_Looped(X,Jday = J)
#
# You can ignore all the warnings, remember Errors=bad, warnings=not so much 
# So lets move on and build our summary graph
par(cex.axis = 0.7, cex.lab = 0.8)
plot(J, Y[1, ], type = "l", xlab = "Day of Year", 
       ylab = "Surface Short Wave Rad(W/m^2)")
for (i in 2:n) {
  lines(J, Y[i, ], type = "l", col = i)
}

  Solar_Looped.seq <- multisensi(model=Solar_Looped, reduction=NULL, center=FALSE,
                                 design.args = list( Tx = c(5,15,25), 
                                                     Trange = c(2,9,16), 
                                                     slope = c(0.1,0.2,0.3),
                                                     aspect = c(0.1,.5,1.0),
                                                     lat=c(0.1,.77,1.1)))

  print(Solar_Looped.seq, digits = 2)
#
# 3.2 Graphical representation of sensitivity indices
#
  dev.off() # Clean up previous par()
  plot(Solar_Looped.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)#normalized the upper subplot shows the extreme (tirets), #inter-quartile (grey) and median (bold line) output values
  title(xlab = "Days of the Year.")
  plot(Solar_Looped.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
  title(xlab = "Days of the Year.")


#
# Take note of section 3.3 Calculating simulations apart, as there are 
# several ways to invoke the multisensi() function.
# First by building our X (design input) and Y (model run on X). Here 
# here is us doing the same sensitivities by passing previously run 
# models (Y)
  X <- expand.grid(Tx = c(5,15,25), 
                   Trange = c(2,9,16), 
                   slope = c(0.1,0.2,0.3),
                   aspect = c(0.1,.5,1.0),
                   lat=c(0.1,.77,1.1))
# Look at our input
  head(X,10)
  Y <- Solar_Looped(X,Jday=J) # can be performed outside R if necessary
# Look at our model output
  head(Y,10)
# Notice on the next line that “model=Solar_Looped” is replaced with our 
# model output “Y” and we add in the input into “design=X”. This 
# is exactly the same as above, though with the model run 
# external to the multisensi() function:
  Solar_Looped.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE) 



# 4 Multivariate sensitivity analysis based on PCA
# read the vignette, though note we are using the multisensi() function
# to run our model (i.e. no “design” variable, and model=Solar_Looped)
  Solar_Looped.pca <- multisensi(model=Solar_Looped, reduction=basis.ACP, scale=FALSE,
                                 design.args = list( Tx = c(5,15,25), 
                                                     Trange = c(2,9,16), 
                                                     slope = c(0.1,0.2,0.3),
                                                     aspect = c(0.1,.5,1.0),
                                                     lat=c(0.1,.77,1.1)))

  summary(Solar_Looped.pca, digits = 2)
# 4.2 Graphical representation for PCA based analysis with 
# explanation in vignette. These graphs require the plot window to be larger
# and might give "Error in plot.new() : figure margins too large". 
# If so expand the plot window.
  dev.off()
  plot(Solar_Looped.pca, graph = 1)
  plot(Solar_Looped.pca, graph = 2)
  plot(Solar_Looped.pca, graph = 3)
#
# 5.1 Polynomial reduction of the multivariate output
# Skip 5.1 Polynomial reduction for now and move on to
# 6 Alternative methods of sensitivity analysis
# 6.1 With Sobol2007 implemented in the package sensitivity
# 
  library(sensitivity)
  m <- 10000
  Xb <- data.frame(Tx = runif(m, min = 5, max = 30), 
                   Trange = runif(m, min = 2,max = 16), 
                   slope = runif(m, min = 0.0, max = 0.2),
                   aspect = runif(m, min = 0.0, max = 0.2),
                   lat=runif(m, min = 0.0, max = 1.1))

  Solar_Looped.seq.sobol <- multisensi(design = sobol2007, model = Solar_Looped,
                                       reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                       design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                       analysis.args = list(keep.outputs = FALSE))
#
# Note, this is a good time time get a drink of water and/or pee as 
# it is running the function m=10,000 times (a few minutes).
#
  print(Solar_Looped.seq.sobol, digits = 2)
  dev.off()
  plot(Solar_Looped.seq.sobol, normalized = TRUE, color = terrain.colors)
  

  # NetRad2 <- function(X, t = T) {
  #   out <- matrix(nrow = nrow(X), ncol = length(t), NA)
  #   for (i in 1:nrow(X)) {
  #     out[i, ] <- NetRad(lat=X$lat[i], 
  #                        Tx=X$Tx[i], 
  #                        Tn=(X$Tx[i]-X$Trange[i]),
  #                        albedo=X$albedo[i],
  #                        slope=X$slope[i],
  #                        aspect=X$aspect[i],
  #                        Jday=t, units = "Wm2")
  #     
  #   }
  #   out <- as.data.frame(out)
  #   names(out) <- paste("t", t, sep = "")
  #   return(out)
  # }
  
  NetRad2 <- function(X, t = T) {
    out <- matrix(nrow = nrow(X), ncol = length(t), NA)
    for (i in 1:nrow(X)) {
      out[i, ] <- NetRad(lat=X$lat[i], 
                         Tx=X$Tx[i], 
                         Tn=(X$Tx[i]-X$Trange[i]),
                         albedo=0.18,
                         slope=0,
                         aspect=0,
                         Jday=t, units = "Wm2")
      
    }
    out <- as.data.frame(out)
    names(out) <- paste("t", t, sep = "")
    return(out)
  }
    # X <- expand.grid(lat=c(0.1,.77,1.1), #c(0*pi/180,55*pi/180,80*pi/180)
  #                  Tx = c(5,15,25),
  #                  Trange = c(2,9,16),
  #                  albedo= c(0.18),
  #                  slope = c(0),
  #                  aspect = c(0))
  
  X <- expand.grid(lat=c(0.1,.77,1.1), #c(0*pi/180,55*pi/180,80*pi/180)
                   Tx = c(5,15,25),
                   Trange = c(2,9,16))
  # Look at our input
  head(X,10)
  Y <- NetRad2(X, t = J) # can be performed outside R if necessary
  # Look at our model output
  head(Y,10)
  NetRad2.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE) 
  
  NetRad2.pca <- multisensi(design = X, model=Y, reduction=basis.ACP, center=FALSE)
  
  m <- 10000
  Xb <- data.frame(lat=runif(m, min = 0.1, max = 1.1),
                   Tx = runif(m, min = 10, max = 25), 
                   Trange = runif(m, min = 2,max = 16))
  
  NetRad2.seq.sobol <- multisensi(design = sobol2007, model = NetRad2,
                                 reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                 design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m,], nboot = 100),
                                 analysis.args = list(keep.outputs = FALSE))
  
  print(NetRad2.seq.sobol, digits = 2)
  dev.off()
  plot(NetRad2.seq.sobol, normalized = TRUE, color = terrain.colors)
  plot(NetRad2.seq.sobol, normalized = FALSE, color = terrain.colors)
  
  PET_fromTemp <- function (Jday, Tmax_C, Tmin_C, lat_radians, AvgT = (Tmax_C + Tmin_C)/2, albedo = 0.18, TerrestEmiss = 0.97, aspect = 0, slope = 0, forest = 0, PTconstant=1.26, AEparams=list(vp=NULL, opt="linear"))
  {
    cloudiness <- EstCloudiness(Tmax_C, Tmin_C)
    DailyRad <- NetRad(lat_radians, Jday, Tmax_C, Tmin_C, albedo, forest, slope, aspect, AvgT, cloudiness, TerrestEmiss, AvgT, AEparams=AEparams)
    potentialET <- PTpet(DailyRad, AvgT, PTconstant)
    potentialET[which(potentialET < 0)] <- 0
    potentialET[which(Tmax_C == -999 | Tmin_C == -999)] <- (-999)
    return(potentialET)
  }
  
  PET_fromTemp2 <- function(X, t = T) {
    out <- matrix(nrow = nrow(X), ncol = length(t), NA)
    for (i in 1:nrow(X)) {
      out[i, ] <- PET_fromTemp(Jday=t,
                         Tmax_C=X$Tx[i], 
                         Tmin_C=X$Tn[i],
                         lat_radians=X$lat)
      
    }
    out <- as.data.frame(out)
    names(out) <- paste("t", t, sep = "")
    return(out)
  }
  
  X <- expand.grid(lat=c(0.0*pi/180,0.7*pi/180,1.1*pi/180), #c(0*pi/180,55*pi/180,80*pi/180)
                   Tx = c(5,15,25),
                   Tn = c(-5,0,5))
  # Look at our input
  head(X,10)
  Y <- PET_fromTemp2(X, t = J) # can be performed outside R if necessary
  # Look at our model output
  head(Y,10)
  
  par(cex.axis = 0.7, cex.lab = 0.8)
  plot(J, Y[1, ], type = "l", xlab = "Day of Year", 
       ylab = "Evapotranspiration")
  for (i in 2:n) {
    lines(J, Y[i, ], type = "l", col = i)
  }
  
  PET_fromTemp2.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE) 
  
  PET_fromTemp2.pca <- multisensi(design = X, model=Y, reduction=basis.ACP, center=FALSE)
                                                     
    m <- 10000
  Xb <- data.frame(lat=runif(m, min = 0.0, max = 1.1*pi/180),
                   JDay = runif(m, min = 1, max = 365),
                   Tx = runif(m, min = 10, max = 25), 
                   Tn = runif(m, min = -5,max = 10))
  
  PET_Looped.seq.sobol <- multisensi(design = sobol2007, model = PET_fromTemp2,
                                       reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                       design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                       analysis.args = list(keep.outputs = FALSE))
  
  print(PET_Looped.seq.sobol, digits = 2)
  dev.off()
  plot(PET_Looped.seq.sobol, normalized = TRUE, color = terrain.colors)
  plot(PET_Looped.seq.sobol, normalized = FALSE, color = terrain.colors)
  