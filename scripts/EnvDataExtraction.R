
library(dplyr)
library(raster)
library(maps)
library(sf)
library(rgdal)
library(rgeos)
library(rerddapXtracto)
library(zoo)

gridSTB.all.df <- read.csv("gridSTB.all.df.csv")

# function to remove NaNs (you end up with some from xtracto function usually)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

# set up for xtracto
dataInfo_SST1d <- rerddap::info('jplMURSST41')
parameter_SST1d <- 'analysed_sst'

dataInfo_SSTanom1d <- rerddap::info('jplMURSST41anom1day')
parameter_SSTanom1d <-'sstAnom'

dataInfo_NPP8d <- rerddap::info('erdMH1pp8day')
parameter_NPP8d <- 'productivity'

xcoord.subset <- gridSTB.all.df$Lon
ycoord.subset <- gridSTB.all.df$Lat

tcoord.subset_current <- gridSTB.all.df$Date
zcoord <- rep(0,length(xcoord.subset))

# extract data at all points
SST1d_current <- rxtracto(dataInfo_SST1d, parameter = parameter_SST1d, xcoord = xcoord.subset, ycoord = ycoord.subset,  
                                 tcoord = tcoord.subset_current) 

SSTanom1d_current <- rxtracto(dataInfo_SSTanom1d, parameter = parameter_SSTanom1d, xcoord = xcoord.subset, ycoord = ycoord.subset,  
                                     tcoord = tcoord.subset_current)

NPP8d_current <- rxtracto(dataInfo_NPP8d, parameter = parameter_NPP8d, xcoord = xcoord.subset, ycoord = ycoord.subset, 
                                 zcoord = zcoord, tcoord = tcoord.subset_current)

# merge with grid df
gridSTB.all.df <- cbind(gridSTB.all.df, SST1d_current$`mean analysed_sst`,SSTanom1d_current$`mean sstAnom`, NPP8d_current$`mean productivity`)
gridSTB.all.df[is.nan(gridSTB.all.df)] <- NA
colnames(gridSTB.training.df)[11:13] <- c("SST_current", "SSTanom_current", "NPP_current") # Check these column numbers!!!!

##############

## UPWELLING INDEX
## Set up polygons to extract RS data
# Chiswell Offshore 
xcoord_Chiswell.off <- c(171.6, 172.5, 172.5, 171.6, 171.6)
ycoord_Chiswell.off <- c(-39.25, -39.25, -40.25, -40.25, -39.25)
# Chiswell onshore 
xcoord_Chiswell.on <- c(172.1, 172.6, 172.6, 172.1, 172.1)
ycoord_Chiswell.on <- c(-40.55, -40.55, -40.8, -40.8, -40.55)

tcoord_2wksprior.range <- c("2009-09-17", "2017-03-31")


# Chiswell offshore MUR SST
SST.MUR_Chiswell.off <- rxtractogon(dataInfo_SST1d, parameter=parameter_SST1d, tcoord = tcoord_2wksprior.range, 
                                    xcoord = xcoord_Chiswell.off, ycoord = ycoord_Chiswell.off)
SST.MUR_Chiswell.off.ar <- SST.MUR_Chiswell.off[[1]]
SST.MUR_Chiswell.off.means <- apply(SST.MUR_Chiswell.off.ar, c(3), FUN=mean, na.rm=TRUE) # get mean SST within polygon for each date 

SST.MUR_Chiswell.on <- rxtractogon(dataInfo_SST1d, parameter=parameter_SST1d, tcoord = tcoord_2wksprior.range, 
                                   xcoord = xcoord_Chiswell.on, ycoord = ycoord_Chiswell.on)
SST.MUR_Chiswell.on.ar <- SST.MUR_Chiswell.on[[1]]
SST.MUR_Chiswell.on.means <- apply(SST.MUR_Chiswell.on.ar, c(3), FUN=mean, na.rm=TRUE) # get mean SST within polygon for each date 

# Make data frame for each, and merge based on date
dates.MUR <- as.character(SST.MUR_Chiswell.off[[6]])
SST.MUR_Chiswell.df <- as.data.frame(cbind(dates.MUR, SST.MUR_Chiswell.off.means, SST.MUR_Chiswell.on.means))
colnames(SST.MUR_Chiswell.df) <- c("DateTime", "mean.SST.MUR_Chiswell.off", "mean.SST.MUR_Chiswell.on")
SST.MUR_Chiswell.df$DateTime  <- as.POSIXct(as.character(SST.MUR_Chiswell.df$DateTime, format="%d-%m-%Y %H:%M:%S"))
SST.MUR_Chiswell.df$Date <- as.Date(SST.MUR_Chiswell.df$DateTime)

# Make SST columns numeric
SST.MUR_Chiswell.df[,c("mean.SST.MUR_Chiswell.off","mean.SST.MUR_Chiswell.on")] <- as.numeric(as.character(unlist(SST.MUR_Chiswell.df[,c("mean.SST.MUR_Chiswell.off","mean.SST.MUR_Chiswell.on")])))

# Calculate upwelling index
SST.MUR_Chiswell.df$ChiswellUpwellingIndex.MUR_2wkprior <- SST.MUR_Chiswell.df$mean.SST.MUR_Chiswell.off - SST.MUR_Chiswell.df$mean.SST.MUR_Chiswell.on


###
write.csv(gridSTB.all.df, "gridSTB.all.df_Dec2020.csv")
write.csv(SST.MUR_Chiswell.df, "SST.MUR_Chiswell.df_Dec2020.csv")
