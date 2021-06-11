
library(dplyr)
library(zoo)
library(maps)

gridSTB.all.df <- read.csv("gridSTB.all.df_Dec2020.csv")
UpwellingIndex.df <- read.csv("SST.MUR_Chiswell.df_Dec2020.csv")
wind.df <- read.csv("wind.df_20200414.csv")

# tidy up grid dataset a bit
head(gridSTB.all.df)
gridSTB.all.df$X <- NULL
colnames(gridSTB.all.df)[10:12] <- c("SST_current", "SSTanom_current", "NPP_current")

hist(gridSTB.all.df$SST_current)
hist(gridSTB.all.df$SSTanom_current)
hist(gridSTB.all.df$NPP_current) # needs to be transformed...
hist(log(gridSTB.all.df$NPP_current)) # better

gridSTB.all.df$logNPP_current <- log(gridSTB.all.df$NPP_current)

gridSTB.all.df$LonLat <- gridSTB.all.df$Lon * gridSTB.all.df$Lat # This creates interaction term

gridSTB.all.df$Day <- as.numeric(format(as.Date(gridSTB.all.df$Date), "%d")) # Create "day" value, use for cleaning below

# order values by pointID
gridSTB.all.df <- gridSTB.all.df[order(gridSTB.all.df$PointID),]

# Compute weekly and monthly mean values
gridSTB.all.df$SSTweekly <- rollmeanr(na.locf(as.numeric(as.character(gridSTB.all.df$SST_current))), 7, fill=NA)
gridSTB.all.df$SSTweekly[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 7 ] <- NA

gridSTB.all.df$SSTmonthly <- rollmeanr(na.locf(as.numeric(as.character(gridSTB.all.df$SST_current))), 30, fill=NA)
gridSTB.all.df$SSTmonthly[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 30 ] <- NA

gridSTB.all.df$SSTanomweekly <- rollmeanr(na.locf(as.numeric(as.character(gridSTB.all.df$SSTanom_current))), 7, fill=NA)
gridSTB.all.df$SSTanomweekly[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 7 ] <- NA

gridSTB.all.df$SSTanommonthly <- rollmeanr(na.locf(as.numeric(as.character(gridSTB.all.df$SSTanom_current))), 30, fill=NA)
gridSTB.all.df$SSTanommonthly[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 30 ] <- NA

gridSTB.all.df$logNPP_current[1:6] <- 7.016538
gridSTB.all.df$logNPPweekly <- rollmeanr(na.locf(as.numeric(as.character(gridSTB.all.df$logNPP_current))), 7, fill=NA)
gridSTB.all.df$logNPPweekly[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 7 ] <- NA

gridSTB.all.df$logNPPmonthly <- rollmeanr(na.locf(as.numeric(as.character(gridSTB.all.df$logNPP_current))), 30, fill=NA)
gridSTB.all.df$logNPPmonthly[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 30 ] <- NA

# Get values from 1 week prior
#SST
gridSTB.all.df$SST_1wkprior <- lag(gridSTB.all.df$SST_current, n=7)
gridSTB.all.df$SST_1wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 8 ] <- NA

gridSTB.all.df$SSTweekly_1wkprior <- lag(gridSTB.all.df$SSTweekly, n=7)
gridSTB.all.df$SSTweekly_1wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 8 ] <- NA

gridSTB.all.df$SSTmonthly_1wkprior <- lag(gridSTB.all.df$SSTmonthly, n=7)
gridSTB.all.df$SSTmonthly_1wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 8 ] <- NA

#SSTanom
gridSTB.all.df$SSTanom_1wkprior <- lag(gridSTB.all.df$SSTanom_current, n=7)
gridSTB.all.df$SSTanom_1wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 8 ] <- NA

gridSTB.all.df$SSTanomweekly_1wkprior <- lag(gridSTB.all.df$SSTanomweekly, n=7)
gridSTB.all.df$SSTanomweekly_1wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 8 ] <- NA

gridSTB.all.df$SSTanommonthly_1wkprior <- lag(gridSTB.all.df$SSTanommonthly, n=7)
gridSTB.all.df$SSTanommonthly_1wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 8 ] <- NA

#NPP
gridSTB.all.df$logNPP_1wkprior <- lag(gridSTB.all.df$logNPP_current, n=7)
gridSTB.all.df$logNPP_1wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 8 ] <- NA

gridSTB.all.df$logNPPweekly_1wkprior <- lag(gridSTB.all.df$logNPPweekly, n=7)
gridSTB.all.df$logNPPweekly_1wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 8 ] <- NA

gridSTB.all.df$logNPPmonthly_1wkprior <- lag(gridSTB.all.df$logNPPmonthly, n=7)
gridSTB.all.df$logNPPmonthly_1wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 8 ] <- NA


# Get values from 2 weeks prior
#SST
gridSTB.all.df$SST_2wkprior <- lag(gridSTB.all.df$SST_current, n=14)
gridSTB.all.df$SST_2wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 15 ] <- NA

gridSTB.all.df$SSTweekly_2wkprior <- lag(gridSTB.all.df$SSTweekly, n=14)
gridSTB.all.df$SSTweekly_2wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 15 ] <- NA

gridSTB.all.df$SSTmonthly_2wkprior <- lag(gridSTB.all.df$SSTmonthly, n=14)
gridSTB.all.df$SSTmonthly_2wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 15 ] <- NA

#SSTanom
gridSTB.all.df$SSTanom_2wkprior <- lag(gridSTB.all.df$SSTanom_current, n=14)
gridSTB.all.df$SSTanom_2wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 15 ] <- NA

gridSTB.all.df$SSTanomweekly_2wkprior <- lag(gridSTB.all.df$SSTanomweekly, n=14)
gridSTB.all.df$SSTanomweekly_2wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 15 ] <- NA

gridSTB.all.df$SSTanommonthly_2wkprior <- lag(gridSTB.all.df$SSTanommonthly, n=14)
gridSTB.all.df$SSTanommonthly_2wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 15 ] <- NA

#NPP
gridSTB.all.df$logNPP_2wkprior <- lag(gridSTB.all.df$logNPP_current, n=14)
gridSTB.all.df$logNPP_2wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 15 ] <- NA

gridSTB.all.df$logNPPweekly_2wkprior <- lag(gridSTB.all.df$logNPPweekly, n=14)
gridSTB.all.df$logNPPweekly_2wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 15 ] <- NA

gridSTB.all.df$logNPPmonthly_2wkprior <- lag(gridSTB.all.df$logNPPmonthly, n=14)
gridSTB.all.df$logNPPmonthly_2wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 15 ] <- NA


# Get values from 2 weeks prior
#SST
gridSTB.all.df$SST_3wkprior <- lag(gridSTB.all.df$SST_current, n=21)
gridSTB.all.df$SST_3wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 22 ] <- NA

gridSTB.all.df$SSTweekly_3wkprior <- lag(gridSTB.all.df$SSTweekly, n=21)
gridSTB.all.df$SSTweekly_3wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 22 ] <- NA

gridSTB.all.df$SSTmonthly_3wkprior <- lag(gridSTB.all.df$SSTmonthly, n=21)
gridSTB.all.df$SSTmonthly_3wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 22 ] <- NA

#SSTanom
gridSTB.all.df$SSTanom_3wkprior <- lag(gridSTB.all.df$SSTanom_current, n=21)
gridSTB.all.df$SSTanom_3wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 22 ] <- NA

gridSTB.all.df$SSTanomweekly_3wkprior <- lag(gridSTB.all.df$SSTanomweekly, n=21)
gridSTB.all.df$SSTanomweekly_3wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 22 ] <- NA

gridSTB.all.df$SSTanommonthly_3wkprior <- lag(gridSTB.all.df$SSTanommonthly, n=21)
gridSTB.all.df$SSTanommonthly_3wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 22 ] <- NA

#NPP
gridSTB.all.df$logNPP_3wkprior <- lag(gridSTB.all.df$logNPP_current, n=21)
gridSTB.all.df$logNPP_3wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 22 ] <- NA

gridSTB.all.df$logNPPweekly_3wkprior <- lag(gridSTB.all.df$logNPPweekly, n=21)
gridSTB.all.df$logNPPweekly_3wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 22 ] <- NA

gridSTB.all.df$logNPPmonthly_3wkprior <- lag(gridSTB.all.df$logNPPmonthly, n=21)
gridSTB.all.df$logNPPmonthly_3wkprior[gridSTB.all.df$Month == 10 & gridSTB.all.df$Day < 22 ] <- NA


## Upwelling index
# Compute weekly and monthly mean values 
head(UpwellingIndex.df)
colnames(UpwellingIndex.df)[6] <- "Upwelling"

UpwellingIndex.df$Upwellingweekly <- rollmeanr(na.locf(as.numeric(as.character(UpwellingIndex.df$Upwelling))), 7, fill=NA)
UpwellingIndex.df$Upwellingmonthly <- rollmeanr(na.locf(as.numeric(as.character(UpwellingIndex.df$Upwelling))), 30, fill=NA)

# Get daily, weekly, and monthly values at 1, 2 and 3 weeks prior
UpwellingIndex.df$Upwelling_1wkprior <- lag(UpwellingIndex.df$Upwelling, n=7)
UpwellingIndex.df$Upwellingweekly_1wkprior <- lag(UpwellingIndex.df$Upwellingweekly, n=7)
UpwellingIndex.df$Upwellingmonthly_1wkprior <- lag(UpwellingIndex.df$Upwellingmonthly, n=7)

UpwellingIndex.df$Upwelling_2wkprior <- lag(UpwellingIndex.df$Upwelling, n=14)
UpwellingIndex.df$Upwellingweekly_2wkprior <- lag(UpwellingIndex.df$Upwellingweekly, n=14)
UpwellingIndex.df$Upwellingmonthly_2wkprior <- lag(UpwellingIndex.df$Upwellingmonthly, n=14)

UpwellingIndex.df$Upwelling_3wkprior <- lag(UpwellingIndex.df$Upwelling, n=21)
UpwellingIndex.df$Upwellingweekly_3wkprior <- lag(UpwellingIndex.df$Upwellingweekly, n=21)
UpwellingIndex.df$Upwellingmonthly_3wkprior <- lag(UpwellingIndex.df$Upwellingmonthly, n=21)

# merge with grid dataset based on data
UpwellingIndex_ForMerge.df <- UpwellingIndex.df[5:17]
gridSTB.all.df <- merge(gridSTB.all.df, UpwellingIndex_ForMerge.df, by="Date", all.x=TRUE)


## Wind data
head(wind.df)
wind.df <- wind.df[2:3]
colnames(wind.df)[2] <- "WindSpeed"

wind.df$Windweekly <- rollmeanr(na.locf(as.numeric(as.character(wind.df$WindSpeed))), 7, fill=NA)
wind.df$Windsum30d <- rollsumr(na.locf(as.numeric(as.character(wind.df$WindSpeed))), 30, fill=NA)

wind.df$WindSpeed_1wkprior <- lag(wind.df$WindSpeed, n=7)
wind.df$Windweekly_1wkprior <- lag(wind.df$Windweekly, n=7)
wind.df$Windsum30d_1wkprior <- lag(wind.df$Windsum30d, n=7)

wind.df$WindSpeed_2wkprior <- lag(wind.df$WindSpeed, n=14)
wind.df$Windweekly_2wkprior <- lag(wind.df$Windweekly, n=14)
wind.df$Windsum30d_2wkprior <- lag(wind.df$Windsum30d, n=14)

wind.df$WindSpeed_3wkprior <- lag(wind.df$WindSpeed, n=21)
wind.df$Windweekly_3wkprior <- lag(wind.df$Windweekly, n=21)
wind.df$Windsum30d_3wkprior <- lag(wind.df$Windsum30d, n=21)

# merge with grid dataset based on date
wind.df_ForMerge.df <- wind.df
wind.df_ForMerge.df[2:4] <- NULL
gridSTB.all.df <- merge(gridSTB.all.df, wind.df_ForMerge.df, by="Date", all.x=TRUE)

################################################
write.csv(gridSTB.all.df, "gridSTB.all.df_RSdata_Dec2020.csv")

################################################
## Split data into western, central, and eastern datasets
# plot grid locations, overlay box locations from lag analysis
NZ <- map_data("nz")

xcoord_K <- c(172.0839134, 172.3881724, 172.4475603, 172.1435296, 172.0839134)
ycoord_K <- c(-40.72340937, -40.53994884, -40.5943087, -40.78297424, -40.72340937)

xcoord_F <- c(172.7446722, 172.7462092, 172.8381209, 172.8398008, 172.7446722)
ycoord_F <- c(-40.48152264, -40.18924643, -40.18924178, -40.48212295, -40.48152264)

xcoord_CENTRAL <- c(173.2508098, 173.2523467, 173.3442584, 173.3459384, 173.2508098)
ycoord_CENTRAL <- c(-39.99492332, -39.70052883, -39.70052415, -39.995528, -39.99492332)

xcoord_MARU1 <- c(173.2465488, 173.4368066, 173.4383912, 173.2499935, 173.2465488)
ycoord_MARU1 <- c(-39.04809017, -39.04692657, -39.1981604, -39.19875097, -39.04809017)

xcoord_MARU2 <- c(174.1873588, 174.3776166, 174.3792012, 174.1908035, 174.1873588)
ycoord_MARU2 <- c(-39.9594731, -39.95832478, -40.1075695, -40.10815229, -39.9594731)

xcoord_MARU3 <- c(174.2713988, 174.4616566, 174.4632412, 174.2748435, 174.2713988)
ycoord_MARU3 <- c(-40.42686437, -40.425724, -40.57393412, -40.57451286, -40.42686437)

xcoord_MARU4 <- c(173.3043888, 173.4946466, 173.4962312, 173.3078335, 173.3043888)
ycoord_MARU4 <- c(-40.32669439, -40.32555231, -40.47398499, -40.4745646, -40.32669439)

xcoord_MARU5 <- c(172.0846488, 172.2749066, 172.2764912, 172.0880935, 172.0846488)
ycoord_MARU5 <- c(-40.02607566, -40.02492847, -40.17402636, -40.17460857, -40.02607566)

grid.map <- ggplot() +
  geom_polygon(aes(x=xcoord_K, y=ycoord_K), color="dark gray", size=1, fill="light blue") +
  geom_polygon(aes(x=xcoord_F, y=ycoord_F), color="dark gray", size=1, fill="light blue") +
  geom_polygon(aes(x=xcoord_CENTRAL, y=ycoord_CENTRAL), color="dark gray", size=1, fill="light blue") +
  geom_polygon(aes(x=xcoord_MARU1, y=ycoord_MARU1), color="dark gray", size=1, fill="light blue") +
  geom_polygon(aes(x=xcoord_MARU2, y=ycoord_MARU2), color="dark gray", size=1, fill="light blue") +
  geom_polygon(aes(x=xcoord_MARU3, y=ycoord_MARU3), color="dark gray", size=1, fill="light blue") +
  geom_polygon(aes(x=xcoord_MARU4, y=ycoord_MARU4), color="dark gray", size=1, fill="light blue") +
  geom_polygon(aes(x=xcoord_MARU5, y=ycoord_MARU5), color="dark gray", size=1, fill="light blue") +
  geom_point(data = gridSTB.all.df[1:250,], aes(x=Lon, y=Lat)) +
  geom_polygon(data = NZ, aes(x=long, y=lat, group=group), color="black", fill="dark gray") +
  geom_vline(xintercept = 172.71, color = "purple", size = 1.25, linetype = "dashed") +
  geom_vline(xintercept = 174.16, color = "purple", size = 1.25, linetype = "dashed") +
  coord_quickmap(xlim=c(170.5,176), ylim=c(-42.05,-38.75)) + xlab("Longitude") + ylab("Latitude")
grid.map

#ggsave(grid.map, filename = "grid.map_Dec2020.png", device = "png", height=140, width=174, units="mm")

# Create dataset for each region
gridSTB.west.df <- gridSTB.all.df[gridSTB.all.df$Lon < 172.71,]
ggplot() +
  geom_point(data = gridSTB.west.df[1:117,], aes(x=Lon, y=Lat)) +
  geom_polygon(data = NZ, aes(x=long, y=lat, group=group), color="black", fill="dark gray") +
  coord_quickmap(xlim=c(170.5,176), ylim=c(-42.05,-38.75))

gridSTB.central.df <- gridSTB.all.df[gridSTB.all.df$Lon > 172.71 & gridSTB.all.df$Lon < 174.16,]
ggplot() +
  geom_point(data = gridSTB.central.df[1:93,], aes(x=Lon, y=Lat)) +
  geom_polygon(data = NZ, aes(x=long, y=lat, group=group), color="black", fill="dark gray") +
  coord_quickmap(xlim=c(170.5,176), ylim=c(-42.05,-38.75))

gridSTB.east.df <- gridSTB.all.df[gridSTB.all.df$Lon > 174.16,]
ggplot() +
  geom_point(data = gridSTB.east.df[1:40,], aes(x=Lon, y=Lat)) +
  geom_polygon(data = NZ, aes(x=long, y=lat, group=group), color="black", fill="dark gray") +
  coord_quickmap(xlim=c(170.5,176), ylim=c(-42.05,-38.75))

write.csv(gridSTB.west.df, "gridSTB.west.df_RSdata_Dec2020.csv")
write.csv(gridSTB.central.df, "gridSTB.central.df_RSdata_Dec2020.csv")
write.csv(gridSTB.east.df, "gridSTB.east.df_RSdata_Dec2020.csv")
