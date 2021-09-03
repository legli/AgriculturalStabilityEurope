library(raster)
library(rgdal)
library(tidyr)
library(countrycode)
library(foreign)
library(vegan)

####### read data

## soil quality data
rasterQuality <- raster("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/soilQualityCroplands/RES_016_02_Toth2013_sqi_fig5_crop11.tif")

## soil groups
dfSoilGroup <- read.dbf("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/soilGroups/DSMW_DOMSOI_regions_europe_agriculturalAreasCLC_proj.dbf")

# regional shape
mapRegion <- readOGR("spatial/regions_europe.shp")
mapRegion <- spTransform(mapRegion,crs(rasterQuality))
dfRegion <- mapRegion@data
dfRegion$ISO2 <- substr(dfRegion$NUTS_ID,1,2)
dfRegion$Country <- countrycode(dfRegion$ISO2, 'iso2c', 'iso3c')
dfRegion[which(dfRegion$ISO2=="EL"),"ISO2"] <- "GR"
dfRegion[which(dfRegion$ISO2=="UK"),"ISO2"] <- "GB"
dfRegion$Country <- countrycode(dfRegion$ISO2, 'iso2c', 'iso3c')

####### read data extract soil quality data 
plot(rasterQuality)
lines(mapRegion)

## in each region
dfExtractRegion <- raster::extract(rasterQuality,mapRegion,fun=mean,na.rm=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE)@data
head(dfExtractRegion)

names(dfExtractRegion) <- c("Region","soilQuality")

write.csv(dfExtractRegion,"datasetsDerived/soilQuality_regional.csv",row.names = F)
rm(dfExtractRegion)

####### diversity of soil groups

## regional
nrow(unique(dfSoilGroup[,c("NUTS_ID","DOMSOI")]))==nrow(dfSoilGroup)
sum(is.na(dfSoilGroup$Area))
sum(dfSoilGroup$Area==0)
dfSoilGroupDiversity <- aggregate(Area~NUTS_ID+Country,dfSoilGroup,function(i){exp(diversity(i))})
head(dfSoilGroupDiversity)
names(dfSoilGroupDiversity) <- c("Region","Country","soilDiversity")
hist(dfSoilGroupDiversity$soilDiversity)
write.csv(dfSoilGroupDiversity[,c("Region","soilDiversity")],"datasetsDerived/soilDiversity_regional.csv",row.names = F)


rm(list=ls())
