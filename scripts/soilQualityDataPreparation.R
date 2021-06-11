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

# national shape
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry <- spTransform(mapCountry,crs(rasterQuality))
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso3c')
mapCountry <- mapCountry[mapCountry@data$Country %in%unique(dfRegion$Country),]


####### read data extract soil quality data 
plot(rasterQuality)
lines(mapRegion)
lines(mapCountry,col="red")

## in each region
dfExtractRegion <- raster::extract(rasterQuality,mapRegion,fun=mean,na.rm=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE)@data
head(dfExtractRegion)

names(dfExtractRegion) <- c("Region","soilQuality")

write.csv(dfExtractRegion,"datasetsDerived/soilQuality_regional.csv",row.names = F)
rm(dfExtractRegion)

## in each country
dfExtractCountry <- raster::extract(rasterQuality,mapCountry,fun=mean,na.rm=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE)@data
head(dfExtractCountry)

dfExtractCountry <- dfExtractCountry[,2:3]
names(dfExtractCountry) <- c("Country","soilQuality")

write.csv(dfExtractCountry,"datasetsDerived/soilQuality_national.csv",row.names = F)



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

## national 
dfSoilGroupCountry <- aggregate(Area~Country+DOMSOI,dfSoilGroup,sum)
nrow(unique(dfSoilGroupCountry[,c("Country","DOMSOI")]))==nrow(dfSoilGroupCountry)
sum(is.na(dfSoilGroupCountry$Area))
sum(dfSoilGroupCountry$Area==0)
dfSoilGroupCountryDiversity <- aggregate(Area~Country,dfSoilGroupCountry,function(i){exp(diversity(i))})
head(dfSoilGroupCountryDiversity)
names(dfSoilGroupCountryDiversity) <- c("Country","soilDiversity")
hist(dfSoilGroupCountryDiversity$soilDiversity)
write.csv(dfSoilGroupCountryDiversity,"datasetsDerived/soilDiversity_national.csv",row.names = F)


rm(list=ls())
