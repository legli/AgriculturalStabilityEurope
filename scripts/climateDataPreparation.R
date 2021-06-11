library(ncdf4)
library(raster)
library(rgdal)
library(countrycode)
library(tidyr)


## Coordinates
dfID <- data.frame(x=rep(seq(-179.75,179.75,0.5),360),y=as.numeric(sapply(seq(89.75,-89.75,-0.5),function(i){rep(i,720)})))
dfID$cellID <- 1:nrow(dfID)

## Shapes

# regional
mapRegion <- readOGR("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/Corine2000/u2006_clc2000_v2020_20u1_fgdb/Raster/regions_europe_agriculturalAreasCLC.shp")
# plot(mapRegion)

# national
mapCountry <- readOGR("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/Corine2000/u2006_clc2000_v2020_20u1_fgdb/Raster/countries_europe_agriculturalAreasCLC.shp")
# plot(mapCountry)


## temporal mask from growing calendar

wd <- "C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/ALL_CROPS_ArcINFO_0.5deg_filled"
strFolders <- dir(wd)

lsCrops <- lapply(strFolders, function(folder){
  crop <- strsplit(folder,".",fixed=T)[[1]][1]
  print(crop)
  plantAgg=as.data.frame(raster(paste0(wd,"/",folder,"/plant.start.asc")))  
  totAgg=as.data.frame(raster(paste0(wd,"/",folder,"/tot.days.asc"))) 
  dfGrowing <- cbind(plantAgg,totAgg)
  names(dfGrowing) <- paste0(names(dfGrowing),crop)
  dfGrowing
})
dfCrops <- do.call(cbind,lsCrops)
dfCrops <- cbind(dfID[,c("x","y")],dfCrops)
head(dfCrops)
names(dfCrops)

# get months for which any crop can be grown
vecYear1 <- c(31,59,90,120,151,181,212,243,273,304,334,365)
vecYear2 <- c(365,396,424,455,485,516,546,577,608,638,669,699)
m <- 0 
repeat{
  m <- m+1
  print(m)
  dfCrops[,paste0("month",m)] <- apply(dfCrops,1,function(r){
    res <- NA # no crop can be grown
    if (sum(r[seq(3,52,2)]<=vecYear1[m],na.rm=T)>0|sum((r[seq(3,52,2)]+r[seq(4,52,2)])>vecYear2[m],na.rm=T)>0)
    {res <- 1} # at least one crop can be grown
    res
  })
  if (m==12)
    break
}
sapply(53:64,function(i){table(dfCrops[,i])}) 

names(dfCrops)
dfCrops <- dfCrops[,c(1:2,53:64)]


## extract climate data by spatial and temporal mask
# oben ncdf-files
ncTempAbs <- nc_open("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/climate/air.mon.mean.v501.nc")
ncPrecAbs <- nc_open("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Data/climate/precip.mon.total.v501.nc")

## create dataframes with correct coordinate (start with x = 0.25 to correct for origin)
dfClimateXY <- data.frame(x=rep(c(seq(0.25,179.75,0.5),seq(-179.75,-0.25,0.5)),360),y=as.numeric(sapply(seq(89.75,-89.75,-0.5),function(i){rep(i,720)})))

# stack masked temperature values
lsTemp <- lapply(61:117,function(y){ # 61 refers to 1961
  print(y)
  dfTemp <- dfClimateXY

  for(m in 1:12){
    temp <- ncvar_get(ncTempAbs, attributes(ncTempAbs$var)$names[1],start=c(1,1,(y*12+m)),count=c(720,360,1))
    dfTemp[,paste0("m",m)] <- as.numeric(temp)
  }
  dfTemp <- merge(dfTemp,dfCrops,by=c("x","y"))
  dfTemp[,3:14] <- dfTemp[,3:14]*dfTemp[,15:26] # mask by cropland and growing period
  dfTemp[,paste0("temp",1900+y)] <- rowMeans(dfTemp[,3:14],na.rm=T)
  
  # rasterize
  rasterTemp <- dfTemp[,c("x","y",paste0("temp",1900+y))]
  coordinates(rasterTemp) <- ~ x + y  
  gridded(rasterTemp) <- TRUE
  rasterTemp <- raster(rasterTemp)
  crs(rasterTemp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  rasterTemp
  })
stackTemp <- stack(lsTemp)
names(stackTemp)
plot(stackTemp[[50]])

# stack masked precipitation values
lsPrec <- lapply(61:117,function(y){ # 61 refers to 1961
  print(y)
  dfPrec <- dfClimateXY
  
  for(m in 1:12){
    prec <- ncvar_get(ncPrecAbs, attributes(ncPrecAbs$var)$names[1],start=c(1,1,(y*12+m)),count=c(720,360,1))
    dfPrec[,paste0("m",m)] <- as.numeric(prec)
  }
  dfPrec <- merge(dfPrec,dfCrops,by=c("x","y"))
  dfPrec[,3:14] <- dfPrec[,3:14]*dfPrec[,15:26] # mask by cropland and growing period
  dfPrec[,paste0("prec",1900+y)] <- rowMeans(dfPrec[,3:14],na.rm=T)
  
  # raterize
  rasterPrec <- dfPrec[,c("x","y",paste0("prec",1900+y))]
  coordinates(rasterPrec) <- ~ x + y  
  gridded(rasterPrec) <- TRUE
  rasterPrec <- raster(rasterPrec)
  crs(rasterPrec) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  rasterPrec
})
stackPrec <- stack(lsPrec)
names(stackPrec)
plot(stackPrec[[50]])

## extract climate data in each region
# temp
dfExtractRegionTemp <- raster::extract(stackTemp,mapRegion,fun=mean,na.rm=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE)@data
head(dfExtractRegionTemp)
names(dfExtractRegionTemp)
dfExtractRegionTemp <- dfExtractRegionTemp[,c(1,3:59)]
names(dfExtractRegionTemp) <- c("Region",1961:2017)

dfExtractRegionTempG <-  dfExtractRegionTemp %>% gather(Year, meanTemp,names(dfExtractRegionTemp)[2:58])
head(dfExtractRegionTempG)

write.csv(dfExtractRegionTempG,"datasetsDerived/temperature_regional.csv",row.names = F)

# prec
dfExtractRegionPrec <- raster::extract(stackPrec,mapRegion,fun=mean,na.rm=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE)@data
head(dfExtractRegionPrec)
names(dfExtractRegionPrec)
dfExtractRegionPrec <- dfExtractRegionPrec[,c(1,3:59)]

names(dfExtractRegionPrec) <- c("Region",1961:2017)

# change structure of dataset
dfExtractRegionPrecG <-  dfExtractRegionPrec %>% gather(Year, meanPrec,names(dfExtractRegionPrec)[2:58])
head(dfExtractRegionPrecG)

write.csv(dfExtractRegionPrecG,"datasetsDerived/precipitation_regional.csv",row.names = F)

## extract climate data in each country
# temp
dfExtractCountryTemp <- raster::extract(stackTemp,mapCountry,fun=mean,na.rm=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE)@data
head(dfExtractCountryTemp)
names(dfExtractCountryTemp)

names(dfExtractCountryTemp) <- c("Country",1961:2017)

dfExtractCountryTempG <-  dfExtractCountryTemp %>% gather(Year, meanTemp,names(dfExtractCountryTemp)[2:58])
head(dfExtractCountryTempG)

write.csv(dfExtractCountryTempG,"datasetsDerived/temperature_national.csv",row.names = F)

# prec
dfExtractCountryPrec <- raster::extract(stackPrec,mapCountry,fun=mean,na.rm=TRUE,weights=TRUE,normalizeWeights=TRUE,sp=TRUE)@data
head(dfExtractCountryPrec)
names(dfExtractCountryPrec)

names(dfExtractCountryPrec) <- c("Country",1961:2017)

# change structure of dataset
dfExtractCountryPrecG <-  dfExtractCountryPrec %>% gather(Year, meanPrec,names(dfExtractCountryPrec)[2:58])
head(dfExtractCountryPrecG)

write.csv(dfExtractCountryPrecG,"datasetsDerived/precipitation_national.csv",row.names = F)


rm(list=ls())
