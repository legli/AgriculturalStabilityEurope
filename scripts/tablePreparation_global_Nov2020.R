library(rgdal)
library(tidyr)
library(vegan)
library(countrycode)
library(rworldmap) 

######## DATA PREPARATION

#### currently exisiting regions  
levelMap <- getMap()
plot(levelMap)
vecLevel <- levelMap@data$ISO3 

vecEurope <- c("Austria","Belgium","Belgium-Luxembourg","Bulgaria","Croatia",
               "Cyprus","Czechia","Czechoslovakia","Denmark","Estonia",
               "Finland","France","Germany","Greece",
               "Hungary","Ireland","Italy","Latvia",
               "Lithuania","Luxembourg","Malta","Netherlands",
               "Poland","Portugal","Romania","Slovakia",
               "Slovenia","Spain","Sweden","United Kingdom")

#### Cropland data
dfCropland <- read.csv("datasets/cropland_global.csv")
head(dfCropland)

dfCropland <- dfCropland[which(dfCropland$Area%in%vecEurope),]
dfCropland$Value <- dfCropland$Value*1000 # change to ha

# only keep target years
dfCropland <- dfCropland[which(dfCropland$Year%in%1978:2017),]
names(dfCropland)[12] <- "croplandArea"
unique(dfCropland$Area) 

# target columns
dfCropland <- dfCropland[,c("Area","Year","croplandArea")]
nrow(unique(dfCropland[,c("Area","Year")])) == nrow(dfCropland) # check duplicates

#### Fertilizer data
dfFertilizerArchive <- read.csv("datasets/fertilizerArchive_global.csv")
names(dfFertilizerArchive)[4] <- "Area"
dfFertilizerArchive <- dfFertilizerArchive[which(dfFertilizerArchive$Area%in%vecEurope),]
unique(dfFertilizerArchive$Area)

dfFertilizerNew <- read.csv("datasets/fertilizerNew_global.csv")
dfFertilizerNew <- dfFertilizerNew[which(dfFertilizerNew$Area%in%vecEurope),]
unique(dfFertilizerNew$Area)

dfFertilizer <- rbind(dfFertilizerArchive[,c("Area","Year","Item","Value")],dfFertilizerNew[,c("Area","Year","Item","Value")])
dfFertilizer <- dfFertilizer[which(dfFertilizer$Item=="Nitrogenous fertilizers"| dfFertilizer$Item=="Nutrient nitrogen N (total)"),]
dfFertilizer <- dfFertilizer[,c("Area","Year","Value")]
names(dfFertilizer)[3] <- "Nitrogen"
head(dfFertilizer)

# only keep target years
dfFertilizer <- dfFertilizer[which(dfFertilizer$Year%in%1978:2017),]
unique(dfFertilizer$Area)

nrow(unique(dfFertilizer[,c("Area","Year")])) == nrow(dfFertilizer) # check duplicates

#### Irrigation
dfIrrigation <- read.csv("datasets/irrigationEquippedArea_global.csv")
head(dfIrrigation)

dfIrrigation <- dfIrrigation[which(dfIrrigation$Area%in%vecEurope),]

# only keep target years
dfIrrigation <- dfIrrigation[which(dfIrrigation$Year%in%1978:2017),]
names(dfIrrigation)[12] <- "Irrigation"
unique(dfIrrigation$Area) # Ireland and Luxemburg missing!

# target columns
dfIrrigation <- dfIrrigation[,c("Area","Year","Irrigation")]
nrow(unique(dfIrrigation[,c("Area","Year")])) == nrow(dfIrrigation) # check duplicates



######## CALCULATE VARIABLES FOR THE 5 TIME PERIOS

# get regions across datasets

## summarize per time frame 
lsAll <- lapply(vecEurope,function(lev){
  # detrend yields
  show(as.character(lev))
  lsAggregate <- lapply(c(1978,1988,1998,2008),function(yearStart){
  
    dfCroplandLevel <- dfCropland[which(dfCropland$Area==lev&dfCropland$Year>=yearStart&dfCropland$Year<=(yearStart+9)),]
    dfFertilizerLevel <- dfFertilizer[which(dfFertilizer$Area==lev&dfFertilizer$Year>=yearStart&dfFertilizer$Year<=(yearStart+9)),]
    dfIrrigationLevel <- dfIrrigation[which(dfIrrigation$Area==lev&dfIrrigation$Year>=yearStart&dfIrrigation$Year<=(yearStart+9)),]

    dfSummary <- data.frame(Country=lev, timePeriod= yearStart)
    dfSummary$meanCropland <- mean(dfCroplandLevel$croplandArea,na.rm=T)
    dfSummary$meanNitrogen <- mean(dfFertilizerLevel$Nitrogen,na.rm=T)
    dfSummary$irrigation <- mean(dfIrrigationLevel$Irrigation,na.rm=T)
    na.omit(dfSummary)
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Country","timePeriod")])) == nrow(dfAll) # check duplicates

unique(dfAll$timePeriod)
unique(dfAll$Country) # Luxmburg and Ireland missing

## calculate nitrogen per ha
dfAll$fertilizer <- dfAll$meanNitrogen/dfAll$meanCropland

# adapdt Belgium-Luxemburg and Czechoslovakia
dfAll[which(dfAll$Country=="Belgium-Luxembourg"),]
dfAll[which(dfAll$Country=="Belgium"),]
dfAll[which(dfAll$Country=="Luxembourg"),]
dfAll <- dfAll[-which(dfAll$Country=="Belgium-Luxembourg"&dfAll$timePeriod==1998),]
dfAll[which(dfAll$Country=="Belgium-Luxembourg"),"Country"] <- "Belgium"

dfAll[which(dfAll$Country=="Czechoslovakia"),]
dfAll[which(dfAll$Country=="Czechia"),]
dfAll[which(dfAll$Country=="Slovakia"),]

dfAll <- dfAll[-which(dfAll$Country=="Czechoslovakia"&dfAll$timePeriod==1988),]

dfCzechia <- dfAll[which(dfAll$Country=="Czechoslovakia"),] 
dfCzechia$Country <- "Czechia"
dfAll <- rbind(dfAll,dfCzechia)
dfAll[which(dfAll$Country=="Czechoslovakia"),"Country"] <- "Slovakia"


# remove unused factors
unique(dfAll$Country) ## 26 countries
dfAll$Country <- factor(dfAll$Country)

## save dataframe
names(dfAll)
dfAll <- dfAll[,c("Country","timePeriod",
                 "fertilizer","irrigation")]

write.csv(dfAll, "datasetsDerived/dataInputs_national.csv",row.names=F)

rm(list=ls())
