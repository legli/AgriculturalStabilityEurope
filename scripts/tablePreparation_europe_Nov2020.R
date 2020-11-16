library(vegan)
library(rgdal)
library(tidyr)
library(countrycode)
library(codyn)
library(openxlsx)

######## DATA PREPARATION

#### agricultural production data
dfProduction <- read.csv("datasets/agriculturalProduction_europe.csv")
head(dfProduction,)
names(dfProduction)[1] <- "Item"
names(dfProduction)[3] <- "Level"
names(dfProduction)[4] <- "Year"
unique(dfProduction$Item)
unique(dfProduction$strucpro)
range(dfProduction$Year)


# only keep the regions covering 99.9% of the total cropland area 
dfCropland <- dfProduction[which(dfProduction$Item=="UAA"),]
sum(is.na(dfCropland))
dfCroplandMean <- aggregate(values~Level,dfCropland,mean)
head(dfCroplandMean)
dfCroplandMean$propArea <- dfCroplandMean$values/sum(dfCroplandMean$values)
dfCroplandMean <- dfCroplandMean[order(dfCroplandMean$propArea,decreasing = T),]
dfCroplandMean$cumArea <- cumsum(dfCroplandMean$propArea)
ind <- which(dfCroplandMean$cumArea>=0.999)[1]
dfCropland <- dfCropland[which(dfCropland$Level%in%dfCroplandMean[1:ind,"Level"]),] # this are the target regions
vecLevel <- unique(dfCropland$Level)

# only keep area harvested and production
dfProduction <- dfProduction[which(dfProduction$strucpro%in%c("AR","PR")),]
dfProduction <- dfProduction[which(dfProduction$Level%in%vecLevel),] # only keep current regions
# split area harvested and production in two columns (from long to wide)
dfProduction <- dfProduction %>% spread(strucpro, values)
head(dfProduction)
names(dfProduction)[4] <- "AreaHarvested"
names(dfProduction)[5] <- "Production"

# set NA for harvested data for which there is no production data and vice versa 
dfProduction[is.na(dfProduction$Production),"AreaHarvested"] <- NA
dfProduction[is.na(dfProduction$AreaHarvested),"Production"] <- NA
dfProduction <- dfProduction[which(!is.na(dfProduction$AreaHarvested) & !is.na(dfProduction$Production)),]
# remove zero areas and production
dfProduction <- dfProduction[-which(dfProduction$AreaHarvested==0 |  dfProduction$Production==0),]

## change to tons and hectare
dfProduction$Production <- dfProduction$Production*1000
dfProduction$AreaHarvested <- dfProduction$AreaHarvested*1000

# add calories and make crops consistent with target crop file
dfCalories <- read.xlsx("datasets/targetCrops_europe.xlsx")
names(dfCalories)[1] <- "Item"
dfCalories <- dfCalories[which(!is.na(dfCalories$Item)&!is.na(dfCalories$Calories)),] # subset crops that are not targeted (no calorie values, not included in other datasets etc.)
setdiff(dfProduction$Item,dfCalories$Item)

dfProduction <- merge(dfProduction,dfCalories[,c("Item","Calories","NameFAO")],by="Item")
names(dfProduction)

# change production to calories 
dfProduction$Production <- dfProduction$Production*dfProduction$Calories
# calculate individual yields and remove very unrealistic outliers
dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
hist(dfProduction$Yield)
dfProduction<-dfProduction[dfProduction$Yield<min(boxplot(dfProduction$Yield)$out),]

# keep necessary columns only 
dfProduction <- dfProduction[,c("Level","Item","NameFAO","Year","AreaHarvested","Production")]

# only keep crops with 10 entries per time period
dfProduction$timePeriod=0
dfProduction[dfProduction$Year%in%c(1978:1987),"timePeriod"] = 1978
dfProduction[dfProduction$Year%in%c(1988:1997),"timePeriod"] = 1988
dfProduction[dfProduction$Year%in%c(1998:2007),"timePeriod"] = 1998
dfProduction[dfProduction$Year%in%c(2008:2017),"timePeriod"] = 2008

sum(is.na(dfProduction))
dfProduction$sum <- 1
dfCount <- aggregate(sum~Level+timePeriod+Item,dfProduction,sum)
head(dfCount)

dfProduction <- merge(dfProduction[,c("Level","Item","NameFAO","Year","AreaHarvested","Production","timePeriod")],dfCount)
dfProduction <- dfProduction[which(dfProduction$sum==10),c("Level","Item","NameFAO","Year","AreaHarvested","Production")]
length(unique(dfProduction$Item)) ## 28 crops

# save target crops
dfCrops <- unique(dfCalories[which(dfCalories$NameFAO%in%unique(dfProduction$NameFAO)),])
write.xlsx(dfCrops[order(dfCrops[,"NameFAO"],dfCrops[,"Name"]),c("NameFAO","Name")],"datasetsDerived/cropsFinal.xlsx")


nrow(unique(dfProduction[,c("Level","Year","Item")])) == nrow(dfProduction) # check duplicates

## add country
names(dfProduction)[1] <- "Region"
dfProduction$Country <- substr(dfProduction$Region,1,2)
dfProduction[which(dfProduction$Country=="EL"),"Country"] <- "GR"
dfProduction[which(dfProduction$Country=="UK"),"Country"] <- "GB"

## aggregate by country
head(dfProduction)
dfProductionCountry <- aggregate(cbind(Production,AreaHarvested)~Country+Item+NameFAO+Year,dfProduction,sum)

#### calculate total production
# regional level
sum(is.na(dfProduction))
dfProductionSum <- aggregate(cbind(Production,AreaHarvested)~Region+Year,dfProduction,sum)
head(dfProductionSum)
nrow(unique(dfProductionSum[,c("Region","Year")])) == nrow(dfProductionSum) # check duplicates

# country level
sum(is.na(dfProductionCountry))
dfProductionSumCountry <- aggregate(cbind(Production,AreaHarvested)~Country+Year,dfProductionCountry,sum)
head(dfProductionSumCountry)
nrow(unique(dfProductionSumCountry[,c("Country","Year")])) == nrow(dfProductionSumCountry) # check duplicates

#### calculate effective diversity (exp of shannon)
# regional level
dfShannon <- aggregate(AreaHarvested~Region+Year,dfProduction,function(x){exp(diversity(x,index="shannon"))})
head(dfShannon)
names(dfShannon)[3] <- "diversity"
nrow(dfShannon)==nrow(dfProductionSum)
nrow(unique(dfShannon[,c("Region","Year")])) == nrow(dfShannon) # check duplicates

# country level
dfShannonCountry <- aggregate(AreaHarvested~Country+Year,dfProductionCountry,function(x){exp(diversity(x,index="shannon"))})
head(dfShannonCountry)
names(dfShannonCountry)[3] <- "diversity"
nrow(dfShannonCountry)==nrow(dfProductionSumCountry)
nrow(unique(dfShannonCountry[,c("Country","Year")])) == nrow(dfShannonCountry) # check duplicates

#### Climate
# regional level
dfTemp <- read.csv("datasetsDerived/temperature_regional.csv")
head(dfTemp)
dfPrec <- read.csv("datasetsDerived/precipitation_regional.csv")
head(dfPrec)

# country level
dfTempCountry <- read.csv("datasetsDerived/temperature_national.csv")
head(dfTempCountry)
dfPrecCountry <- read.csv("datasetsDerived/precipitation_national.csv")
head(dfPrecCountry)


#### Soil quality
# regional level
dfSoil <- read.csv("datasetsDerived/soilQuality_regional.csv")
head(dfSoil)

# national level
dfSoilCountry <- read.csv("datasetsDerived/soilQuality_national.csv")
head(dfSoilCountry)
dfSoilCountry$Country <- countrycode(dfSoilCountry$Country, 'iso3c', 'iso2c')

#### Soil diversity
# regional level
dfSoilDiversity <- read.csv("datasetsDerived/soilDiversity_regional.csv")
head(dfSoilDiversity)

# country level
dfSoilDiversityCountry <- read.csv("datasetsDerived/soilDiversity_national.csv")
head(dfSoilDiversityCountry)


######## CALCULATE VARIABLES FOR THE 4 TIME PERIOS

### regional level
# get regions across datasets
vecLevelFinal <- Reduce(intersect,list(dfProduction$Region,dfProductionSum$Region,dfShannon$Region,dfTemp$Region,dfPrec$Region,dfSoilDiversity$Region,dfSoil$Region))

## summarize per time frame 
lsAll <- lapply(vecLevelFinal,function(reg){
  # total production
  show(as.character(reg))
  lsAggregate <- lapply(c(1978,1988,1998,2008),function(yearStart){

    sumRegion <- sum(dfProductionSum$Region==reg&dfProductionSum$Year>=yearStart&dfProductionSum$Year<=(yearStart+9))
    if(sumRegion==10){  
      # subset data for the target region
      dfProductionLevel <- dfProduction[which(dfProduction$Region==reg&dfProduction$Year>=yearStart&dfProduction$Year<=(yearStart+9)),]
      dfProductionSumLevel <- dfProductionSum[which(dfProductionSum$Region==reg&dfProductionSum$Year>=yearStart&dfProductionSum$Year<=(yearStart+9)),]
      dfProductionSumLevel$ProductionDet <- resid(lm(Production ~ Year^2,data=dfProductionSumLevel))
      dfShannonLevel <- dfShannon[which(dfShannon$Region==reg&dfShannon$Year>=yearStart&dfShannon$Year<=(yearStart+9)),]
      dfTempLevel <- dfTemp[which(dfTemp$Region==reg&dfTemp$Year>=yearStart&dfTemp$Year<=(yearStart+9)),]
      dfPrecLevel <- dfPrec[which(dfPrec$Region==reg&dfPrec$Year>=yearStart&dfPrec$Year<=(yearStart+9)),]
      
      dfSoilLevel <- dfSoil[which(dfSoil$Region==reg),]
      dfSoilDiversityLevel <- dfSoilDiversity[which(dfSoilDiversity$Region==reg),]
      
      # calculate all metrics
      dfSummary <- data.frame(Region=reg,Country=unique(dfProductionLevel$Country), timePeriod= yearStart)
      dfSummary$stability <- mean(dfProductionSumLevel$Production,na.rm=T)/sd(dfProductionSumLevel$ProductionDet,na.rm=T)
      dfSummary$production <- mean(dfProductionSumLevel$Production,na.rm=T)
      dfSummary$areaHarvested <- mean(dfProductionSumLevel$AreaHarvested,na.rm=T)
      dfSummary$diversity <- mean(dfShannonLevel$diversity,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfTempLevel$meanTemp,na.rm=T)/sd(dfTempLevel$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfPrecLevel$meanPrec,na.rm=T)/sd(dfPrecLevel$meanPrec,na.rm=T))
      dfSummary$soilQuality <- mean(dfSoilLevel$soilQuality,na.rm=T)
      dfSummary$soilDiversity <-  mean(dfSoilDiversityLevel$soilDiversity,na.rm=T)
      
      ## crop specific detrended production
      for (j in unique(dfProductionLevel$Item))
      {
        dfProductionLevel[which(dfProductionLevel$Item==j),"ProductionDet"] = resid(lm(Production ~ Year^2,data=dfProductionLevel[which(dfProductionLevel$Item==j),]))
      }      
      ## asynchrony cacluation
      dfSummary$asynchrony <- 1-synchrony(dfProductionLevel,time.var="Year",species.var="Item",abundance.var="ProductionDet") 
      
      na.omit(dfSummary)
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Region","timePeriod")])) == nrow(dfAll) # check duplicates
unique(dfAll$timePeriod)
head(dfAll)

# check NA
sum(is.na(dfAll))
## omit NA
dfAll <- na.omit(dfAll)
length(unique(dfAll$Region)) ## 193 regions
nrow(dfAll) ## 447 data points


## add data from national level
dfCountry <- read.csv("datasetsDerived/dataInputs_national.csv")
head(dfCountry)
dfCountry$Country <- countrycode(dfCountry$Country, 'country.name', 'iso2c')
length(unique(dfCountry$Country))

length(unique(dfAll$Country))
dfAllTot <- merge(dfAll,dfCountry[,c("Country","timePeriod","fertilizer","irrigation")],by=c("Country","timePeriod"),all.x=T)
dfAllTot[which(is.na(dfAllTot$irrigation)),c("Region","timePeriod")]

dfAll <- merge(dfAll,dfCountry[,c("Country","timePeriod","fertilizer","irrigation")],by=c("Country","timePeriod"))

# remove Netherlands
dfAll <- dfAll[-which(dfAll$Country=="NL"),]
unique(dfAll$Country) ## 23 countries
dfAll$Country <- factor(dfAll$Country)

## save dataframe
names(dfAll)
dfAll <- dfAll[,c("Region","Country","timePeriod",
                  "stability","production","areaHarvested",
                  "asynchrony","diversity","soilQuality","soilDiversity","fertilizer","irrigation",
                  "instabilityTemp","instabilityPrec")]

write.csv(dfAll, "datasetsDerived/dataFinal_regional.csv",row.names=F)


### national level
# get regions across datasets
vecCountryFinal <- Reduce(intersect,list(dfProductionCountry$Country,dfProductionSumCountry$Country,dfShannonCountry$Country,dfTempCountry$Country,dfPrecCountry$Country,dfSoilDiversityCountry$Country,dfSoilCountry$Country))

## summarize per time frame 
lsAllCountry <- lapply(vecCountryFinal,function(lev){
  # total production
  show(as.character(lev))
  lsAggregate <- lapply(c(1978,1988,1998,2008),function(yearStart){
    
    sumCountry <- sum(dfProductionSumCountry$Country==lev&dfProductionSumCountry$Year>=yearStart&dfProductionSumCountry$Year<=(yearStart+9))
    if(sumCountry==10){  
      # subset data for the target country
      dfProductionLevel <- dfProductionCountry[which(dfProductionCountry$Country==lev&dfProductionCountry$Year>=yearStart&dfProductionCountry$Year<=(yearStart+9)),]
      dfProductionSumLevel <- dfProductionSumCountry[which(dfProductionSumCountry$Country==lev&dfProductionSumCountry$Year>=yearStart&dfProductionSumCountry$Year<=(yearStart+9)),]
      dfProductionSumLevel$ProductionDet <- resid(lm(Production ~ Year^2,data=dfProductionSumLevel))
      dfShannonLevel <- dfShannonCountry[which(dfShannonCountry$Country==lev&dfShannonCountry$Year>=yearStart&dfShannonCountry$Year<=(yearStart+9)),]
      dfTempLevel <- dfTempCountry[which(dfTempCountry$Country==lev&dfTempCountry$Year>=yearStart&dfTempCountry$Year<=(yearStart+9)),]
      dfPrecLevel <- dfPrecCountry[which(dfPrecCountry$Country==lev&dfPrecCountry$Year>=yearStart&dfPrecCountry$Year<=(yearStart+9)),]
      
      dfSoilLevel <- dfSoilCountry[which(dfSoilCountry$Country==lev),]
      dfSoilDiversityLevel <- dfSoilDiversityCountry[which(dfSoilDiversityCountry$Country==lev),]
      
      # calculate all metrics
      dfSummary <- data.frame(Country=lev,timePeriod= yearStart)
      dfSummary$stability <- mean(dfProductionSumLevel$Production,na.rm=T)/sd(dfProductionSumLevel$ProductionDet,na.rm=T)
      dfSummary$production <- mean(dfProductionSumLevel$Production,na.rm=T)
      dfSummary$areaHarvested <- mean(dfProductionSumLevel$AreaHarvested,na.rm=T)
      dfSummary$diversity <- mean(dfShannonLevel$diversity,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfTempLevel$meanTemp,na.rm=T)/sd(dfTempLevel$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfPrecLevel$meanPrec,na.rm=T)/sd(dfPrecLevel$meanPrec,na.rm=T))
      dfSummary$soilQuality <- mean(dfSoilLevel$soilQuality,na.rm=T)
      dfSummary$soilDiversity <-  mean(dfSoilDiversityLevel$soilDiversity,na.rm=T)
      
      ## crop specific detrended  production
      for (j in unique(dfProductionLevel$Item))
      {
        dfProductionLevel[which(dfProductionLevel$Item==j),"ProductionDet"] = resid(lm(Production ~ Year^2,data=dfProductionLevel[which(dfProductionLevel$Item==j),]))
      }      
      ## asynchrony cacluation
      dfSummary$asynchrony <- 1-synchrony(dfProductionLevel,time.var="Year",species.var="Item",abundance.var="ProductionDet") 
      
      na.omit(dfSummary)
    }
  })
  do.call(rbind,lsAggregate)
})
dfAllCountry <- do.call(rbind,lsAllCountry)
head(dfAllCountry)
nrow(unique(dfAllCountry[,c("Country","timePeriod")])) == nrow(dfAllCountry) # check duplicates
unique(dfAllCountry$timePeriod)
head(dfAllCountry)

# check NA
sum(is.na(dfAllCountry))
## omit NA
dfAllCountry <- na.omit(dfAllCountry)
length(unique(dfAllCountry$Country)) ## 25 countries
nrow(dfAllCountry) ## 66 data points


## add data from national level
length(unique(dfAllCountry$Country))
dfAllCountry <- merge(dfAllCountry,dfCountry[,c("Country","timePeriod","fertilizer","irrigation")],by=c("Country","timePeriod"))

# remove Netherlands
dfAllCountry <- dfAllCountry[-which(dfAllCountry$Country=="NL"),]
unique(dfAllCountry$Country) ## 23 countries
dfAllCountry$Country <- factor(dfAllCountry$Country)

## save dataframe
names(dfAllCountry)
dfAllCountry <- dfAllCountry[,c("Country","timePeriod",
                  "stability","production","areaHarvested",
                  "asynchrony","diversity","soilQuality","soilDiversity","fertilizer","irrigation",
                  "instabilityTemp","instabilityPrec")]

write.csv(dfAllCountry, "datasetsDerived/dataFinal_national.csv",row.names=F)


rm(list=ls())

