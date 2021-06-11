library(vegan)
library(tidyr)
library(countrycode)
library(codyn)
library(openxlsx)

######## DATA PREPARATION

#### agricultural production data
dfProduction <- read.csv("datasets/agriculturalProduction_europe.csv")
head(dfProduction,)
names(dfProduction)[1] <- "Item"
names(dfProduction)[3] <- "Region"
names(dfProduction)[4] <- "Year"
unique(dfProduction$Item)
unique(dfProduction$strucpro)
range(dfProduction$Year)


#### only keep the regions covering 99% of the total arable land area in the target period (1978-2017)
dfArableLand <- dfProduction[which(dfProduction$Item=="ARA"&dfProduction$Year%in%1978:2017),]
sum(is.na(dfArableLand))
dfArableLand$values <- dfArableLand$values*1000 # to ha
mean(aggregate(values~Year,dfArableLand,sum)[,2]) # mean annual arable land: 79488834
dfArableLandMean <- aggregate(values~Region,dfArableLand,mean) 
head(dfArableLandMean)
dfArableLandMean$propArea <- dfArableLandMean$values/sum(dfArableLandMean$values)
dfArableLandMean <- dfArableLandMean[order(dfArableLandMean$propArea,decreasing = T),]
dfArableLandMean$cumArea <- cumsum(dfArableLandMean$propArea)
ind <- which(dfArableLandMean$cumArea>=0.99)[1]
vecRegion <- as.character(dfArableLandMean[1:(ind-1),"Region"])
dfProduction <- dfProduction[which(dfProduction$Region%in%vecRegion),] # only keep current regions

#### only keep crops covering 99% of the harvested area in the target period (of the arable crops with caloric values)
dfCalories <- read.xlsx("datasets/targetCrops_europe.xlsx")
names(dfCalories)[1] <- "Item"
dfCalories <- dfCalories[which(!is.na(dfCalories$Item)&!is.na(dfCalories$Calories)),] # subset crops that are not targeted (no calorie values, not included in other datasets etc.)
setdiff(dfProduction$Item,dfCalories$Item)

# only keep area harvested and production
dfProduction <- dfProduction[which(dfProduction$strucpro%in%c("AR","PR")),]

# find most abundant crop
dfCrops <- merge(dfProduction[which(dfProduction$strucpro=="AR"&dfProduction$Year%in%1978:2017),],dfCalories[,c("Item","Calories","NameFAO")],by="Item")
dfCropsMean <- aggregate(values~Item,dfCrops,mean)
head(dfCropsMean)
dfCropsMean$propArea <- dfCropsMean$values/sum(dfCropsMean$values)
dfCropsMean <- dfCropsMean[order(dfCropsMean$propArea,decreasing = T),]
dfCropsMean$cumArea <- cumsum(dfCropsMean$propArea)
ind <- which(dfCropsMean$cumArea>0.99)[1]
vecCrops <- as.character(dfCropsMean[1:(ind-1),"Item"])

# only keep target crops
dfProduction <- dfProduction[which(dfProduction$Item%in%vecCrops),]

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
dfProduction <- merge(dfProduction,dfCalories[,c("Item","Calories","NameFAO")],by="Item")
names(dfProduction)

# change production to calories 
dfProduction$Production <- dfProduction$Production*dfProduction$Calories
# calculate crop-specific yields 
dfProduction$Yield <-dfProduction$Production / dfProduction$AreaHarvested
hist(dfProduction$Yield)

# keep necessary columns only 
dfProduction <- dfProduction[,c("Region","Item","NameFAO","Year","AreaHarvested","Production","Yield")]

# only keep crops with 10 entries per time period
dfProduction$timePeriod=0
dfProduction[dfProduction$Year%in%c(1978:1987),"timePeriod"] = 1978
dfProduction[dfProduction$Year%in%c(1988:1997),"timePeriod"] = 1988
dfProduction[dfProduction$Year%in%c(1998:2007),"timePeriod"] = 1998
dfProduction[dfProduction$Year%in%c(2008:2017),"timePeriod"] = 2008

sum(is.na(dfProduction))
dfProduction$sum <- 1
dfCount <- aggregate(sum~Region+timePeriod+Item,dfProduction,sum)
head(dfCount)

dfProduction <- merge(dfProduction[,c("Region","Item","NameFAO","Year","AreaHarvested","Production","Yield","timePeriod")],dfCount)
nRow <- nrow(dfProduction)
dfProduction <- dfProduction[which(dfProduction$sum>=8),c("Region","Item","NameFAO","Year","AreaHarvested","Production","Yield")]
length(unique(dfProduction$Item)) ## 18 crops
nrow(dfProduction)/nRow # 0.7921437 of data left

# get proportion of arable land covered
mean(aggregate(AreaHarvested~Year,dfProduction,sum)[,2])/mean(aggregate(values~Year,dfArableLand,sum)[,2]) #0.4192659

# save target crops
dfCrops <- unique(dfCalories[which(dfCalories$NameFAO%in%unique(dfProduction$NameFAO)),])
write.xlsx(dfCrops[order(dfCrops[,"NameFAO"],dfCrops[,"Name"]),c("NameFAO","Name")],"datasetsDerived/cropsFinal.xlsx")
nrow(unique(dfProduction[,c("Region","Year","Item")])) == nrow(dfProduction) # check duplicates

## add country
dfProduction$Country <- substr(dfProduction$Region,1,2)
dfProduction[which(dfProduction$Country=="EL"),"Country"] <- "GR"
dfProduction[which(dfProduction$Country=="UK"),"Country"] <- "GB"

## aggregate by country
head(dfProduction)
dfProductionCountry <- aggregate(cbind(Production,AreaHarvested)~Country+Item+Year,dfProduction,sum)
dfProductionCountry$Yield <- dfProductionCountry$Production/dfProductionCountry$AreaHarvested

#### calculate total production
# regional level
sum(is.na(dfProduction))
dfProductionSum <- aggregate(cbind(Production,AreaHarvested)~Region+Year,dfProduction,sum)
head(dfProductionSum)
dfProductionSum$Yield <- dfProductionSum$Production/dfProductionSum$AreaHarvested
nrow(unique(dfProductionSum[,c("Region","Year")])) == nrow(dfProductionSum) # check duplicates

# country level
sum(is.na(dfProductionCountry))
dfProductionSumCountry <- aggregate(cbind(Production,AreaHarvested)~Country+Year,dfProductionCountry,sum)
head(dfProductionSumCountry)
dfProductionSumCountry$Yield <- dfProductionSumCountry$Production/dfProductionSumCountry$AreaHarvested
nrow(unique(dfProductionSumCountry[,c("Country","Year")])) == nrow(dfProductionSumCountry) # check duplicates

#### calculate crop richness
# regional level
dfRichness <- aggregate(Item~Region+Year,dfProduction,function(x){length(unique(x))})
head(dfRichness)
names(dfRichness)[3] <- "richness"
nrow(dfRichness)==nrow(dfProductionSum)
nrow(unique(dfRichness[,c("Region","Year")])) == nrow(dfRichness) # check duplicates

# country level
dfRichnessCountry <- aggregate(Item~Country+Year,dfProductionCountry,function(x){length(unique(x))})
head(dfRichnessCountry)
names(dfRichnessCountry)[3] <- "richness"
nrow(dfRichnessCountry)==nrow(dfProductionSumCountry)
nrow(unique(dfRichnessCountry[,c("Country","Year")])) == nrow(dfRichnessCountry) # check duplicates


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
dfSoil <- na.omit(dfSoil)
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
vecLevelFinal <- Reduce(intersect,list(dfArableLand$Region,dfProduction$Region,dfProductionSum$Region,dfRichness$Region,dfShannon$Region,dfTemp$Region,dfPrec$Region,dfSoilDiversity$Region,dfSoil$Region))

## summarize per time frame 
lsAll <- lapply(vecLevelFinal,function(reg){
  # total production
  show(as.character(reg))
  lsAggregate <- lapply(c(1978,1988,1998,2008),function(yearStart){

    sumRegion <- sum(dfProductionSum$Region==reg&dfProductionSum$Year>=yearStart&dfProductionSum$Year<=(yearStart+9))
    if(sumRegion>=8){  
      # subset data for the target region
      dfArableLandLevel <- dfArableLand[which(dfArableLand$Region==reg&dfArableLand$Year%in%yearStart:(yearStart+9)),]
      dfProductionLevel <- dfProduction[which(dfProduction$Region==reg&dfProduction$Year%in%yearStart:(yearStart+9)),]
      dfProductionSumLevel <- dfProductionSum[which(dfProductionSum$Region==reg&dfProductionSum$Year%in%yearStart:(yearStart+9)),]
      dfProductionSumLevel$ProductionDet <- resid(lm(Production ~ Year^2,data=dfProductionSumLevel))
      dfRichnessLevel <- dfRichness[which(dfRichness$Region==reg&dfRichness$Year%in%yearStart:(yearStart+9)),]
      dfShannonLevel <- dfShannon[which(dfShannon$Region==reg&dfShannon$Year%in%yearStart:(yearStart+9)),]
      dfTempLevel <- dfTemp[which(dfTemp$Region==reg&dfTemp$Year%in%yearStart:(yearStart+9)),]
      dfPrecLevel <- dfPrec[which(dfPrec$Region==reg&dfPrec$Year%in%yearStart:(yearStart+9)),]
      dfSoilLevel <- dfSoil[which(dfSoil$Region==reg),]
      dfSoilDiversityLevel <- dfSoilDiversity[which(dfSoilDiversity$Region==reg),]
      
      ## calculate all metrics
      dfSummary <- data.frame(Region=reg,Country=unique(dfProductionLevel$Country), timePeriod= yearStart)
      
      # detrend crop-specific production for asynchrony calculation
      for (j in unique(dfProductionLevel$Item))
      {
        dfProductionLevel[which(dfProductionLevel$Item==j),"ProductionDet"] = resid(lm(Production ~ Year^2,data=dfProductionLevel[which(dfProductionLevel$Item==j),]))
      }      
      dfSummary$cropAsynchrony <- 1 - round(synchrony(dfProductionLevel,time.var="Year",species.var="Item",abundance.var="ProductionDet"),10) 
      dfSummary$cropAsynchronyOriginal <- 1 - round(synchrony(dfProductionLevel,time.var="Year",species.var="Item",abundance.var="Production"),10) # non-detrendet calcualtion for comparison
      dfSummary$productionStability <- mean(dfProductionSumLevel$Production,na.rm=T)/sd(dfProductionSumLevel$ProductionDet,na.rm=T)
      dfSummary$productionStabilityOriginal <- mean(dfProductionSumLevel$Production,na.rm=T)/sd(dfProductionSumLevel$Production,na.rm=T) # non-detrendet calcualtion for comparison
      dfSummary$cropStability <- mean(dfProductionSumLevel$Production)/sum(aggregate(ProductionDet~Item,dfProductionLevel,sd)[,2])
      dfSummary$cropStabilityOriginal <- mean(dfProductionSumLevel$Production)/sum(aggregate(Production~Item,dfProductionLevel,sd)[,2]) # non-detrendet calcualtion for comparison
      dfSummary$production <- mean(dfProductionSumLevel$Production,na.rm=T)
      dfSummary$areaHarvested <- mean(dfProductionSumLevel$AreaHarvested,na.rm=T)
      dfSummary$arableLand <- mean(dfArableLandLevel$values,na.rm=T)
      dfSummary$richness <- mean(dfRichnessLevel$richness,na.rm=T)
      dfSummary$diversity <- mean(dfShannonLevel$diversity,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfTempLevel$meanTemp,na.rm=T)/sd(dfTempLevel$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfPrecLevel$meanPrec,na.rm=T)/sd(dfPrecLevel$meanPrec,na.rm=T))
      dfSummary$soilQuality <- mean(dfSoilLevel$soilQuality,na.rm=T)
      dfSummary$soilDiversity <-  mean(dfSoilDiversityLevel$soilDiversity,na.rm=T)
      dfSummary
    }
  })
  do.call(rbind,lsAggregate)
})
dfAll <- do.call(rbind,lsAll)
head(dfAll)
nrow(unique(dfAll[,c("Region","timePeriod")])) == nrow(dfAll) # check duplicates
unique(dfAll$timePeriod)
head(dfAll)
sum(is.na(dfAll))
length(unique(dfAll$Region)) ## 171 regions
nrow(dfAll) ## 462 data points

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
length(unique(dfAll$Country)) ## 23 countries
dfAll$Country <- factor(dfAll$Country)

# check correlation of detrended and non-detrended variables
cor(dfAll$productionStability,dfAll$productionStabilityOriginal,method="p")
cor(dfAll$productionStability,dfAll$productionStabilityOriginal,method="s")
cor(dfAll$cropStability,dfAll$cropStabilityOriginal,method="p")
cor(dfAll$cropStability,dfAll$cropStabilityOriginal,method="s")
cor(dfAll$cropAsynchrony,dfAll$cropAsynchronyOriginal,method="p")
cor(dfAll$cropAsynchrony,dfAll$cropAsynchronyOriginal,method="s")
# -> closely correlated

# European arable land covered 
mean(aggregate(areaHarvested~timePeriod,dfAll[which(dfAll$richness>1),],sum)[,2])/mean(aggregate(values~Year,dfArableLand,sum)[,2])*100#  41.93512%

## save dataframe
names(dfAll)
dfAll$coverage <- dfAll$areaHarvested/dfAll$arableLand
dfAll <- dfAll[,c("Region","Country","timePeriod",
                  "productionStability","cropStability","cropAsynchrony",
                  "production","coverage",
                  "diversity","richness","soilQuality","soilDiversity","fertilizer","irrigation",
                  "instabilityTemp","instabilityPrec")]

write.csv(dfAll, "datasetsDerived/dataFinal_regional.csv",row.names=F)


### national level
# get regions across datasets
vecCountryFinal <- Reduce(intersect,list(dfProductionCountry$Country,dfProductionSumCountry$Country,dfRichnessCountry$Country,dfShannonCountry$Country,dfTempCountry$Country,dfPrecCountry$Country,dfSoilDiversityCountry$Country,dfSoilCountry$Country))

## summarize per time frame 
lsAllCountry <- lapply(vecCountryFinal,function(lev){
  # total production
  show(as.character(lev))
  lsAggregate <- lapply(c(1978,1988,1998,2008),function(yearStart){
    
    sumCountry <- sum(dfProductionSumCountry$Country==lev&dfProductionSumCountry$Year>=yearStart&dfProductionSumCountry$Year<=(yearStart+9))
    if(sumCountry==10){  
      # subset data for the target country
      dfProductionLevel <- dfProductionCountry[which(dfProductionCountry$Country==lev&dfProductionCountry$Year%in%yearStart:(yearStart+9)),]
      dfProductionSumLevel <- dfProductionSumCountry[which(dfProductionSumCountry$Country==lev&dfProductionSumCountry$Year%in%yearStart:(yearStart+9)),]
      dfProductionSumLevel$ProductionDet <- resid(lm(Production ~ Year^2,data=dfProductionSumLevel))
      dfRichnessLevel <- dfRichnessCountry[which(dfRichnessCountry$Country==lev&dfRichnessCountry$Year%in%yearStart:(yearStart+9)),]
      dfShannonLevel <- dfShannonCountry[which(dfShannonCountry$Country==lev&dfShannonCountry$Year%in%yearStart:(yearStart+9)),]
      dfTempLevel <- dfTempCountry[which(dfTempCountry$Country==lev&dfTempCountry$Year%in%yearStart:(yearStart+9)),]
      dfPrecLevel <- dfPrecCountry[which(dfPrecCountry$Country==lev&dfPrecCountry$Year%in%yearStart:(yearStart+9)),]
      dfSoilLevel <- dfSoilCountry[which(dfSoilCountry$Country==lev),]
      dfSoilDiversityLevel <- dfSoilDiversityCountry[which(dfSoilDiversityCountry$Country==lev),]
      
      # calculate all metrics
      dfSummary <- data.frame(Country=lev,timePeriod= yearStart)
      
      # detrend crop-specific production for asynchrony calculation
      for (j in unique(dfProductionLevel$Item))
      {
        dfProductionLevel[which(dfProductionLevel$Item==j),"ProductionDet"] = resid(lm(Production ~ Year^2,data=dfProductionLevel[which(dfProductionLevel$Item==j),]))
      }      
      dfSummary$cropAsynchrony <- 1-round(synchrony(dfProductionLevel,time.var="Year",species.var="Item",abundance.var="ProductionDet"),10) 
      dfSummary$cropAsynchronyOriginal <- 1 - round(synchrony(dfProductionLevel,time.var="Year",species.var="Item",abundance.var="Production"),10) # non-detrendet calcualtion for comparison
      dfSummary$productionStability <- mean(dfProductionSumLevel$Production,na.rm=T)/sd(dfProductionSumLevel$ProductionDet,na.rm=T)
      dfSummary$productionStabilityOriginal <- mean(dfProductionSumLevel$Production,na.rm=T)/sd(dfProductionSumLevel$Production,na.rm=T) # non-detrendet calcualtion for comparison
      dfSummary$cropStability <- mean(dfProductionSumLevel$Production)/sum(aggregate(ProductionDet~Item,dfProductionLevel,sd)[,2])
      dfSummary$cropStabilityOriginal <- mean(dfProductionSumLevel$Production)/sum(aggregate(Production~Item,dfProductionLevel,sd)[,2]) # non-detrendet calcualtion for comparison
      dfSummary$production <- mean(dfProductionSumLevel$Production,na.rm=T)
      dfSummary$areaHarvested <- mean(dfProductionSumLevel$AreaHarvested,na.rm=T)
      dfSummary$richness <- mean(dfRichnessLevel$richness,na.rm=T)
      dfSummary$diversity <- mean(dfShannonLevel$diversity,na.rm=T)
      dfSummary$instabilityTemp <- -(mean(dfTempLevel$meanTemp,na.rm=T)/sd(dfTempLevel$meanTemp,na.rm=T))
      dfSummary$instabilityPrec <- -(mean(dfPrecLevel$meanPrec,na.rm=T)/sd(dfPrecLevel$meanPrec,na.rm=T))
      dfSummary$soilQuality <- mean(dfSoilLevel$soilQuality,na.rm=T)
      dfSummary$soilDiversity <-  mean(dfSoilDiversityLevel$soilDiversity,na.rm=T)
      dfSummary
    }
  })
  do.call(rbind,lsAggregate)
})
dfAllCountry <- do.call(rbind,lsAllCountry)
head(dfAllCountry)
nrow(unique(dfAllCountry[,c("Country","timePeriod")])) == nrow(dfAllCountry) # check duplicates
unique(dfAllCountry$timePeriod)
head(dfAllCountry)
sum(is.na(dfAllCountry))
length(unique(dfAllCountry$Country)) ## 24 countries
nrow(dfAllCountry) ## 59 data points


## add data from national level
length(unique(dfAllCountry$Country))
dfAllCountry <- merge(dfAllCountry,dfCountry[,c("Country","timePeriod","fertilizer","irrigation")],by=c("Country","timePeriod"))

# remove Netherlands
dfAllCountry <- dfAllCountry[-which(dfAllCountry$Country=="NL"),]
length(unique(dfAllCountry$Country)) ## 23 countries
dfAllCountry$Country <- factor(dfAllCountry$Country)

# check correlation of detrended and non detrenden variables
cor(dfAllCountry$productionStability,dfAllCountry$productionStabilityOriginal,method="p")
cor(dfAllCountry$productionStability,dfAllCountry$productionStabilityOriginal,method="s")
cor(dfAllCountry$cropStability,dfAllCountry$cropStabilityOriginal,method="p")
cor(dfAllCountry$cropStability,dfAllCountry$cropStabilityOriginal,method="s")
cor(dfAllCountry$cropAsynchrony,dfAllCountry$cropAsynchronyOriginal,method="p")
cor(dfAllCountry$cropAsynchrony,dfAllCountry$cropAsynchronyOriginal,method="s")
# -> closely correlated

## save dataframe
names(dfAllCountry)
dfAllCountry <- dfAllCountry[,c("Country","timePeriod",
                  "productionStability","cropStability","cropAsynchrony",
                  "production","areaHarvested",
                  "diversity","richness","soilQuality","soilDiversity","fertilizer","irrigation",
                  "instabilityTemp","instabilityPrec")]

write.csv(dfAllCountry, "datasetsDerived/dataFinal_national.csv",row.names=F)


rm(list=ls())

