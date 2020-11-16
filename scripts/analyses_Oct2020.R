library(ggplot2)
library(ggpubr)
library(sf)
library(rgdal)
library(raster)
library(plyr)
library(RColorBrewer)
library(fitdistrplus)
library(countrycode)
library(grid)
library(gridExtra)
library(car)
library(scales)
library(nlme)
library(MuMIn)
library(tidyr)
library(openxlsx)
library(lme4)

source("scripts/functionsAnalyses_Oct2020.R")

############################################################################################
###################           GLOBALS               ########################################
############################################################################################

vecColors <- brewer.pal(5,"PuBu")
lev <- c("Diversity","Asynchrony")
myColors <- c("#4daf4a",vecColors[5])
names(myColors) <- factor(lev,levels=lev)

############################################################################################
###################              DATA               ########################################
############################################################################################

###### Regional level
dfRegion <- read.csv("datasetsDerived/dataFinal_regional.csv")
dfRegion$asynchrony <- round(dfRegion$asynchrony,10)
hist(dfRegion$stability)
hist(dfRegion$asynchrony)
nrow(dfRegion)
length(unique(dfRegion$Region))
length(unique(dfRegion[which(dfRegion$timePeriod==1978),"Region"]))
length(unique(dfRegion[which(dfRegion$timePeriod==1988),"Region"]))
length(unique(dfRegion[which(dfRegion$timePeriod==1998),"Region"]))
length(unique(dfRegion[which(dfRegion$timePeriod==2008),"Region"]))
sum(table(dfRegion$Region)>1)/length(unique(dfRegion$Region))

nrow(dfRegion)/16 # epv

###### Country level
dfCountry <- read.csv("datasetsDerived/dataFinal_national.csv")
hist(dfCountry$stability)
nrow(dfCountry)
length(unique(dfCountry$Country))
length(unique(dfCountry[which(dfCountry$timePeriod==1978),"Country"]))
length(unique(dfCountry[which(dfCountry$timePeriod==1988),"Country"]))
length(unique(dfCountry[which(dfCountry$timePeriod==1998),"Country"]))
length(unique(dfCountry[which(dfCountry$timePeriod==2008),"Country"]))
sum(table(dfCountry$Country)>1)/length(unique(dfCountry$Country))
nrow(dfCountry)/8

############################################################################################
###################          ANALYSES               ########################################
############################################################################################



###### Regional level

#### 1: determinants of production stability
# check distribution of response variables
hist(dfRegion$stability)
fitdist(dfRegion$stability,"norm")$aic - fitdist(dfRegion$stability,"lnorm")$aic #log normally distributed

### regression analyses
## transformations
dfTransRegion=with(dfRegion,data.frame(Region,Country,
                                         stability = log(stability),
                                         asynchrony,
                                         diversity,
                                         soil,soilDiversity,
                                         irrigation=sqrt(irrigation),
                                         fertilizer=sqrt(fertilizer),
                                         instabilityTemp,instabilityPrec,
                                         areaHarvested = log(areaHarvested),
                                         timePeriod
))

# scale predictors for standardized regression
dfPredictorsRegion=sapply(dfTransRegion[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterRegion=data.frame(Region=dfTransRegion[,1],Country=dfTransRegion[,2],stability=dfTransRegion[,3],dfPredictorsRegion)
head(dfCenterRegion)

## check colinearity
cor(dfCenterRegion[,3:13],method='s') 

## regression models
# stability
modDiversityRegion <- lm(stability~diversity+fertilizer+irrigation+soil+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                           diversity:fertilizer+diversity:irrigation+diversity:soil+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod
                          ,data=dfCenterRegion)
summary(modDiversityRegion)

modAsynchronyRegion <- lm(stability~asynchrony+fertilizer+irrigation+soil+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                           asynchrony:fertilizer+asynchrony:irrigation+asynchrony:soil+asynchrony:soilDiversity+asynchrony:instabilityTemp+asynchrony:instabilityPrec+asynchrony:timePeriod
                         ,data=dfCenterRegion)
summary(modAsynchronyRegion)

modDiversityRegionLME <- lme(stability~diversity+fertilizer+irrigation+soil+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                               diversity:fertilizer+diversity:irrigation+diversity:soil+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod+soil:instabilityTemp,
                              random=~1|Country/Region,method = "ML",
                              data=dfCenterRegion)
summary(modDiversityRegionLME)
r.squaredGLMM(modDiversityRegionLME)
fixed.effects(modDiversityRegionLME)
random.effects(modDiversityRegionLME)
vif(modDiversityRegionLME)

modAsynchronyRegionLME <- lme(stability~asynchrony+fertilizer+irrigation+soil+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                                asynchrony:fertilizer+asynchrony:irrigation+asynchrony:soil+asynchrony:soilDiversity+asynchrony:instabilityTemp+asynchrony:instabilityPrec+asynchrony:timePeriod+soil:instabilityTemp,
                               random=~1|Country/Region,method = "ML",
                               data=dfCenterRegion)

summary(modAsynchronyRegionLME)
r.squaredGLMM(modAsynchronyRegionLME)
fixed.effects(modAsynchronyRegionLME)
random.effects(modAsynchronyRegionLME)
vif(modAsynchronyRegionLME)

# test if lme is better
anova(modDiversityRegionLME,modDiversityRegion)
anova(modAsynchronyRegionLME,modAsynchronyRegion)

#### 2: realtionship between diversity and asynchrony related to time and country
dfDiversityAsynchrony <- dfRegion[,c("Country","Region","timePeriod","asynchrony","diversity")]
dfDiversityAsynchrony$timePeriod <- factor(dfDiversityAsynchrony$timePeriod,levels = c(1978,1988,1998,2008))

cor.test(dfDiversityAsynchrony$diversity,dfDiversityAsynchrony$asynchrony,method='s')

# test if correlation between diversity and asynchrony decreases over time
modDiversityTime <- lmer(asynchrony ~ diversity + (1+diversity|timePeriod), data = dfDiversityAsynchrony)
modDiversityCountry <- lmer(asynchrony ~ diversity + (1+diversity|Country), data = dfDiversityAsynchrony)
modDiversityLM <- lm(asynchrony ~ diversity,data = dfDiversityAsynchrony)
summary(modDiversityLM)
anova(modDiversityTime,modDiversityLM)  # AIC: -187.53, -174.52
anova(modDiversityCountry,modDiversityLM)  # AIC: -221.76, -174.52

modDiversityTimeFixed=fixef(modDiversityTime)
r.squaredGLMM(modDiversityTime) 
modDiversityTimeGroup <-   coef(modDiversityTime)$timePeriod

modDiversityCountryFixed=fixef(modDiversityCountry)
r.squaredGLMM(modDiversityCountry) 
modDiversityCountryGroup <-   coef(modDiversityCountry)$Country


###### Country level

#### 1: determinants of production stability
# # check distribution of response variables
hist(dfCountry$stability)

fitdist(dfCountry$stability,"norm")$aic - fitdist(dfCountry$stability,"lnorm")$aic # log normally distributed

# ### regression analyses
# ## transformations
dfTransCountry=with(dfCountry,data.frame(Country,
                                         stability = log(stability),
                                         asynchrony,
                                         diversity,
                                         irrigation=sqrt(irrigation),
                                         fertilizer=sqrt(fertilizer),
                                         suitability,soil,
                                         instabilityTemp,instabilityPrec,
                                         areaHarvested = log(areaHarvested),
                                         timePeriod
))


# scale predictors for standardized regression
dfPredictorsCountry=sapply(dfTransCountry[,-c(1:2)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterCountry=data.frame(Country=dfTransCountry[,1],stability=dfTransCountry[,2],dfPredictorsCountry)
head(dfCenterCountry)

## check colinearity
cor(dfCenterCountry[,3:11],method='s')

## regression models
# stability
modDiversityCountry <- lm(stability~diversity+fertilizer+soil+instabilityTemp+instabilityPrec+timePeriod+
                          diversity:fertilizer+diversity:soil+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod,
                          data=dfCenterCountry)
summary(modDiversityCountry)
vif(modDiversityCountry)

modAsynchronyCountry <- lm(stability~asynchrony+fertilizer+soil+instabilityTemp+instabilityPrec+timePeriod+
                           diversity:fertilizer+diversity:soil+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod,
                           data=dfCenterCountry)
summary(modAsynchronyCountry)
vif(modAsynchronyCountry)

modDiversityCountryLME <- lme(stability~diversity+fertilizer+soil+instabilityTemp+instabilityPrec+timePeriod+
                              diversity:fertilizer+diversity:soil+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod,
                              random=~1|Country,method = "ML",
                              data=dfCenterCountry)
summary(modDiversityCountryLME)
r.squaredGLMM(modDiversityCountryLME)
fixed.effects(modDiversityCountryLME)
random.effects(modDiversityCountryLME)
vif(modDiversityCountryLME)

modAsynchronyCountryLME <- lme(stability~asynchrony+fertilizer+soil+instabilityTemp+instabilityPrec+timePeriod+
                               diversity:fertilizer+diversity:soil+diversity:instabilityTemp+diversity:instabilityPrec+diversity:timePeriod,
                               random=~1|Country,method = "ML",
                               data=dfCenterCountry)
summary(modAsynchronyCountryLME)
r.squaredGLMM(modAsynchronyCountryLME)
fixed.effects(modAsynchronyCountryLME)
random.effects(modAsynchronyCountryLME)
vif(modAsynchronyCountryLME)

# test if lme is better
anova(modDiversityCountryLME,modDiversityCountry)
anova(modAsynchronyCountryLME,modAsynchronyCountry)


#### 2: explore realtionship between diversity and asynchrony in relation to time
dfDiversityAsynchronyCountry <- dfCountry[,c("Country","timePeriod","asynchrony","diversity")]
dfDiversityAsynchronyCountry$timePeriod <- factor(dfDiversityAsynchronyCountry$timePeriod,levels = c(1978,1988,1998,2008))

cor.test(dfDiversityAsynchronyCountry$diversity,dfDiversityAsynchronyCountry$asynchrony,method='s')

# test if correlation between diversity and asynchrony decreases over time
modDiversityTimeCountry <- lmer(asynchrony ~ diversity + (1+diversity|timePeriod), data = dfDiversityAsynchronyCountry)
modDiversityLMCountry <- lm(asynchrony ~ diversity,data = dfDiversityAsynchronyCountry)
summary(modDiversityLMCountry)
anova(modDiversityTimeCountry,modDiversityLMCountry)  # AIC: -7.2749, -13.2748

modDiversityTimeFixedCountry=fixef(modDiversityTimeCountry)
r.squaredGLMM(modDiversityTimeCountry) 
modDiversityTimeGroupCountry <-   coef(modDiversityTimeCountry)$timePeriod



############################################################################################
###################           FIGURES               ########################################
############################################################################################

##### Fig 1/S1: maps of resopnse and predictors

# regional level
mapRegion <- readOGR("spatial/regions_europe.shp")
mapRegion$Region <- mapRegion$NUTS_ID
dfRegionAgg <- aggregate(cbind(stability,diversity,asynchrony,fertilizer,soil,instabilityTemp,instabilityPrec)~Region,dfRegion,mean)

a1 <- funMaps(dfRegionAgg,variable="stability",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","a")
b1 <- funMaps(dfRegionAgg,variable="diversity",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Diversity","b")
c1 <- funMaps(dfRegionAgg,variable="asynchrony",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Asynchrony","c")
d1 <- funMaps(dfRegionAgg,variable="fertilizer",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Fertilizer","d")
e1 <- funMaps(dfRegionAgg,variable="soil",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Soil quality","e")
f1 <- funMaps(dfRegionAgg,variable="instabilityTemp",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Temprature instability","f")
g1 <- funMaps(dfRegionAgg,variable="instabilityPrec",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Precipitation instability","g")

jpeg("results/Fig1.jpeg", width = 16.9,height = 10, units = 'cm',res = 600)
grid.arrange(a1,b1,c1,d1,e1,f1,g1,
             layout_matrix = rbind(c(1,2,3,4),
                                   c(5,6,7,8)))
dev.off()

# national level
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso3c')
dfCountryAgg <- aggregate(cbind(stability,diversity,asynchrony,fertilizer,soil,instabilityTemp,instabilityPrec)~Country,dfCountry,mean)

as1 <- funMaps(dfCountryAgg,variable="stability",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Stability","a")
bs1 <- funMaps(dfCountryAgg,variable="diversity",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Diversity","b")
cs1 <- funMaps(dfCountryAgg,variable="asynchrony",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Asynchrony","c")
ds1 <- funMaps(dfCountryAgg,variable="fertilizer",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Fertilizer","d")
es1 <- funMaps(dfCountryAgg,variable="soil",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Soil quality","e")
fs1 <- funMaps(dfCountryAgg,variable="instabilityTemp",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Temprature instability","f")
gs1 <- funMaps(dfCountryAgg,variable="instabilityPrec",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Precipitation instability","g")

jpeg("results/FigS1.jpeg", width = 16.9,height = 10, units = 'cm',res = 600)
  grid.arrange(as1,bs1,cs1,ds1,es1,fs1,gs1,
               layout_matrix = rbind(c(1,2,3,4),
                                     c(5,6,7,8)))
dev.off()


## Fig 2/S2: barplots of  stability model: asynchrony & diversity

## barplot subnational
dfDiversity <- data.frame(summary(modDiversityRegionLME)$tTable[2:7,c(1,2,5)])
names(dfDiversity) <- c("Effect","SE","pVal")
colnames(dfDiversity)
dfDiversity$nam <- c("Diversity/Asynchrony","sqrt(N use intensity)","Suitability","Temperature instability","Precipitation instability","Time")
dfDiversity$Model <- "Diversity"

dfAsynchrony <- data.frame(summary(modAsynchronyRegionLME)$tTable)[2:7,c(1,2,5)]
names(dfAsynchrony) <- c("Effect","SE","pVal")
colnames(dfAsynchrony)
dfAsynchrony$nam <- c("Diversity/Asynchrony","sqrt(N use intensity)","Suitability","Temperature instability","Precipitation instability","Time")
dfAsynchrony$Model <-  "Asynchrony"

dfCombined <- rbind(dfDiversity,dfAsynchrony)
dfCombined$Model <- factor(dfCombined$Model, levels = unique(dfCombined$Model))
dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
dfCombined$labHeight <- dfCombined$Effect
dfCombined[which(dfCombined$Effect>0),"labHeight"] <- dfCombined[which(dfCombined$Effect>0),"Effect"] + dfCombined[which(dfCombined$Effect>0),"SE"] + 0.03
dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"] - dfCombined[which(dfCombined$Effect<0),"SE"] - 0.03
dfCombined$lab <- ""
dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))
dfCombined <- dfCombined[unlist(lapply(1:6,function(i)seq(i,12,6))),]
dfText <- data.frame(xpos=sort(c(1:6-0.22,1:6+0.22)),ypos=dfCombined$labHeight,lab=dfCombined$lab,Model=dfCombined$Model)

a2 <- ggplot(data=dfCombined, aes(x=nam, y=Effect, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.4,0.4, by = 0.1),1),limits=c(-0.4,0.4)) +
  # scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Model",values = myColors)+
  geom_hline(yintercept=0)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 

# plot
jpeg("results/Fig2.jpeg", width = 8, height = 8, units = 'cm', res = 600)
  a2
dev.off()

## barplot national
dfDiversity <- data.frame(summary(modDiversityCountry)$coefficients[2:7,c(1,2,4)])
names(dfDiversity) <- c("Effect","SE","pVal")
colnames(dfDiversity)
dfDiversity$nam <- c("Diversity/Asynchrony","sqrt(N use intensity)","Suitability","Temperature instability","Precipitation instability","Time")
dfDiversity$Model <- "Diversity"

dfAsynchrony <- data.frame(summary(modAsynchronyCountry)$coefficients)[2:7,c(1,2,4)]
names(dfAsynchrony) <- c("Effect","SE","pVal")
colnames(dfAsynchrony)
dfAsynchrony$nam <- c("Diversity/Asynchrony","sqrt(N use intensity)","Suitability","Temperature instability","Precipitation instability","Time")
dfAsynchrony$Model <-  "Asynchrony"

dfCombined <- rbind(dfDiversity,dfAsynchrony)
dfCombined$Model <- factor(dfCombined$Model, levels = unique(dfCombined$Model))
dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
dfCombined$labHeight <- dfCombined$Effect
dfCombined[which(dfCombined$Effect>0),"labHeight"] <- dfCombined[which(dfCombined$Effect>0),"Effect"] + dfCombined[which(dfCombined$Effect>0),"SE"] + 0.03
dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"] - dfCombined[which(dfCombined$Effect<0),"SE"] - 0.03
dfCombined$lab <- ""
dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))
dfCombined <- dfCombined[unlist(lapply(1:6,function(i)seq(i,12,6))),]
dfText <- data.frame(xpos=sort(c(1:6-0.22,1:6+0.22)),ypos=dfCombined$labHeight,lab=dfCombined$lab,Model=dfCombined$Model)

as2 <- ggplot(data=dfCombined, aes(x=nam, y=Effect, fill=Model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.4,0.4, by = 0.1),1),limits=c(-0.4,0.4)) +
  # scale_y_discrete("Standardized regression coefficient", seq(-0.3,0.3,0.1))+
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "Model",values = myColors)+
  geom_hline(yintercept=0)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm")) 

# plot
jpeg("results/FigS2.jpeg", width = 8, height = 8, units = 'cm', res = 600)
  as2
dev.off()


## Fig 3: significant interactions
  
dfPredictCountry <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,suitability=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0)

a3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="suitability",modS=modDiversityRegionLME,xlabel="Diversity",ylabel=expression(paste("Production stability (",mu,"/",sigma,")")),
                     modLabel="Suitability",yVal1=0,yVal2=20)

b3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="instabilityTemp",modS=modDiversityRegionLME,xlabel="Diversity",ylabel="",
                     modLabel="Temperature instability",yVal1=0,yVal2=20)

c3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="asynchrony",moderator="timePeriod",modS=modAsynchronyRegionLME,xlabel="Asynchrony",ylabel=expression(paste("Production stability (",mu,"/",sigma,")")),
                     modLabel="Time period",yVal1=0,yVal2=20)

jpeg("results/Fig3.jpeg", width = 16.9, height = 8, units = 'cm',res = 600)
  ggarrange(a3,b3,c3,
            labels = letters[1:3],font.label=list(size=8),
            ncol = 3, nrow = 1)
dev.off()


#### Fig. 4: asynchrony diversity relationships

a4 <- ggplot(dfDiversityAsynchrony, aes(x=diversity, y=asynchrony, color = timePeriod)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors[2:5], labels = c("1978-1987","1988-1997","1998-2007","2008-2017")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modDiversityTimeGroup[,1],slope = modDiversityTimeGroup[,2],color=vecColors[2:5])+
  # geom_abline(intercept = summary(modDiversityLMGlobal)$coefficients[1,1],slope = summary(modDiversityLMGlobal)$coefficients[2,1],color="black",linetype=3)+
  theme_classic() +  
  xlab("Diversity") +
  ylab("Asynchrony") + 
  theme(axis.title.x = element_text(size=8,vjust = 2)) +  
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +    
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) + 
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm")) 

dfSlope <- modDiversityCountryGroup
dfSlope$Country <- rownames(dfSlope)
names(dfSlope)[2] <- "slope"
dfCountryAgg <- merge(dfCountryAgg,dfSlope[,c("Country","slope")])
b4 <- funMaps(dfCountryAgg,variable="slope",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Slope","")

jpeg("results/Fig4.jpeg", width = 16.9, height = 8, units = 'cm',res = 600)
  ggarrange(a4,b4,
            labels = letters[1:2],font.label=list(size=8),
            ncol=2,nrow = 1,widths =  c(1,1), align="h")
dev.off()



####### Tables

# regional
Table1a <- funTables(modDiversityRegionLME,2,F, 
                      c("(Intercept)","Diversity","sqrt(Fertilizer)","Suitability","Temperature instability","Precipitation instability","Time",
                         "Diversity:sqrt(Fertilizer)","Diversity:Suitability","Diversity:Temperature instability","Diversity:Precipitation instability","Diversity:Time"))
Table1b <- funTables(modAsynchronyRegionLME,2,F,                      
                     c("(Intercept)","Asynchrony","sqrt(Fertilizer)","Suitability","Temperature instability","Precipitation instability","Time",
                         "Asynchrony:sqrt(Fertilizer)","Asynchrony:Suitability","Asynchrony:Temperature instability","Asynchrony:Precipitation instability","Asynchrony:Time"))

write.xlsx(cbind(Table1a,Table1b),"results/Table1.xlsx")

# national
TableS1a <- funTables(modDiversityCountry,2,T, 
                     c("(Intercept)","Diversity","sqrt(Fertilizer)","Suitability","Temperature instability","Precipitation instability","Time",
                       "Diversity:sqrt(Fertilizer)","Diversity:Suitability","Diversity:Temperature instability","Diversity:Precipitation instability","Diversity:Time"))
TableS1b <- funTables(modAsynchronyCountry,2,T,                      
                     c("(Intercept)","Asynchrony","sqrt(Fertilizer)","Suitability","Temperature instability","Precipitation instability","Time",
                       "Asynchrony:sqrt(Fertilizer)","Asynchrony:Suitability","Asynchrony:Temperature instability","Asynchrony:Precipitation instability","Asynchrony:Time"))

write.xlsx(cbind(TableS1a,TableS1b),"results/TableS1.xlsx")

rm(list=ls())



#### Fgs. S4-S6: # response curves (one figure for each level of organization)
# national level
# 
# empty <- ggplot() + theme_void()
# 
# dfPredictCountry <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0,areaHarvested=0)
# dfPredictRegion <- data.frame(diversity=rep(0,1000),asynchrony=0,irrigation=0,fertilizer=0,suitability=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0)
# 
# a1s <- funPlot(predictor="diversity",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,trans="",xlabel="Diversity",ylabel=expression(paste("Production stability (",mu,"/",sigma,")")),modD=modDiversityRegionLME,modA=NULL,posX=-9999,posY=-9999)
# b1s <- funPlot(predictor="asynchrony",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,trans="",xlabel="Asynchrony",ylabel="",modD=NULL,modA=modAsynchronyRegionLME,posX=-9999,posY=-9999)
# c1s <- funPlot(predictor="fertilizer",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",modD=modDiversityRegionLME,modA=modAsynchronyRegionLME,posX=-9999,posY=-9999)
# d1s <- funPlot(predictor="suitability",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,trans="",xlabel="Suitability",ylabel=expression(paste("Production stability (",mu,"/",sigma,")")),modD=modDiversityRegionLME,modA=modAsynchronyRegionLME,posX=-9999,posY=-9999)
# e1s <- funPlot(predictor="instabilityTemp",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="",modD=modDiversityRegionLME,modA=modAsynchronyRegionLME,posX=-9999,posY=-9999)
# f1s <- funPlot(predictor="instabilityPrec",dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,dfOriginal=dfRegion,trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",modD=modDiversityRegionLME,modA=modAsynchronyRegionLME,posX=-9999,posY=-9999)
# 
# a4s <- funPredRange(predictor="diversity",      dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="",xlabel="Diversity",ylabel=expression(paste("Yield stability (",mu,"/",sigma,")")),0,100,"gray30")
# b4s <- funPredRange(predictor="fertilizer",     dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="sqrt",xlabel="Fertilizer (t/ha)",ylabel="",0,100,"gray30")
# c4s <- funPredRange(predictor="irrigation",     dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="sqrt",xlabel="Irrigation (%)",ylabel="",0,100,"gray30")
# d4s <- funPredRange(predictor="instabilityTemp",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="",xlabel=expression(paste("Temperature instability (-(",mu,"/",sigma,"))")),ylabel="",0,100,"gray30")
# e4s <- funPredRange(predictor="instabilityPrec",dfPredict=dfPredictCountry,dfCenter=dfCenterCountry,dfLog=dfTransCountry,dfOriginal=dfCountry,modS=modStabilityCountry,transResponse = "log",trans="",xlabel=expression(paste("Precipitation instability (-(",mu,"/",sigma,"))")),ylabel="",0,100,"gray30")
# 
# jpeg("results/FigS1.jpeg", width = 16.9,height = 16, units = 'cm',res = 600)
# 
# ggarrange(a1s,b1s,c1s,d1s,e1s,f1s,
#           labels = letters[1:6],font.label=list(size=8),
#           ncol = 3, nrow = 2)
# 
# dev.off()
# 



