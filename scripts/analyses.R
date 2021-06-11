library(ggplot2)
library(ggpubr)
library(sf)
library(rgdal)
library(raster)
library(RColorBrewer)
library(fitdistrplus)
library(countrycode)
library(gridExtra)
library(car)
library(scales)
library(nlme)
library(MuMIn)
library(tidyr)
library(openxlsx)

source("scripts/functionsAnalyses.R")
vecColors <- brewer.pal(5,"PuBu")

############################################################################################
###################              DATA               ########################################
############################################################################################

dfRegion <- read.csv("datasetsDerived/dataFinal_regional.csv")
dfRegion <- dfRegion[which(dfRegion$richness>1),] # only keep regions with richness > 1
hist(dfRegion$coverage )
mean(dfRegion$coverage )*100 # 44.77661%
nrow(dfRegion)
length(unique(dfRegion$Region))
length(unique(dfRegion[which(dfRegion$timePeriod==1978),"Region"]))
length(unique(dfRegion[which(dfRegion$timePeriod==1988),"Region"]))
length(unique(dfRegion[which(dfRegion$timePeriod==1998),"Region"]))
length(unique(dfRegion[which(dfRegion$timePeriod==2008),"Region"]))
sum(table(dfRegion$Region)>1)/length(unique(dfRegion$Region))*100 # 81.98758

############################################################################################
###################          ANALYSES               ########################################
############################################################################################

###### Regional level

#### 1: determinants of production stability
# check distribution of response variable
fitdist(dfRegion$productionStability,"norm")$aic - fitdist(dfRegion$productionStability,"lnorm")$aic # +234.0703 <- log normally distributed
hist(log(dfRegion$productionStability))

### regression analyses
## transformations
dfTransRegion=with(dfRegion,data.frame(Region,Country,
                                       productionStability = log(productionStability),
                                       diversity,
                                       soilQuality,soilDiversity,
                                       irrigation=irrigation,
                                       fertilizer=sqrt(fertilizer),
                                       instabilityTemp,instabilityPrec,
                                       timePeriod
))

# scale predictors for standardized regression
dfPredictorsRegion=sapply(dfTransRegion[,-c(1:3)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterRegion=data.frame(Region=dfTransRegion[,1],Country=dfTransRegion[,2],
                          productionStability=dfTransRegion[,3],
                          dfPredictorsRegion)
head(dfCenterRegion)

## check colinearity
round(cor(dfCenterRegion[,3:11],method='s'),2)

### regression models
modStabilityRegion <- lm(productionStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                           diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec
                          ,data=dfCenterRegion)
summary(modStabilityRegion)


modStabilityRegionLME <- lme(productionStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                               diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec,
                              random=~1|Country,method = "REML",
                              data=dfCenterRegion)
summary(modStabilityRegionLME)
r.squaredGLMM(modStabilityRegionLME) # R2m = 0.300913; R2c = 0.5753768
fixed.effects(modStabilityRegionLME)
random.effects(modStabilityRegionLME)
vif(modStabilityRegionLME) # max. 2.61

# test if lme is better
anova(modStabilityRegionLME,modStabilityRegion) # AIC LME = 609.1527; AIC LM = 677.0014

#### 2: relationship between diversity and crop stability/asynchrony related to time and country
dfDiversityRegion <- dfRegion[,c("Country","Region","timePeriod","cropStability","cropAsynchrony","diversity")]
dfDiversityRegion$timePeriod <- factor(dfDiversityRegion$timePeriod,levels = c(1978,1988,1998,2008))

fitdist(dfDiversityRegion$cropStability,"norm")$aic - fitdist(dfDiversityRegion$cropStability,"lnorm")$aic # +28.00691 -> log normally distributed
fitdist(1-dfDiversityRegion$cropAsynchrony,"norm")$aic - fitdist(1-dfDiversityRegion$cropAsynchrony,"lnorm")$aic # normally distributed
dfDiversityRegion$cropStability <- log(dfDiversityRegion$cropStability) # transform to log

# test if correlation between diversity and crop stability decreases over time and space
modStabilityTime <- lme(cropStability ~ diversity, random=~1+diversity|timePeriod, method="REML",control=lmeControl(opt='optim'),data = dfDiversityRegion)
modStabilitySpace <- lme(cropStability ~ diversity, random=~1+diversity|Country, method="REML",control=lmeControl(opt='optim'),data = dfDiversityRegion)
modStabilityLM <- lm(cropStability ~ diversity,data = dfDiversityRegion)
summary(modStabilityLM)
anova(modStabilityTime,modStabilityLM)  # AIC: 462.5516 456.8005 
anova(modStabilitySpace,modStabilityLM)  # AIC: 309.2566 456.8005 

modStabilityTimeFixed=fixef(modStabilityTime)
r.squaredGLMM(modStabilityTime) # R2m = 0.0005962524; R2c = 0.008823999
modStabilityTimeGroup <-   coef(modStabilityTime)
modStabilityTimeGroup$timePeriod <- as.numeric(row.names(modStabilityTimeGroup))

summary(modStabilitySpace)
modStabilitySpaceFixed=fixef(modStabilitySpace)
r.squaredGLMM(modStabilitySpace) # R2m = 0.0003679698; R2c = 0.5166253
modStabilitySpaceGroup <-   coef(modStabilitySpace)
modStabilitySpaceGroup$Country <- row.names(modStabilitySpaceGroup)

# test if correlation between diversity and asynchrony decreases over time and space
modAsynchronyTime <- lme(cropAsynchrony ~ diversity, random=~1+diversity|timePeriod, method="REML",control=lmeControl(opt='optim'),data = dfDiversityRegion)
modAsynchronySpace <- lme(cropAsynchrony ~ diversity, random=~1+diversity|Country, method="REML",control=lmeControl(opt='optim'),data = dfDiversityRegion)
modAsynchronyLM <- lm(cropAsynchrony ~ diversity,data = dfDiversityRegion)
summary(modAsynchronyLM)
anova(modAsynchronyTime,modAsynchronyLM)  # AIC: -232.8146 -210.3701 
anova(modAsynchronySpace,modAsynchronyLM)  # AIC: -271.2696 -210.3701 

modAsynchronyTimeFixed=fixef(modAsynchronyTime)
r.squaredGLMM(modAsynchronyTime)  # R2m = 0.2643791; R2c = 0.4935418
modAsynchronyTimeGroup <-   coef(modAsynchronyTime)
modAsynchronyTimeGroup$timePeriod <- as.numeric(row.names(modAsynchronyTimeGroup))

modAsynchronySpaceFixed=fixef(modAsynchronySpace)
r.squaredGLMM(modAsynchronySpace) # R2m = 0.1492527; R2c = 0.4537047
modAsynchronySpaceGroup <-   coef(modAsynchronySpace)
modAsynchronySpaceGroup$Country <- row.names(modAsynchronySpaceGroup)


############################################################################################
###################           FIGURES               ########################################
############################################################################################

##### Fig 1: maps of resopnse and predictors

# regional level
mapRegion <- readOGR("spatial/regions_europe.shp")
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso2c')

mapRegion$Region <- mapRegion$NUTS_ID
dfRegionAgg <- aggregate(cbind(productionStability,cropStability,cropAsynchrony,diversity,fertilizer,irrigation,soilQuality,soilDiversity,instabilityTemp,instabilityPrec)~Region,dfRegion,mean)

a1 <- funMaps(dfRegionAgg,variable="productionStability",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Production stability","a")
b1 <- funMaps(dfRegionAgg,variable="cropStability",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Crop stability","a")
c1 <- funMaps(dfRegionAgg,variable="cropAsynchrony",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Crop asynchrony","c")
d1 <- funMaps(dfRegionAgg,variable="diversity",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Crop diversity","d")
e1 <- funMaps(dfRegionAgg,variable="fertilizer",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Nitrogen use (t/ha)","e")
f1 <- funMaps(dfRegionAgg,variable="irrigation",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Irrigation (%)","f")
g1 <- funMaps(dfRegionAgg,variable="soilQuality",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Soil productivity","g")
h1 <- funMaps(dfRegionAgg,variable="soilDiversity",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Soil type diversity","h")
i1 <- funMaps(dfRegionAgg,variable="instabilityTemp",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Temperature instability","i")
j1 <- funMaps(dfRegionAgg,variable="instabilityPrec",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Precipitation instability","j")

jpeg("results/Fig1.jpeg", width = 16.9,height = 8, units = 'cm',res = 600)
  grid.arrange(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,ncol=5,nrow=2)
dev.off()


## Fig 2: barplots of  stability model
dfStability <- data.frame(summary(modStabilityRegionLME)$tTable[2:9,c(1,2,5)])
names(dfStability) <- c("Effect","SE","pVal")
dfStability$nam <- c("Crop diversity","sqrt(Nitrogen use)","Irrigation","Soil productivity","Soil type diversity","Temperature instability","Precipitation instability","Time")
dfStability$nam <- factor(dfStability$nam, levels = unique(dfStability$nam))

dfStability$labHeight <- dfStability$Effect
dfStability[which(dfStability$Effect>0),"labHeight"] <- dfStability[which(dfStability$Effect>0),"Effect"] + dfStability[which(dfStability$Effect>0),"SE"] + 0.03
dfStability[which(dfStability$Effect<0),"labHeight"] <- dfStability[which(dfStability$Effect<0),"Effect"] - dfStability[which(dfStability$Effect<0),"SE"] - 0.03
dfStability$lab <- ""
dfStability[which(dfStability$pVal<0.05),"lab"] <- "*"
dfStability[which(dfStability$pVal<0.01),"lab"] <- "**"
dfStability[which(dfStability$pVal<0.001),"lab"] <- "***"
dfStability[which(dfStability$pVal>=0.05),"lab"] <- "NS"
dfStability$lab <- factor(dfStability$lab, levels = unique(dfStability$lab))
dfText <- data.frame(xpos=1:8,ypos=dfStability$labHeight,lab=dfStability$lab)

a2 <- ggplot(data=dfStability, aes(x=nam, y=Effect)) +
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
  geom_hline(yintercept=0)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.5), "cm")) 

# plot
jpeg("results/Fig2.jpeg", width = 8, height = 8, units = 'cm', res = 600)
  a2
dev.off()


## Fig 3: interactions
  
dfTab <- data.frame(summary(modStabilityRegionLME)$tTable[,c(1,2,4,5)])
dfPredictRegion <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,soilQuality=0,soilDiversity=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0)

a3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="fertilizer",modS=modStabilityRegionLME,xlabel="",ylabel="Production stability",
                      modLabel="Nitrogen use",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[10,4],2)))
b3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="irrigation",modS=modStabilityRegionLME,xlabel="Crop diversity",ylabel="",
                     modLabel="Irrigation",yVal1=0,yVal2=20,pVal=paste0("p = ","<0.05"))
c3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="soilQuality",modS=modStabilityRegionLME,xlabel="",ylabel="",
                     modLabel="Soil productivity",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[12,4],2)))
d3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="soilDiversity",modS=modStabilityRegionLME,xlabel="",ylabel="Production stability",
                     modLabel="Soil type diversity",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[13,4],2)))
e3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="instabilityTemp",modS=modStabilityRegionLME,xlabel="Crop diversity",ylabel="",
                     modLabel="Temperature instability",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[14,4],2)))
f3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="instabilityPrec",modS=modStabilityRegionLME,xlabel="",ylabel="",
                     modLabel="Precipitation instability",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[15,4],2)))


jpeg("results/Fig3.jpeg", width = 16.9, height = 12, units = 'cm',res = 600)
  ggarrange(a3,b3,c3,d3,e3,f3,
            labels = letters[1:6],font.label=list(size=8),
            ncol = 3, nrow = 2)
dev.off()


#### Fig. 4: spatiotemporal relatinoship of crop diversity and crop stability/crop asynchrony 
slopeLM <- exp(coef(modStabilityLM)[1]+coef(modStabilityLM)[2])-exp(coef(modStabilityLM)[1])

a4 <- ggplot(dfDiversityRegion, aes(x=diversity, y=exp(cropStability))) +
  geom_point(size=0.7) +
  # scale_colour_manual(name="Time interval",values = vecColors[2:5], labels = c("1978-1987","1988-1997","1998-2007","2008-2017")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = exp(coef(modStabilityLM)[1]),slope = slopeLM)+
  theme_classic() +
  xlab("Crop diversity") +
  ylab("Crop stability") +
  theme(axis.title.x = element_text(size=8,vjust = 2)) +
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = "none")

names(modStabilitySpaceGroup)
# backtransform
modStabilitySpaceGroup$interceptStability <-  exp(modStabilitySpaceGroup$`(Intercept)`)
modStabilitySpaceGroup$slopeStability <-  exp(modStabilitySpaceGroup$`(Intercept)`+modStabilitySpaceGroup$diversity)-modStabilitySpaceGroup$interceptStability
                                            
b4 <- funMaps(modStabilitySpaceGroup[,c("Country","slopeStability")],variable="slopeStability",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Slope","")

c4 <- ggplot(dfDiversityRegion, aes(x=diversity, y=cropAsynchrony, color = timePeriod)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors[2:5], labels = c("1978-1987","1988-1997","1998-2007","2008-2017")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modAsynchronyTimeGroup[,1],slope = modAsynchronyTimeGroup[,2],color=vecColors[2:5])+
  theme_classic() +
  xlab("Crop diversity") +
  ylab("Crop asynchrony") +
  theme(axis.title.x = element_text(size=8,vjust = 2)) +
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = c(0.8, 0.17))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) +
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,0.2,0.2), "cm"))

names(modAsynchronySpaceGroup)
modAsynchronySpaceGroup$slopeAsynchrony <- modAsynchronySpaceGroup$diversity
d4 <- funMaps( modAsynchronySpaceGroup[,c("Country","slopeAsynchrony")],variable="slopeAsynchrony",st_as_sf(mapCountry),"Country",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Slope","")

jpeg("results/Fig4.jpeg", width = 16.9, height = 15, units = 'cm',res = 600)
  ggarrange(a4,b4,c4,d4,
            labels = letters[1:4],font.label=list(size=8),
            ncol=2,nrow = 2,widths =  c(1,1), align="h")
dev.off()

#### Fig. 5: relatinoship of crop diversity and crop stability/crop asynchrony for selected countries
a5 <- ggplot(dfDiversityRegion[which(dfDiversityRegion$Country=="IT"),], aes(x=diversity, y=exp(cropStability))) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors[2:5], labels = c("1978-1987","1988-1997","1998-2007","2008-2017")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modStabilitySpaceGroup[which(modStabilitySpaceGroup$Country=="IT"),"interceptStability"],slope = modStabilitySpaceGroup[which(modStabilitySpaceGroup$Country=="IT"),"slopeStability"])+
  theme_classic() +
  xlab("") +
  ylab("Crop stability") +
  theme(axis.title.x = element_text(size=8,vjust = 2)) +
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = "none")+
  xlim(0,6)+
  ylim(0,22)+
  annotate("text", x=0.6, y=22, label="Italy            " ,size=2.5)


b5 <- ggplot(dfDiversityRegion[which(dfDiversityRegion$Country=="GB"),], aes(x=diversity, y=exp(cropStability))) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors[2:5], labels = c("1978-1987","1988-1997","1998-2007","2008-2017")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = modStabilitySpaceGroup[which(modStabilitySpaceGroup$Country=="GB"),"interceptStability"],slope = modStabilitySpaceGroup[which(modStabilitySpaceGroup$Country=="GB"),"slopeStability"])+
  theme_classic() +
  xlab("") +
  ylab("") +
  theme(axis.title.x = element_text(size=8,vjust = 2)) +
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = "none")+
  xlim(0,4)+
  ylim(0,22)+
  annotate("text", x=0.4, y=22, label="  United Kingdom" ,size=2.5)

c5 <- ggplot(dfDiversityRegion[which(dfDiversityRegion$Country=="SE"),], aes(x=diversity, y=cropAsynchrony)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors[2:5], labels = c("1978-1987","1988-1997","1998-2007","2008-2017")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = exp(coef(modStabilityLM)[1]),slope = slopeLM)+
  geom_abline(intercept = modAsynchronySpaceGroup[which(modAsynchronySpaceGroup$Country=="SE"),1],slope = modAsynchronySpaceGroup[which(modAsynchronySpaceGroup$Country=="SE"),2])+
  theme_classic() +
  xlab("Crop diversity") +
  ylab("Crop asynchrony") +
  theme(axis.title.x = element_text(size=8,vjust = 2)) +
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = "none")+
  xlim(0,5)+
  ylim(0,1)+
  annotate("text", x=0.5, y=1, label="Sweden        " ,size=2.5)


d5 <- ggplot(dfDiversityRegion[which(dfDiversityRegion$Country=="DE"),], aes(x=diversity, y=cropAsynchrony)) +
  geom_point(size=0.7) +
  scale_colour_manual(name="Time interval",values = vecColors[2:5], labels = c("1978-1987","1988-1997","1998-2007","2008-2017")) +
  scale_radius(range = c(2,12)) +
  geom_abline(intercept = exp(coef(modStabilityLM)[1]),slope = slopeLM)+
  geom_abline(intercept = modAsynchronySpaceGroup[which(modAsynchronySpaceGroup$Country=="DE"),1],slope = modAsynchronySpaceGroup[which(modAsynchronySpaceGroup$Country=="DE"),2])+
  theme_classic() +
  xlab("Crop diversity") +
  ylab("") +
  theme(axis.title.x = element_text(size=8,vjust = 2)) +
  theme(axis.text.x = element_text(size=8)) +
  theme(axis.title.y = element_text(size=8)) +
  theme(axis.text.y = element_text(size=8)) +
  theme(legend.position = "none")+
  xlim(0,7)+
  ylim(0,1)+
  annotate("text", x=0.7, y=1, label="Germany       " ,size=2.5)

jpeg("results/Fig5.jpeg", width = 16.9, height = 15, units = 'cm',res = 600)
  ggarrange(a5,b5,c5,d5,
            labels = letters[1:4],font.label=list(size=8),
            ncol=2,nrow = 2,widths =  c(1,1), align="h")
dev.off()

############################################################################################
###################           Bootstrapping               ########################################
############################################################################################

# prepare tables
bootnum <- 1000
boot_est <-  boot_se <- matrix(NA, ncol = 15, nrow = bootnum,
                               dimnames = list(NULL, c("(Intercept)", c("diversity","fertilizer","irrigation","soilQuality","soilDiversity","instabilityTemp","instabilityPrec","timePeriod",
                                                                        "diversity:fertilizer","diversity:irrigation","diversity:soilQuality","diversity:soilDiversity","diversity:instabilityTemp","diversity:instabilityPrec"))))

boot_est2 <-  boot_se2 <- matrix(NA, ncol = 2, nrow = bootnum,
                               dimnames = list(NULL, c("(Intercept)", "diversity")))

boot_est3 <-   matrix(NA, ncol = nrow(modStabilitySpaceGroup), nrow = bootnum,
                               dimnames = list(NULL, modStabilitySpaceGroup$Country))

boot_est4 <-   matrix(NA, ncol = nrow(modAsynchronyTimeGroup), nrow = bootnum,
                      dimnames = list(NULL, modAsynchronyTimeGroup$timePeriod))

boot_est5 <-   matrix(NA, ncol = nrow(modAsynchronySpaceGroup), nrow = bootnum,
                      dimnames = list(NULL, modAsynchronySpaceGroup$Country))

# bootstrap and rerun models
set.seed(123456)
for (i in 1:bootnum) {
  show(i)
  data_id <- sample(1:dim(dfCenterRegion)[1], replace = T)
  boot_mod <- lme(productionStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                    diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec,
                  random=~1|Country,
                  data=dfCenterRegion[data_id,],
                  method="ML",
                  na.action=na.exclude)
  
  boot_est[i, names(coef(boot_mod))] <- coef(summary(boot_mod))[, "Value"]
  boot_se[i, names(coef(boot_mod))] <- coef(summary(boot_mod))[, "Std.Error"]
  
  boot_mod2 <- lm(cropStability ~ diversity,data = dfDiversityRegion[data_id,])
  boot_est2[i, names(coef(boot_mod2))] <- coef(summary(boot_mod2))[, "Estimate"]
  boot_se2[i, names(coef(boot_mod2))] <- coef(summary(boot_mod2))[, "Std. Error"]
  
  tryCatch({
    boot_mod3 <- lme(cropStability ~ diversity, random=~1+diversity|Country, method="REML",control=lmeControl(opt='optim'),data = dfDiversityRegion[data_id,])
    boot_mod3Group <-   coef(boot_mod3)
    boot_mod3Group$Country <- row.names(boot_mod3Group)
    boot_est3[i, boot_mod3Group$Country]  <-   boot_mod3Group$diversity
    
  }, error=function(e){show(conditionMessage(e))})
  
  boot_mod4 <- lme(cropAsynchrony ~ diversity, random=~1+diversity|timePeriod, method="REML",control=lmeControl(opt='optim'),data = dfDiversityRegion[data_id,])
  boot_mod4Group <-   coef(boot_mod4)
  boot_mod4Group$timePeriod <- row.names(boot_mod4Group)
  boot_est4[i, boot_mod4Group$timePeriod]  <-   boot_mod4Group$diversity
  
  boot_mod5 <- lme(cropAsynchrony ~ diversity, random=~1+diversity|Country, method="REML",control=lmeControl(opt='optim'),data = dfDiversityRegion[data_id,])
  boot_mod5Group <-   coef(boot_mod5)
  boot_mod5Group$Country <- row.names(boot_mod5Group)
  boot_est5[i, boot_mod5Group$Country]  <-   boot_mod5Group$diversity
  
}


# Overview of estimates and measures 
boot_mean <- apply(boot_est, 2, function(x) mean(x,na.rm=T))
boot_median <- apply(boot_est, 2, function(x) median(x,na.rm=T))
boot_025per <- apply(boot_est, 2, function(x) quantile(x, 0.025,na.rm=T))
boot_975per <- apply(boot_est, 2, function(x) quantile(x, 0.975,na.rm=T))
boot_direction <- apply(boot_est, 2, function(x) (sum(x>0,na.rm=T)/sum(!is.na(x)))*100)

overview <- round(cbind(coef(summary(modStabilityRegionLME))[, "Value"], coef(summary(modStabilityRegionLME))[, "Std.Error"], coef(summary(modStabilityRegionLME))[, "p-value"],
                        boot_direction, 
                        boot_mean,boot_median, boot_025per, 
                        boot_975per), 4)

overview[which(overview[,1]<0),"boot_direction"] <- 100-overview[which(overview[,1]<0),"boot_direction"]
overview
colnames(overview) <- c("Estimate","SE","p-value","Direction","Mean","Median","2.5% Quantile","97.5% Quantile")
write.xlsx(overview,"results/TableS2.xlsx",row.names=T)

boot_direction <- apply(boot_est2, 2, function(x) (sum(x>0,na.rm=T)/sum(!is.na(x)))*100)
boot_mean <- apply(boot_est2, 2, function(x) mean(x,na.rm=T))
boot_median <- apply(boot_est2, 2, function(x) median(x,na.rm=T))
boot_025per <- apply(boot_est2, 2, function(x) quantile(x, 0.025,na.rm=T))
boot_975per <- apply(boot_est2, 2, function(x) quantile(x, 0.975,na.rm=T))
overview <- round(cbind(summary(modStabilityLM)$coef[,1],summary(modStabilityLM)$coef[,2],summary(modStabilityLM)$coef[,4],
                        boot_direction, 
                        boot_mean,boot_median, boot_025per, 
                        boot_975per), 4)
overview[which(overview[,1]<0),"boot_direction"] <- 100-overview[which(overview[,1]<0),"boot_direction"]
colnames(overview) <- c("Estimate","SE","p-value","Direction","Mean","Median","2.5% Quantile","97.5% Quantile")
write.xlsx(overview,"results/TableS3.xlsx",row.names=T)

boot_direction <- apply(boot_est3, 2, function(x) (sum(x>0,na.rm=T)/sum(!is.na(x)))*100)
boot_mean <- apply(boot_est3, 2, function(x) mean(x,na.rm=T))
boot_median <- apply(boot_est3, 2, function(x) median(x,na.rm=T))
boot_025per <- apply(boot_est3, 2, function(x) quantile(x, 0.025,na.rm=T))
boot_975per <- apply(boot_est3, 2, function(x) quantile(x, 0.975,na.rm=T))
rownames(modStabilitySpaceGroup)==names(boot_975per)
overview <- round(cbind(modStabilitySpaceGroup$diversity,boot_direction, 
                        boot_mean,boot_median, boot_025per, 
                        boot_975per), 4)
overview[which(overview[,1]<0),"boot_direction"] <- 100-overview[which(overview[,1]<0),"boot_direction"]
colnames(overview) <- c("Estimate","Direction","Mean","Median","2.5% Quantile","97.5% Quantile")
write.xlsx(overview,"results/TableS4.xlsx",row.names=T)

boot_direction <- apply(boot_est4, 2, function(x) (sum(x>0,na.rm=T)/sum(!is.na(x)))*100)
boot_mean <- apply(boot_est4, 2, function(x) mean(x,na.rm=T))
boot_median <- apply(boot_est4, 2, function(x) median(x,na.rm=T))
boot_025per <- apply(boot_est4, 2, function(x) quantile(x, 0.025,na.rm=T))
boot_975per <- apply(boot_est4, 2, function(x) quantile(x, 0.975,na.rm=T))
rownames(modAsynchronyTimeGroup)==names(boot_975per)
overview <- round(cbind(modAsynchronyTimeGroup$diversity,boot_direction, 
                        boot_mean,boot_median, boot_025per, 
                        boot_975per), 4)
overview[which(overview[,1]<0),"boot_direction"] <- 100-overview[which(overview[,1]<0),"boot_direction"]
colnames(overview) <- c("Estimate","Direction","Mean","Median","2.5% Quantile","97.5% Quantile")
write.xlsx(overview,"results/TableS5.xlsx",row.names=T)

boot_direction <- apply(boot_est5, 2, function(x) (sum(x>0,na.rm=T)/sum(!is.na(x)))*100)
boot_mean <- apply(boot_est5, 2, function(x) mean(x,na.rm=T))
boot_median <- apply(boot_est5, 2, function(x) median(x,na.rm=T))
boot_025per <- apply(boot_est5, 2, function(x) quantile(x, 0.025,na.rm=T))
boot_975per <- apply(boot_est5, 2, function(x) quantile(x, 0.975,na.rm=T))
rownames(modAsynchronySpaceGroup)==names(boot_975per)
overview <- round(cbind(modAsynchronySpaceGroup$diversity,boot_direction, 
                        boot_mean, boot_median, boot_025per, 
                        boot_975per), 4)
overview[which(overview[,1]<0),"boot_direction"] <- 100-overview[which(overview[,1]<0),"boot_direction"]
colnames(overview) <- c("Estimate","Direction","Mean","Median","2.5% Quantile","97.5% Quantile")
write.xlsx(overview,"results/TableS6.xlsx",row.names=T)


rm(list=ls())




