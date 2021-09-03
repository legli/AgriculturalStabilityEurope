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
myColors <- c("#4daf4a","#045A8D", "#ff7f00")

############################################################################################
###################              DATA               ########################################
############################################################################################

dfRegion <- read.csv("datasets/dataFinal_regional.csv")
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

###### European analyses

#### 1: determinants of production stability
# check distribution of response variable
fitdist(dfRegion$productionCaloriesStability,"norm")$aic - fitdist(dfRegion$productionCaloriesStability,"lnorm")$aic # +234.0703 <- log normally distributed
fitdist(dfRegion$productionProteinStability,"norm")$aic - fitdist(dfRegion$productionProteinStability,"lnorm")$aic # +282.273 <- log normally distributed
fitdist(dfRegion$productionFatStability,"norm")$aic - fitdist(dfRegion$productionFatStability,"lnorm")$aic # +261.9873 <- log normally distributed


### regression analyses
## transformations
dfTransRegion=with(dfRegion,data.frame(Region,Country,
                                       productionCaloriesStability = log(productionCaloriesStability),
                                       productionProteinStability = log(productionProteinStability),
                                       productionFatStability = log(productionFatStability),
                                       diversity,
                                       soilQuality,soilDiversity,
                                       irrigation=irrigation,
                                       fertilizer=sqrt(fertilizer),
                                       instabilityTemp,instabilityPrec,
                                       timePeriod
))

# scale predictors for standardized regression
dfPredictorsRegion=sapply(dfTransRegion[,-c(1:5)],function(x)scale(x,center = T,scale=T)[,1])
dfCenterRegion=data.frame(Region=dfTransRegion[,1],Country=dfTransRegion[,2],
                          productionCaloriesStability=dfTransRegion[,3],productionProteinStability=dfTransRegion[,4],productionFatStability=dfTransRegion[,5],
                          dfPredictorsRegion)
head(dfCenterRegion)

## check colinearity
round(cor(dfCenterRegion[,3:13],method='s'),2)

### regression models
modStabilityCaloriesRegion <- lm(productionCaloriesStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                           diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec
                         ,data=dfCenterRegion)
summary(modStabilityCaloriesRegion)

modStabilityProteinRegion <- lm(productionProteinStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                                   diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec
                                 ,data=dfCenterRegion)
summary(modStabilityProteinRegion)

modStabilityFatRegion <- lm(productionFatStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                                   diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec
                                 ,data=dfCenterRegion)
summary(modStabilityFatRegion)


modStabilityCaloriesRegionLME <- lme(productionCaloriesStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                               diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec,
                             random=~1|Country,method = "REML",
                             data=dfCenterRegion)
r.squaredGLMM(modStabilityCaloriesRegionLME) # R2m = 0.300913; R2c = 0.5753768
max(vif(modStabilityCaloriesRegionLME)) # max. 2.61
anova(modStabilityCaloriesRegionLME,modStabilityCaloriesRegion) # AIC LME = 609.1527; AIC LM = 677.0014 -> LME is better

modStabilityProteinRegionLME <- lme(productionProteinStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                               diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec,
                             random=~1|Country,method = "REML",
                             data=dfCenterRegion)
r.squaredGLMM(modStabilityProteinRegionLME) # R2m = 0.3046367; R2c = 0.5693723
max(vif(modStabilityProteinRegionLME)) # max. 2.61
anova(modStabilityProteinRegionLME,modStabilityProteinRegion) # AIC LME = 618.0708; AIC LM = 680.7654 -> LME is better

modStabilityFatRegionLME <- lme(productionFatStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                               diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec,
                             random=~1|Country,method = "REML",
                             data=dfCenterRegion)
r.squaredGLMM(modStabilityFatRegionLME) # R2m = 0.2181067; R2c = 0.2990076
max(vif(modStabilityFatRegionLME)) # max. 2.73
anova(modStabilityFatRegionLME,modStabilityFatRegion) # AIC LME = 622.5513; AIC LM = 624.6317 -> LME is slightly better



###### Analyses for selected countries
table(dfRegion$Country)
funMod <- function(response,country){
  dfCountry <- dfCenterRegion[which(dfCenterRegion$Country==country),]
  dfCountrydependentVariable <- dfCountry[,response]
  lm(dfCountrydependentVariable~diversity+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod,
                                data=dfCountry)
}
modStabilityCaloriesDE <- funMod("productionCaloriesStability","DE")
modStabilityCaloriesES <- funMod("productionCaloriesStability","ES")
modStabilityCaloriesFR <- funMod("productionCaloriesStability","FR")
modStabilityCaloriesIT <- funMod("productionCaloriesStability","IT")


############################################################################################
###################           FIGURES               ########################################
############################################################################################

##### Fig 1: maps of resopnse and predictors

# regional level
mapRegion <- readOGR("spatial/regions_europe.shp")
mapCountry <- readOGR("spatial/countries_global.shp")
mapCountry$Country <-  countrycode(mapCountry$Area, 'country.name', 'iso2c')
mapCountry@bbox <- mapRegion@bbox

mapRegion$Region <- mapRegion$NUTS_ID
dfRegionAgg <- aggregate(cbind(productionCaloriesStability,productionProteinStability,productionFatStability,diversity,fertilizer,irrigation,soilQuality,soilDiversity,instabilityTemp,instabilityPrec)~Region,dfRegion,mean)

a1 <- funMaps(dfRegionAgg,variable="productionCaloriesStability",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Caloric stability","a")
b1 <- funMaps(dfRegionAgg,variable="productionProteinStability",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Protein stability","b")
c1 <- funMaps(dfRegionAgg,variable="productionFatStability",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Fat stability","c")
d1 <- funMaps(dfRegionAgg,variable="diversity",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Crop diversity","d")
e1 <- funMaps(dfRegionAgg,variable="fertilizer",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Nitrogen use (t/ha)","e")
f1 <- funMaps(dfRegionAgg,variable="irrigation",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Irrigation (%)","f")
g1 <- funMaps(dfRegionAgg,variable="soilQuality",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Soil productivity","g")
h1 <- funMaps(dfRegionAgg,variable="soilDiversity",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Soil type diversity","h")
i1 <- funMaps(dfRegionAgg,variable="instabilityTemp",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Temperature instability","i")
j1 <- funMaps(dfRegionAgg,variable="instabilityPrec",st_as_sf(mapRegion),"Region",seq(0,50,length.out = 11),c("#FFFFE5","#78C679","#004529"),"Precipitation instability","j")

jpeg("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/StabilityEurope/Revision/Revision2/Results/Fig1.jpeg", width = 17.4,height = 8, units = 'cm',res = 600)
  grid.arrange(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,ncol=5,nrow=2)
dev.off()


## Fig 2: barplots of  stability model
dfStabilityCalories <- data.frame(summary(modStabilityCaloriesRegionLME)$tTable[2:9,c(1,2,5)])
names(dfStabilityCalories) <- c("Effect","SE","pVal")
dfStabilityCalories$nam <- c("Crop diversity","sqrt(Nitrogen use)","Irrigation","Soil productivity","Soil type diversity","Temperature instability","Precipitation instability","Time")
dfStabilityCalories$nam <- factor(dfStabilityCalories$nam, levels = unique(dfStabilityCalories$nam))
dfStabilityCalories$Nutrient <- "Calories"

dfStabilityProtein <- data.frame(summary(modStabilityProteinRegionLME)$tTable[2:9,c(1,2,5)])
names(dfStabilityProtein) <- c("Effect","SE","pVal")
dfStabilityProtein$nam <- c("Crop diversity","sqrt(Nitrogen use)","Irrigation","Soil productivity","Soil type diversity","Temperature instability","Precipitation instability","Time")
dfStabilityProtein$nam <- factor(dfStabilityProtein$nam, levels = unique(dfStabilityProtein$nam))
dfStabilityProtein$Nutrient <- "Protein"

dfStabilityFat <- data.frame(summary(modStabilityFatRegionLME)$tTable[2:9,c(1,2,5)])
names(dfStabilityFat) <- c("Effect","SE","pVal")
dfStabilityFat$nam <- c("Crop diversity","sqrt(Nitrogen use)","Irrigation","Soil productivity","Soil type diversity","Temperature instability","Precipitation instability","Time")
dfStabilityFat$nam <- factor(dfStabilityFat$nam, levels = unique(dfStabilityFat$nam))
dfStabilityFat$Nutrient <- "Fat"

dfCombined <- rbind(dfStabilityCalories,dfStabilityProtein,dfStabilityFat)
dfCombined$Nutrient <- factor(dfCombined$Nutrient, levels = c("Calories","Protein","Fat"))


dfCombined$labHeight <- dfCombined$Effect
dfCombined[which(dfCombined$Effect>0),"labHeight"] <- dfCombined[which(dfCombined$Effect>0),"Effect"] + dfCombined[which(dfCombined$Effect>0),"SE"] + 0.03
dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"] - dfCombined[which(dfCombined$Effect<0),"SE"] - 0.03
dfCombined$lab <- ""
dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))

dfCombined <- dfCombined[unlist(lapply(1:8,function(i)seq(i,24,8))),]

dfText <- data.frame(xpos=sort(c(1:8-0.3,1:8,1:8+0.3)),ypos=dfCombined$labHeight,lab=dfCombined$lab,Nutrient=dfCombined$Nutrient)

a2 <- ggplot(data=dfCombined, aes(x=nam, y=Effect,fill=Nutrient)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                position=position_dodge(.9)) +
  geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
  theme_classic() +  
  xlab("") +
  scale_y_continuous(breaks = round(seq(-0.4,0.4, by = 0.1),1),limits=c(-0.4,0.4)) +
  ylab("Standardized regression coefficient") +
  theme(axis.title.y=element_text(size=8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
  theme(axis.text.y = element_text(size=8))+
  scale_fill_manual(name = "",values = myColors)+
  geom_hline(yintercept=0)+
  theme(legend.position = c(0.9, 0.8))+
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))+
  theme(legend.key.size = unit(0.2,"cm")) +
  theme(plot.margin = unit(c(0.2,0.3,-0.5,0.5), "cm")) 

# plot
jpeg("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/StabilityEurope/Revision/Revision2/Results/Fig2.jpeg", width = 12.9, height = 8, units = 'cm', res = 600)
  a2
dev.off()


## Fig 3: interactions
  
dfTab <- data.frame(summary(modStabilityCaloriesRegionLME)$tTable[,c(1,2,4,5)])
dfPredictRegion <- data.frame(diversity=rep(0,1000),irrigation=0,fertilizer=0,soilQuality=0,soilDiversity=0,instabilityTemp=0,instabilityPrec=0,timePeriod=0)

a3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="fertilizer",modS=modStabilityCaloriesRegionLME,xlabel="",ylabel="Caloric stability",
                      modLabel="Nitrogen use",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[10,4],2)))
b3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="irrigation",modS=modStabilityCaloriesRegionLME,xlabel="Crop diversity",ylabel="",
                     modLabel="Irrigation",yVal1=0,yVal2=20,pVal=paste0("p = ","<0.05"))
c3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="soilQuality",modS=modStabilityCaloriesRegionLME,xlabel="",ylabel="",
                     modLabel="Soil productivity",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[12,4],2)))
d3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="soilDiversity",modS=modStabilityCaloriesRegionLME,xlabel="",ylabel="Caloric stability",
                     modLabel="Soil type diversity",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[13,4],2)))
e3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="instabilityTemp",modS=modStabilityCaloriesRegionLME,xlabel="Crop diversity",ylabel="",
                     modLabel="Temperature instability",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[14,4],2)))
f3 <- funInteraction(dfPredict=dfPredictRegion,dfCenter=dfCenterRegion,dfLog=dfTransRegion,effect="diversity",moderator="instabilityPrec",modS=modStabilityCaloriesRegionLME,xlabel="",ylabel="",
                     modLabel="Precipitation instability",yVal1=0,yVal2=20,pVal=paste0("p = ",round(dfTab[15,4],2)))


jpeg("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/StabilityEurope/Revision/Revision2/Results/Fig3.jpeg", width = 17.4, height = 12, units = 'cm',res = 600)
  ggarrange(a3,b3,c3,d3,e3,f3,
            labels = letters[1:6],font.label=list(size=8),
            ncol = 3, nrow = 2)
dev.off()


## Fig 4: barplots of  country models
a4 <- funEffect(modStabilityCaloriesDE,"Standardized regression coefficient",F)
b4 <- funEffect(modStabilityCaloriesES,"",F)
c4 <- funEffect(modStabilityCaloriesFR,"Standardized regression coefficient",T)
d4 <- funEffect(modStabilityCaloriesIT,"",T)

# plot
jpeg("C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/StabilityEurope/Revision/Revision2/Results/Fig4.jpeg", width = 17.4, height = 12, units = 'cm', res = 600)
  ggarrange(a4,b4,c4,d4,
            labels = letters[1:4],font.label=list(size=8),
            ncol = 2, nrow = 2,heights=c(1,1.3))
dev.off()

############################################################################################
###################           Bootstrapping               ##################################
############################################################################################

# prepare tables
bootnum <- 1000
boot_est1 <-  boot_est2 <-  boot_est3 <-    
                              matrix(NA, ncol = 15, nrow = bootnum,
                               dimnames = list(NULL, c("(Intercept)", c("diversity","fertilizer","irrigation","soilQuality","soilDiversity","instabilityTemp","instabilityPrec","timePeriod",
                                                                        "diversity:fertilizer","diversity:irrigation","diversity:soilQuality","diversity:soilDiversity","diversity:instabilityTemp","diversity:instabilityPrec"))))

boot_est_country1 <- boot_est_country2 <- boot_est_country3 <- boot_est_country4 <-  
                    matrix(NA, ncol = 7, nrow = bootnum,
                    dimnames = list(NULL, c("(Intercept)", c("diversity","soilQuality","soilDiversity","instabilityTemp","instabilityPrec","timePeriod"))))



# bootstrap and rerun models
set.seed(123456)
for (i in 1:bootnum) {
  show(i)
  data_id <- sample(1:dim(dfCenterRegion)[1], replace = T)
  dfTarget <- dfCenterRegion[data_id,]
  boot_mod1 <- lme(productionCaloriesStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                    diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec,
                  random=~1|Country,
                  data=dfTarget,
                  method="REML",
                  na.action=na.exclude)
  
  boot_mod2 <- lme(productionProteinStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                     diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec,
                   random=~1|Country,
                   data=dfTarget,
                   method="REML",
                   na.action=na.exclude)
  
  boot_mod3 <- lme(productionFatStability~diversity+fertilizer+irrigation+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod+
                     diversity:fertilizer+diversity:irrigation+diversity:soilQuality+diversity:soilDiversity+diversity:instabilityTemp+diversity:instabilityPrec,
                   random=~1|Country,
                   data=dfTarget,
                   method="REML",
                   na.action=na.exclude)
  
  boot_est1[i, names(coef(boot_mod1))] <- coef(summary(boot_mod1))[, "Value"]
  boot_est2[i, names(coef(boot_mod2))] <- coef(summary(boot_mod2))[, "Value"]
  boot_est3[i, names(coef(boot_mod3))] <- coef(summary(boot_mod3))[, "Value"]

  boot_mod_country1 <- lm(productionCaloriesStability~diversity+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod,
                         data=dfTarget[which(dfTarget$Country=="DE"),])
  boot_mod_country2 <- lm(productionCaloriesStability~diversity+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod,
                            data=dfTarget[which(dfTarget$Country=="ES"),])
  boot_mod_country3 <- lm(productionCaloriesStability~diversity+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod,
                            data=dfTarget[which(dfTarget$Country=="FR"),])
  boot_mod_country4 <- lm(productionCaloriesStability~diversity+soilQuality+soilDiversity+instabilityTemp+instabilityPrec+timePeriod,
                            data=dfTarget[which(dfTarget$Country=="IT"),])
  
  boot_est_country1[i, names(coef(boot_mod_country1))] <- coef(summary(boot_mod_country1))[, "Estimate"]
  boot_est_country2[i, names(coef(boot_mod_country2))] <- coef(summary(boot_mod_country2))[, "Estimate"]
  boot_est_country3[i, names(coef(boot_mod_country3))] <- coef(summary(boot_mod_country3))[, "Estimate"]
  boot_est_country4[i, names(coef(boot_mod_country4))] <- coef(summary(boot_mod_country4))[, "Estimate"]

}


# Overview of estimates and measures 
funTable <- function(bootMatrix,mod,name){
  boot_mean <- apply(bootMatrix, 2, function(x) mean(x,na.rm=T))
  boot_median <- apply(bootMatrix, 2, function(x) median(x,na.rm=T))
  boot_025per <- apply(bootMatrix, 2, function(x) quantile(x, 0.025,na.rm=T))
  boot_975per <- apply(bootMatrix, 2, function(x) quantile(x, 0.975,na.rm=T))
  boot_direction <- apply(bootMatrix, 2, function(x) (sum(x>0,na.rm=T)/sum(!is.na(x)))*100)
  
  dfSummary <- coef(summary(mod))
  ind <- 4
  if(ncol(dfSummary)==5){ind <- 5}
  overview <- round(cbind(dfSummary[, 1], dfSummary[, 2], dfSummary[, ind],
                          boot_direction, 
                          boot_mean,boot_median, boot_025per, 
                          boot_975per), 4)
  
  overview[which(overview[,1]<0),"boot_direction"] <- 100-overview[which(overview[,1]<0),"boot_direction"]
  colnames(overview) <- c("Estimate","SE","p-value","Direction","Mean","Median","2.5% Quantile","97.5% Quantile")
  dfOverview <- as.data.frame(overview)
  dfOverview$model <- name
  dfOverview$variable <- row.names(dfOverview)
  dfOverview[,c("variable","Estimate","SE","p-value","Direction","Mean","Median", "2.5% Quantile", "97.5% Quantile", "model")]
}
tableS2 <- rbind(funTable(boot_est1,modStabilityCaloriesRegionLME,"Calories"),funTable(boot_est2,modStabilityProteinRegionLME,"Protein"),
                 funTable(boot_est3,modStabilityFatRegionLME,"Fat"))

write.xlsx(tableS2,"C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/StabilityEurope/Revision/Revision2/Results/TableS2.xlsx",row.names=F)

tableS3 <- rbind(funTable(boot_est_country1,modStabilityCaloriesDE,"DE"),funTable(boot_est_country2,modStabilityCaloriesES,"ES"),
                 funTable(boot_est_country3,modStabilityCaloriesFR,"FR"),funTable(boot_est_country4,modStabilityCaloriesIT,"IT"))

write.xlsx(tableS3,"C:/Users/egli/Nextcloud/Cloud/PhD_Leipzig/Publications/StabilityEurope/Revision/Revision2/Results/TableS3.xlsx",row.names=F)

rm(list=ls())




