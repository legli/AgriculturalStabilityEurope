## libraries
library(eurostat)
library(rgdal)
library(rgeos)


# get NUTS geodata
shpNUTS2 <- readOGR("spatial","NUTS2_europe")
names(shpNUTS2)
head(shpNUTS2@data)
plot(shpNUTS2, col = ifelse(shpNUTS2$NUTS_ID == "SE12",'red','white'))

#### get production file 
dfAgricultureOld <- get_eurostat("apro_cpnhr_h",time_format = "raw")
head(dfAgricultureOld)
unique(dfAgricultureOld$strucpro)
dfAgricultureNew <- get_eurostat("apro_cpnhr",time_format = "raw")
head(dfAgricultureNew)
unique(dfAgricultureNew$strucpro)

dfAgriculture <- rbind(dfAgricultureOld,dfAgricultureNew)

head(dfAgriculture)
head(dfAgriculture)
unique(dfAgriculture$crops)
unique(dfAgriculture$strucpro)
unique(dfAgriculture$geo)
unique(dfAgriculture$time)
dfAgriculture$time <- as.numeric(dfAgriculture$time) # change year to numeric

# only include harvested area (AR; 1000ha), production (PR; 1000t), main area (MA; 1000ha)
dfAgriculture <- dfAgriculture[which(dfAgriculture$strucpro%in%c("AR","PR","MA")&
                                       dfAgriculture$time>=1978&dfAgriculture$time<=2017),]
head(dfAgriculture)

# remove NA
dfAgriculture <- dfAgriculture[which(!is.na(dfAgriculture$values)),]


#### select target regions
# keep relevant regions (highest resolution and with most entries)
dfAgriculture$geo <- as.character(dfAgriculture$geo)

## keep relevant regions
dfAgricultureRed <- dfAgriculture[which(dfAgriculture$geo%in% 
                                          c("AT11","AT12","AT13","AT21","AT22","AT31","AT32","AT33","AT34",
                                            "BE10","BE21","BE22","BE23","BE24","BE25","BE31","BE32","BE33","BE34","BE35",
                                            "BG31","BG32","BG33","BG34","BG41","BG42",
                                            "CZ01","CZ02","CZ03","CZ04","CZ05","CZ06","CZ07","CZ08",
                                            "CY",
                                            "DE1","DE2","DE4","DE5","DE6","DE7","DE8","DE9","DEA","DEB","DEC","DED","DEE","DEF","DEG", # only few entries with higher resolution
                                            "DK01","DK02","DK03","DK04","DK05",
                                            "EE",
                                            "EL11","EL12","EL13","EL14","EL21","EL22","EL23","EL24","EL25","EL30","EL41","EL42","EL43","EL51","EL52","EL53","EL54","EL61","EL62","EL63","EL64","EL65",
                                            "ES11","ES12","ES13","ES21","ES22","ES23","ES24","ES30","ES41","ES42","ES43","ES51","ES52","ES53","ES61","ES62","ES70",
                                            "FI19","FI1B","FI1C","FI1D","FI20",
                                            "FR10","FR21","FR22","FR23","FR24","FR25","FR26","FR30","FR41","FR42","FR43","FR51","FR52","FR53","FR61","FR62","FR63","FR71","FR72","FR81","FR82","FR83","FR91","FR92","FR93","FR94",
                                            "HU1","HU21","HU22","HU23","HU31","HU32","HU33", # HU1 has more entries than HU10
                                            "HR03","HR04",
                                            "IE01","IE02",
                                            "ITC1","ITC2","ITC3","ITC4","ITD5","ITE3","ITF1","ITF2","ITF3","ITF4","ITF5","ITF6","ITG1","ITG2","ITH3","ITH4","ITI1","ITI2","ITI4",
                                            "LT",
                                            "LU",
                                            "LV",
                                            "MT",
                                            "NL11","NL12","NL13","NL21","NL22","NL23","NL31","NL32","NL33","NL34","NL41","NL42",
                                            "PL11","PL12","PL21","PL22","PL31","PL32","PL33","PL34","PL41","PL42","PL43","PL51","PL52","PL61","PL62","PL63",
                                            "PT11","PT15","PT16","PT17","PT18","PT20","PT30",
                                            "RO11","RO12","RO21","RO22","RO31","RO32","RO41","RO42",
                                            "SE11","SE12","SE21","SE22","SE23","SE31","SE32","SE33",
                                            "SI03","SI04",
                                            "SK01","SK02","SK03","SK04",
                                            "UKC","UKD","UKE","UKF","UKG","UKH","UKJ","UKK","UKL","UKM","UKN")),] # no higher resolution

dfAgricultureRed <- as.data.frame(dfAgricultureRed)

#### get spatial resolution
sort(as.character(unique(shpNUTS2@data$NUTS_ID)))
sort(as.character(unique(dfAgricultureRed$geo)))

unique(dfAgricultureRed[-which(dfAgricultureRed$geo%in%shpNUTS2@data$NUTS_ID),"geo"])
# remove non matching regions of higher resolution
dfAgricultureRed <- dfAgricultureRed[-which(dfAgricultureRed$geo%in%c("ITD5","ITE3","FR93","FR94","FR91","FR92",
                                                                      "EL11","EL12","EL13","EL14","EL21","EL22","EL23","EL24","EL25")),]

# add every row to closest resolution
dfNUTS2 <- shpNUTS2@data
dfGeo <- dfNUTS2
dfGeo$geo <- NA
r <- 0
repeat {
  r <- r +1 
  if (dfGeo[r,"NUTS_ID"]%in%unique(dfAgricultureRed$geo))
  {
    dfGeo[r,"geo"] <- as.character(dfGeo[r,"NUTS_ID"])
  }
  if (is.na(dfGeo[r,"geo"])&substr(dfGeo[r,"NUTS_ID"],1,3)%in%unique(dfAgricultureRed$geo))
  {
    dfGeo[r,"geo"]<- substr(as.character(dfGeo[r,"NUTS_ID"]),1,3)
  }  
  if (is.na(dfGeo[r,"geo"])&substr(dfGeo[r,"NUTS_ID"],1,2)%in%unique(dfAgricultureRed$geo))
  {
    dfGeo[r,"geo"] <- substr(as.character(dfGeo[r,"NUTS_ID"]),1,2)
  }    
  if (r==nrow(dfGeo))
    break;
}
head(dfGeo)
nrow(dfGeo)
dfGeo <- dfGeo[order(dfGeo$NUTS_ID),]
dfGeo[,c("NUTS_ID","geo")]
dfGeo <- unique(dfGeo[!is.na(dfGeo$geo),c("NUTS_ID","geo")])

## only keep final target regions in agricultural production data
dfAgricultureFinal <- dfAgricultureRed[which(dfAgricultureRed$geo%in%unique(dfGeo$geo)),]
head(dfAgricultureFinal)
sort(as.character(unique(dfAgricultureFinal$geo)))
length(unique(dfAgricultureFinal$geo))
write.csv(dfAgricultureFinal,"datasets/agriculturalProduction_europe.csv",row.names = F)

## export necessary geoinfo
nrow(dfGeo)
shpNUTS2 <- merge(shpNUTS2,dfGeo,by="NUTS_ID")
sum(shpNUTS2@data$NUTS_ID==shpNUTS2@data$geo,na.rm=T)
# disolve shapefile by availabe resolution
shpGeo <- gUnaryUnion(shpNUTS2, id = shpNUTS2@data$geo)
plot(shpGeo)

IDs <- data.frame(NUTS_ID=sapply(slot(shpGeo, "polygons"), function(x) slot(x, "ID")))
rownames(IDs)  <- IDs$NUTS_ID
shpGeo <- SpatialPolygonsDataFrame(shpGeo,IDs)
sort(as.character(shpGeo@data$NUTS_ID))
plot(shpGeo, col = ifelse(shpGeo@data$NUTS_ID == "SE12",'red','white'))
plot(shpGeo, col = ifelse(shpGeo@data$NUTS_ID == "CY",'red','white'))

writeOGR(shpGeo, dsn = 'spatial', layer = 'regions_europe', driver = "ESRI Shapefile",overwrite_layer = T)


rm(list=ls())

