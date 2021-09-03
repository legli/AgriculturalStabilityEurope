
############### function to create maps

funMaps <- function(dfInput,variable,map,level,b,colVec,tit,lab)
{
  
  ## get df
  dfTarget <- dfInput
  
  b <- seq(min(dfTarget[,variable]),max(dfTarget[,variable]),length.out = 11)
  labScale <- b

  # restrict df (for right colors)
  dfTarget[which(dfTarget[,variable]>max(b)),variable] <- max(b)  
  dfTarget[which(dfTarget[,variable]<min(b)),variable] <- min(b)
  dfTarget$dim <- dfTarget[,variable]
 

  ## join to map
  mapsBivariate <- fortify(map,region=level)
  mapsBivariate = merge(mapsBivariate, dfTarget[,c(level,"dim")], by=level)
  
  ## plot
  fig = ggplot() + 
    geom_sf(data = st_as_sf(mapCountry), fill = "white",size=0.1) +
    geom_sf(data = mapsBivariate, aes(fill = dim),size=0.1) +
    scale_fill_gradientn(colours=colVec,
                         values=rescale(b),
                         na.value="white", guide="colourbar",
                         name=tit,limits=c(min(b),max(b)),breaks=b, 
                         labels=round(labScale,2))+
    theme(legend.position = c(0.85, 0.5)) +
    theme_void() +
    coord_sf(xlim = c(-25,30),
             ylim = c(35,70)) +
    guides(fill = guide_colorbar(barheight = 5,barwidth = 0.2,title.position = "left"))+
    theme(legend.title = element_text(size=6,angle = 90),legend.text = element_text(size=6))+
    labs(title = lab)+
    theme(title = element_text(hjust = 0, face= "bold",size=6))
    
  
  fig
}

############### function to create barplot of main effects

funEffect <- function(mod,yTitle,label){
 
  dfStability <- data.frame(coef(summary(mod))[2:7,c(1,2,4)])
  names(dfStability) <- c("Effect","SE","pVal")
  dfStability$nam <- c("Crop diversity","Soil productivity","Soil type diversity","Temperature instability","Precipitation instability","Time")
  dfStability$nam <- factor(dfStability$nam, levels = unique(dfStability$nam))

  dfStability$labHeight <- dfStability$Effect
  dfStability[which(dfStability$Effect>0),"labHeight"] <- dfStability[which(dfStability$Effect>0),"Effect"] + dfStability[which(dfStability$Effect>0),"SE"] + 0.05
  dfStability[which(dfStability$Effect<0),"labHeight"] <- dfStability[which(dfStability$Effect<0),"Effect"] - dfStability[which(dfStability$Effect<0),"SE"] - 0.05
  dfStability$lab <- ""
  dfStability[which(dfStability$pVal<0.05),"lab"] <- "*"
  dfStability[which(dfStability$pVal<0.01),"lab"] <- "**"
  dfStability[which(dfStability$pVal<0.001),"lab"] <- "***"
  dfStability[which(dfStability$pVal>=0.05),"lab"] <- "NS"
  dfStability$lab <- factor(dfStability$lab, levels = unique(dfStability$lab))
  
  dfText <- data.frame(xpos=1:6,ypos=dfStability$labHeight,lab=dfStability$lab)
  
  fig <- ggplot(data=dfStability, aes(x=nam, y=Effect)) +
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                  position=position_dodge(.9)) +
    geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
    theme_classic() +  
    xlab("") +
    scale_y_continuous(breaks = round(seq(-0.6,0.6, by = 0.2),1),limits=c(-0.6,0.6)) +
    ylab(yTitle) +
    theme(axis.title.y=element_text(size=8)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=8))+
    theme(axis.text.y = element_text(size=8))+
    geom_hline(yintercept=0)+
    theme(legend.position = c(0.9, 0.8))+
    theme(legend.title = element_text(size = 8),
          legend.text = element_text(size = 8))+
    theme(legend.key.size = unit(0.2,"cm")) +
    theme(plot.margin = unit(c(0.2,0.3,-0.5,0.5), "cm")) 
  
    if(label==F){
      fig <- fig + scale_x_discrete(labels=c("","","","","","")) 
    }
  fig
}


############### function to visualize interaction effects
funInteraction <- function(dfPredict,dfCenter,dfLog,effect,moderator,modS,xlabel,ylabel,modLabel,yVal1,yVal2,pVal){
  
  dfPredictNew <- dfPredict
  dfPredictNew[,effect] <-  seq(min(dfCenter[,effect]), max(dfCenter[,effect]), length.out = 1e3)
  
  # combined model
  dfPredictNew$Mean <- predict(modS,newdata = dfPredictNew,level=0)    
  dfPredictNew[,moderator] <-  1
  dfPredictNew$High <- predict(modS,newdata = dfPredictNew,level=0)    
  dfPredictNew[,moderator] <-  -1
  dfPredictNew$Low <- predict(modS,newdata = dfPredictNew,level=0)    
  
  dfPredictNew$Mean <- exp(dfPredictNew$Mean)
  dfPredictNew$High <- exp(dfPredictNew$High)
  dfPredictNew$Low <- exp(dfPredictNew$Low)
  
  dfPredictNew[,effect] <- dfPredictNew[,effect]*sd(dfLog[,effect])+mean(dfLog[,effect])  
  
  dfFinal <- dfPredictNew[,c(effect,"Mean","High","Low")]  %>% gather(Moderator, Stability, "Mean":"Low")
  dfFinal$Moderator <- factor(dfFinal$Moderator,levels = c("Low","Mean","High"))
  ggplot(data = dfFinal, aes(x = dfFinal[,effect], y = Stability,color=Moderator)) +
    # geom_point() +
    geom_line(size=0.5)+
    theme_classic() +
    theme(axis.title=element_text(size=8),axis.text=element_text(size=6)) +
    xlab(xlabel)+
    ylab(ylabel)+
    ylim(yVal1,yVal2)+
    theme(legend.position = c(0.8,0.17),legend.title = element_text(size = 6),legend.text = element_text(size = 6),legend.key.size = unit(0.5, "lines"))+
    guides(shape = guide_legend(override.aes = list(size = 0.5)),color = guide_legend(override.aes = list(size = 0.5)))+
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))+
    scale_color_manual(name=modLabel,values = c("#E69F00","#999999","#56B4E9"))+
    annotate("text", x=2, y=1, label= pVal,size=2)
}
