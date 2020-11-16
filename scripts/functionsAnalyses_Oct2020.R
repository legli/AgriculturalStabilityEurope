
############### Figure 1, 3, 5

funMaps <- function(dfInput,variable,map,level,b,colVec,tit,lab)
{
  
  ## get df
  dfTarget <- dfInput
  
  b <- seq(min(dfTarget[,variable]),max(dfTarget[,variable]),length.out = 11)
  labScale <- b
  # if (max(dfTarget[,variable])>max(b))
  # {
  #   labScale[length(labScale)] <- paste0(">",max(b))
  # }
  # 
  # if (min(dfTarget[,variable])<min(b))
  # {
  #   labScale[1] <- paste0("<",min(b))
  # } 
  
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
    coord_sf(xlim = extent(mapsBivariate)[1:2],
             ylim = extent(mapsBivariate)[3:4]) +
    guides(fill = guide_colorbar(barheight = 5,barwidth = 0.2,title.position = "left"))+
    theme(legend.title = element_text(size=6,angle = 90),legend.text = element_text(size=6))+
    labs(title = lab)+
    theme(title = element_text(hjust = 0, face= "bold",size=8))
  
  fig
}

############### Figure 2

funEffect <- function(mod1,noPred,namPred,tit,color,yLength,b,rnd,yname,textPos,textNeg){
 
  dfCombined <-  data.frame(summary(mod1)$tTable[2:noPred,c(1,2,5)])
  names(dfCombined) <- c("Effect","SE","pVal")
  dfCombined$nam  <- namPred
  
  dfCombined$nam <- factor(dfCombined$nam, levels = unique(dfCombined$nam))
  dfCombined$labHeight <- dfCombined$Effect + dfCombined$SE + textPos
  dfCombined[which(dfCombined$Effect<0),"labHeight"] <- dfCombined[which(dfCombined$Effect<0),"Effect"]- dfCombined[which(dfCombined$Effect<0),"SE"] - textNeg
  dfCombined$lab <- ""
  dfCombined[which(dfCombined$pVal<0.05),"lab"] <- "*"
  dfCombined[which(dfCombined$pVal<0.01),"lab"] <- "**"
  dfCombined[which(dfCombined$pVal<0.001),"lab"] <- "***"
  dfCombined[which(dfCombined$pVal>=0.05),"lab"] <- "NS"
  dfCombined$lab <- factor(dfCombined$lab, levels = unique(dfCombined$lab))
  dfText <- data.frame(xpos=1:length(namPred),ypos=dfCombined$labHeight,lab=dfCombined$lab)
  
  
  fig <- ggplot(data=dfCombined, aes(x=nam, y=Effect)) +
    geom_bar(stat="identity", position=position_dodge(), fill=color)+
    geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), width=.1,
                  position=position_dodge(.9)) +
    geom_text(data=dfText,aes(x=xpos,y=ypos,label=lab),size=2)+  
    theme_classic() +  
    xlab("") +
    scale_y_continuous(breaks = round(seq(-yLength,yLength, by = b),rnd),limits=c(-yLength,yLength)) +
    ylab(yname) +
    theme(axis.title.y=element_text(size=8)) +
    theme(axis.text.y = element_text(size=8))+
    geom_hline(yintercept=0,size=0)+
    theme(plot.margin = unit(c(0.2,0.3,-0.5,0.2), "cm"))+
    theme(legend.position = "none")+
    ggtitle(tit)+
    theme(plot.title = element_text(size=8))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size=6))

  fig
}


############### Figures 3
funInteraction <- function(dfPredict,dfCenter,dfLog,effect,moderator,modS,xlabel,ylabel,modLabel,yVal1,yVal2){
  
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
    theme(legend.position = c(0.8,0.2),legend.title = element_text(size = 6),legend.text = element_text(size = 6),legend.key.size = unit(0.5, "lines"))+
    guides(shape = guide_legend(override.aes = list(size = 0.5)),color = guide_legend(override.aes = list(size = 0.5)))+
    theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))+
    scale_color_manual(name=modLabel,values = c("#E69F00","#999999","#56B4E9"))
}


############### Table 1/S1
funTables <- function(modN,r,linear,namCountry)
{
  if (linear)
  {
    dfNationalTab <- data.frame(summary(modN)$coefficients)
    dfNationalTab2 <- data.frame(est=c(summary(modN)$r.squared,AIC(modN)),tVal=c(NA,NA),pVal=c(NA,NA),Name=c("R2","AIC"))
    rownames(dfNationalTab2) <- c("R2","AIC")
  }
  if(!linear)
  {
    dfNationalTab <- data.frame(summary(modN)$tTable[,c(1,2,4,5)])
    dfNationalTab2 <- data.frame(est=c(r.squaredGLMM(modN),AIC(modN)),tVal=c(NA,NA,NA),pVal=c(NA,NA,NA),Name=c("R2m","R2c","AIC"))
    rownames(dfNationalTab2) <- c("R2m","R2c","AIC")
  }
  # dfNationalTab <- data.frame(summary(modN)$coefficients)
  indLow <- which(dfNationalTab$p.value<0.0001)
  dfNationalTab <- round(dfNationalTab,r)
  row.names(dfNationalTab) <- namCountry
  dfNationalTab$Value <- paste0(dfNationalTab$Value," (",dfNationalTab$Std.Error,")")
  dfNationalTab[indLow,4] <- "<0.0001"
  dfNationalTab <- dfNationalTab[,c(1,3,4)]
  colnames(dfNationalTab) <- c("Estimate (SE)","T","p-value") 
  dfNationalTab$Name <- namCountry
  dfNationalTab2$est <- round(dfNationalTab2$est,r)
  colnames(dfNationalTab2) <- colnames(dfNationalTab)
  dfFinal <- rbind(dfNationalTab,dfNationalTab2)
  dfFinal[,c("Name","Estimate (SE)","T","p-value")]
}



############### Figures S4-S6
# funPlot <- function(predictor,dfPredict,dfCenter,dfLog,dfOriginal,trans,xlabel,ylabel,modD,modA, posX,posY){
#   
#   dfPredictNew <- dfPredict
#   dfPredictNew[,predictor] <-  seq(min(dfCenter[,predictor]), max(dfCenter[,predictor]), length.out = 1e3)
# 
#   if (!is.null(modD)){
#     predDiversity <- exp(data.frame(predict(modD,newdata = dfPredictNew,level=0))) 
#                                             
#     
#     if (trans==""){
#       predDiversity$variable <- dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor])
#       
#     }
#     if (trans=="sqrt"){
#       predDiversity$variable <- (dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor]))^2
#     }    
#     predDiversity$Model <- factor("Diversity",levels=lev)
#     names(predDiversity)[1] <- "fit"
#     pred <- predDiversity
#   }
#   
#   if (!is.null(modA)){
#     predAsynchrony <- exp(data.frame(predict(modA,newdata = dfPredictNew,level=0))) 
# 
#     
#     if (trans==""){
#       predAsynchrony$variable <- dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor])
#       
#     }
#     if (trans=="sqrt"){
#       predAsynchrony$variable <- (dfPredictNew[,predictor]*sd(dfLog[,predictor])+mean(dfLog[,predictor]))^2
#     }  
#     predAsynchrony$Model <- factor("Asynchrony",levels=lev)
#     names(predAsynchrony)[1] <- "fit"
#     if (is.null(modD)){pred <- predAsynchrony}
#     if (!is.null(modD)){pred <- rbind(pred,predAsynchrony)}
#   }
#   
#   dfOriginal$variable <- dfOriginal[,predictor]
#   dfOriginal$Model <- ""
#   ggplot(data = dfOriginal, aes(x = variable, y = stability, color=Model)) +
#     # geom_point() +
#     geom_line(data = pred, aes(y = fit,color=Model),size=0.5)+
#     # geom_ribbon(data = pred, aes(y = fit, ymin = lwr, ymax = upr, fill = Model), alpha = 0.5,colour=NA) +
#     theme_classic() +
#     theme(axis.title=element_text(size=8),axis.text=element_text(size=8)) +
#     xlab(xlabel)+
#     ylab(ylabel)+
#     ylim(0,20)+
#     scale_colour_manual(name = "Model",values = myColors)+
#     scale_fill_manual(name = "Model",values = myColors)+
#     theme(legend.position = c(posX, posY))+
#     theme(legend.title = element_text(size = 8),
#           legend.text = element_text(size = 8))+    
#     theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"))
# }
# 


