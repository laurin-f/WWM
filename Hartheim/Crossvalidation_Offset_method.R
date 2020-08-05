
#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","ggforce","dplyr")
check.packages(packages)

load(paste0(samplerpfad,"Hartheim_CO2.RData"))

#################################

data$ymd <- as_date(data$date)
data$hm <- hour(data$date)*60+minute(data$date)
data_sub <- subset(data, Position == 7)
data_PSt0 <- subset(data_sub, Pumpstufe == 0)

data_PSt0$offset <-  data_PSt0$CO2_inj - data_PSt0$CO2_ref

data_kal <- aggregate(data_PSt0[,grep("CO2|offset",colnames(data_PSt0))] ,list(tiefe = data_PSt0$tiefe), mean, na.rm=T)



data_PSt0$offset <- as.numeric(as.character(factor(data_PSt0$tiefe, levels=data_kal$tiefe,labels=data_kal$offset)))

##################
#mit glm oder gam
data_PSt0$preds_glm <- NA
data_PSt0$preds_gam <- NA
data_PSt0$preds_drift <- NA
days<-unique(data_PSt0$ymd)
combs <- combn(seq_along(days),6)
ncv <- ncol(combs)
R2 <- data.frame(glm=rep(NA,ncv),gam=rep(NA,ncv),drift=rep(NA,ncv),offset=rep(NA,ncv))

for(j in 1:ncv){
#cv_sample <- sample(1:length(days),4)
  print(j)
cv_sample <-  combs[,j]
fitdays <- days[cv_sample]

  for(i in (1:7)*-3.5){
    #fm <- glm(CO2_roll_inj ~ CO2_roll_ref + hour + CO2_roll_ref * hour,data=subset(data_PSt0,tiefe==i))
    test <- subset(data_PSt0,tiefe==i & ymd %in% fitdays)
    if(any(!is.na(test$CO2_roll_ref))){
      fm <- glm(CO2_roll_inj ~ CO2_roll_ref,data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      fm_drift <- glm(offset ~ poly(date_int,2),data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      
      fm_gam <- mgcv::gam(CO2_roll_inj ~ s(CO2_roll_ref) + s(hm),data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      
      ID <- which(data_PSt0$tiefe==i & !is.na(data_PSt0$CO2_roll_ref))
      
      data_PSt0$preds_glm[ID] <- predict(fm,newdata = data_PSt0[ID,])
      data_PSt0$preds_drift[ID] <- predict(fm_drift,newdata = data_PSt0[ID,])
      data_PSt0$preds_gam[ID] <- predict(fm_gam,newdata = data_PSt0[ID,])
    }
  }
  

  data_PSt0$cv <- ifelse(data_PSt0$ymd %in% fitdays,"fit","val")
  
  fmcv_glm <- glm(CO2_roll_inj ~ preds_glm,data=subset(data_PSt0,cv=="val" & tiefe >= -7))
  fmcv_gam <- glm(CO2_roll_inj ~ preds_gam,data=subset(data_PSt0,cv=="val"& tiefe >= -7))
  fmcv_drift <- glm(CO2_roll_inj ~ I(CO2_ref + preds_drift),data=subset(data_PSt0,cv=="val"& tiefe >= -7))
  fmcv_offset <- glm(CO2_roll_inj ~ I(CO2_ref + offset),data=subset(data_PSt0,cv=="val"& tiefe >= -7))
  
  R2$glm[j] <- 1 - fmcv_glm$deviance/fmcv_glm$null.deviance
  R2$gam[j] <- 1 - fmcv_gam$deviance/fmcv_gam$null.deviance
  R2$drift[j] <- 1 - fmcv_drift$deviance/fmcv_drift$null.deviance
  R2$offset[j] <- 1 - fmcv_offset$deviance/fmcv_offset$null.deviance
}
########################

#data_sub$CO2_tracer <- data_sub$CO2_roll_inj - (data_sub$CO2_roll_ref + data_sub$offset)
# data_sub$CO2_tracer <- data_sub$CO2_inj - (data_sub$CO2_ref + data_sub$offset)
# data_sub$CO2_tracer_glm <- data_sub$CO2_inj - (data_sub$preds_glm)
# data_sub$CO2_tracer_gam <- data_sub$CO2_inj - (data_sub$preds_gam)
# data_sub$CO2_tracer_drift <- data_sub$CO2_inj - (data_sub$CO2_ref + data_sub$preds_drift)
# 
# data_sub$tracer_pos <- data_sub$CO2_tracer > 0
# data_sub$CO2_ref_offst <- ifelse(data_sub$tracer_pos, data_sub$CO2_ref + data_sub$offset, data_sub$CO2_inj)

R2_long <- tidyr::pivot_longer(R2,everything())
ggplot(R2_long)+
  #geom_point(aes(name,value))+ylim(c(0.8,1))
  geom_boxplot(aes(name,value))
#data_PSt0$cv <- ifelse(data_PSt0$ymd %in% fitdays,0.3,0.6)
ggplot(subset(data_PSt0,cv=="val"))+
  #geom_point(aes(CO2_roll_inj,preds_glm,col=paste("glm",cv)),alpha=0.3)+
  #geom_point(aes(CO2_roll_inj,CO2_roll_ref + preds_drift,col=paste("drift",cv)),alpha=0.3)+
  #geom_point(aes(CO2_roll_inj,CO2_roll_ref + offset,col=paste("offset",cv)),alpha=0.3)+
  geom_point(aes(CO2_roll_inj,preds_gam,col=paste("gam",cv)),alpha=0.3)+
  geom_abline(intercept = 0,slope=1)


ggplot(data_PSt0)+
  geom_line(aes(date,CO2_roll_inj,col="inj"))+
  geom_line(aes(date,preds_gam,col=cv))+
  facet_wrap(~tiefe,scales="free")
ggplot(data_PSt0)+
  geom_line(aes(date,CO2_roll_ref +preds_drift,col="drift"))+
  geom_line(aes(date,preds_glm,col="glm"))+
  geom_line(aes(date,preds_gam,col="gam"))+
  geom_line(aes(date,CO2_roll_inj,col="inj"))+
  facet_wrap(~tiefe,scales="free")

test <- subset(data_PSt0,tiefe==i & ymd %in% fitdays)
any(!is.na(test$CO2_roll_ref))
