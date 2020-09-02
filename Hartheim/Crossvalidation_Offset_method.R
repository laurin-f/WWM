
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
theme_set(theme_classic())

load(paste0(samplerpfad,"Hartheim_CO2.RData"))

#################################

data$ymd <- as_date(data$date)
data$hm <- hour(data$date)*60+minute(data$date)
data_sub <- subset(data, Position == 7:8)
data_PSt0 <- subset(data_sub, Pumpstufe == 0)

data_PSt0$offset <-  data_PSt0$CO2_inj - data_PSt0$CO2_ref

data_kal <- aggregate(data_PSt0[,grep("CO2|offset",colnames(data_PSt0))] ,list(tiefe = data_PSt0$tiefe), mean, na.rm=T)



data_PSt0$offset <- as.numeric(as.character(factor(data_PSt0$tiefe, levels=data_kal$tiefe,labels=data_kal$offset)))

##################
#mit glm oder gam
data_PSt0$preds_glm <- NA
data_PSt0$preds_gam <- NA
data_PSt0$preds_drift <- NA
data_PSt0$offset_cv <- NA
days<-unique(data_PSt0$ymd)
combs <- combn(seq_along(days),length(days)-1)
ncv <- ncol(combs)
rmse <- data.frame(glm=rep(NA,ncv),gam=rep(NA,ncv),drift=rep(NA,ncv),offset=rep(NA,ncv))
R2 <- rmse
#j<-1
#fitdays <- days[-(5:6)]
for(j in 1:ncv){
#cv_sample <- sample(1:length(days),4)
  print(j)
  fitdays <- days[combs[,j]]

  for(i in (1:7)*-3.5){
    #fm <- glm(CO2_roll_inj ~ CO2_roll_ref + hour + CO2_roll_ref * hour,data=subset(data_PSt0,tiefe==i))
    test <- subset(data_PSt0,tiefe==i & ymd %in% fitdays)
    if(any(!is.na(test$CO2_roll_ref))){
      fit_df <- subset(data_PSt0,tiefe==i & ymd %in% fitdays)
      offset_cv <- mean(fit_df$CO2_roll_inj - fit_df$CO2_roll_ref,na.rm=T)
      
      fm <- glm(CO2_roll_inj ~ CO2_roll_ref,data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      fm_drift <- glm(offset ~ poly(date_int,2),data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      
      fm_gam <- mgcv::gam(CO2_roll_inj ~ s(CO2_roll_ref) + s(hm),data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      
      ID <- which(data_PSt0$tiefe==i & !is.na(data_PSt0$CO2_roll_ref) & !data_PSt0$ymd %in% fitdays)
      
      data_PSt0$preds_glm[ID] <- predict(fm,newdata = data_PSt0[ID,])
      data_PSt0$preds_drift[ID] <- predict(fm_drift,newdata = data_PSt0[ID,])
      data_PSt0$preds_gam[ID] <- predict(fm_gam,newdata = data_PSt0[ID,])
      data_PSt0$offset_cv[ID] <- offset_cv
    }
  }
  

  data_PSt0$cv <- ifelse(data_PSt0$ymd %in% fitdays,"fit","val")
  
  sub_temp <- subset(data_PSt0,cv=="val" & tiefe >= -24)
  
  
  R2_glm <- R2(actual=sub_temp$CO2_roll_inj,preds=sub_temp$preds_glm)
  R2_gam <- R2(actual=sub_temp$CO2_roll_inj,preds=sub_temp$preds_gam)
  R2_drift <- R2(actual=sub_temp$CO2_roll_inj,preds=sub_temp$preds_drift + sub_temp$CO2_roll_ref)
  R2_offset <- R2(actual=sub_temp$CO2_roll_inj,preds=sub_temp$offset_cv + sub_temp$CO2_roll_ref)
  
  
  R2$glm[j] <- R2_glm
  R2$gam[j] <- R2_gam
  R2$drift[j] <- R2_drift
  R2$offset[j] <- R2_offset
  
  rmse_glm <- RMSE(obs=sub_temp$CO2_roll_inj,mod=sub_temp$preds_glm)
  rmse_gam <- RMSE(obs=sub_temp$CO2_roll_inj,mod=sub_temp$preds_gam)
  rmse_drift <- RMSE(obs=sub_temp$CO2_roll_inj,mod=sub_temp$preds_drift + sub_temp$CO2_roll_ref)
  rmse_offset <- RMSE(obs=sub_temp$CO2_roll_inj,mod=sub_temp$offset_cv + sub_temp$CO2_roll_ref)
  
  
  rmse$glm[j] <- rmse_glm
  rmse$gam[j] <- rmse_gam
  rmse$drift[j] <- rmse_drift
  rmse$offset[j] <-  rmse_offset
}

save(R2,file=paste0(samplerpfad,"R2cv.RData"))
load(file=paste0(samplerpfad,"R2cv.RData"))
#R2(data_PSt0$CO2_roll_ref+data_PSt0$offset,data_PSt0$CO2_roll_inj)
########################

#data_sub$CO2_tracer <- data_sub$CO2_roll_inj - (data_sub$CO2_roll_ref + data_sub$offset)
# data_sub$CO2_tracer <- data_sub$CO2_inj - (data_sub$CO2_ref + data_sub$offset)
# data_sub$CO2_tracer_glm <- data_sub$CO2_inj - (data_sub$preds_glm)
# data_sub$CO2_tracer_gam <- data_sub$CO2_inj - (data_sub$preds_gam)
# data_sub$CO2_tracer_drift <- data_sub$CO2_inj - (data_sub$CO2_ref + data_sub$preds_drift)
# 
# data_sub$tracer_pos <- data_sub$CO2_tracer > 0
# data_sub$CO2_ref_offst <- ifelse(data_sub$tracer_pos, data_sub$CO2_ref + data_sub$offset, data_sub$CO2_inj)
#rmse_old <- rmse
#rmse <- rmse_old[-1,]
rmse_long <- tidyr::pivot_longer(rmse,everything())
R2_long <- tidyr::pivot_longer(R2,everything())

#rmse <- rmse_old
ggplot(subset(rmse_long,name %in% c("glm","gam","offset")))+
  geom_point(aes(name,value))+
  scale_y_log10()
  #geom_boxplot(aes(name,value))
ggplot(subset(R2_long,name %in% c("glm","gam","offset")))+
  geom_point(aes(name,value))+
  scale_y_log10()+theme_bw()
R2_table <- subset(R2_long,name %in% c("gam","offset")) %>% group_by(name) %>% summarise_all(list(min=min,mean=mean,max=max))
write.csv(R2_table,file=paste0(samplerpfad,"R2cv_table.csv"))
#data_PSt0$cv <- ifelse(data_PSt0$ymd %in% fitdays,0.3,0.6)
ggplot(data_PSt0)+
  geom_line(aes(date,CO2_roll_inj,col=cv,linetype=as.factor(tiefe)))+
  geom_line(aes(date,preds_gam,col="gam",linetype=as.factor(tiefe)))+
  geom_line(aes(date,preds_glm,col="glm",linetype=as.factor(tiefe)))+
  scale_linetype_manual(values=rep(1,8))

ggplot(subset(data_PSt0))+
  #geom_point(aes(CO2_roll_inj,preds_glm,col=paste("glm",cv)),alpha=0.3)+
  #geom_point(aes(CO2_roll_inj,CO2_roll_ref + preds_drift,col=paste("drift",cv)),alpha=0.3)+
  #geom_point(aes(CO2_roll_inj,CO2_roll_ref + offset,col=paste("offset",cv)),alpha=0.3)+
  geom_point(aes(CO2_roll_inj,preds_gam,col="offset model"),alpha=0.3)+
  geom_point(aes(CO2_roll_inj,offset_cv + CO2_roll_ref,col="offset"),alpha=0.3)+
  facet_wrap(~ymd)+
  geom_abline(intercept = 0,slope=1)
ggplot(subset(data_PSt0))+
  #geom_point(aes(CO2_roll_inj,preds_glm,col=paste("glm",cv)),alpha=0.3)+
  #geom_point(aes(CO2_roll_inj,CO2_roll_ref + preds_drift,col=paste("drift",cv)),alpha=0.3)+
  #geom_point(aes(CO2_roll_inj,CO2_roll_ref + offset,col=paste("offset",cv)),alpha=0.3)+
  geom_point(aes(CO2_roll_inj,preds_glm,col=as.factor(ymd)),alpha=0.3)+
  geom_abline(intercept = 0,slope=1)
ggplot(subset(data_PSt0))+
  #geom_point(aes(CO2_roll_inj,preds_glm,col=paste("glm",cv)),alpha=0.3)+
  #geom_point(aes(CO2_roll_inj,CO2_roll_ref + preds_drift,col=paste("drift",cv)),alpha=0.3)+
  #geom_point(aes(CO2_roll_inj,CO2_roll_ref + offset,col=paste("offset",cv)),alpha=0.3)+
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
ggplot(subset(data_sub,tiefe %in% c(-3.5,-7)))+
  geom_line(aes(date,CO2_tracer_glm,col="glm"))+
  geom_line(aes(date,CO2_tracer_gam,col="gam"))+
  facet_wrap(~tiefe,scales="free",ncol=1)


ggplot(data_sub)+
  geom_line(aes(date,CO2_tracer_glm,col="glm"))+
  geom_line(aes(date,CO2_tracer_gam,col="gam"))+
  facet_wrap(~tiefe,scales="free")+
  geom_rect(data = subset(Pumpzeiten, Pumpstufe != 0),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="injection"),alpha=0.3)+
  xlim(range(data_sub$date))
ggplot(data_sub)+geom_point(aes(date,Fz))
