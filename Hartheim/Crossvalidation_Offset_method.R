
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
data_sub <- subset(data, Position == 8)
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

method <- "LOOCV"
#method <- "k-fold CV"
if(method=="LOOCV"){
combs <- combn(seq_along(days),length(days)-1)
ncv <- ncol(combs)
}
if(method=="k-fold CV"){
fold <- 5
folds <- sample(rep(1:5,length(days)),length(days),replace=F)
ncv <- fold
}

rmse <- data.frame(glm=rep(NA,ncv*7),gam=rep(NA,ncv*7),drift=rep(NA,ncv*7),offset=rep(NA,ncv*7),tiefe=rep(NA,ncv*7))
R2 <- rmse


count <- 1
#j<-1
#fitdays <- days[-(5:6)]
calc <-T
if(calc==T){
for(j in 1:ncv){
#cv_sample <- sample(1:length(days),4)
  print(j)
  if(method=="LOOCV"){
    fitdays <- days[combs[,j]]
  }
  if(method=="k-fold CV"){
    fitdays <- days[folds!=j]
  }

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
  

      #data_PSt0$cv <- ifelse(data_PSt0$ymd %in% fitdays,"fit","val")
      
      sub_temp <- subset(data_PSt0,!data_PSt0$ymd %in% fitdays & tiefe == i)
      
      # ranges <- range(sub_temp[,c("CO2_roll_inj","preds_gam","preds_glm")])
      # plot(sub_temp$CO2_roll_inj,sub_temp$preds_gam,xlim=ranges,ylim=ranges)
      # points(sub_temp$CO2_roll_inj,sub_temp$preds_glm,col=2)
      # abline(0,1)
      
      R2_glm <- R2(actual=sub_temp$CO2_roll_inj,preds=sub_temp$preds_glm)
      R2_gam <- R2(actual=sub_temp$CO2_roll_inj,preds=sub_temp$preds_gam)
      R2_drift <- R2(actual=sub_temp$CO2_roll_inj,preds=sub_temp$preds_drift + sub_temp$CO2_roll_ref)
      R2_offset <- R2(actual=sub_temp$CO2_roll_inj,preds=sub_temp$offset_cv + sub_temp$CO2_roll_ref)
      
      
      R2$glm[count] <- R2_glm
      R2$gam[count] <- R2_gam
      R2$drift[count] <- R2_drift
      R2$offset[count] <- R2_offset
      R2$tiefe[count] <- i 
      rmse$tiefe[count] <- i 
      
      rmse_glm <- RMSE(obs=sub_temp$CO2_roll_inj,mod=sub_temp$preds_glm)
      rmse_gam <- RMSE(obs=sub_temp$CO2_roll_inj,mod=sub_temp$preds_gam)
      rmse_drift <- RMSE(obs=sub_temp$CO2_roll_inj,mod=sub_temp$preds_drift + sub_temp$CO2_roll_ref)
      rmse_offset <- RMSE(obs=sub_temp$CO2_roll_inj,mod=sub_temp$offset_cv + sub_temp$CO2_roll_ref)
      
      
      rmse$glm[count] <- rmse_glm
      rmse$gam[count] <- rmse_gam
      rmse$drift[count] <- rmse_drift
      rmse$offset[count] <-  rmse_offset
      count <-  count +1
    }
  }
}
}
#save(R2,rmse,data_PSt0,file=paste0(samplerpfad,"R2cv_pos8.RData"))

#save(R2,rmse,data_PSt0,file=paste0(samplerpfad,"R2cv_k_fold.RData"))

load(file=paste0(samplerpfad,"R2cv.RData"))
for(i in (1:7)*-3.5){
  
  fm2 <- mgcv::gam(CO2_roll_inj ~ s(CO2_roll_ref) + s(hour),data=subset(data_PSt0,tiefe==i))
  
  ID <- which(data_PSt0$tiefe==i)
  
  data_PSt0$preds2[ID] <- predict(fm2,newdata = data_PSt0[ID,])
}
#load(file=paste0(samplerpfad,"R2cv_k_fold.RData"))
rmse_agg <- rmse %>% group_by(tiefe) %>% summarise(gam_rmse=mean(gam,na.rm=T))
R2_agg <- R2 %>% group_by(tiefe) %>% summarise(gam_R2=mean(gam),gam_R2_median=median(gam))

sd_agg <- subset(data_PSt0,tiefe!=0) %>% group_by(tiefe) %>% summarise(CO2_sd=sd(CO2_roll_inj,na.rm=T),gam_rmse = RMSE(preds2,CO2_roll_inj),gam_R2 = R2(preds2,CO2_roll_inj),gam_rmse_cv =RMSE(preds_gam,CO2_roll_inj),gam_R2_cv = R2(preds_gam,CO2_roll_inj),offset_RMSE = RMSE(offset +CO2_roll_ref,CO2_roll_inj),offset_R2 = R2(offset +CO2_roll_ref,CO2_roll_inj))
1-0.319^2
sd_agg$proz <- sd_agg$gam_rmse/sd_agg$CO2_sd
sd_agg$proz_cv <- sd_agg$gam_rmse_cv/sd_agg$CO2_sd
apply(sd_agg,2,range)

test <- subset(data_PSt0,tiefe==-24.5)
testgam <- mgcv::gam(CO2_roll_inj ~ s(CO2_roll_ref) + s(hour),data=test)
1-testgam$deviance/testgam$null.deviance
sd_agg$gam_R2_cv
1-sd_agg$proz_cv^2
sd_agg$gam_R2
1-sd_agg$proz^2

preds <- data_PSt0$preds_gam
actual <- data_PSt0$CO2_roll_inj
NA_vals <- is.na(preds)|is.na(actual)
preds <- preds[!NA_vals]
actual <- actual[!NA_vals]
n <- length(actual)
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
sqrt(rss/n)
rmse <- RMSE(preds,actual)
sd <- sd(actual)
(rmse/sd)^2
rss/tss
sqrt(tss/length(actual))
RMSE_cv <- RMSE(data_PSt0$preds_gam,data_PSt0$CO2_roll_inj)
RMSEfit <- RMSE(data_PSt0$preds2,data_PSt0$CO2_roll_inj)
R2fit <- R2(data_PSt0$preds2,data_PSt0$CO2_roll_inj)
range(sd_agg$gam_rmse)
range(sd_agg$CO2_sd)
sd(data_PSt0$CO2_roll_inj,na.rm=T)
RMSE_cv/RMSEfit
RMSE_cv_offset <- RMSE(data_PSt0$offset_cv +data_PSt0$CO2_roll_ref ,data_PSt0$CO2_roll_inj)
RMSE_fit_offset <- RMSE(data_PSt0$offset +data_PSt0$CO2_roll_ref ,data_PSt0$CO2_roll_inj)
RMSE_cv_offset/RMSE_fit_offset
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
summary(rmse)
rmse_long <- tidyr::pivot_longer(rmse,!matches("tiefe"))
R2_long <- tidyr::pivot_longer(R2,!matches("tiefe"))

#rmse <- rmse_old
ggplot(subset(rmse_long,name %in% c("glm","gam","offset")))+
  geom_point(aes(name,value,col=as.factor(tiefe)))+
  scale_y_log10()
  #geom_boxplot(aes(name,value))
ggplot(subset(R2_long,name %in% c("glm","gam","offset")))+
  geom_point(aes(name,value,col=tiefe))+
  scale_y_log10()+theme_bw()
R2$gam
R2_table <- subset(R2_long,name %in% c("gam","glm","offset")) %>% group_by(name) %>% summarise_all(list(min=min,mean=mean,max=max))
rmse_table <- subset(rmse_long,name %in% c("gam","glm","offset")&!is.na(value)) %>% group_by(name) %>% summarise_all(list(min=min,mean=mean,max=max))
write.csv(R2_table,file=paste0(samplerpfad,"R2cv_table.csv"))
write.csv(rmse_table,file=paste0(samplerpfad,"rmsecv5folg_table.csv"))
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
