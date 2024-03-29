####pearson r^2#############################
#  Objective functions 

#NSE ist gleich R2
#R2 ist (pearson r)^2 aber nur für lineare regression
#R2 ist 1 - (RMSE/sd(obs))^2 = 1- nrmse^2

#
#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","ggforce","dplyr","directlabels","hydroGOF")
check.packages(packages)
theme_set(theme_classic())

load(paste0(samplerpfad,"Hartheim_CO2.RData"))

#################################

data$ymd <- as_date(data$date)
data$hm <- hour(data$date)*60+minute(data$date)
data$Position[data$date > ymd_h("2020.07.29 12") & data$date < Pumpzeiten$ende[18]] <- 8
data <- data %>% 
  group_by(tiefe) %>% 
  mutate(
    VWC_roll = RcppRoll::roll_mean(VWC,n=50,fill=NA),
    
  ) %>% 
  ungroup() %>% 
  as.data.frame()
data_sub <- subset(data, Position == 8 & !ymd %in% ymd("2020-07-24","2020-07-25"))
data_PSt0 <- subset(data_sub, Pumpstufe == 0)
data_PSt0 <- data_PSt0 %>% select(-preds_gam)


data_PSt0$offset <-  data_PSt0$CO2_inj - data_PSt0$CO2_ref

# data_PSt0$preds_glm <- data_PSt0$preds
# data_PSt0$preds_gam <- data_PSt0$preds2
# data_PSt0$preds_drift <- data_PSt0$preds_drift + data_PSt0$CO2_roll_ref 

#data_kal <- aggregate(data_PSt0[,grep("CO2|offset",colnames(data_PSt0))] ,list(tiefe = data_PSt0$tiefe), mean, na.rm=T)



#data_PSt0$offset <- as.numeric(as.character(factor(data_PSt0$tiefe, levels=data_kal$tiefe,labels=data_kal$offset)))

##################
#mit glm oder gam
#data_PSt0$preds_cv_glm <- NA
#data_PSt0$preds_cv_gam <- NA
data_PSt0$preds_cv_drift <- NA
#data_PSt0$preds_cv_no_ref <- NA
#data_PSt0$preds_cv_SWC_T <- NA
data_PSt0$preds_cv_SWC_T_gam <- NA


#data_PSt0$offset_cv <- NA
days<-unique(data_PSt0$ymd)

#data_PSt0$preds_SWC_T <- NA
data_PSt0$preds_SWC_T_gam <- NA

for(i in (1:7)*-3.5){
    fm_drift <- glm(offset ~ poly(date_int,2),data=subset(data_PSt0,tiefe==i))
    #fm_SWC_T <- glm(CO2_roll_inj ~ poly(VWC_roll,2) + poly(T_soil,2),data=subset(data_PSt0,tiefe==i))
    fm_SWC_T_gam <- mgcv::gam(CO2_roll_inj ~ poly(date_int,2) + s(hour) + poly(VWC_roll,2) + poly(T_soil,2),data=subset(data_PSt0,tiefe==i))
    
    ID <- which(data_PSt0$tiefe==i & !is.na(data_PSt0$CO2_roll_ref))
    
    data_PSt0$preds_drift[ID] <- data_PSt0[ID,]$CO2_roll_ref + predict(fm_drift,newdata = data_PSt0[ID,])
    
    #data_PSt0$preds_SWC_T[ID] <- predict(fm_SWC_T,newdata = data_PSt0[ID,])
    data_PSt0$preds_SWC_T_gam[ID] <- predict(fm_SWC_T_gam,newdata = data_PSt0[ID,])

  }
data_PSt0$preds_SWC_T_gam[is.na(data_PSt0$preds_drift)] <- NA

method <- "LOOCV"
#method <- "k-fold CV"
if(method=="LOOCV"){
combs <- combn(seq_along(days)[-c(1,13)],length(days)-3)
ncv <- ncol(combs)
}
if(method=="k-fold CV"){
fold <- 5
folds <- sample(rep(1:5,length(days)),length(days),replace=F)
ncv <- fold
}

# rmse <- data.frame(glm=rep(NA,ncv*7),gam=rep(NA,ncv*7),drift=rep(NA,ncv*7),offset=rep(NA,ncv*7),tiefe=rep(NA,ncv*7))
# R2 <- rmse
combs <- sapply(c(2,seq(5,length(days)-2,2)),function(x) (1:length(days))[-(x:(x+1))])
ncv <- ncol(combs)

count <- 1
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
      
      #fm <- glm(CO2_roll_inj ~ CO2_roll_ref,data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      fm_drift <- glm(offset ~ poly(date_int,2),data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      #fm_SWC_T <- glm(CO2_roll_inj ~  poly(VWC_roll,2) + poly(T_soil,2),data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      fm_SWC_T_gam <- mgcv::gam(CO2_roll_inj ~  poly(date_int,2) + s(hour) + poly(VWC_roll,2) + poly(T_soil,2),data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      
      #fm_no_ref <- glm(CO2_roll_inj ~ poly(date_int,3) + poly(hour,4) ,data=subset(data_PSt0,tiefe==i& ymd %in% fitdays))
      #fm_no_ref <- mgcv::gam(CO2_roll_inj ~ poly(date_int,2) + s(hour),data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      #fm_gam <- mgcv::gam(CO2_roll_inj ~ s(CO2_roll_ref) + s(hour),data=subset(data_PSt0,tiefe==i & ymd %in% fitdays))
      
      ID <- which(data_PSt0$tiefe==i & !is.na(data_PSt0$CO2_roll_ref) & !data_PSt0$ymd %in% fitdays)
      
      #data_PSt0$preds_cv_glm[ID] <- predict(fm,newdata = data_PSt0[ID,])

      data_PSt0$preds_cv_drift[ID] <- data_PSt0[ID,]$CO2_roll_ref + predict(fm_drift,newdata = data_PSt0[ID,])
      #data_PSt0$preds_cv_no_ref[ID] <- predict(fm_no_ref,newdata = data_PSt0[ID,])
      #data_PSt0$preds_cv_SWC_T[ID] <- predict(fm_SWC_T,newdata = data_PSt0[ID,])
      data_PSt0$preds_cv_SWC_T_gam[ID] <- predict(fm_SWC_T_gam,newdata = data_PSt0[ID,])
      #data_PSt0$preds_cv_gam[ID] <- predict(fm_gam,newdata = data_PSt0[ID,])
      
      
      count <-  count +1
    }
  }
}
}


data_PSt0_2 <- subset(data_PSt0, !is.na(preds_drift)& !is.na(preds_cv_drift))

data_agg <- data_PSt0_2 %>% 
  select(c(tiefe,CO2_roll_inj,matches("preds_.*(gam|drift)"))) %>% 
  group_by(tiefe) %>% 
  summarise_at(vars(matches("preds_")),list(R2=~R2(.,CO2_roll_inj),
                                            #R2cor=~cor(.,CO2_roll_inj,use = "complete.obs")^2,
                                            #R2gof=~hydroGOF::gof(.,CO2_roll_inj,digits=7)["R2",],
                                            rmse=~RMSE(.,CO2_roll_inj),
                                            nrmse = ~hydroGOF::nrmse(.,CO2_roll_inj),
                                            #nse = ~hydroGOF::NSE(.,CO2_roll_inj)
                                            )) %>% 
  tidyr::pivot_longer(matches("preds"),names_pattern = "preds_(cv)?_?(.+)_(R2|n?rmse)$",names_to = c("cv","func",".value"))


R2(test$CO2_roll_inj,test$preds_drift)




data_agg$cv[is.na(data_agg$cv)] <- "full"
data_agg$cv[nchar(data_agg$cv)==0] <- "full"

# data_PSt0_2$date_int <- as.integer(data_PSt0_2$date)
# 
# 
# data_PSt0_2 <- data_PSt0_2 %>% 
#   select(matches("(tiefe|date|preds_.*(gam$|drift$)|CO2_roll)")) %>% 
#   group_by(tiefe) %>% 
#   mutate_at(
#     vars(matches("preds_.*(gam$|drift$)")),list(
#       #mav=~RcppRoll::roll_mean(.,60*24,fill=NA),
#       loess=~predict(loess(. ~ date_int,span=0.5))
#       )
#     )%>% 
#   
#   ungroup() %>% 
#   as.data.frame()
# 
# data_PSt0_2[paste0(grep("preds_.*(gam$|drift$)",colnames(data_PSt0_2),value = T),"_diff")] <-  data_PSt0_2$CO2_roll_inj - data_PSt0_2[grep("preds_.*(gam$|drift$)",colnames(data_PSt0_2))] 
# data_PSt0_2[paste0(grep("preds_.*(gam$|drift$)",colnames(data_PSt0_2),value = T),"_noise")] <-  data_PSt0_2[grep("preds_.*(gam$|drift$)",colnames(data_PSt0_2))] - data_PSt0_2[grep("preds_.+_loess",colnames(data_PSt0_2))]
# ggplot(data_PSt0_2)+
#   geom_line(aes(date,preds_drift,col="drift",linetype=as.factor(tiefe)))+
#   geom_line(aes(date,preds_SWC_T_gam,col="SWC",linetype=as.factor(tiefe)))+
#   geom_line(aes(date,CO2_roll_inj,col="inj",linetype=as.factor(tiefe)))
#   #geom_line(aes(date,preds_drift_mav,col=as.factor(tiefe)))+
#   geom_line(aes(date,preds_drift_loess,col=as.factor(tiefe)))
# ggplot(subset(data_PSt0_2))+
#   geom_line(aes(date,preds_cv_drift_diff,col="cv_drift"))+
#   geom_line(aes(date,preds_cv_SWC_T_gam_diff,col="cv_SWC_T_gam"))+
#   facet_wrap(~tiefe)


# ggplot(data_PSt0_2)+
#   geom_line(aes(date,preds_SWC_T_gam,col=as.factor(tiefe)))+
#   geom_line(aes(date,preds_SWC_T_gam_mav,col=as.factor(tiefe)))+
#   geom_line(aes(date,preds_SWC_T_gam_loess,col=as.factor(tiefe)))
#ggplot(data_agg)+geom_col(aes(func,R2,fill=as.factor(tiefe)),position = "dodge")+facet_wrap(~factor(cv,level=c("cv","full"),labels=c("Cross-Validation fit","full fit")))
ggplot(data_agg)+geom_col(aes(func,1-R2,fill=as.factor(tiefe)),position = "dodge")+facet_wrap(~factor(cv,level=c("cv","full"),labels=c("Cross-Validation fit","full fit")))+ggsave(paste0(plotpfad_harth,"cv_barplot.jpg"),width=7,height=5)
ggplot(data_agg)+geom_col(aes(func,rmse,fill=as.factor(tiefe)),position = "stack")+facet_wrap(~factor(cv,level=c("cv","full"),labels=c("Cross-Validation fit","full fit")))+ggsave(paste0(plotpfad_harth,"cv_barplot.jpg"),width=7,height=5)

agg <- data_agg %>% group_by(func,cv) %>% summarise_at(c("R2","nrmse"),list(min=min,max=max,mean=mean),na.rm=T)

agg
agg[agg$func %in% c("drift","SWC_T_gam"),]
agg[agg$func %in% c("drift","SWC_T_gam"),c("func","cv","mean")]
range(subset(data,Position %in% 7:8 & tiefe > -10)$CO2_tracer_drift,na.rm = T)



data_PSt0_3 <- subset(data_PSt0, Pumpstufe == 0 & date > ymd("2020-07-24") & date < ymd("2020-07-30") )
#gam
# ggplot(data_PSt0)+
#   geom_line(aes(date,CO2_roll_inj,linetype=as.factor(tiefe),col="inj"))+
#   geom_line(aes(date,preds_cv_gam,linetype=as.factor(tiefe),col="gam"))+
#   scale_linetype_manual(values = rep(1,8))
# #no_ref
# ggplot(data_PSt0)+
#   geom_line(aes(date,CO2_roll_inj,linetype=as.factor(tiefe),col="inj"))+
#   geom_line(aes(date,preds_cv_no_ref,linetype=as.factor(tiefe),col="no_ref"))+
#   scale_linetype_manual(values = rep(1,8))
#drift

col <- scales::hue_pal()(4)
cv_plt <- 
  ggplot(data_PSt0_3)+
  geom_line(aes(date,CO2_roll_inj,linetype=as.factor(tiefe),col="inj"),lwd=1)+
  geom_line(aes(date,preds_cv_drift,linetype=as.factor(tiefe),col="ref adj"))+
  #geom_line(aes(date,preds_cv_no_ref,linetype=as.factor(tiefe),col="gam"))+
  #geom_line(aes(date,preds_cv_SWC_T,linetype=as.factor(tiefe),col="SWC T glm"))+
  geom_line(aes(date,preds_cv_SWC_T_gam,linetype=as.factor(tiefe),col="SWC T gam"))+
  scale_linetype_manual(values = rep(1,8))+
  scale_color_manual(values = c(1,col))+
  scale_x_datetime(expand=expansion(add=c(0,3600*17)))+
  guides(linetype=F)+
  labs(title="Cross Validation",y="",col="")+
  directlabels::geom_dl(aes(date,CO2_roll_inj,label=paste(-tiefe,"cm")),method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8))

full_plt <- ggplot(data_PSt0_3)+
  geom_line(aes(date,CO2_roll_inj,linetype=as.factor(tiefe),col="inj"),lwd=1)+
  geom_line(aes(date,preds_drift,linetype=as.factor(tiefe),col="ref adj"))+
  #geom_line(aes(date,preds_no_ref,linetype=as.factor(tiefe),col="gam"))+
  #geom_line(aes(date,preds_SWC_T,linetype=as.factor(tiefe),col="SWC T glm"))+
  geom_line(aes(date,preds_SWC_T_gam,linetype=as.factor(tiefe),col="SWC T gam"))+
  scale_linetype_manual(values = rep(1,8))+
  scale_color_manual(values = c(1,col))+
  scale_x_datetime(expand=expansion(add=c(0,3600*18)))+
  guides(linetype=F,col=F)+
  labs(title="full fit",y=expression(CO[2]*" [ppm]"))+
  directlabels::geom_dl(aes(date,CO2_roll_inj,label=paste(-tiefe,"cm")),method = list(dl.trans(x = x + 0.1), "last.points", cex = 0.8))

ggpubr::ggarrange(full_plt,cv_plt,ncol=2,common.legend = T,legend="top")+ggsave(paste0(plotpfad_harth,"SWC_T_gam.jpg"),width = 8,height = 6)


ggplot(data_PSt0)+
  geom_line(aes(date,CO2_roll_inj,linetype=as.factor(tiefe),col="inj"))+
  geom_line(aes(date,preds_cv_SWC_T,linetype=as.factor(tiefe),col="SWC_T"))+
  scale_linetype_manual(values = rep(1,8))

ggplot(data_PSt0)+geom_point(aes(CO2_roll_inj, T_soil, col=as.factor(tiefe)))
ggplot(data_PSt0)+geom_point(aes(CO2_roll_inj, VWC, col=as.factor(tiefe)))
ggplot(data_PSt0)+geom_point(aes(CO2_roll_inj, CO2_roll_ref, col=as.factor(tiefe)))
ggplot(data_PSt0)+geom_point(aes(CO2_roll_inj, VWC, col=T_soil))+facet_wrap(~tiefe)

###########################
#correlation between CO2_inj and ref VWC and T
cor_mat <- data_PSt0 %>% 
  group_by(tiefe) %>% 
  summarise(cor_ref = cor(CO2_roll_inj,CO2_roll_ref,use="na.or.complete"),
            cor_preds = cor(CO2_roll_inj,preds_drift,use="na.or.complete"),
            cor_VWC = cor(CO2_roll_inj,VWC,use="na.or.complete"),
            cor_T = cor(CO2_roll_inj,T_soil,use="na.or.complete")
            )


ggplot(cor_mat)+
  geom_point(aes(tiefe,cor_preds,col="preds"))+
  geom_point(aes(tiefe,cor_ref,col="ref"))+
  geom_point(aes(tiefe,cor_VWC,col="VWC"))#+
  geom_point(aes(tiefe,cor_T,col="T"))
#############################################################
  #ref has higher pearson r than VWC
  

VWC_p <- ggplot(data_PSt0)+geom_line(aes(date, VWC, col=as.factor(tiefe)))
T_p <- ggplot(data_PSt0)+geom_line(aes(date, T_soil, col=as.factor(tiefe)))
W_p <- ggplot(data_PSt0)+geom_line(aes(date,WindVel_30m_ms , col=as.factor(tiefe)))
CO2_p <- ggplot(data_PSt0)+geom_line(aes(date, CO2_roll_inj, col=as.factor(tiefe)))


egg::ggarrange(VWC_p,T_p,CO2_p)

#######################
#(RMSE / SD)^2 == R2

sd_agg_cv <- data_PSt0 %>% group_by(tiefe) %>%
  filter(!is.na(preds_cv_drift)) %>% summarise(sd=sd(CO2_roll_inj,na.rm = T)) %>% mutate(cv="cv")
sd_agg <- data_PSt0 %>% group_by(tiefe) %>%
  filter(!is.na(preds_drift)) %>% summarise(sd=sd(CO2_roll_inj,na.rm = T)) %>% mutate(cv="full")
data_agg_2 <- merge(data_agg,rbind(sd_agg,sd_agg_cv),all.x = T)
#data_agg_2 <- subset(data_agg_2,cv == "cv")

rmse_sd <- (1- data_agg_2$rmse^2 / data_agg_2$sd^2)
data_agg_2$R2 == rmse_sd
plot(data_agg_2$R2)
points(rmse_sd,col=2,pch=20)

########################################
# ALT                                    #
########################################
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

sd_agg$proz <- sd_agg$gam_rmse/sd_agg$CO2_sd
sd_agg$proz_cv <- sd_agg$gam_rmse_cv/sd_agg$CO2_sd
apply(sd_agg,2,range)


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
  geom_line(aes(date,preds_gam,col="cv"))#+
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
