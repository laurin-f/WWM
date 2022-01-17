

#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_ms <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Methodenpaper/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
aufbereitete_ds<-paste0(hauptpfad,"Daten/aufbereiteteDaten/DS_Labor/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr","png","cowplot","magick","ggforce","imputeTS")
check.packages(packages)
theme_set(theme_classic())


load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(datapfad_harth,"PPC_DS.RData"))

ggplot(subset(data,Position==7))+
  #geom_point(aes(date,preds_SWC_WS,col=as.factor(Pumpstufe)))
  #geom_point(aes(date,VWC_roll,col=as.factor(Pumpstufe)))
  #geom_point(aes(Wind_roll,preds_SWC_WS,col=as.factor(Pumpstufe)))
  #geom_point(aes(VWC_roll,preds_SWC_WS,col=as.factor(Pumpstufe)))
  geom_point(aes(T_Soil,preds_SWC_WS,col=as.factor(Pumpstufe)))
for(pos in 7:8){
dataranges <- data %>% 
  filter(Position==pos&!is.na(Pumpstufe)) %>% 
  mutate(period=ifelse(Pumpstufe > 0,"inj","cal")) %>% 
  group_by(tiefe,period) %>% 
  summarise(across(c("VWC_roll","T_soil","Wind_roll"),list(range=~range(.,na.rm = T)))) %>% 
  tidyr::pivot_longer(c("VWC_roll_range","T_soil_range","Wind_roll_range"))

ggplot(dataranges)+
  geom_boxplot(aes(tiefe,value,group=paste(tiefe,period),fill=period,col=period),lwd=1)+facet_wrap(~name,scales="free")+labs(title=ifelse(pos==7,"windy period","calm period"))+ggsave(paste0(plotpfad_ms,"SWC_T_WS_ranges_",ifelse(pos==7,"windy","calm"),".png"),width=7,height=3)
}
data$PPC <- NA
for(i in unique(data$tiefe)){
  data$PPC[data$date %in% PPC$date & data$tiefe == i] <- PPC$PPC
}
data <- data %>% 
  #filter(Position %in% 7:8) %>% 
  group_by(tiefe) %>% 
  mutate(
    PPC = imputeTS::na_interpolation(PPC,maxgap = 60),
    Wind_roll = RcppRoll::roll_mean(WindVel_30m_ms,n=60*12,fill=NA),
  ) %>% 
  
  ungroup() %>% 
  as.data.frame()

# range(data_sub$date)
# ggplot(data_sub)+
#   geom_line(aes(date,CO2_roll_inj,group=tiefe))
# ggplot()+
#  geom_line(data=subset(data,date > min(data_sub$date) & date < max(data_sub$date) & Pumpstufe!=0),aes(date,PPC*8,col="PPC"))+
#  #geom_line(data=subset(data,date > min(data_sub$date) & date < max(data_sub$date)),aes(date,PPC*8,col="PPC"))+
#  geom_line(data=subset(data,Position %in% 7:8),aes(date,Wind_roll,col="Wind"))


#injektion 1 weg
#data$Position[data$date > Pumpzeiten$start[10] & data$date < Pumpzeiten$start[12]] <- NA
#injektion 1 her
data$Position[data$date > Pumpzeiten$start[10] & data$date < Pumpzeiten$start[12]] <- 7

data$Position[data$date > Pumpzeiten$start[10] & data$date < ymd_h("2020.07.01 18")] <- NA


data_PSt0 <- lapply(na.omit(unique(data$Position)),function(x) subset(data, Pumpstufe == 0 & Position == x))


j_78 <- which(sapply(data_PSt0, function(x) unique(x$Position %in% 7:8)))
##################
#mit glm oder gam

#data$preds_SWC_T <- NA
data$preds_SWC_WS <- NA
#data$preds_SWC_PPC <- NA
#data$preds_SWC_T_Wind <- NA
#data$offset_drift <- NA
#data$preds_drift <- NA

fm_list <- list()
for(j in j_78){

  for(i in (1:7)*-3.5){
    
    #fm_drift <- glm(offset ~ poly(date_int,2),data=subset(data_PSt0[[j]],tiefe==i))
    
    #fm_SWC_T <- mgcv::gam(CO2_roll_inj ~  poly(date_int,2) + s(hour) + poly(VWC_roll,2) + poly(T_soil,2),data=subset(data_PSt0[[j]],tiefe==i & !is.na(VWC_roll)))
    fm_SWC_WS <- mgcv::gam(CO2_roll_inj ~ poly(date_int,2) + s(hour) + poly(VWC_roll,2) + poly(T_soil,2) + poly(Wind_roll,2),data=subset(data_PSt0[[j]],tiefe==i & !is.na(VWC_roll)))
    #fm_SWC_PPC <- mgcv::gam(CO2_roll_inj ~ poly(date_int,2) + s(hour) + poly(VWC_roll,2) + poly(T_soil,2) + poly(PPC,2),data=subset(data_PSt0[[j]],tiefe==i & !is.na(PPC)))
    
    pos <- na.omit(unique(data$Position))[j]
    ID <- which(data$tiefe==i & data$Position == pos)
    
    #data$offset_drift[ID] <- predict(fm_drift,newdata = data[ID,])
    #data$preds_drift[ID] <- data$CO2_roll_ref[ID] + data$offset_drift[ID]
    
    #data$preds_SWC_T[ID] <- predict(fm_SWC_T,newdata = data[ID,])
    
    data$preds_SWC_WS[ID] <- predict(fm_SWC_WS,newdata = data[ID,])
    #data$preds_SWC_PPC[ID] <- predict(fm_SWC_PPC,newdata = data[ID,])
    #data$preds_SWC_T_Wind[ID] <- predict(fm_SWC_T_Wind,newdata = data[ID,])
    fm_list[[paste0("depth_",-i,"Pos",j+1)]] <- fm_SWC_WS
  }
}

fm_list
i <- -3.5
j <- 7

for(j in 7:8){
for(i in (1:7)*-3.5){
data_sub <- data %>%
  filter(Position==j&tiefe == i &Pumpstufe == 0) %>% 
#  group_by(tiefe) %>% 
  select("date_int","hour","VWC_roll","T_soil","Wind_roll") %>% 
  summarise(across(1:5,~seq(min(.),max(.),len=10))) %>% 
  mutate(across(3:5,round,1),
         hour = round(hour)) %>% 
  as.data.frame()
  
data_expand <- expand.grid(data_sub)

data_expand$CO2_preds <- predict(fm_list[[paste0("depth_",abs(i),"Pos",j)]],newdata=data_expand)

data_long <- tidyr::pivot_longer(data_expand,1:5) %>% 
  group_by(value,name) %>% 
  summarise(min=min(CO2_preds),max=max(CO2_preds),mean=mean(CO2_preds))  

ggplot(data_long)+
  geom_ribbon(aes(x=value,ymin=min,ymax=max,col=name,fill=name),linetype=2,alpha=0.1)+
  geom_line(aes(value,mean,col=name))+facet_wrap(~name,scales="free")+
  labs(title = paste("tiefe",i,"Pos",j))+
  ggsave(paste0(plotpfad_ms,"marginalplot_tiefe",abs(i),"Pos",j,".png"),width=7,height=6)
}
}
lapply(fm_list,summary)
print(summary(fm_list[[paste0("depth_",abs(i),"Pos",j)]]))

#range(data_sub$date)
data$CO2_tracer_SWC_WS <- data$CO2_roll_inj - (data$preds_SWC_WS)
#data$CO2_tracer_SWC_PPC <- data$CO2_roll_inj - (data$preds_SWC_PPC)
#data$CO2_tracer_drift <- data$CO2_roll_inj - (data$preds_drift)

# data_sub <- subset(data,Position %in% 7:8 )
# ggplot(data_sub)+
#   geom_line(aes(date,CO2_roll_inj,group=tiefe))+
#   #geom_line(aes(date,preds_drift,group=tiefe,col="drift"))+
#   geom_line(aes(date,preds_SWC_T,group=tiefe,col="SWC_T"))+
#   #geom_line(aes(date,preds_SWC_PPC,group=tiefe,col="SWC_PPC"))+
#   geom_line(aes(date,preds_SWC_WS,group=tiefe,col="SWC_WS"))


# ggplot(data_sub)+
#   geom_line(aes(date,PPC*10))+
#   geom_line(aes(date,Wind_roll))
# RMSE(data_sub$preds_drift,data_sub$preds_SWC_WS)
# 
# RMSE(data_sub$preds_drift,data_sub$preds_SWC_T)
# ggplot(data_sub)+
#   geom_line(aes(date,Wind,group=tiefe,col="Wind"))+
#   geom_line(aes(date,Wind_roll,group=tiefe,col="Wind_roll"))
#   


###########################
#tracer uncertainty

data_uncert <- data %>% 
  select(matches("(date|tiefe|preds_(SWC_T|drift|SWC_WS)|CO2_roll|Position|Pumpstufe|CO2_tracer_)")) %>% 
  filter(Position %in% 7:8) %>% 
  group_by(tiefe,Position) %>% 
  mutate(across(c("preds_drift","preds_SWC_T","preds_SWC_WS"),
                list(
                  min = ~. + max(CO2_roll_inj[Pumpstufe==0] - .[Pumpstufe==0],na.rm=T),
                  q25 = ~. + quantile(CO2_roll_inj[Pumpstufe==0] - .[Pumpstufe==0],probs=0.75,na.rm=T),
                  q75 = ~. + quantile(CO2_roll_inj[Pumpstufe==0] - .[Pumpstufe==0],probs=0.25,na.rm=T),
                  max = ~. + min(CO2_roll_inj[Pumpstufe==0] - .[Pumpstufe==0],na.rm=T)
                )
  )
  ) %>% 
  mutate(across(matches("preds_.+(max|min|q\\d+)"),
                list(
                  CO2_tracer= ~CO2_roll_inj - .
                ),
                .names="{.fn}_{.col}"
  )
  ) %>% 
  rename_with(~str_remove(.,"preds_"),matches("^CO2_tracer_preds")) %>% 
  #select(-matches("preds_")) %>% 
  ungroup() %>%
  as.data.frame()

data_uncert <- data_uncert %>% 
  mutate(
    CO2_tracer_drift_mingradient = case_when(
      tiefe >= -7 ~ CO2_tracer_drift_max,
      tiefe == -10.5 ~ CO2_tracer_drift_q75,
      tiefe == -14 ~ CO2_tracer_drift,
      tiefe == -17.5 ~ CO2_tracer_drift_q25,
      tiefe <= -21 ~ CO2_tracer_drift_min
    ),
    CO2_tracer_drift_maxgradient = case_when(
      tiefe >= -7 ~ CO2_tracer_drift_min,
      tiefe == -10.5 ~ CO2_tracer_drift_q25,
      tiefe == -14 ~ CO2_tracer_drift,
      tiefe == -17.5 ~ CO2_tracer_drift_q75,
      tiefe <= -21 ~ CO2_tracer_drift_max
    )
  )


save(data,Pumpzeiten,data_uncert,file=paste0(samplerpfad,"Hartheim_CO2.RData"))


ggplot(data_uncert)+
  geom_line(aes(date,CO2_tracer_drift_min - CO2_tracer_drift_max,col=as.factor(tiefe)))
  

