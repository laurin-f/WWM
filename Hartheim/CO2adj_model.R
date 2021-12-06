

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

data_PSt0 <- lapply(na.omit(unique(data$Position)),function(x) subset(data, Pumpstufe == 0 & Position == x))

j_78 <- which(sapply(data_PSt0, function(x) unique(x$Position %in% 7:8)))
##################
#mit glm oder gam

#data$preds_SWC_T <- NA
data$preds_SWC_WS <- NA
data$preds_SWC_PPC <- NA
#data$preds_SWC_T_Wind <- NA
#data$offset_drift <- NA
#data$preds_drift <- NA


for(j in j_78){

  for(i in (1:7)*-3.5){
    
    #fm_drift <- glm(offset ~ poly(date_int,2),data=subset(data_PSt0[[j]],tiefe==i))
    
    #fm_SWC_T <- mgcv::gam(CO2_roll_inj ~  poly(date_int,2) + s(hour) + poly(VWC_roll,2) + poly(T_soil,2),data=subset(data_PSt0[[j]],tiefe==i & !is.na(VWC_roll)))
    fm_SWC_WS <- mgcv::gam(CO2_roll_inj ~ poly(date_int,2) + s(hour) + poly(VWC_roll,2) + poly(T_soil,2) + poly(Wind_roll,2),data=subset(data_PSt0[[j]],tiefe==i & !is.na(VWC_roll)))
    fm_SWC_PPC <- mgcv::gam(CO2_roll_inj ~ poly(date_int,2) + s(hour) + poly(VWC_roll,2) + poly(T_soil,2) + poly(PPC,2),data=subset(data_PSt0[[j]],tiefe==i & !is.na(PPC)))
    
    pos <- na.omit(unique(data$Position))[j]
    ID <- which(data$tiefe==i & data$Position == pos)
    
    #data$offset_drift[ID] <- predict(fm_drift,newdata = data[ID,])
    #data$preds_drift[ID] <- data$CO2_roll_ref[ID] + data$offset_drift[ID]
    
    #data$preds_SWC_T[ID] <- predict(fm_SWC_T,newdata = data[ID,])
    
    data$preds_SWC_WS[ID] <- predict(fm_SWC_WS,newdata = data[ID,])
    data$preds_SWC_PPC[ID] <- predict(fm_SWC_PPC,newdata = data[ID,])
    #data$preds_SWC_T_Wind[ID] <- predict(fm_SWC_T_Wind,newdata = data[ID,])
    
  }
}

range(data_sub$date)
data$CO2_tracer_SWC_WS <- data$CO2_roll_inj - (data$preds_SWC_WS)
data$CO2_tracer_SWC_PPC <- data$CO2_roll_inj - (data$preds_SWC_PPC)
#data$CO2_tracer_drift <- data$CO2_roll_inj - (data$preds_drift)

data_sub <- subset(data,Position %in% 7:8)
ggplot(data_sub)+
  #geom_line(aes(date,CO2_roll_inj,group=tiefe))+
  #geom_line(aes(date,preds_drift,group=tiefe,col="drift"))+
  #geom_line(aes(date,preds_SWC_T,group=tiefe,col="SWC_T"))+
  geom_line(aes(date,preds_SWC_PPC,group=tiefe,col="SWC_PPC"))+
  geom_line(aes(date,preds_SWC_WS,group=tiefe,col="SWC_WS"))

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
  select(matches("(date|tiefe|preds_(SWC_T|drift)|CO2_roll|Position|Pumpstufe|CO2_tracer_)")) %>% 
  filter(Position %in% 7:8) %>% 
  group_by(tiefe) %>% 
  mutate(across(c("preds_drift","preds_SWC_T"),
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

