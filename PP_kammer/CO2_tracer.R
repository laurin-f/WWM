hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

theme_set(theme_classic())
##
load(file = paste(datapfad_PP_Kammer,"injectionrates.RData"))
#######################################
#####################################
#read probes
data_ls <- vector("list",length = length(dates_ls))
for(Versuch in seq_along(dates_ls)){

#CO2 Werte für i-te injektion inklusive 2 tage vorher und nachher
data <- read_sampler(datelim = dates_ls[[Versuch]] + (3600*24*2 * c(-1,1)))
data$tiefe <- data$tiefe

data$Versuch <- Versuch
inj$date <- round_date(inj$date,"10 mins")
#Spalte in inj hat 1er während der injektion
inj_id <- daterange_id(data,dates_ls[[Versuch]])
data$inj <- ifelse(inj_id,1,0)
#Spalte cal hat 1er vor der injektion und danach mit 20 h Abstand nach der injektion da hier der Tracer noch sichtbar ist
cal_id <- daterange_id(data,dates_ls[[Versuch]] + (3600*20 * c(0,1)))
data$cal <- ifelse(cal_id,0,1)
data <- merge(data,inj[inj$Versuch == Versuch,c("date","CO2_mol_m2_s")],all = T)

data <- data %>% 
  group_by(tiefe) %>% 
  mutate(inj_mol_m2_s = imputeTS::na_interpolation(CO2_mol_m2_s),
         inj_mol_m2_s = ifelse(inj == 1 ,inj_mol_m2_s,0),
         offset = CO2_smp2 - CO2_smp1,
         date_int = as.numeric(date),
         CO2_ref = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
         CO2_inj = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
  )

data_cal <- subset(data, cal == 1)


####################################
#adj CO2 tracer
#######################################

data$CO2_refadj <- NA
data$offset_drift <- NA
for(i in 1:7){
  fm_drift <- glm(offset ~ poly(date_int,1),data=subset(data_cal,tiefenstufe==i))
  ID <- which(data$tiefenstufe==i)
  data$offset_drift[ID] <- predict(fm_drift,newdata = data[ID,])
  data$CO2_refadj[ID] <- data$CO2_ref[ID] + data$offset_drift[ID]
}
data$CO2_tracer_drift <- data$CO2_inj - (data$CO2_refadj)
data$T_soil <- data$T_C

data_ls[[Versuch]] <- data
}

data <- do.call(rbind,data_ls)

save(data,file=paste0(datapfad_PP_Kammer,"data_tracer.RData"))
#########################
#plots


#####################
#CO2 inj un refadj plot
ggplot(subset(data,!is.na(Versuch)))+
  geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_refadj,col=as.factor(tiefe),linetype="ref adj"))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe),linetype="inj"))+
  facet_wrap(~Versuch,scales = "free_x",ncol=1)
  #geom_vline(xintercept = dates_ls[[Versuch]])


###################
#tracer plot
ggplot(subset(data,!is.na(Versuch)))+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(tiefe)))+
  facet_wrap(~Versuch,scales = "free_x",ncol=1)
  
#  geom_vline(xintercept = ymd_hm("2022.07.06 15:00"))

######################
#injection rate plot
ggplot(subset(data,!is.na(Versuch)))+
  geom_line(aes(date,inj_mol_m2_s,col=as.factor(cal),group=1))+
  facet_wrap(~Versuch,scales = "free_x",ncol=1)
