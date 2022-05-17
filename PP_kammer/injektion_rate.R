hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)


injection_dates <- read_ods(paste0(metapfad_PP,"injektionen.ods"))
injection_dates$Start <- dmy_hm(injection_dates$Start)
injection_dates$Ende <- dmy_hm(injection_dates$Ende)

#i <- nrow(injection_dates)

#datelim <- c(injection_dates$Start[i],injection_dates$Ende[i])
# datelim <- ymd_hm("22/05/09 13:00","22/05/12 16:00")
# datelim <- ymd_hm("22/04/12 00:00","22/04/15 16:00")
# dates_ls <- vector("list",nrow(injection_dates))
# for(i in 1:nrow(injection_dates)){
#   dates_ls[[i]] <- c(injection_dates$Start[i],injection_dates$Ende[i])
# }
dates_ls <- split(injection_dates[,-3],nrow(injection_dates)) %>% 
  lapply(.,function(x) as_datetime(as.numeric(x)))

inj_ls <- lapply(dates_ls,injection_arduino,                                                               plot="facets",
                 return_ls = F,
                 t_init=2)
inj <- do.call(rbind,inj_ls)
#inj <- injection_arduino(datelim,plot="flux",return_ls = T,t_init=2)
A_inj <- 1^2*pi /10^6 #m2
CO2_mol_per_s <- inj$CO2_mumol_per_s / 10^6
inj$CO2_mol_m2_s <- CO2_mol_per_s/A_inj


save(inj,file = paste(datapfad_PP_Kammer,"injectionrates.RData"))

#######################################
#####################################
#read probes
i <- 1
data <- read_sampler(datelim = dates_ls[[i]] + (3600*24*2 * c(-1,1)))
data$tiefe <- data$tiefe
data$Versuch <- i
inj$date <- round_date(inj$date,"10 mins")

inj_id <- daterange_id(data,dates_ls[[i]])
data$inj <- ifelse(inj_id,1,0)
cal_id <- daterange_id(data,dates_ls[[i]] + (3600*20 * c(0,1)))
data$cal <- ifelse(cal_id,0,1)
data <- merge(data,inj[,c("date","CO2_mol_m2_s")],all = T)

data <- data %>% 
  group_by(tiefe) %>% 
  mutate(inj_mol_m2_s = imputeTS::na_interpolation(CO2_mol_m2_s),
         inj_mol_m2_s = ifelse(inj == 1 ,inj_mol_m2_s,0),
         offset = CO2_smp2 - CO2_smp1,
         date_int = as.numeric(date),
         CO2_ref = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
         CO2_inj = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
         )

ggplot(data)+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))
data_cal <- subset(data, cal == 1)

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

data$half_hour <- round_date(data$date,"180 mins")
mod_dates <- sort(unique(data$half_hour))

#comsol<- run_comsol(data=data,mod_dates = mod_dates,offset_method = "drift",overwrite = F,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")

comsol<- run_comsol_nruns(data=data,mod_dates = mod_dates[1:10],offset_method = "drift",overwrite = F,read_all = T,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs",nruns=50,long=F)
head(comsol)
ggplot(comsol)
ggplot(data)+
  geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_refadj,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))

ggplot(data)+
  geom_line(aes(date,CO2_tracer,col=as.factor(tiefe)))
ggplot(data)+
  geom_line(aes(date,inj_mumol_s,col=as.factor(cal),group=1))


cal_period <- 
  data$preds_drift <- NA
# data$preds_drift_amp <- NA
#data$preds_no_ref <- NA

#unique(unlist(lapply(data_PSt0,"[","Position")))
#for(j in j_78){
for(j in seq_along(data_PSt0)[-c(5)]){
  for(i in (1:7)*-3.5){
    fm_drift <- glm(offset ~ poly(date_int,2),data=subset(data_PSt0[[j]],tiefe==i))
    
    pos <- na.omit(unique(data$Position))[j]
    ID <- which(data$tiefe==i & data$Position == pos)
    data$offset_drift[ID] <- predict(fm_drift,newdata = data[ID,])
    data$preds_drift[ID] <- data$CO2_roll_ref[ID] + data$offset_drift[ID]
    
  }
  #svMisc::progress(j,max.value = length(data_PSt0))
}
########################
data$CO2_tracer_drift <- data$CO2_roll_inj - (data$preds_drift)


ggplot(data)+
  geom_line(aes(date,inj_mumol_s))

ggplot(data)+
  geom_line(aes(date,CO2_smp1,col=as.factor(tiefe),linetype=as.factor(cal)))+
  geom_line(aes(date,CO2_smp2,col=as.factor(tiefe)))
