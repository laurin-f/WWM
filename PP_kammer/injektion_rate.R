hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
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
#liste mit Start und Endzeitpunkten der injektionen
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
#CO2 Werte für i-te injektion inklusive 2 tage vorher und nachher
data <- read_sampler(datelim = dates_ls[[i]] + (3600*24*2 * c(-1,1)))
data$tiefe <- data$tiefe

data$Versuch <- i
inj$date <- round_date(inj$date,"10 mins")
#Spalte in inj hat 1er während der injektion
inj_id <- daterange_id(data,dates_ls[[i]])
data$inj <- ifelse(inj_id,1,0)
#Spalte cal hat 1er vor der injektion und danach mit 20 h Abstand nach der injektion da hier der Tracer noch sichtbar ist
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

data$half_hour <- round_date(data$date,"60 mins")
mod_dates <- sort(unique(data$half_hour[data$inj == 1]))


ggplot(data)+
  geom_line(aes(date,CO2_refadj,col=as.factor(inj),group=tiefe))
ggplot(data)+
  geom_line(aes(date,CO2_refadj,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))
#comsol<- run_comsol(data=data,mod_dates = mod_dates,offset_method = "drift",overwrite = F,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")


comsol<- interp_comsol_inj(data=data,
                          mod_dates = mod_dates,
                          offset_method = "drift",
                          overwrite = T,
                          read_all = F,
                          modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs",
                          nruns=50,
                          long=T)


comsol_old<- run_comsol(data=data,
                          mod_dates = mod_dates,
                          offset_method = "drift",
                          overwrite = F,
                          read_all = F,
                          modelname = "Diffusion_freeSoil_anisotropy_optim_3DS",
                          long=T)



ggplot()+
  geom_line(data=comsol,aes(date,DSD0,group=as.factor(tiefe),col="inter",linetype="inter"))+
  geom_line(data=comsol_old,aes(date,DSD0,group=as.factor(tiefe),col="old",linetype="old"))#+
  
DSD0_plt <- ggplot(comsol)+
  geom_line(aes(date,DSD0,col=as.factor(tiefe)))

tracer_plt <- ggplot(data)+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(-tiefe)))+
  xlim(range(mod_dates))
names(data)


egg::ggarrange(DSD0_plt,tracer_plt)
ggplot(data)+
  geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_refadj,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))

ggplot(data)+
  geom_line(aes(date,CO2_tracer,col=as.factor(tiefe)))
ggplot(data)+
  geom_line(aes(date,inj_mumol_s,col=as.factor(cal),group=1))


####################
#PP

##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)
datelim <- range(mod_dates)
data_PPC <- read_PP(datelim = range(data$date))

if(nrow(data_PPC) > 0){
  data_PPC <- subset(data_PPC,id != 5)
  dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)
  
  data_PPC <- data_PPC %>% 
    group_by(id) %>%
    mutate(dt = diff_time(date,"secs"),
           P_diff = abs(c(NA,diff(P_filter)))/!!dt,
           PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
           PPC_diff = abs(c(NA,diff(PPC5))),
           P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA))
  
  data_PPC$id[data_PPC$id == 6] <- "outside"
  data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
  
  
  
  PP_plot <- 
    ggplot(data_PPC)+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,PPC5,col=id))+
    coord_cartesian(xlim=datelim)+
    scale_fill_grey()+
    guides(fill=F)+
    labs(x="",y="PPC (Pa/s)")
  
  P_roll_plot <- ggplot(subset(data_PPC,id %in% 1:4) )+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,P_roll,col=id),alpha=0.5, linetype=2)+
    coord_cartesian(xlim=datelim)+
    scale_fill_grey()+
    guides(fill=F,col=F)+
    labs(x="",y=expression(P["moving average"]))
}


ggplot(subset(data_PPC,id %in% 1:4))+
  geom_line(aes(date,PPC_diff,col=id))
which(diff(data_PPC$PPC5))
egg::ggarrange(DSD0_plt,PP_plot)
egg::ggarrange(DSD0_plt,P_roll_plot)
########################
data$CO2_tracer_drift <- data$CO2_roll_inj - (data$preds_drift)


ggplot(data)+
  geom_line(aes(date,inj_mumol_s))

ggplot(data)+
  geom_line(aes(date,CO2_smp1,col=as.factor(tiefe),linetype=as.factor(cal)))+
  geom_line(aes(date,CO2_smp2,col=as.factor(tiefe)))
