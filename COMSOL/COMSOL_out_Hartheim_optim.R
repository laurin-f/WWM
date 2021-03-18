###################################
#dateien für COMSOL exportieren####

#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
plotpfad_comsol <- paste0(hauptpfad,"Dokumentation/Berichte/plots/COMSOL/")
COMSOL_exepath <- "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
COMSOL_progammpath <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","ggforce","dplyr")

check.packages(packages)

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))

data$CO2_tracer_drift2 <- data$CO2_tracer_drift

data$date_hour <- round_date(data$date,"60 mins")
data$date_3_hours <- round_date(data$date,"3 hours")
mod_dates <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 & data$date %in% data$date_hour& data$date > ymd_h("2020.07.10 00")]))
mod_dates_short <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 & data$date %in% data$date_3_hours]))

# data_min_inj <- data %>% 
#   filter(Position == 8 & as.numeric(inj_mol_m2_s) > 0) %>% 
#   mutate(inj_mol_m2_s = min(inj_mol_m2_s, na.rm=T), CO2_tracer_min_inj = CO2_tracer_drift)
# data_max_inj <- data %>% 
#   filter(Position == 8 & as.numeric(inj_mol_m2_s) > 0) %>% 
#   mutate(inj_mol_m2_s = max(inj_mol_m2_s, na.rm=T), CO2_tracer_max_inj = CO2_tracer_drift)
# 
# DS_anisotrop_min_inj <- run_comsol(data=data_min_inj,mod_dates = mod_dates_short,offset_method = "min_inj",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
# DS_anisotrop_max_inj <- run_comsol(data=data_max_inj,mod_dates = mod_dates_short,offset_method = "min_inj",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
date_pattern <- "\\d{2}(_\\d{2}){2,3}"

DS_anisotrop_drift <- run_comsol(data=data,mod_dates = (mod_dates),offset_method = "drift2",overwrite = F,plot=F,read_all = T,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")



# DS_anisotrop_no_ref <- run_comsol(data=data,mod_dates = rev(mod_dates),offset_method = "no_ref",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
# DS_anisotrop <- run_comsol(data=data,mod_dates = rev(mod_dates),offset_method = "gam",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
# 
# 
# DS_df <- run_comsol(data=data,mod_dates = mod_dates,offset_method = "gam",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_optim_3DS")
# 
#bei disturbance ist eine 20cm Schicht unter der Sonde als disturbed im Modell (beim einbau) und hat einen höheren DS3 (DS3*30)
# DS_dist <- run_comsol(data=data,mod_dates = mod_dates,offset_method = "gam",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_disturbance_optim_3DS")
# 

######################
#long-format
#######################


# DS_long <- tidyr::pivot_longer(DS_df,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_dist_long <- tidyr::pivot_longer(DS_dist,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_anisotrop_long <- tidyr::pivot_longer(DS_anisotrop,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_anisotrop_long_min_inj <- tidyr::pivot_longer(DS_anisotrop_min_inj,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_anisotrop_long_no_ref <- tidyr::pivot_longer(DS_anisotrop_no_ref,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# colnames(DS_anisotrop_drift)

DS_anisotrop_drift[,paste0("DS_roll",1:3)] <- zoo::rollapply(DS_anisotrop_drift[,paste0("DS",1:3)],width=10,mean,fill=NA)
DS_anisotrop_drift[,paste0("DSD0_roll",1:3)] <- zoo::rollapply(DS_anisotrop_drift[,paste0("DSD0",1:3)],width=10,mean,fill=NA)

DS_anisotrop_long_drift <- tidyr::pivot_longer(DS_anisotrop_drift,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_anisotrop_long_drift_amp <- tidyr::pivot_longer(DS_anisotrop_drift_amp,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# 
# save(DS_anisotrop_long,DS_anisotrop,file=paste0(comsolpfad,"DS_anisotrop_gam.RData"))
# save(DS_anisotrop_long_no_ref,DS_anisotrop_no_ref,file=paste0(comsolpfad,"DS_anisotrop_no_ref.RData"))
save(DS_anisotrop_long_drift,DS_anisotrop_drift,file=paste0(comsolpfad,"DS_anisotrop_drift.RData"))
#save(DS_anisotrop_long_drift_amp,DS_anisotrop_drift,file=paste0(comsolpfad,"DS_anisotrop_drift_amp.RData"))





#############################################
#           PLOTS                           #
#############################################
ggplot()+
  #  geom_line(data=DS_anisotrop_long,aes(date,DSD0,col=tiefe,linetype="gam"))+
  geom_line(data=DS_anisotrop_long_drift,aes(date,DSD0,col=tiefe,linetype="drift"))#+
  
#  geom_line(data=DS_anisotrop_long_min_inj,aes(date,DSD0,col=tiefe,linetype="min_inj"))#+
ggplot(DS_long)+
  geom_line(aes(date,DSD0,col=tiefe,linetype="no disturbance"))+
  geom_line(data=DS_dist_long,aes(date,DSD0,col=tiefe,linetype="with disturbance"))
ggplot(DS_long)+
  geom_line(aes(date,DSD0,col=tiefe,linetype="isotrop"))+
  geom_line(data=DS_anisotrop_long,aes(date,DSD0,col=tiefe,linetype="anisotrop"))+ggsave(paste0(plotpfad_comsol,"DS_anisotrop.png"),width=5,height=3)
ggplot()+
  geom_line(data=DS_anisotrop_long,aes(date,DSD0,col=tiefe,linetype="gam"))+
  geom_line(data=DS_anisotrop_long_no_ref,aes(date,DSD0,col=tiefe,linetype="no ref"))#+
  xlim(range(DS_anisotrop_long_no_ref$date))
  #ggsave(paste0(plotpfad_comsol,"DS_anisotrop.png"),width=5,height=3)



ggplot()+
  geom_line(data=DS_anisotrop_long,aes(date,Fz,col=tiefe,linetype="gam"))+
  geom_line(data=DS_anisotrop_long_drift,aes(date,Fz,col=tiefe,linetype="drift"))+
  geom_line(data=DS_anisotrop_long_no_ref,aes(date,Fz,col=tiefe,linetype="no_ref"))#+

range(DS_anisotrop$DSD03)
range(DS_df$DSD03)
ggplot(DS_dist)+geom_line(aes(date,DS3))
run_comsol(data,mod_dates,)
