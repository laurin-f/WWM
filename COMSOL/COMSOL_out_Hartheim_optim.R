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
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
COMSOL_exepath <- "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
COMSOL_progammpath <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","ggforce","dplyr")

check.packages(packages)

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))


data$date_hour <- round_date(data$date,"hours")
mod_dates <- sort(unique(data$date[data$Position %in% 8 & data$Pumpstufe != 0 & data$date %in% data$date_hour]))

mod_dates <- tail(mod_dates,20)

DS_df <- run_comsol(data=data,mod_dates = mod_dates,offset_method = "gam",overwrite = T,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_optim_3DS")
DS_dist <- run_comsol(data=data,mod_dates = mod_dates,offset_method = "gam",overwrite = T,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_disturbance_optim_3DS")
colnames(DS_long)
DS_long <- tidyr::pivot_longer(DS_df,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
DS_dist_long <- tidyr::pivot_longer(DS_dist,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
ggplot(DS_long)+
  geom_line(aes(date,DSD0,col=tiefe,linetype="no disturbance"))+
  geom_line(data=DS_dist_long,aes(date,DSD0,col=tiefe,linetype="with disturbance"))
ggplot(DS_dist)+geom_line(aes(date,DS3))
run_comsol(data,mod_dates,)
