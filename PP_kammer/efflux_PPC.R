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


##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

Versuch <- 31

datelim <- c(pp_chamber$Start[Versuch]-3600*24*1,pp_chamber$Ende[Versuch]+3600*24*1)

gga_data_T <- !is.na(pp_chamber$GGA_kammermessung[Versuch])
flux_ls <- chamber_arduino(datelim=datelim,gga_data = gga_data_T,return_ls = T,t_init=2,plot="",t_offset = 60,t_min=4,gga=pp_chamber$GGA_kammermessung[Versuch])
flux <- flux_ls[[1]]
flux_data <- flux_ls[[2]]

range(flux$date)
datelim
data_PPC <- read_PP(datelim = datelim)

data_merge <- merge(subset(flux,!is.na(CO2_GGA_mumol_per_s_m2)),subset(data_PPC,id == 1))

data_merge$CO2_flux_roll <- RcppRoll::roll_mean(data_merge$CO2_GGA_mumol_per_s_m2,5,fill=NA)
data_merge$CH4_flux_roll <- RcppRoll::roll_mean(data_merge$CH4_mumol_per_s_m2,5,fill=NA)
ggplot(data_merge)+
  #geom_point(aes(PPC,CO2_flux_roll,col="roll"))+
  geom_smooth(aes(PPC,CO2_GGA_mumol_per_s_m2,col="CO2"),method="lm")+
  labs(x="PPC (Pa/s)",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
  geom_point(aes(PPC,CO2_GGA_mumol_per_s_m2,col="CO2"))

ggplot(data_merge)+
#  geom_point(aes(PPC,CH4_flux_roll*10^3,col="CH4"),alpha=0.2)+
  geom_smooth(aes(PPC,CH4_mumol_per_s_m2*10^3,col="CH4"),method="lm")+
  geom_point(aes(PPC,CH4_mumol_per_s_m2*10^3,col="CH4"))+
  labs(x="PPC (Pa/s)",y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
  scale_color_manual(values = scales::hue_pal()(2)[2])

CH4_timeline <- ggplot(data_merge)+
  geom_line(aes(date,CH4_mumol_per_s_m2*10^3,col="CH4"),alpha=0.4)+
  geom_line(aes(date,CH4_flux_roll*10^3,col="CH4"))+
  scale_color_manual(values = scales::hue_pal()(2)[2])+
  labs(x="",y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),col="")
CO2_timeline <- ggplot(data_merge)+
  geom_line(aes(date,CO2_GGA_mumol_per_s_m2,col="CO2"),alpha=0.4)+
  geom_line(aes(date,CO2_flux_roll,col="CO2"))+
  labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")

PPC_plot <- ggplot(data_merge)+
  geom_line(aes(date,PPC))+
  labs(x="",y="PPC (Pa/s)")

egg::ggarrange(CO2_timeline,CH4_timeline,PPC_plot,ncol=1,heights = c(2,2,1))
CO2_fm <- glm(CO2_GGA_mumol_per_s_m2~PPC,data=data_merge)
1- CO2_fm$deviance/CO2_fm$null.deviance
1- CH4_fm$deviance/CH4_fm$null.deviance
CO2_roll_fm <- glm(CO2_flux_roll~PPC,data=data_merge)
CH4_fm <- glm(CH4_mumol_per_s_m2~PPC,data=data_merge)
CH4_roll_fm <- glm(CH4_flux_roll~PPC,data=data_merge)

summary(CO2_fm)
summary(CO2_roll_fm)
summary(CH4_fm)
summary(CH4_roll_fm)
