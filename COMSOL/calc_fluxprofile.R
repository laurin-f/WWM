#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_tracer<- paste0(metapfad,"Tracereinspeisung/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg","dplyr")

check.packages(packages)
theme_set(theme_classic())

######################################
#data_agg
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(file=paste0(klimapfad,"klima_data.RData"))

#######################
#einheiten anpassen für COMSOl
#Tracersignal in COMSOL Einheit umrechnen
offset_method <- "gam"
######################

load(file=paste0(comsolpfad,"F_df_gam_3DS_pos8_ext.RData"))

F_df_pos8 <- F_df
# load(file=paste0(comsolpfad,"F_df_gam_3DS_ext2.RData"))
# F_df_ext2<-rbind(F_df,F_df_pos8)
load(file=paste0(comsolpfad,"F_df_gam_3DS_ext.RData"))
F_df<-rbind(F_df,F_df_pos8)

load(paste0(comsolpfad,"DS_anisotrop_gam.RData"))
F_df <- DS_anisotrop
names(F_df) <- str_replace(names(F_df),"(\\d)$","_\\1")

pos8_date <- min(data$date[which(data$Position ==8 & data$Pumpstufe != 0)])
Versuch2_date <- ymd_h("2020.07.10 00")
F_df$Versuch <- ifelse(F_df$date < pos8_date,ifelse(F_df$date < Versuch2_date,"1","2"),"3")



#R_soil[µmol/m²und s]=(0.14*SWC@20cm[vol%]-0.05)*0.66*exp(0.076*T_soil@3 cm)



soil_wide$R_soil <- (0.14*soil_wide$mean_VWC_20-0.05)*0.66*exp(0.076*soil_wide$mean_T_2)
soil_wide$R_min <- (0.14*soil_wide$VWC_min_20-0.05)*0.66*exp(0.076*soil_wide$T_min_2)
soil_wide$R_max <- (0.14*soil_wide$VWC_max_20-0.05)*0.66*exp(0.076*soil_wide$T_max_2)

#soil_wide$R_soil <- (0.14*soil_wide$mean_VWC_20+0.2)*0.66*exp(0.076*soil_wide$mean_T_2)


data$CO2_ref_mol_m3 <- ppm_to_mol(data$CO2_ref,"ppm",out_class = "units",T_C = data$T_soil,p_kPa = data$PressureActual_hPa/10)

data_wide_CO2 <- tidyr::pivot_wider(data[data$date %in% F_df$date,],date,names_from=tiefenstufe,values_from = CO2_ref_mol_m3,names_prefix = "CO2_ref_")
F_df <- merge(F_df,data_wide_CO2)


##################################################################
#anstatt glm geht es viel schneller jeweils den mittelwert von 3 
################
#flux
dC_dz_mol_0_7 <- rowMeans(cbind(F_df$CO2_ref_0 - F_df$CO2_ref_1,F_df$CO2_ref_1 - F_df$CO2_ref_2)/-3.5)
dC_dz_mol_10_17 <- rowMeans(cbind(F_df$CO2_ref_3 - F_df$CO2_ref_4,F_df$CO2_ref_4 - F_df$CO2_ref_5)/-3.5)
#mol/m^3/cm
#einheit in mol / m3 /cm

#Ficks Law
#Fz_mumol_per_s_m2 <- F_df$DS_1[k]  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2

F_df$Fz_10_17 <- F_df$DS_2   * dC_dz_mol_10_17 * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2


sub7u8 <- subset(data, Position %in% 7:8) 
sub7u8$tiefe_pos <- -sub7u8$tiefe 

data_wide <- tidyr::pivot_wider(sub7u8,date,names_from = tiefe_pos,values_from = c(T_soil,PressureActual_hPa))
for(i in c("T_soil_","PressureActual_hPa_")){
  data_wide[,paste0(i,1)] <- rowMeans(data_wide[,paste0(i,0:2*3.5)])
  data_wide[,paste0(i,2)] <- rowMeans(data_wide[,paste0(i,3:5*3.5)])
  data_wide[,paste0(i,3)] <- rowMeans(data_wide[,paste0(i,6:7*3.5)])
}



F_df<- merge(F_df,data_wide[,c("date",paste0("T_soil_",1:3),paste0("PressureActual_hPa_",1:3))])
for(i in 1:3){
  F_df[,paste0("D0",i)] <- D0_T_p(T_C=F_df[,paste0("T_soil_",i)],p_kPa = F_df[,paste0("PressureActual_hPa_",i)]/10,unit="m^2/s")
  F_df[,paste0("DSD0_",i)] <- F_df[,paste0("DS_",i)]/F_df[,paste0("D0",i)]
}
