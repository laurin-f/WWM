###################################
#dateien für COMSOL exportieren####

#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units")
check.packages(packages)

#dataset laden
pars_fs <- read.table((paste0(metapfad,"COMSOL/parameter_freeSoil.csv")),sep=";",stringsAsFactors = F)

z_soil_ch <- str_split(pars_fs[pars_fs[,1] == "z_soil",2],"\\s",simplify = T)
unit <- str_remove_all(z_soil_ch[,2],"\\[|\\]")

z_soil <- set_units(as.numeric(z_soil_ch[,1]),unit,mode="standard")
z_soil_cm <- as.numeric(set_units(z_soil,"cm"))


# 
# #tiefen in Modellkoordinaten umrechnen
# data_agg$Z_mod <- data_agg$tiefe + z_box
# data_agg$R_mod <- 0
# 
# #injection rate in mol / (m^2*s)
# #Fläche der injection 
# A_inj <- set_units(1^2*pi,"mm^2")
# 
# unique(data_agg$Fz) #ml/min
# injection_rates <- ppm_to_mol(data_agg$Fz,unit_in = "cm^3/min",out_class = "units")
# inj_mol_cm2_s <- set_units(injection_rates,"mol/s")/A_inj
# data_agg$inj_mol_m2_s <- set_units(inj_mol_cm2_s,"mol/m^2/s")
# 
# data_sub <- subset(data_agg, respi_sim == "nein" & Versuch %in% c(3:10) & Pumpstufe %in% c(1.5,3))
# 
# input_pars <- data_sub[data_sub$tiefenstufe == 0, c("ID","inj_mol_m2_s", "CO2_mol_per_m3","DS","material","Pumpstufe")]
# colnames(input_pars) <- c("ID","injection_rate","CO2_atm","DS_glm","material","Pumpstufe")
# ggplot(input_pars)+geom_point(aes(Pumpstufe,DS_glm,col=material))
# ggplot(input_pars)+
#   #geom_boxplot(aes(material,DS_glm/D0_T_p(15),fill=material))+
#   geom_point(aes(material,DS_glm/D0_T_p(15),col=material))
# input_pars
# 
# 
# save(input_pars,file=paste0(samplerpfad,"tracereinspeisung_sandkiste_sub.RData"))
# #CO2_atm
# CO2_atm <- sort(unique(input_pars$CO2_atm))
# write.table(paste0("CO2_atm ",paste(CO2_atm,collapse=", ")),file = paste0(comsolpfad,"CO2_atm.txt"),row.names = F,col.names = F,quote=F)
# 
# 
# #injection_rates
# injection_rates <- sort(unique(input_pars$injection_rate))
# write.table(paste0("injection_rate ",paste(injection_rates,collapse=", ")),file = paste0(comsolpfad,"injection_rates.txt"),row.names = F,col.names = F,quote=F)


############
#Punkte aus Mesh identifizieren die nah an Z und R liegen
#messtiefen

meas_depths_fs <- (z_soil_cm-(0:7*3.5))
meas_points_fs <- data.frame(R=0,Z=meas_depths_fs)

write.table(meas_points_fs,file = paste0(metapfad,"COMSOL/meas_points_freeSoil.txt"),row.names = F,col.names = F)

