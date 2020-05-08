###################################
#dateien für COMSOL exportieren####

#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units")
check.packages(packages)

#dataset laden
load(paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))

#tiefen in Modellkoordinaten umrechnen
data_agg$Z_mod <- data_agg$tiefe + 37
data_agg$R_mod <- 0

#injection rate in mol / (m^2*s)
#Fläche der injection 
A_inj <- set_units(1^2*pi,"mm^2")

unique(data_agg$Fz) #ml/min
injection_rates <- ppm_to_mol(data_agg$Fz,unit_in = "cm^3/min",out_class = "units")
inj_mol_cm2_s <- set_units(injection_rates,"mol/s")/A_inj
data_agg$inj_mol_m2_s <- set_units(inj_mol_cm2_s,"mol/m^2/s")

#CO2_atm
CO2_atm <- sort(data_agg$CO2_mol_per_m3[data_agg$tiefenstufe==0])
CO2_atm
#CO2 in mol per m3
data_agg$CO2_mol_per_m3 <- ppm_to_mol(data_agg$CO2)


input_pars <- data_agg[ data_agg$tiefenstufe == 0,c("ID","inj_mol_m2_s", "CO2_mol_per_m3","tiefenstufe")]
colnames(input_pars) <- c("ID","injection_rate","CO2_atm") 
input_pars
subset(input_pars,ID=="PSt_3_Nr_5nein")



############
#Punkte aus Mesh identifizieren die nah an Z und R liegen
#messtiefen
meas_depths <- (37-(0:7*3.5))
meas_points <- data.frame(R=0,Z=meas_depths)

write.table(meas_points,file = paste0(comsolpfad,"meas_points.txt"),row.names = F,col.names = F)

