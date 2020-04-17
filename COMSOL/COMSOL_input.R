###################################
#dateien für COMSOL exportieren####

#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

#dataset laden
load(paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))

#tiefen in Modellkoordinaten umrechnen
data_agg$Z_mod <- data_agg$tiefe +40
data_agg$R_mod <- 0

#CO2 in mol per m3
data_agg$CO2_mol_per_m3 <- ppm_to_mol(data_agg$CO2)


############
#Punkte aus Mesh identifizieren die nah an Z und R liegen
#messtiefen
meas_depths <- (40-(1:7*3.5))
#outfile von COMSOl lesen in der die Mesh Koordinaten stehen
mesh <- read.table(paste0(comsolpfad,"conc_test.txt"),skip=9,col.names = c("r","z","CO2_mol_per_m3"))


id<-rep(NA,length(meas_depths))
for(i in seq_along(meas_depths)){
  id[i]<-which( mesh$r < 0.1 & mesh$r > 0 & abs((mesh$z) - meas_depths[i]-0.1)< 0.2)
}
#kontrolle
mesh[id,]
#werte zuweisen
data_agg$Z_adj <- mesh[id,"z"]
data_agg$R_adj <- mesh[id,"r"]

#eventuell leicht verschieben
# data_agg$Z_mod <- data_agg$Z_mod - 0.1
# data_agg$R_mod <- data_agg$R_mod + 0.1
########################
#export data 
########################

#versuch 3
#txt
write.table(data_agg[data_agg$PSt_Nr == "PSt_5_Nr_3",c("Z_mod","CO2_mol_per_m3")],file = paste0(comsolpfad,"PSt_5_Nr_3.txt"),row.names = F,col.names = F)
#csv 
write.table(data_agg[data_agg$PSt_Nr == "PSt_5_Nr_3",c("Z_mod","R_mod","CO2_mol_per_m3")],file = paste0(comsolpfad,"PSt_5_Nr_3.csv"),row.names = F,col.names = F,sep=",")

#Versuch 4
#txt
write.table(data_agg[data_agg$PSt_Nr == "PSt_5_Nr_4",c("Z_mod","CO2_mol_per_m3")],file = paste0(comsolpfad,"PSt_5_Nr_4.txt"),row.names = F,col.names = F)
#csv 
write.table(data_agg[data_agg$PSt_Nr == "PSt_5_Nr_4",c("Z_mod","R_mod","CO2_mol_per_m3")],file = paste0(comsolpfad,"PSt_5_Nr_4.csv"),row.names = F,col.names = F,sep=",")

#adj
 # write.table(data_agg[data_agg$PSt_Nr == "PSt_5_Nr_3",c("Z_adj","R_adj","CO2_mol_per_m3")],file = paste0(comsolpfad,"PSt_5_Nr_3.csv"),row.names = F,col.names = F,sep=",")
