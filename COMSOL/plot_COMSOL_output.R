#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

meas_points <- data.frame(R=0,Z=meas_depths)
write.table(meas_points,file=paste0(comsolpfad,"meas_points.txt"),row.names = F,col.names = F)

test <- read.table(paste0(comsolpfad,"conc_test.txt"),skip=9,col.names = c("r","z","CO2"))
test<- round(test,9)
CO2_mod <- read.table(paste0(comsolpfad,"conc_tiefen.txt"),skip=9,col.names = c("r","z","CO2_mol_per_m3"))
CO2_mod$tiefe <- CO2_mod$z - 40

p_kPa <- 101.3
T_deg <- 20
p_Pa <- p_kPa*1000#PA = N/m2 = kg/(m s2)
#Temperatur
T_K <- T_deg+273.15 #K
#allgemeine Gaskonstante
R <- 8.314 #kg m2/(s2 mol K)
mol_per_m3 <- p_Pa/(R*T_K) #kg/(m s2) / kg m2 * (s2 mol K)/ K = mol/m3

CO2_mod$CO2 <- CO2_mod$CO2_mol_per_m3 / mol_per_m3 *10^6

meas_depths <- (40-(1:7*3.5))
model <- subset(test,r== 0 & z %in% meas_depths)

model$tiefe <- model$z - 40
ggplot(test)+geom_point(aes(r,z,col=log(CO2)))+scale_color_viridis_c()
ggplot(test)+geom_point(aes(z,log(CO2)))
ggplot(model)+geom_point(aes(CO2,z))  


ggplot()+
  geom_point(data = subset(data,Versuch == 3), aes(CO2_rollapply, tiefe, col=PSt_Nr))+
  geom_point(data=CO2_mod,aes(CO2,tiefe))+
  labs(x="CO2 [ppm]",y="tiefe [cm]")
