###################################
#dateien für COMSOL exportieren####

#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units")
check.packages(packages)

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))

Kammer_flux$date

data$CO2_mol_per_m3 <- ppm_to_mol(data$CO2_tracer_glm,"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$Ta_2m)
data$CO2_mol_per_m3[is.na(data$CO2_mol_per_m3)]<- 0
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0

A_inj <- set_units(1^2*pi,"mm^2")

inj_mol_min <- ppm_to_mol(round(data$Fz,6),"cm^3/min",out_class = "units",p_kPa = data$PressureActual_hPa/10,T_C = data$Ta_2m)
inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/A_inj
data$inj_mol_m2_s <- set_units(inj_mol_mm2_s,"mol/m^2/s")

z_soil_cm <-  150
data$z <- z_soil_cm +data$tiefe
data$r <- 0
?order


kammer_dates <- ymd_hm(c("2020.07.08 12:00","2020.07.14 16:00"))
kammer_sub <- lapply(kammer_dates,function(x) subset(data[,c("tiefe","date","z","r","CO2_mol_per_m3","inj_mol_m2_s","DSD0_PTF_max","DSD0_PTF_min","Ta_2m","PressureActual_hPa","CO2_ref")], date==x))


ggplot(kammer_sub[[1]])+geom_point(aes(CO2_mol_per_m3,z))
ggplot(kammer_sub[[2]])+geom_line(aes(CO2_mol_per_m3,z),orientation = "y")


sub_j <- kammer_sub[[2]]
sub_j <- sub_j[order(sub_j$z),]
unique(sub_j$inj_mol_m2_s)
D0 <- D0_T_p(sub_j$Ta_2m,p_kPa = sub_j$PressureActual_hPa/10)
sub_j$DS_max_m2_s <- sub_j$DSD0_PTF_max * D0 / 10^4
sub_j$DS_min_m2_s <- sub_j$DSD0_PTF_min * D0 / 10^4
ggplot(sub_j)+
  geom_point(aes(DS_max_m2_s,tiefe))+
  geom_point(aes(DS_min_m2_s,tiefe))+
  geom_vline(xintercept = c(4.15e-6,1.02e-6))
for(i in 1:7){
  write.table(sub_j[sub_j$tiefe == (1:7*-3.5)[i],"CO2_mol_per_m3"],paste0(metapfad_comsol,"dom",i,".csv"),col.names = F,row.names = F,sep=",")
}
write.table(na.omit(sub_j[,c("r","z","CO2_mol_per_m3")]),paste0(metapfad_comsol,"CO2_obs",".csv"),col.names = F,row.names = F,sep=",")

# for(j in seq_along(kammer_sub)){
#   sub_j <- kammer_sub[[j]]
#   sub_j <- sub_j[order(sub_j$z),]
#   for(i in 1:7){
#     write.table(sub_j[sub_j$tiefe == (1:7*-3.5)[i],"CO2_mol_per_m3"],paste0(metapfad_comsol,"tiefe",i,"_",as_date(kammer_dates[j]),".csv"),col.names = F,row.names = F,sep=",")
#   }
#   write.table(na.omit(sub_j[,c("r","z","CO2_mol_per_m3")]),paste0(metapfad_comsol,"CO2_obs_",as_date(kammer_dates[j]),".csv"),col.names = F,row.names = F,sep=",")
# 
# }
# 

slope_0_7cm <- glm(CO2_ref ~ tiefe, data= subset(sub_j,tiefe >= -7))#ppm/cm
#slope_0_20cm <- glm(CO2_inj ~ tiefe, data= subset(CO2_obs,tiefe >= -15))#ppm/cm
#slope_0_20cm <- glm(CO2_ref ~ tiefe, data= subset(CO2_obs,tiefe >= -15))#ppm/cm
ggplot(sub_j)+geom_point(aes(CO2_ref,tiefe))
plot(sub_j$tiefe,sub_j$CO2_ref)
abline(slope_0_7cm)
dC_dz <- -slope_0_7cm$coefficients[2]
#dC_dz <- -slope_0_20cm$coefficients[2]
dC_dz#ppm/cm
#DS = -FZ * dz / dC

dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units")#mol/m^3/cm

Fz_mumol_per_s_m2 <- 5.733e-6  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2

Fz_mumol_per_s_m2
subset(Kammer_flux,day=="2020-07-14")
