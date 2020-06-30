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
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units")
check.packages(packages)

#dataset laden
pars_fs <- read.table((paste0(metapfad,"COMSOL/parameter_freeSoil.csv")),sep=";",stringsAsFactors = F)

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(file=paste0(klimapfad,"klima_data.RData"))

z_soil_ch <- str_split(pars_fs[pars_fs[,1] == "z_soil",2],"\\s",simplify = T)
unit <- str_remove_all(z_soil_ch[,2],"\\[|\\]")

z_soil <- set_units(as.numeric(z_soil_ch[,1]),unit,mode="standard")
z_soil_cm <- as.numeric(set_units(z_soil,"cm"))

meas_depths_fs <- (z_soil_cm-(0:7*3.5))
meas_points_fs <- data.frame(R=0,Z=meas_depths_fs)

write.table(meas_points_fs,file = paste0(metapfad,"COMSOL/meas_points_freeSoil.txt"),row.names = F,col.names = F)

data$t_mins <- as.numeric(difftime(data$date,min(data$date[data$Pumpstufe == 1.5],na.rm=T)-(10*3600),units = "secs"))
#data$t_mins <- as.numeric(difftime(data$date,min(data$date[data$Pumpstufe == 1.5],na.rm=T),units = "secs"))
data$CO2_mol_per_m3 <- ppm_to_mol(data$CO2_tracer,"ppm")
data$CO2_mol_per_m3[is.na(data$CO2_mol_per_m3)]<- 0
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0

A_inj <- set_units(1^2*pi,"mm^2")

inj_mol_min <- ppm_to_mol(round(data$Fz,6),"cm^3/min",out_class = "units")
inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/A_inj
data$inj_mol_m2_s <- set_units(inj_mol_mm2_s,"mol/m^2/s")

data_sub <- subset(data,t_mins >= 0 & t_mins < 3600*24)
data_sub$inj_mol_m2_s[is.na(data_sub$inj_mol_m2_s)] <- data_sub$inj_mol_m2_s[!is.na(data_sub$inj_mol_m2_s)][1]
plot(data_sub$t_mins,data_sub$CO2_mol_per_m3)
any(is.na(data_sub[,c("date","t_mins","inj_mol_m2_s","CO2_mol_per_m3")]))
write.table(data_sub[data_sub$tiefe == 0,c("t_mins","inj_mol_m2_s")],paste0(metapfad_comsol,"td_inj.csv"),col.names = F,row.names = F,sep=",")
for(i in 1:7){
  write.table(data_sub[data_sub$tiefe == (1:7*-3.5)[i],c("t_mins","CO2_mol_per_m3")],paste0(metapfad_comsol,"td_dom",i,".csv"),col.names = F,row.names = F,sep=",")
} 
##############
#DS Hartheim

# Hartheim 25–30 cm 1091 0.91 0.29 0.80 1.69 42 0.51
# Hartheim 50–55 cm mean ka=220 μm² 1.92 2.64 48 0.85
# Hartheim 70–75 cm mean ka=734 μm² 2.14 2.43 36 0.41
# Hartheim 95–100 cm 4515 1.35 0.24 1.24 2.08
DS_eps <- read.table(paste0(metapfad_harth,"DS_eps_Maier.txt"),stringsAsFactors = F,header = T)

DS_eps$top1 <- as.numeric(str_extract(DS_eps$Depth,"^\\d+"))
DS_eps$top1[1] <- 0
DS_eps$bottom1 <- as.numeric(str_extract(DS_eps$Depth,"\\d+$"))
DS_eps$bottom1[1] <- DS_eps$top1[2]


DS_eps$top <- c(0,rowMeans(cbind(DS_eps$top1[-1],DS_eps$bottom1[-nrow(DS_eps)])))
DS_eps$bottom <- c(DS_eps$top[-1],z_soil_cm)
DS_eps$tiefe <- rowMeans(DS_eps[,c("top","bottom")])

DS_eps$z <- z_soil_cm - DS_eps$bottom
DS_eps$z_diff <- DS_eps$bottom - DS_eps$top


soil_VWC <- subset(soil_long,unit=="VWC")
soil_T <- subset(soil_long,unit=="T")
VWC <- aggregate(list(VWC=soil_VWC$value),by=list(date=soil_VWC$date,tiefe= soil_VWC$tiefe),mean)
T_agg <- aggregate(list(T_C=soil_T$value),by=list(date=soil_T$date,tiefe= soil_T$tiefe),mean)


kammer_date <- ymd_h("2020-06-09 11")


#ggplot(subset(soil_VWC,unit=="VWC"))+geom_line(aes(date,value,col=as.factor(tiefe),linetype=plot))
#ggplot(VWC)+geom_line(aes(date,VWC,col=as.factor(tiefe)))
maxdate <- max(VWC$date)
VWC_sub <- subset(VWC,date==maxdate)
T_sub <- subset(T_agg,date==maxdate)

#ggplot(VWC_sub)+geom_path(aes(VWC,-tiefe))
DS_eps$VWC <- approx(VWC_sub$tiefe,VWC_sub$VWC,DS_eps$tiefe,rule=2)$y
DS_eps$T_C <- approx(T_sub$tiefe,T_sub$T_C,DS_eps$tiefe,rule=2)$y
ggplot()+
  geom_path(data = VWC_sub,aes(VWC,-tiefe,col="original"))+
  geom_point(data = DS_eps,aes(VWC,-tiefe,col="approx"))



#sheet 3 der .xls einlesen
soil.xls<-readxl::read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 3)

#aggregieren der Horizonte
soil<-aggregate(soil.xls[,4:41],list(Horizon=soil.xls$Horizon),function(x) mean(x,na.rm = T))
#bulk density in g/cm3
soil$tiefe <- c(Ah1=0 , Ah2=10 , AhC=20, C=40)

soil[,c("Horizon","PV","tiefe")]
DS_eps$PV <- approx(soil$tiefe,soil$PV,DS_eps$tiefe,rule=2,method = "constant")$y
DS_eps[,c("tiefe","PV")]
DS_eps$eps <- (DS_eps$PV - DS_eps$VWC)/100

DS_eps$DSD0 <- DS_eps$c * DS_eps$eps^DS_eps$d



DS_eps$DS_cm2_s <- DS_eps$DSD0 * D0_T_p(DS_eps$T_C)
DS_eps$DS_m2_s <- DS_eps$DS_cm2_s / 10^4
DS_eps$DS_m2_s
###############################
#COMSOL input parameter sweep
#data_agg

A_inj <- set_units(1^2*pi,"mm^2")
injection_ml_min <- na.omit(unique(round(data_agg$Fz,2)))
injection_ml_min <- unique(CO2_obs$Fz)



injection_rates <- ppm_to_mol(injection_ml_min,unit_in = "cm^3/min",out_class = "units")
inj_mol_m2_s <- sort(set_units(injection_rates,"mol/s")/set_units(A_inj,"m^2"))


CO2_atm <- 0

min_DS <- c(2e-6,2e-6,6e-7,6e-7)
max_DS <- c(5e-6,5e-6,15e-7,15e-7)
#step <- c(1e-7,1e-7,1e-7)
step <- (max_DS - min_DS) / 10
schichten <- length(min_DS)
DS_1bis8 <- matrix(paste0("DS_",1:schichten," range(",min_DS,",",step,",",max_DS,")"),schichten,1)

pars_Hartheim <- rbind(paste0("injection_rate ",paste(inj_mol_m2_s,collapse = ", ")),paste0("CO2_atm ",paste(CO2_atm,collapse=", ")),DS_1bis8)

write.table(pars_Hartheim,
            file = paste0(metapfad_comsol,"parameter_Hartheim.txt"),
            row.names = F,col.names = F,quote=F)



