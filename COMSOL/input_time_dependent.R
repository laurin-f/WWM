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



data$t_mins <- as.numeric(difftime(data$date,min(data$date[data$Pumpstufe == 1.5],na.rm=T),units = "mins"))

offset_method <- "drift"
data$CO2_mol_per_m3 <- ppm_to_mol(data[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$T_soil)

data$CO2_mol_per_m3[data$tiefe == 0]<- 0
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0

A_inj <- set_units(1^2*pi,"mm^2")

inj_mol_min <- ppm_to_mol(round(data$Fz,6),"cm^3/min",out_class = "units",p_kPa = data$PressureActual_hPa/10,T_C = data$Ta_2m)
inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/A_inj
data$inj_mol_m2_s <- as.numeric(set_units(inj_mol_mm2_s,"mol/m^2/s"))

data_sub <- subset(data,t_mins >= 0 & t_mins <= 60*24)
data_sub$inj_mol_m2_s[is.na(data_sub$inj_mol_m2_s)] <- data_sub$inj_mol_m2_s[!is.na(data_sub$inj_mol_m2_s)][1]

ggplot(data_sub)+geom_line(aes(t_mins,CO2_mol_per_m3,col=as.factor(tiefe)))
ggplot(data_sub)+geom_line(aes(CO2_mol_per_m3,tiefe,col=as.factor(t_mins)),orientation = "y")+guides(col=F)
any(is.na(data_sub[,c("date","t_mins","inj_mol_m2_s","CO2_mol_per_m3")]))

write.table(data_sub[data_sub$tiefe == 0,c("t_mins","inj_mol_m2_s")],paste0(metapfad_comsol,"td_inj.csv"),col.names = F,row.names = F,sep=",")
for(i in 1:7){
  write.table(data_sub[data_sub$tiefe == (1:7*-3.5)[i],c("t_mins","CO2_mol_per_m3")],paste0(metapfad_comsol,"td_dom",i,".csv"),col.names = F,row.names = F,sep=",")
} 


CO2_mod_raw <- read.csv(paste0(comsolpfad,"Probe_table_td",".txt"),skip=5,sep="",header=F)
colnames_CO2_raw <- readLines(paste0(comsolpfad,"Probe_table_td",".txt"),n=5)
colnames_CO2 <- str_extract_all(colnames_CO2_raw[5],"Time|DS_\\d|Probe \\d",simplify = T)
colnames(CO2_mod_raw) <- str_replace_all(colnames_CO2,c("Probe "="CO2mod_tiefenstufe","Time"="t_mins"))


DS_mod <- read.csv(paste0(comsolpfad,"Objective_table_td",".txt"),skip=5,sep="",header=F)
colnames_DS <- read.csv(paste0(comsolpfad,"Objective_table_td",".txt"),skip=4,nrows =1,sep="",header=F,stringsAsFactors = F)
colnames(DS_mod) <- str_remove(colnames_DS[-1],"comp1.")
best_DS <- tail(DS_mod[grep("DS",colnames(DS_mod))],1)

CO2_mod <- tidyr::pivot_longer(CO2_mod_raw,-1,names_prefix = "CO2mod_tiefenstufe",values_to = "CO2_mod_mol_m3",names_to = "tiefenstufe")
range(data_sub$t_mins)
obs_mod <- merge(data_sub,CO2_mod,all=T)
ggplot()+
  geom_line(data=CO2_mod,aes(t_mins,CO2_mod_mol_m3,col=as.factor(tiefenstufe)))+
  geom_line(data=data_sub,aes(t_mins,CO2_mol_per_m3,col=as.factor(tiefenstufe)))
