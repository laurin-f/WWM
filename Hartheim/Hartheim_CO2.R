#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)


###################
#metadaten

load(file=paste0(klimapfad,"klima_data.RData"))

#Pumpzeiten
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad_harth,"Tracereinspeisung_Hartheim.xlsx"))
Pumpzeiten$ende <- as_datetime(Pumpzeiten$ende)
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]

#tiefenoffset
tiefen_offset <- read.table(paste0(metapfad_harth,"sampler_tiefen_offset.txt"),header = T)

#Metadata Pumpstufen flux
flux <- read.csv(paste0(metapfad,"Tracereinspeisung/Pumpstufen_flux.txt"))



datelim <- range(c(Pumpzeiten$start,Pumpzeiten$ende),na.rm = T)
datelim <- min(c(Pumpzeiten$start,Pumpzeiten$ende),na.rm = T)

data <- read_sampler("sampler1u2",datelim = datelim, format = "long")

colnames(data) <- str_replace_all(colnames(data),c("smp1" = "inj", "smp2" = "ref"))


data <- merge(data,klima,by="date",all.x = T)
#Pumpstufe und Versuch aus metadaten auf dataframe übetragen


#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
sec_bis_steadystate <- rep(10,nrow(Pumpzeiten))*3600
sec_cut_off <- rep(0,nrow(Pumpzeiten))*3600


#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
cols2data <- c("Pumpstufe","Position")
data[,cols2data] <- NA

for(i in seq_along(Pumpzeiten$Pumpstufe)){
  #Pumpzeiten_lim <- data$date > (Pumpzeiten$start[i] + sec_bis_steadystate[i]) & if(!is.na(Pumpzeiten$ende[i])) {data$date < (Pumpzeiten$ende[i] - sec_cut_off[i])}else{ T }
  Pumpzeiten_lim <- data$date > ifelse(Pumpzeiten$Pumpstufe[i] == 0,Pumpzeiten$start[i] + sec_bis_steadystate[i],Pumpzeiten$start[i]) & if(!is.na(Pumpzeiten$ende[i])) {data$date < (Pumpzeiten$ende[i] - sec_cut_off[i])}else{ T }
  for(j in cols2data){
    data[Pumpzeiten_lim,j] <- Pumpzeiten[i,j]
  }
}




#kurven glätten mit rollapply
for(j in c("_inj","_ref")){
 data[,paste0("CO2_roll",j)] <- NA
for(i in unique(data$tiefe)){
  tiefe.i <- data$tiefe == i
  data[tiefe.i,paste0("CO2_roll",j)] <- zoo::rollapply(data[tiefe.i,paste0("CO2",j)],width=50,mean,fill=NA)
}
}

############################
#Fz mit zeitlichem drift bestimmen
flux$date <- ymd_hms(flux$date)
data$Fz <- NA
for(i in na.omit(unique(data$Pumpstufe))){
  flux_i <- subset(flux, Pumpstufe == i)
  id <- which(data$Pumpstufe == i)
  if(nrow(flux_i) > 1){
    data[id,"Fz"] <- approx(x=(flux_i$date),y=flux_i$tracer_ml_per_min,xout=(data$date[id]),rule=2)$y
  }else{
    data[id,"Fz"] <- flux_i$tracer_ml_per_min
  }
}

##########################
#soil data to data

data <- merge(data,soil_wide,by="date",all.x = T)


data <- variable_to_depths("VWC_min")
data <- variable_to_depths("VWC_max")
data <- variable_to_depths("VWC")
data <- variable_to_depths("T_min","T_soil_min")
data <- variable_to_depths("T_max","T_soil_max")
data <- variable_to_depths("T","T_soil")

data <- data[,!grepl("_\\d+$",colnames(data))]

############################
#DS PTF
DS_eps <- read.table(paste0(metapfad_harth,"DS_eps_Maier.txt"),stringsAsFactors = F,header = T)

DS_eps$top1 <- as.numeric(str_extract(DS_eps$Depth,"^\\d+"))
DS_eps$top1[1] <- 0
DS_eps$bottom1 <- as.numeric(str_extract(DS_eps$Depth,"\\d+$"))
DS_eps$bottom1[1] <- DS_eps$top1[2]
DS_eps$top <- c(0,rowMeans(cbind(DS_eps$top1[-1],DS_eps$bottom1[-nrow(DS_eps)])))


data$c_PTF <- approx(DS_eps$top,DS_eps$c,-data$tiefe,method = "constant",rule = 2)$y
data$d_PTF <- approx(DS_eps$top,DS_eps$d,-data$tiefe,method = "constant",rule = 2)$y

#sheet 3 der .xls einlesen
soil.xls<-readxl::read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 3)


#aggregieren der Horizonte
soil <- soil.xls %>% 
  group_by(Horizon) %>%
  summarise(PV=mean(PV),PV_min=min(PV),PV_max=max(PV))

#bulk density in g/cm3
 soil$tiefe <- c(Ah1=0 , Ah2=10 , AhC=20, C=40)

 
data <- data[,!grepl("(VWC|T_soil)_(min|max)?_?\\d+",colnames(data))]

data$PV <- approx(soil$tiefe,soil$PV,-data$tiefe,method = "constant",rule = 2)$y
data$PV_min <- approx(soil$tiefe,soil$PV_min,-data$tiefe,method = "constant",rule = 2)$y
data$PV_max <- approx(soil$tiefe,soil$PV_max,-data$tiefe,method = "constant",rule = 2)$y
data$eps <- (data$PV - data$VWC)/100
data$eps_min <- (data$PV_min - data$VWC_max)/100
data$eps_max <- (data$PV_max - data$VWC_min)/100
data$DSD0_PTF <- data$c_PTF * data$eps^data$d_PTF
data$DSD0_PTF_min <- data$c_PTF * data$eps_min^data$d_PTF
data$DSD0_PTF_max <- data$c_PTF * data$eps_max^data$d_PTF


#F = -DS * dC/Dz
data_wide <- tidyr::pivot_wider(data,id_cols=date,names_from = tiefe,values_from = grep("CO2|DSD0_PTF|T_soil|PressureActual",colnames(data)))
data_wide <- as.data.frame(data_wide)
tiefen <- 0:7*-3.5

for(j in c("min_","max_","")){
for(i in 2:length(tiefen)){
dC_ppm <- data_wide[,paste0("CO2_ref_",tiefen[i])]-data_wide[,paste0("CO2_ref_",tiefen[i-1])]
dz_m <- 3.5/100


DSD0 <- (data_wide[,paste0("DSD0_PTF_",j,tiefen[i])] + data_wide[,paste0("DSD0_PTF_",j,tiefen[i-1])])/2
#DSD0 <- 1/((1/data_wide[,paste0("DSD0_PTF_",j,tiefen[i])] + 1/data_wide[,paste0("DSD0_PTF_",j,tiefen[i-1])])/2)#harmonic mean

T_soil <- (data_wide[,paste0("T_soil_",j,tiefen[i])] + data_wide[,paste0("T_soil_",j,tiefen[i-1])]) /2
p_hPa <- data_wide[,paste0("PressureActual_hPa_",tiefen[i])]

dC_mol_m3 <- ppm_to_mol(dC_ppm,unit_in = "ppm",T_C = T_soil,p_kPa = p_hPa/10)
dC_mumol_m3 <- dC_mol_m3*10^6

#D0_cm2_s <- D0_T_p(T_soil,p_hPa/10)
D0_m2_s <- D0_T_p(T_soil,p_hPa/10,"m^2/s")

  j2 <- j
if(j == "") j2 <- "mean_"

data_wide[,paste0("F_z_",j2,paste(c(-tiefen[i-1],-tiefen[i]),collapse="-"))] <- DSD0 * D0_m2_s * dC_mumol_m3/dz_m #m2/s * mumol/m^3 /m = mumol/s/m2
}
}



F_z_wide <- data_wide[,grep("date|F_z",colnames(data_wide))]

F_PTF <- tidyr::pivot_longer(F_z_wide,grep("F_z",colnames(F_z_wide)),names_pattern = "F_z_(min|max|mean)_(\\d+\\.?\\d*-\\d+\\.?\\d*)",names_to = c(".value","tiefe"))


F_PTF_agg <- F_PTF %>%
  group_by(round_date(date,"hour"),tiefe) %>%
  summarise(date = mean(date),min=min(min),max=max(max),mean=mean(mean))

###############
#tiefen Offset


data$hour <- hour(data$date) 
data$date_int <- as.numeric(data$date)


data_PSt0 <- lapply(na.omit(unique(data$Position)),function(x) subset(data, Pumpstufe == 0 & Position == x))

for(i in seq_along(data_PSt0)){
  data_PSt0[[i]]$offset <-  data_PSt0[[i]]$CO2_inj - data_PSt0[[i]]$CO2_ref
}
data_kal <- lapply(data_PSt0, function(x) aggregate(x[,grep("CO2|offset",colnames(x))] ,list(tiefe = x$tiefe), mean, na.rm=T))


data$offset <- NA
for(i in seq_along(data_kal)){
  pos <- na.omit(unique(data$Position))[i]
  posID <- which(data$Position == pos)
  data$offset[posID] <- as.numeric(as.character(factor(data$tiefe[posID], levels=data_kal[[i]]$tiefe,labels=data_kal[[i]]$offset)))
}
##################
#mit glm oder gam
data$preds <- NA
data$preds2 <- NA
data$preds_drift <- NA
data$preds_drift2 <- NA
glmgam <- T
if(glmgam == T){

for(j in seq_along(data_PSt0)[-c(3,5)]){
for(i in (1:7)*-3.5){
  #fm <- glm(CO2_roll_inj ~ CO2_roll_ref + hour + CO2_roll_ref * hour,data=subset(data_PSt0,tiefe==i))
  fm <- glm(CO2_roll_inj ~ CO2_roll_ref,data=subset(data_PSt0[[j]],tiefe==i))
  fm_drift <- glm(offset ~ poly(date_int,2),data=subset(data_PSt0[[j]],tiefe==i))
  
  fm2 <- mgcv::gam(CO2_roll_inj ~ s(CO2_roll_ref) + s(hour),data=subset(data_PSt0[[j]],tiefe==i))
  
  pos <- na.omit(unique(data$Position))[j]
  ID <- which(data$tiefe==i & data$Position == pos& !is.na(data$CO2_ref))
  
  data$preds[ID] <- predict(fm,newdata = data[ID,])
  data$preds_drift[ID] <- predict(fm_drift,newdata = data[ID,])
  data$preds2[ID] <- predict(fm2,newdata = data[ID,])
  }
}
}


########################

#data$CO2_tracer <- data$CO2_roll_inj - (data$CO2_roll_ref + data$offset)
data$CO2_tracer <- data$CO2_inj - (data$CO2_ref + data$offset)
data$CO2_tracer_glm <- data$CO2_inj - (data$preds)
data$CO2_tracer_gam <- data$CO2_inj - (data$preds2)
data$CO2_tracer_drift <- data$CO2_inj - (data$CO2_ref + data$preds_drift)

data$tracer_pos <- data$CO2_tracer > 0
data$CO2_ref_offst <- ifelse(data$tracer_pos, data$CO2_ref + data$offset, data$CO2_inj)

#data$CO2_tracer[data$CO2_tracer < 0 | data$Pumpstufe == 0| is.na(data$Pumpstufe)] <- NA


########################
#
#injektionsrate in mol /m2 /s
A_inj <- set_units(1^2*pi,"mm^2")

inj_mol_min <- ppm_to_mol(round(data$Fz,6),"cm^3/min",out_class = "units",p_kPa = data$PressureActual_hPa/10,T_C = data$Ta_2m)
inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/A_inj
data$inj_mol_m2_s <- set_units(inj_mol_mm2_s,"mol/m^2/s")

##############
#data_wide
data_wide <- tidyr::pivot_wider(data, id_cols = date, names_from = tiefenstufe,values_from = c(CO2_inj,CO2_ref,CO2_tracer,Fz),names_prefix = "tiefe")

data_wide <- data_wide[,-grep("CO2_(inj|tracer)_tiefe0|Fz_tiefe[1-7]",colnames(data_wide))]
colnames(data_wide) <- str_replace(colnames(data_wide),"Fz_tiefe0","injection_ml_per_min")



######################
#data_agg

#data_agg <- aggregate(data[grep("date|CO2|Fz|Pumpstufe|offset|Ta_2m|Pressure|DS",colnames(data))],list(hour=round_date(data$date,"hours"),tiefe=data$tiefe),mean,na.rm=T)
#data_agg$date <- with_tz(data_agg$date,"UTC")
data_agg <- data[grep("date|CO2|Fz|Pumpstufe|offset|Ta_2m|Pressure|DS",colnames(data))] %>%
  group_by(hour=round_date(data$date,"hours"),tiefe=data$tiefe) %>%
  summarise_all(mean,na.rm=T)
  
data_agg <- subset(data_agg, Pumpstufe != 0)
save(data,data_agg,Pumpzeiten,file=paste0(samplerpfad,"Hartheim_CO2.RData"))



###########
#export data


paste("tiefe",rev(unique(data$tiefenstufe)),"=",rev(unique(data$tiefe)),"cm",collapse = ", ")

#ggplot(data_wide)+geom_line(aes(date,injection_ml_per_min))

write.csv(data_wide,file=paste0(datapfad_harth,"co2_profil_",paste(format(range(data_wide$date),"%j"),collapse = "-"),".txt"),row.names = F)

