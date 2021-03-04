
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr","units","ggforce"))

#Pfade
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
arduinopfad<-paste0(hauptpfad,"Daten/Urdaten/Arduino/")
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_prod<- paste0(hauptpfad,"Daten/Metadaten/Produktionseimer/")
aufbereitetpfad_prod<- paste0(hauptpfad,"Daten/aufbereiteteDaten/Produktionseimer/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad_prod <- paste0(hauptpfad,"Dokumentation/Berichte/plots/produktionseimer/")

#Pumpzeiten
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad_prod,"Injektion_zeiten.xlsx"))
Pumpzeiten$ende <- as_datetime(Pumpzeiten$ende)
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]


##############################################
#Metadata Pumpstufen flux
injection_rates <- read.csv(paste0(metapfad_prod,"injection_rates.txt"))
injection_rates$date <- ymd_hms(injection_rates$date)

datelim <- c(min(Pumpzeiten$start,na.rm = T),max(Pumpzeiten$ende))

data <- read_sampler(table.name = "sampler3","long",datelim=datelim,korrektur_dyn=T)


data$date <- round_date(data$date,unit = "minute")
#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
sec_bis_steadystate <- rep(10,nrow(Pumpzeiten))*3600
sec_cut_off <- rep(0,nrow(Pumpzeiten))*3600

###############################################################
#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
cols2data <- c("ID","tracer",paste0("prod_",1:3))
data[,cols2data] <- NA

for(i in 1:nrow(Pumpzeiten)){
  #Pumpzeiten_lim <- data$date > (Pumpzeiten$start[i] + sec_bis_steadystate[i]) & if(!is.na(Pumpzeiten$ende[i])) {data$date < (Pumpzeiten$ende[i] - sec_cut_off[i])}else{ T }
  Pumpzeiten_lim <- data$date > Pumpzeiten$start[i] + sec_bis_steadystate[i] & if(!is.na(Pumpzeiten$ende[i])) {data$date < (Pumpzeiten$ende[i] - sec_cut_off[i])}else{ T }
  for(j in cols2data){
    data[Pumpzeiten_lim,j] <- Pumpzeiten[i,j]
  }
}
data[,cols2data] <- sapply(data[,cols2data],as.numeric)

data$treat <- apply(data[,cols2data[-(1:2)]],1,paste,sep="_",collapse="_")
# 

############################
#Fz mit zeitlichem drift bestimmen

data[,c("inj_ml_min",paste0("prod_",1:3,"_ml_min"))] <- 0

flux <- subset(injection_rates, Pumpstufe == "tracer")
id <- which(data$tracer == 1)
if(nrow(flux) > 1){
  data[id,"inj_ml_min"] <- approx(x=(flux$date),y=flux$tracer_ml_per_min,xout=(data$date[id]),rule=2)$y
}else{
  data[id,"inj_ml_min"] <- flux$tracer_ml_per_min
}
for(i in 1:3){
flux_i <- injection_rates[grep(paste0("(T|t)iefe.{0,2}",i),injection_rates$Pumpstufe),]
id <- which(data[,paste0("prod_",i)] == 1)
if(nrow(flux_i) > 1){
  data[id,paste0("prod_",i,"_ml_min")] <- approx(x=(flux_i$date),y=flux_i$ml_per_min,xout=(data$date[id]),rule=2)$y
}else{
  data[id,paste0("prod_",i,"_ml_min")] <- flux_i$ml_per_min
}
}

########################
#
#injektionsrate in mol /m2 /s
A_inj <- set_units(1^2*pi,"mm^2")

#grundflaeche <- set_units((15:13)^2*pi,"cm^2")
grundflaeche <- set_units((15)^2*pi,"cm^2")

inj_mol_min <- ppm_to_mol(data$inj_ml_min,"cm^3/min",out_class = "units",T_C = data$temp)
inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/A_inj
data$inj_mol_m2_s <- set_units(inj_mol_mm2_s,"mol/m^2/s")

for(i in 1:3){
prod_mol_min <- ppm_to_mol(data[,paste0("prod_",i,"_ml_min")],"cm^3/min",out_class = "units",T_C = data$temp)
prod_mol_mm2_s <- set_units(prod_mol_min,"mol/s")/grundflaeche#[i]
data[,paste0("prod_",i,"_mumol_m2_s")] <- change_unit(prod_mol_mm2_s,unit_out = "micromol/m^2/s")
}
########################################
#save Data

save(data,file=paste0(aufbereitetpfad_prod,"data_prod_eimer.RData"))
load(file=paste0(aufbereitetpfad_prod,"Comsol_out.RData"))

DSD0 <- 0.26

##############################
#data_agg
# colnames(data)


data_agg <- subset(data,!is.na(treat) & !ID %in% c(2:3) & tracer == 0)%>%
  group_by(treat,ID,tiefe)%>%
  summarise_at(vars(matches("CO2|inj_mol|date|temp|prod_\\d_mumol")), mean,na.rm=T)

#######################################################
#calc Fz production
dz <- set_units(3.5/100,"m")

data_agg$CO2_mol_per_m3 <- ppm_to_mol(data_agg$CO2,"ppm",T_C = data_agg$temp)
data_agg$Fz <- set_units(NA,"micromol/m^2/s")
data_agg$Fz_glm <- set_units(NA,"micromol/m^2/s")
data_agg$P <- 0
for(i in unique(data_agg$ID)){
  sub_i <- subset(data_agg,ID == i)
  CO2_mol <- ppm_to_mol(sub_i$CO2,"ppm",out_class = "units",T_C = sub_i$temp)
  dC <- set_units(rep(NA,nrow(sub_i)),"mol/m^3")
  dC[-nrow(sub_i)] <- CO2_mol[-nrow(sub_i)] - CO2_mol[-1]
  
  DS_i <- set_units(DSD0*D0_T_p(sub_i$temp,unit = "m^2/s"),"m^2/s")
  sub_i$Fz <- DS_i  * set_units(dC,"micromol/m^3")/dz #m2/s * mol*10^6/m3/cm*100 = mumol/s/m2
  sub_i$P <- as.numeric(c(sub_i$Fz[1],diff(sub_i$Fz)))
  data_agg[data_agg$ID == i,] <- sub_i
}

####################
#prod_df
data_agg$Fz_30_30cm <- 0
data_agg$Fz_20_30cm <- data_agg$prod_3_mumol_m2_s 
data_agg$Fz_10_20cm <- data_agg$prod_3_mumol_m2_s +  data_agg$prod_2_mumol_m2_s
data_agg$Fz_0_10cm <- data_agg$prod_3_mumol_m2_s +  data_agg$prod_2_mumol_m2_s + data_agg$prod_1_mumol_m2_s
data_agg$P_20_30cm <- data_agg$prod_3_mumol_m2_s 
data_agg$P_10_20cm <- data_agg$prod_2_mumol_m2_s
data_agg$P_0_10cm <- data_agg$prod_1_mumol_m2_s

prod_df <- subset(data_agg,tiefe == -24.5) %>%
  select(matches("Fz_|ID|treat|P_")) %>% 
  tidyr::pivot_longer(matches("(Fz|P)_"),names_pattern="(Fz|P)_(\\d+)_(\\d+)cm",names_to=c(".value","von","bis")) %>% 
  tidyr::pivot_longer(c("von","bis"),values_to="tiefe")

#############
#data_agg2 jeweils nur ein Wert Pro prod_tiefe
data_agg$prod_tiefe <- ifelse(data_agg$tiefe < -20,-21,ifelse(data_agg$tiefe < -11,-14,-7)) 

data_agg2 <- data_agg %>% group_by(prod_tiefe,ID,treat) %>% 
  summarise(Fz=mean(Fz,na.rm=T),P=sum(P,na.rm=T))

data_tot <- data_agg %>% group_by(ID,treat) %>% 
  summarise(P=sum(P,na.rm=T))


prod_tot <- prod_df %>% 
  filter(name=="von") %>% 
  group_by(treat,ID) %>% 
  summarise(P=sum(P,na.rm=T))
  
######################

#save(data_agg,data_agg2,prod_df,file=paste0(aufbereitetpfad_prod,"data_agg_new_cal.RData"))
#wsave(data_agg,data_agg2,prod_df,file=paste0(aufbereitetpfad_prod,"data_agg.RData"))

###############################################
#       PLOTS                                 #
#                                             #
###############################################

################
#timeline
ggplot()+
  geom_line(data=data,aes(date,CO2,col=as.factor(-tiefe)))+
  
  geom_rect(data=subset(Pumpzeiten,tracer==1),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="tracer"),alpha=0.2)+
  geom_vline(data=Pumpzeiten,aes(xintercept=start),alpha=0.3)+
  geom_text(data=Pumpzeiten,aes(start,max(data$CO2,na.rm=T),label=paste(prod_1,prod_2,prod_3,sep="_")),hjust = 0,vjust=0)+
  labs(y=expression(CO[2]~"[ppm]"),col="tiefe [cm]")+
  ggsave(paste0(plotpfad_prod,"CO2_zeitreihe_ges.png"),width=11,height=7)

###############
#produktion timeline
ggplot(data)+
  geom_line(aes(date,prod_1_ml_min,col="prod_1"))+
  geom_line(aes(date,prod_2_ml_min,col="prod_2"))+
  geom_line(aes(date,prod_3_ml_min,col="prod_3"))

########################
#timelines einzelmessungen

ggplot(subset(data,!is.na(ID)))+geom_line(aes(date,CO2,col=as.factor(tiefe)))+facet_wrap(~ID,scales="free")+
  ggsave(paste0(plotpfad_prod,"CO2_einzelmessungen_new_cal.png"),width=11,height=7)

###############
#total Produktion
# ggplot()+
#   geom_point(data=prod_tot,aes(treat,P,col="theory"))+
#   geom_point(data=data_tot,aes(treat,P,col="meas"))

###########
#CO2 tiefenprofile
ggplot(data_agg)+geom_line(aes(CO2,tiefe,col=as.factor(ID)),orientation = "y")+facet_wrap(~treat)

########################
#Flux tiefenprofile
ggplot(data_agg)+
  #geom_vline(xintercept = 0)+
  geom_point(aes(as.numeric(Fz),tiefe+1.75,col=as.factor(ID)),pch=1)+
  geom_point(data=data_agg2,aes(as.numeric(Fz),(prod_tiefe),col=as.factor(ID)))+
  geom_line(data=prod_df,aes(Fz,-as.numeric(tiefe),col=as.factor(ID)),orientation = "y")+
  facet_wrap(~paste("treatment:",treat))+
  #guides(col = guide_legend(override.aes = list(shape=c(16,1,NA),linetype=c(rep("blank",2),"solid"))))+
  #labels(x=expression("Fz ["*"mu"*"mol m"^{-3}*s^{-1}))+
  labs(x=expression(F[CO2]~"["~mu*"mol m"^{-2}*s^{-1}*"]"),y="tiefe [cm]",col="")+
  ggsave(paste0(plotpfad_prod,"Flux_profile_1D_new_korr_fac.png"),width=9,height=7)

########################
#Prod Tiefenprofile
ggplot(data_agg)+
  geom_vline(xintercept = 0)+
  geom_point(aes(P,tiefe+1.75,col="measurement"),pch=1)+
  geom_point(data=data_agg2,aes(P,(prod_tiefe),col="meas_mean"))+
  geom_point(data=subset(prod_df,name=="bis"),aes(P,-as.numeric(tiefe),col="theoretical \nprofile"),orientation = "y")+
  facet_wrap(~paste("treatment:",treat))+
  guides(col = guide_legend(override.aes = list(shape=c(16,1,NA),linetype=c(rep("blank",2),"solid"))))+
  #labels(x=expression("Fz ["*"mu"*"mol m"^{-3}*s^{-1}))+
  labs(x=expression("P ["~mu*"mol m"^{-3}*s^{-1}*"]"),y="tiefe [cm]",col="")
#############################


