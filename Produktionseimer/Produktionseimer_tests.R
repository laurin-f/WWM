
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr","units","ggforce"))

#Pfade
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
arduinopfad<-paste0(hauptpfad,"Daten/Urdaten/Arduino/")
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_prod<- paste0(hauptpfad,"Daten/Metadaten/Produktionseimer/")
plotpfad_prod <- paste0(hauptpfad,"Dokumentation/Berichte/plots/produktionseimer/")

#Pumpzeiten
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad_prod,"Injektion_zeiten.xlsx"))
Pumpzeiten$ende <- as_datetime(Pumpzeiten$ende)
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]


##############################################
#Metadata Pumpstufen flux
injection_rates <- read.csv(paste0(metapfad_prod,"injection_rates.txt"))
injection_rates$date <- ymd_hms(injection_rates$date)

datelim <- min(Pumpzeiten$start,na.rm = T)
data <- read_sampler(table.name = "sampler3","long",datelim=datelim)
data$date <- round_date(data$date,unit = "minute")
#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
sec_bis_steadystate <- rep(12,nrow(Pumpzeiten))*3600
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

ggplot()+
  geom_line(data=data,aes(date,CO2,col=as.factor(-tiefe)))+
  
  geom_rect(data=subset(Pumpzeiten,tracer==1),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="tracer"),alpha=0.2)+
  geom_vline(data=Pumpzeiten,aes(xintercept=start),alpha=0.3)+
  geom_text(data=Pumpzeiten,aes(start,max(data$CO2,na.rm=T),label=paste(prod_1,prod_2,prod_3,sep="_")),hjust = 0,vjust=0)+
  labs(y=expression(CO[2]~"[ppm]"),col="tiefe [cm]")+
  ggsave(paste0(plotpfad_prod,"CO2_zeitreihe_ges.png"),width=11,height=7)
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
flux_i <- injection_rates[grep(paste0("(T|t)iefe.?",i),injection_rates$Pumpstufe),]
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
grundflaeche <- set_units(15^2*pi,"cm^2")

inj_mol_min <- ppm_to_mol(data$inj_ml_min,"cm^3/min",out_class = "units",T_C = data$temp)
inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/A_inj
data$inj_mol_m2_s <- set_units(inj_mol_mm2_s,"mol/m^2/s")

for(i in 1:3){
inj_mol_min <- ppm_to_mol(data[,paste0("prod_",i,"_ml_min")],"cm^3/min",out_class = "units",T_C = data$temp)
inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/grundflaeche
data[,paste0("prod_",i,"_mumol_m2_s")] <- change_unit(inj_mol_mm2_s,unit_out = "micromol/m^2/s")
}
########################################
#subsets


#########################################
#calc DS with COMSOL
tracer_prod <- subset(data,tracer==1)
prod_123 <- subset(data,treat=="1_1_1"& date < min(tracer_prod$date))
prod_mean <- prod_123 %>% 
  group_by(tiefe) %>% 
  summarise(CO2 = mean(CO2,na.rm=T))
tracer_mean <- tracer_prod %>% 
  group_by(tiefe) %>% 
  summarise_at(vars(matches("CO2|inj_mol|date|temp|prod")), mean,na.rm=T)

tracer_mean$tracer <- tracer_mean$CO2 - prod_mean$CO2
tracer_mean$CO2_ref <- prod_mean$CO2
# ggplot()+
#   geom_line(data=prod_mean,aes(CO2,tiefe,col="prod"))+
#   geom_line(data=tracer_mean,aes(CO2,tiefe,col="tracer"))+
#   geom_line(data=tracer_mean,aes(tracer,tiefe,col="tracer"))

#################################################
#runcomsol
colnames(tracer_mean) <- str_replace_all(colnames(tracer_mean),c("^temp$"="T_soil","^tracer$"="CO2_tracer_gam"))

comsol_out <- run_comsol(data=tracer_mean,mod_dates = mean(tracer_prod$date),n_DS=1,modelname = "Diffusion_Sandbox_optim",z_soil_cm = 40,read_all = F,plot=T,overwrite = F)
DS <- comsol_out$DS
Fz <- comsol_out$Fz

###################################################
#
dz <- set_units(3.5/100,"m")
tracer_mean$CO2_ref_mol <- ppm_to_mol(tracer_mean$CO2_ref,"ppm",out_class = "units",T_C = tracer_mean$T_soil)
tracer_mean$dC <- set_units(NA,"mol/m^3")
tracer_mean$dC[-nrow(tracer_mean)] <- tracer_mean$CO2_ref_mol[-nrow(tracer_mean)] - tracer_mean$CO2_ref_mol[-1]

tracer_mean$Fz_mumol_per_s_m2 <- set_units(DS,"m^2/s")  * set_units(tracer_mean$dC,"micromol/m^3")/dz#m2/s * mol*10^6/m3/cm*100 = mumol/s/m2



##############################
#data_agg

ggplot(data)+geom_line(aes(date,CO2,col=as.factor(tiefe)))+facet_wrap(~ID,scales="free_x")

data_agg <- subset(data,!is.na(treat) & ID %in% c(1,4,5,6) & tracer == 0)%>%
  group_by(treat,ID,tiefe)%>%
  summarise_at(vars(matches("CO2|inj_mol|date|temp|prod_\\d_mumol")), mean,na.rm=T)

#######################################################
#calc Fz production
data_agg$Fz <- set_units(NA,"micromol/m^2/s")
data_agg$P <- 0
for(i in unique(data_agg$ID)){
  sub_i <- subset(data_agg,ID == i)
  CO2_mol <- ppm_to_mol(sub_i$CO2,"ppm",out_class = "units",T_C = sub_i$temp)
  dC <- set_units(rep(NA,nrow(sub_i)),"mol/m^3")
  dC[-nrow(sub_i)] <- CO2_mol[-nrow(sub_i)] - CO2_mol[-1]
  
  sub_i$Fz <- (set_units(DS,"m^2/s")  * set_units(dC,"micromol/m^3")/dz) #m2/s * mol*10^6/m3/cm*100 = mumol/s/m2
  sub_i$P <- c(NA,diff(sub_i$Fz))
  data_agg[data_agg$ID == i,] <- sub_i
}


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
  
  

data_agg$prod_tiefe <- ifelse(data_agg$tiefe < -20,-22,ifelse(data_agg$tiefe < -11,-15,-5)) 
data_agg2 <- data_agg %>% group_by(prod_tiefe,ID,treat) %>% 
  summarise(Fz=mean(Fz,na.rm=T),P=sum(P,na.rm=T))




ggplot(data_agg)+geom_line(aes(CO2,tiefe,col=as.factor(ID)),orientation = "y")+facet_wrap(~treat)

ggplot(data_agg)+
  geom_point(aes(P,tiefe+1,col="measurement"),pch=1)+
  geom_point(data=data_agg2,aes(P,(prod_tiefe),col="meas_mean"))+
  geom_point(data=subset(prod_df,name=="bis"),aes(P,-as.numeric(tiefe),col="theoretical \nprofile"),orientation = "y")+
  facet_wrap(~paste("treatment:",treat))#+
  guides(col = guide_legend(override.aes = list(shape=c(16,1,NA),linetype=c(rep("blank",2),"solid"))))+
  #labels(x=expression("Fz ["*"mu"*"mol m"^{-3}*s^{-1}))+
  labs(x=expression("Fz ["~mu*"mol m"^{-3}*s^{-1}*"]"),y="tiefe [cm]",col="")
dev.new()
ggplot(data_agg)+
  geom_point(aes(as.numeric(Fz),tiefe+1,col="measurement"),pch=1)+
  geom_point(data=data_agg2,aes(as.numeric(Fz),(prod_tiefe),col="meas_mean"))+
  geom_line(data=prod_df,aes(Fz,-as.numeric(tiefe),col="theoretical \nprofile"),orientation = "y")+
  facet_wrap(~paste("treatment:",treat))+
  guides(col = guide_legend(override.aes = list(shape=c(16,1,NA),linetype=c(rep("blank",2),"solid"))))+
  #labels(x=expression("Fz ["*"mu"*"mol m"^{-3}*s^{-1}))+
  labs(x=expression("Fz ["~mu*"mol m"^{-3}*s^{-1}*"]"),y="tiefe [cm]",col="")+
  ggsave(paste0(plotpfad_prod,"Produktionsprofile.png"),width=9,height=7)
#############################
#


slope_0_10cm <- glm(CO2_ref ~ tiefe, data= subset(tracer_mean,tiefe > -10))#ppm/cm
slope_10_20cm <- glm(CO2_ref ~ tiefe, data= subset(tracer_mean,tiefe < -10 & tiefe > -20))#ppm/cm
slope_ab20cm <- glm(CO2_ref ~ tiefe, data= subset(tracer_mean,tiefe < -20))#ppm/cm
#plot(obs_j$tiefe,obs_j$CO2_ref)
#abline(slope_0_7cm)
dC_dz <- rep(NA,3)
dC_dz[1] <- -slope_ab20cm$coefficients[2]
dC_dz[2] <- -slope_10_20cm$coefficients[2]
dC_dz[3] <- -slope_0_10cm$coefficients[2]

dC_dz#ppm/cm
#DS = -FZ * dz / dC

dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units",T_C = tracer_mean$T_soil[tracer_mean$tiefe %in% c(-21,-14,-7)])#mol/m^3/cm

Fz_mumol_per_s_m2 <- DS  * dC_dz_mol * 100 * 10^6#m2/s * mol*10^6/m3/cm*100 = mumol/s/m2
  
  tracer_mean[,c("tiefe","Fz_mumol_per_s_m2")]
  diff(tracer_mean$Fz_mumol_per_s_m2)
  
  produktionen <- c(mean(tracer_mean$prod_3_mumol_m2_s),mean(tracer_mean$prod_2_mumol_m2_s),mean(tracer_mean$prod_1_mumol_m2_s))
  produktionstiefen <- c(-40,-20,-10)
  prod_df <- data.frame(tiefe=produktionstiefen,Prod=produktionen,Fz=cumsum(produktionen),Fz_mod=Fz_mumol_per_s_m2,Prod_mod=c(Fz_mumol_per_s_m2[1],diff(Fz_mumol_per_s_m2)))
  
  ggplot(tracer_mean)+
    geom_path(aes(Fz_mumol_per_s_m2,tiefe+1.75))+
    geom_step(data=prod_df,aes(as.numeric(Fz),tiefe,col="meas"),direction = "vh")+
    geom_point(data=prod_df,aes(as.numeric(Fz_mod),tiefe,col="mod"))

