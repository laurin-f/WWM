#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_tracer<- paste0(metapfad,"Tracereinspeisung/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg","dplyr")

check.packages(packages)
theme_set(theme_classic())

######################################
#data_agg
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(file=paste0(klimapfad,"klima_data.RData"))

#######################
#einheiten anpassen für COMSOl
#Tracersignal in COMSOL Einheit umrechnen
offset_method <- "drift"
######################

# load(file=paste0(comsolpfad,"F_df_gam_3DS_pos8_ext.RData"))
# 
# F_df_pos8 <- F_df
# # load(file=paste0(comsolpfad,"F_df_gam_3DS_ext2.RData"))
# # F_df_ext2<-rbind(F_df,F_df_pos8)
# load(file=paste0(comsolpfad,"F_df_gam_3DS_ext.RData"))
# F_df<-rbind(F_df,F_df_pos8)

if(offset_method == "gam"){
load(paste0(comsolpfad,"DS_anisotrop_gam.RData"))
F_df <- DS_anisotrop
}
if(offset_method == "drift"){
load(paste0(comsolpfad,"DS_anisotrop_drift.RData"))
F_df <- DS_anisotrop_drift
DS_anisotrop_long <- DS_anisotrop_long_drift
}
if(offset_method == "no_ref"){
 load(paste0(comsolpfad,"DS_anisotrop_no_ref.RData"))
  F_df <- DS_anisotrop_no_ref
}
names(F_df) <- str_replace(names(F_df),"(\\d)$","_\\1")


pos8_date <- min(data$date[which(data$Position ==8 & data$Pumpstufe != 0)])
Versuch2_date <- ymd_h("2020.07.10 00")
F_df$Versuch <- ifelse(F_df$date < pos8_date,ifelse(F_df$date < Versuch2_date,"1","2"),"3")



#R_soil[µmol/m²und s]=(0.14*SWC@20cm[vol%]-0.05)*0.66*exp(0.076*T_soil@3 cm)



soil_wide$R_soil <- (0.14*soil_wide$mean_VWC_20-0.05)*0.66*exp(0.076*soil_wide$mean_T_2)
soil_wide$R_min <- (0.14*soil_wide$VWC_min_20-0.05)*0.66*exp(0.076*soil_wide$T_min_2)
soil_wide$R_max <- (0.14*soil_wide$VWC_max_20-0.05)*0.66*exp(0.076*soil_wide$T_max_2)

#soil_wide$R_soil <- (0.14*soil_wide$mean_VWC_20+0.2)*0.66*exp(0.076*soil_wide$mean_T_2)


data$CO2_ref_mol_m3 <- ppm_to_mol(data$CO2_ref,"ppm",out_class = "units",T_C = data$T_soil,p_kPa = data$PressureActual_hPa/10)

data_wide_CO2 <- tidyr::pivot_wider(data[data$date %in% F_df$date,],date,names_from=tiefenstufe,values_from = CO2_ref_mol_m3,names_prefix = "CO2_ref_")
F_df <- merge(F_df,data_wide_CO2)


##################################################################
#anstatt glm geht es viel schneller jeweils den mittelwert von 3 
    ################
    #flux
    dC_dz_mol_10_17 <- rowMeans(cbind(F_df$CO2_ref_3 - F_df$CO2_ref_4,F_df$CO2_ref_4 - F_df$CO2_ref_5)/-3.5)
    dC_dz_mol_ab20 <- rowMeans(cbind(F_df$CO2_ref_5 - F_df$CO2_ref_6,F_df$CO2_ref_6 - F_df$CO2_ref_7)/-3.5)
#mol/m^3/cm
    #einheit in mol / m3 /cm

    #Ficks Law
    #Fz_mumol_per_s_m2 <- F_df$DS_1[k]  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
    
    F_df$Fz_10_17 <- F_df$DS_2   * dC_dz_mol_10_17 * 100 * 10^6#m2/s * mol/m3/m = mumol/s/m2
    F_df$Fz_ab20 <- F_df$DS_3   * dC_dz_mol_ab20 * 100 * 10^6#m2/s * mol/m3/m = mumol/s/m2
    

sub7u8 <- subset(data, Position %in% 7:8) 
sub7u8$tiefe_pos <- -sub7u8$tiefe 

data_wide <- tidyr::pivot_wider(sub7u8,date,names_from = tiefe_pos,values_from = c(T_soil,PressureActual_hPa))
for(i in c("T_soil_","PressureActual_hPa_")){
  data_wide[,paste0(i,1)] <- rowMeans(data_wide[,paste0(i,0:2*3.5)])
  data_wide[,paste0(i,2)] <- rowMeans(data_wide[,paste0(i,3:5*3.5)])
  data_wide[,paste0(i,3)] <- rowMeans(data_wide[,paste0(i,6:7*3.5)])
}



F_df<- merge(F_df,data_wide[,c("date",paste0("T_soil_",1:3),paste0("PressureActual_hPa_",1:3))])
for(i in 1:3){
  F_df[,paste0("D0",i)] <- D0_T_p(T_C=F_df[,paste0("T_soil_",i)],p_kPa = F_df[,paste0("PressureActual_hPa_",i)]/10,unit="m^2/s")
  F_df[,paste0("DSD0_",i)] <- F_df[,paste0("DS_",i)]/F_df[,paste0("D0",i)]
}

hours_to_steady <- 0
#Zeitraum bis steady state abschneiden 
for(i in 1:nrow(Pumpzeiten)){
  F_df[F_df$date > (round_date(Pumpzeiten$start,"hours")[i]-3600) & F_df$date < (round_date(Pumpzeiten$start,"hours")[i]+hours_to_steady*3600),c(grep("Fz|DS",colnames(F_df)))]<-NA

  DS_anisotrop_long[DS_anisotrop_long$date > (round_date(Pumpzeiten$start,"hours")[i]-3600) & DS_anisotrop_long$date < (round_date(Pumpzeiten$start,"hours")[i]+hours_to_steady*3600),"DSD0"]<-NA
}

#######
#DS_long


#moving average

rollwidth <- 2*3600
rollwidth2_h <- 12
rollwidth2 <- rollwidth2_h*3600
# rollwidth <- 120
# rollwidth2_h <- 24
# rollwidth2 <- rollwidth2_h *60

# dates <- data.frame(date=seq(min(F_df$date),max(F_df$date),by=10*60))
# F_df <- merge(F_df,dates,all=T)

a <- 1:10

##############
#weighting vector um Zeitlücken nicht im Moving average zusammen zu fügen
# wenn es das datum x -/+ rollwidth gibt wird die anzahl der Datenpunkte in diesem Zeitraum ausgegeben ansonsten 0
w <- sapply(F_df$date, function(x) ifelse((x - rollwidth/2) %in% F_df$date & (x + rollwidth/2) %in% F_df$date ,sum(F_df$date >= x - rollwidth/2 & F_df$date <= x+ rollwidth/2),0))
w2 <- sapply(F_df$date, function(x) ifelse((x - rollwidth2/2) %in% F_df$date & (x + rollwidth2/2) %in% F_df$date ,sum(F_df$date >= x - rollwidth2/2 & F_df$date <= x+ rollwidth2/2),0))

F_df$Fz_roll <- zoo::rollapply(F_df$Fz,width=w,mean,na.rm=F,fill=NA)
F_df$Fz_roll_10_17 <- zoo::rollapply(F_df$Fz_10_17,width=w,mean,na.rm=F,fill=NA)
F_df$Fz_roll_ab20 <- zoo::rollapply(F_df$Fz_ab20,width=w,mean,na.rm=F,fill=NA)
F_df$Fz_roll2 <- zoo::rollapply(F_df$Fz,width=w2,mean,na.rm=F,fill=NA)
F_df$Fz_roll2_10_17 <- zoo::rollapply(F_df$Fz_10_17,width=w2,mean,na.rm=F,fill=NA)

F_df$DS_roll_1 <- zoo::rollapply(F_df$DS_1,width=w,mean,na.rm=F,fill=NA)
F_df$DS_roll_2 <- zoo::rollapply(F_df$DS_2,width=w,mean,na.rm=F,fill=NA)
F_df$DS_roll_3 <- zoo::rollapply(F_df$DS_3,width=w,mean,na.rm=F,fill=NA)

F_df$DSD0_roll_1 <- zoo::rollapply(F_df$DSD0_1,width=w,mean,na.rm=F,fill=NA)
F_df$DSD0_roll_2 <- zoo::rollapply(F_df$DSD0_2,width=w,mean,na.rm=F,fill=NA)
F_df$DSD0_roll_3 <- zoo::rollapply(F_df$DSD0_3,width=w,mean,na.rm=F,fill=NA)
F_df$DSD0_roll2_1 <- zoo::rollapply(F_df$DSD0_1,width=w2,mean,na.rm=F,fill=NA)
F_df$DSD0_roll2_2 <- zoo::rollapply(F_df$DSD0_2,width=w2,mean,na.rm=F,fill=NA)
F_df$DSD0_roll2_3 <- zoo::rollapply(F_df$DSD0_3,width=w2,mean,na.rm=F,fill=NA)
#F_df$Fz_roll <- zoo::rollapply(F_df$Fz,width=3,mean,fill=NA)


# na_rows <- apply(F_df,1,function(x) all(is.na(x[-1])))
# F_df <- F_df[-na_rows,]
# range(F_df$Fz_roll2[F_df$Versuch=="3"],na.rm=T)
# range(F_df$DSD0_roll2_1[F_df$Versuch=="3"],na.rm=T)
# range(F_df$DSD0_roll_2[F_df$Versuch=="3"],na.rm=T)
# range(F_df$DSD0_roll_3[F_df$Versuch=="3"],na.rm=T)
# 
 range(F_df$Fz_roll2[F_df$Versuch!="1"],na.rm=T)
 range(F_df$DSD0_roll2_1[F_df$Versuch!="1"],na.rm=T)
 range(F_df$DSD0_roll2_2[F_df$Versuch!="1"],na.rm=T)
 range(F_df$DSD0_roll2_3[F_df$Versuch!="1"],na.rm=T)

DS_long <-tidyr::pivot_longer(F_df[!grepl("DS(D0)?_roll_\\d$",colnames(F_df))],matches("DS(D0)?_"),names_pattern = "(\\w+)_(\\d)",names_to = c(".value","id"))
DS_long_roll <-tidyr::pivot_longer(F_df[!grepl("DS(D0)?_(min_|max_|sorted_)?\\d$",colnames(F_df))],matches("DS(D0)?_roll"),names_pattern = "(\\w+)_(\\d)",names_to = c(".value","id"))



data$date_hour <- round_date(data$date,"hours")
data_plot2 <- data %>%
  group_by(tiefe,date_hour) %>%
  summarise_at(vars(starts_with("DSD0_")),list(mean=mean,max=max,min=min))
data_plot_mean <- data %>%
  group_by(tiefe,date_hour) %>%
  summarise_at(vars(matches("DSD0_[a-z|A-Z]*$|date")),mean)
data_plot_min <- data %>%
  group_by(tiefe,date_hour) %>%
  summarise_at(vars(matches("DSD0_[a-z|A-Z]*_min$")),min)
data_plot_max <- data %>%
  group_by(tiefe,date_hour) %>%
  summarise_at(vars(matches("DSD0_[a-z|A-Z]*_max$")),max)

data_plot2 <- merge(data_plot_mean,data_plot_max)  
data_plot2 <- merge(data_plot2,data_plot_min)  

data_plot <- data %>%
  group_by(tiefe,date_hour=round_date(data$date,"hours")) %>%
  summarise(DSD0_PTF_min = min(DSD0_PTF_min,na.rm=T),DSD0_PTF_max = max(DSD0_PTF_max),DSD0_PTF= mean(DSD0_PTF),date=mean(date))

ranges <- c("0 to -10","-10 to -20","> -20")
DS_long$tiefe <- as.numeric(DS_long$id)*-7
DS_long$range <- factor(DS_long$id,levels=1:3,labels=ranges)
DS_anisotrop_long$range <- factor(DS_anisotrop_long$tiefe,levels=1:3,labels=ranges)
DS_long_roll$range <- factor(DS_long_roll$id,levels=1:3,labels=ranges)

soil_agg$date_hour <- round_date(soil_agg$date,"hours")
soil_agg$range <- factor(soil_agg$tiefe,levels=c(2,5,10,20,50,100),labels=ranges[c(1,1,1,2,3,3)])
soil_agg_plot <- soil_agg %>%
  group_by(range,date_hour) %>%
  summarise(DSD0_PTF_min = min(DSD0_PTF_min,na.rm=T),DSD0_PTF_max = max(DSD0_PTF_max,na.rm=T),DSD0_PTF= mean(DSD0_PTF,na.rm=T),date=mean(date))


##################
#save
###################
#save(F_df,soil_agg_plot,soil_wide,DS_long_roll,DS_long,Kammer_flux,file=paste0(comsolpfad,"plotdata_Methodenpaper.RData"))


save(F_df,soil_agg_plot,soil_wide,DS_long_roll,DS_long,Kammer_flux,file=paste0(comsolpfad,"plotdata_Methodenpaper_",offset_method,".RData"))

##########################
#PLOTS             #
###########################

##################################
#plot

#F_plot
ggplot(subset(Kammer_flux))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,col=kammer),width=10000)+
  geom_point(aes(date,CO2flux,col=kammer))+
  labs(col="chamber")+
  ggnewscale::new_scale_color()+
  #geom_point(data=subset(F_df),aes(date,Fz))+
  geom_line(data=subset(F_df),aes(date,Fz,col="0-7 cm"),alpha=0.2)+
  geom_line(data=subset(F_df),aes(date,Fz_10_17,col="10-17 cm"),alpha=0.2)+
  geom_line(data=subset(F_df,date > pos8_date),aes(date,Fz_roll,col="0-7 cm"))+
  geom_line(data=subset(F_df,date > pos8_date),aes(date,Fz_roll_10_17,col="10-17 cm"))+
  geom_line(data=subset(soil_wide),aes(date,R_soil,col="R_soil"))+
  scale_color_manual("gradient method",values=1:4)+
  geom_point(data=subset(F_df,date %in% round_date(date,paste(rollwidth2_h,"hours")) & date < pos8_date),aes(date,Fz_roll2,col="0-7 cm"))+
  geom_line(data=subset(F_df,date < pos8_date) ,aes(date,Fz_roll2,col="0-7 cm"))+
  geom_point(data=subset(F_df,date %in% round_date(date,paste(rollwidth2_h,"hours"))  & date < pos8_date),aes(date,Fz_roll2_10_17,col="10-17 cm"))+
  #scale_color_manual("gradient method",values=1:3)+
  #xlim(c(min(F_df$date[-1]),max(F_df$date[])+3600*5))+
  xlim(ymd_hms(c("2020-07-06 13:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  #xlim(ymd_h(c("2020.07.06 13","2020.07.07 19")))+
  labs(y=expression(CO[2]*"flux ["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))+
  theme(legend.position = "right")+
  ggsave(paste0(plotpfad,"Flux_Kammer_Comsol_gam_3DS.png"),width=7,height = 4)
#F_df_gam <- F_df


#DS_plot
# ggplot(subset(DS_long_roll))+
#   #geom_ribbon(aes(x=date,ymin=DS_min,ymax=DS_max,fill=id),alpha=0.2)+
#   geom_line(aes(date,DSD0_roll,col=id,linetype="2 hours mov avg"))+
#   geom_line(aes(date,DSD0_roll2,col=id,linetype="12 hours mov avg"))
#   #ggsave(paste0(plotpfad,"DS_zeit_gam_3DS.png"),width=8,height = 4)


ggplot(subset(soil_agg_plot))+
  geom_ribbon(aes(x=date,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill=as.factor(range)),alpha=0.15)+
  geom_line(aes(date,DSD0_PTF,col=as.factor(range),linetype="f(eps)"))+
  geom_line(data=DS_long_roll,aes(date,DSD0_roll,col=range,linetype="in situ"))+
  geom_line(data=DS_anisotrop_long,aes(date,DSD0,col=range,linetype="anisotrop fact 1.26"))+
  labs(y=expression(D[S]/D[0]),col="depth [cm]",fill="depth [cm]",linetype="")+
  xlim(range(DS_long$date))+
  #scale_linetype_manual(values=2:1,labels=c(expression(f~(epsilon),"in situ")))+
ggsave(file=paste0(plotpfad,"DS_plot_gam_feps.png"),width=7,height = 3.5)


ggplot(subset(data_plot2))+
  geom_ribbon(aes(x=date_hour,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date_hour,DSD0_PTF,col=as.factor(tiefe)))+
  ggnewscale::new_scale_color()+
  geom_line(data=DS_long,aes(date,DS/D0_T_p(unit="m2/s"),col=as.factor(tiefe),linetype="COMSOL"))+
  xlim(range(DS_long$date))
ggplot(subset(data_plot2))+
  geom_ribbon(aes(x=date_hour,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill="PTF"),alpha=0.2)+
  geom_line(aes(date_hour,DSD0_PTF,col="PTF"))+
  
  geom_ribbon(aes(x=date_hour,ymin=DSD0_Deepoga_min,ymax=DSD0_Deepoga_max,fill="Deepoga"),alpha=0.2)+
  geom_line(aes(date_hour,DSD0_Deepoga,col="Deepoga"))+
  
  geom_ribbon(aes(x=date_hour,ymin=DSD0_Millington_min,ymax=DSD0_Millington_max,fill="Millington"),alpha=0.2)+
  geom_line(aes(date_hour,DSD0_Millington,col="Millington"))+
  geom_ribbon(aes(x=date_hour,ymin=DSD0_Buckingham_min,ymax=DSD0_Buckingham_max,fill="Buckingham"),alpha=0.2)+
  geom_line(aes(date_hour,DSD0_Buckingham,col="Buckingham"))+
  
  ggnewscale::new_scale_color()+
  geom_line(data=DS_long,aes(date,DS/D0_T_p(unit="m2/s"),col=as.factor(tiefe),linetype="COMSOL"))+
  #xlim(range(DS_long$date))+
  facet_wrap(~tiefe,scales="free")


######################################################
#1:1 plot kammer und Comsol Flux vorbereiten
Kammer_agg <- Kammer_flux %>% group_by(day) %>% summarise(kammer_min = min(CO2flux_min),kammer_max = max(CO2flux_max), kammerflux = mean(CO2flux),date =mean(date))

Kammer_agg <- subset(Kammer_agg,day%in%ymd(c("2020.07.08","2020.07.10","2020.07.14")))
Kammer_agg$Fz_mod <- NA
Kammer_agg$date_mod <- as_datetime(NA)
F_sub <- subset(F_df, !is.na(Fz_roll))
for(i in 1:nrow(Kammer_agg)){
  Kammer_agg$Fz_mod[i] <- F_sub$Fz_roll[which.min(abs(F_sub$date - Kammer_agg$date[i]))]
  Kammer_agg$date_mod[i] <- F_sub$date[which.min(abs(F_sub$date - Kammer_agg$date[i]))]
}
Kammer_agg$Fz_mod[2] <- F_sub$Fz_roll[which.min(abs(F_sub$date - (Kammer_agg$date[2]+24*3600)))]
Kammer_agg$date_mod[2] <- F_sub$date[which.min(abs(F_sub$date - (Kammer_agg$date[2]+24*3600)))]

ggplot(Kammer_agg)+
  geom_errorbar(aes(x=Fz_mod,ymin=kammer_min,ymax=kammer_max))+
  geom_abline(slope=1,intercept = 0)
ggplot(Kammer_agg)+
  geom_errorbar(aes(x=date,ymin=kammer_min,ymax=kammer_max,col="obs"))+
  geom_point(aes(date_mod,Fz_mod,col="mod"))

Kammer_sub <- subset(CO2_flux,day%in%ymd(c("2020.07.08","2020.07.10","2020.07.14")) & kammer != "D")
Kammer_sub$Fz_mod <- NA
F_sub <- subset(F_df, !is.na(Fz_roll))
for(i in 1:nrow(Kammer_sub)){
  Kammer_sub$Fz_mod[i] <- F_sub$Fz_roll[which.min(abs(F_sub$date - Kammer_sub$date[i]))]
}
f_range <- range(Kammer_sub[,c("Fz_mod","mumol_per_s_m2")])

#######################
#1:1 plot
ggplot(Kammer_sub)+
  #geom_errorbar(aes(x=Fz_mod,ymin=CO2flux_min,ymax=CO2flux_max,col=kammer))+
  geom_point(aes(x=Fz_mod,y=mumol_per_s_m2,col=kammer))+
  geom_abline(slope=1,intercept = 0)+lims(x = f_range, y = f_range)
ggplot(Kammer_sub)+
  geom_point(aes(date,mumol_per_s_m2,col=kammer))+
  geom_point(aes(date,Fz_mod,col="mod"))
ggplot(Kammer_sub)+
  geom_point(aes(date,kammerflux,col="obs"))+
  geom_errorbar(aes(x=date,ymin=kammer_min,ymax=kammer_max,col="obs"))+
  geom_point(aes(date,Fz_mod,col="mod"))

F_Comsol$day <- as_date(F_Comsol$date)
F_vergleich <- merge(F_Comsol,Kammer_agg,by="day")
ggplot(F_vergleich)+geom_point(aes(kammerflux,Fz))+geom_abline(intercept = 0,slope=1)+geom_errorbar(aes(xmin=kammer_min,xmax=kammer_max,y=Fz))+xlim(c(1,5.5))+ylim(c(1,5.5))

