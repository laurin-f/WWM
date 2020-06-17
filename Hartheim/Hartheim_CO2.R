#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units")
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



datelim <- c("2020.05.18 10:00:00")
smp1 <- read_sampler("sampler1",datelim = datelim, format = "long")
smp2 <- read_sampler("sampler2",datelim = datelim, format = "long")


#smp1_plt <- ggplot(smp1)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

#T_plt <- ggplot(smp1)+geom_line(aes(date,T_C))
#egg::ggarrange(smp1_plt,T_plt,heights=c(4,1))
#smp1_plt+geom_vline(xintercept = Pumpstufen$start)
#ggplot(smp2)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

data <- merge(smp1,smp2,by=c("date","tiefe","tiefenstufe","variable"),all=T,suffixes = c("_inj","_ref"))

data <- merge(data,klima,by="date",all.x = T)

#Pumpstufe und Versuch aus metadaten auf dataframe übetragen


#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
stunden_bis_steadystate <- rep(10,nrow(Pumpzeiten))
stunden_cut_off <- rep(0,nrow(Pumpzeiten))


#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
cols2data <- c("Pumpstufe")
data[,cols2data] <- NA

for(i in seq_along(Pumpzeiten$Pumpstufe)){
  Pumpzeiten_lim <- data$date > (Pumpzeiten$start[i] + stunden_bis_steadystate[i] * 60 * 60) & if(!is.na(Pumpzeiten$ende[i])) {data$date < (Pumpzeiten$ende[i] - stunden_cut_off[i] * 60 * 60)}else{ T }
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
soil_agg_long <- aggregate(list(value=soil_long$value),by=list(date=soil_long$date,tiefe= -soil_long$tiefe,unit= soil_long$unit),mean)
soil_agg <- tidyr::pivot_wider(soil_agg_long, names_from = unit,values_from = value)
colnames(soil_agg) <- str_replace(colnames(soil_agg),"^T$","T_C")
soil_wide <- tidyr::pivot_wider(soil_agg, names_from = tiefe, values_from = c(T_C,VWC))

data <- merge(data,soil_wide,by="date",all.x = T)
data <- data[,-grep("VWC_-2$",colnames(data))]



data <- variable_to_depths("VWC")
data <- variable_to_depths("T_C","T_soil")

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
soil<-aggregate(soil.xls[,4:41],list(Horizon=soil.xls$Horizon),function(x) mean(x,na.rm = T))
#bulk density in g/cm3
soil$tiefe <- c(Ah1=0 , Ah2=10 , AhC=20, C=40)

data$PV <- approx(soil$tiefe,soil$PV,-data$tiefe,method = "constant",rule = 2)$y
data$eps <- (data$PV - data$VWC)/100
data$DSD0_PTF <- data$c_PTF * data$eps^data$d_PTF

#ggplot(data)+geom_line(aes(date,DSD0_PTF,col=as.factor(tiefe)))



# ggplot()+
#   geom_line(data=data,aes(date,VWC,col=as.factor(tiefe)))+
#   geom_point(data=soil_agg,aes(date,VWC,col=as.factor(tiefe)))

###############
#tiefen Offset


data$hour <- hour(data$date)
data_PSt0 <- subset(data, Pumpstufe == 0)


data$tiefe_inj <- data$tiefe +tiefen_offset$offset[1]
data$tiefe_ref <- data$tiefe +tiefen_offset$offset[2] - 3.5


# ggplot(data)+
#   geom_line(aes(date,CO2_ref_adj,col=as.factor(tiefe_inj)))+
#   geom_line(aes(date,CO2_ref,col=as.factor(tiefe_ref)))
# ggplot(data)+
#   geom_line(aes(date,CO2_ref_adj-CO2_ref,col=as.factor(tiefe_inj)))
##################
#mit glm oder gam
for(i in (1:7)*-3.5){
  #fm <- glm(CO2_roll_inj ~ CO2_roll_ref + hour + CO2_roll_ref * hour,data=subset(data_PSt0,tiefe==i))
  fm <- glm(CO2_roll_inj ~ CO2_roll_ref,data=subset(data_PSt0,tiefe==i))
  
  fm2 <- mgcv::gam(CO2_roll_inj ~ s(CO2_roll_ref) + s(hour),data=subset(data_PSt0,tiefe==i))
  
  data$preds[data$tiefe==i] <- predict(fm,newdata = subset(data,tiefe==i))
  data$preds2[data$tiefe==i] <- predict(fm2,newdata = subset(data,tiefe==i))
}
# ggplot(subset(data_PSt0))+
#   geom_abline(slope = 1,intercept = 0)+
#   geom_point(aes(preds,CO2_inj,col="glm"))+
#   geom_point(aes(preds2,CO2_inj,col="gam"))



########################

data_kal <- aggregate(data_PSt0[,grep("CO2",colnames(data_PSt0))] ,list(tiefe = data_PSt0$tiefe), mean, na.rm=T)
data_kal$offset <-  data_kal$CO2_inj - data_kal$CO2_ref
# ggplot(data_kal)+
#   geom_path(aes(CO2_ref,tiefe,col="ref"))+
#   geom_path(aes(CO2_inj,tiefe,col="inj"))+
#   geom_ribbon(aes(xmin=CO2_ref, xmax= CO2_ref + offset,y=tiefe,fill="offset"),alpha = 0.3)

data$offset <- as.numeric(as.character(factor(data$tiefe, levels=data_kal$tiefe,labels=data_kal$offset)))
data$CO2_tracer <- data$CO2_roll_inj - (data$CO2_roll_ref + data$offset)
data$tracer_pos <- data$CO2_tracer > 0
data$CO2_ref_offst <- ifelse(data$tracer_pos, data$CO2_ref + data$offset, data$CO2_roll_inj)
#data$CO2_tracer[data$CO2_tracer < 0 | data$Pumpstufe == 0| is.na(data$Pumpstufe)] <- NA


###############
#plots glm gam offset
ggplot(data)+
  geom_line(aes(date,CO2_inj-preds,col=as.factor(tiefe)))+facet_wrap(~tiefe,scales="free")
ggplot(data)+
  geom_line(aes(date,CO2_roll_inj-preds,col="gam"))+
  geom_line(aes(date,CO2_tracer,col="tracer"))+
  facet_wrap(~tiefe,scales="free")
ggplot(data)+
  geom_line(aes(date,CO2_roll_ref,col="ref"))+
  geom_line(aes(date,preds,col="glm"))+
  geom_line(aes(date,preds2,col="gam"))+
  geom_line(aes(date,CO2_roll_ref+offset,col="ref+offset"))+
  facet_wrap(~tiefe,scales="free")
ggplot(subset(data,Pumpstufe==0))+
  geom_line(aes(date,CO2_roll_inj,col="inj"))+
  geom_line(aes(date,preds,col="glm"))+
  geom_line(aes(date,preds2,col="gam"))+
  geom_line(aes(date,CO2_roll_ref+offset,col="ref+offset"))+
  facet_wrap(~tiefe,scales="free")
ggplot(subset(data,Pumpstufe==0))+
  geom_line(aes(date,CO2_roll_inj - preds,col="glm"))+
  geom_line(aes(date,CO2_roll_inj - preds2,col="gam"))+
  geom_line(aes(date,CO2_roll_inj - (CO2_roll_ref+offset),col="ref+offset"))+
  facet_wrap(~tiefe,scales="free")

##############
#data_wide
data_wide <- tidyr::pivot_wider(data, id_cols = date, names_from = tiefenstufe,values_from = c(CO2_inj,CO2_ref,CO2_tracer,Fz),names_prefix = "tiefe")

data_wide <- data_wide[,-grep("CO2_(inj|tracer)_tiefe0|Fz_tiefe[1-7]",colnames(data_wide))]
colnames(data_wide) <- str_replace(colnames(data_wide),"Fz_tiefe0","injection_ml_per_min")



######################
#data_agg

data_agg <- aggregate(data[grep("date|CO2|Fz|Pumpstufe|offset",colnames(data))],list(hour=round_date(data$date,"hours"),tiefe=data$tiefe),mean,na.rm=T)
data_agg$date <- with_tz(data_agg$date,"UTC")
data_agg <- subset(data_agg, Pumpstufe == 1.5)
save(data,data_agg,file=paste0(samplerpfad,"Hartheim_CO2.RData"))


##################################################################################################
#plots
################################################################################################

offset_plot <- ggplot(subset(data,Pumpstufe==0))+
  geom_line(aes(date,CO2_roll_ref+offset,col=as.factor(tiefe),linetype="ref + offset"),lwd=1)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"),lwd=1)+
  geom_ribbon(aes(x=date,ymin=CO2_ref + offset,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.3)+
  labs(title="keine Injektion",col="tiefe",fill="tiefe",linetype="sampler")

inj <- ggplot(data)+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  annotate("text",x=Pumpzeiten$start[2],y=Inf,label="injection",vjust=1.9,hjust=-0.3)+labs(col="tiefe [cm]",x="",y=expression(CO[2]*" [ppm]"),title="injection sampler")

ref <- ggplot(data)+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  guides(col=F)+labs(y=expression(CO[2]*" [ppm]"),title="reference sampler")
p_plot <- ggplot(data)+geom_ribbon(aes(x=date,ymin=0,ymax=Precip_Intensity_mmhr),col="blue")+labs(y="Precip Intensity mm/h")
colnames(data)
wind_plot <- ggplot(data)+geom_line(aes(x=date,y=WindVel_30m_ms))
Ta_plot <- ggplot(data)+geom_line(aes(x=date,y=Ta_2m))
ggplot(subset(data,tiefe!=0))+
  geom_line(aes(date,Ta_2m,col="ambient"))+
  geom_line(aes(date,T_C,col="injection_box"))+xlim(ymd_h(c("2020.05.29 00","2020.06.05 00")))+
  ggsave(paste0(plotpfad,"Termperatur_injection_box.pdf"))

ggplot(data)+geom_line(aes(date,eps,col=as.factor(tiefe)))
ggplot(data)+geom_line(aes(date,DSD0_PTF*D0_T_p(12)*10^-4,col=as.factor(tiefe)))

VWC_plot <-  ggplot(data)+
  geom_line(aes(date,`VWC_-5`,col="-5"))+
  geom_line(aes(date,`VWC_-10`,col="-10"))+
  geom_line(aes(date,`VWC_-20`,col="-20"))+
  labs(x="",y="Soil VWC [%]",col="tiefe [cm]")
ref  
inj
p <- egg::ggarrange(inj,ref,ncol=1)
p2 <- egg::ggarrange(inj,VWC_plot,p_plot,heights = c(2,1,1))

pdf(paste0(plotpfad,"hartheim_einspeisung1.pdf"),width=9,height=7)
p
dev.off()
pdf(paste0(plotpfad,"einspeisung1_mit_klima.pdf"),width=9,height=9)
p2
dev.off()

  ggplot(data)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,CO2_roll_ref,col=as.factor(tiefe),linetype="ref"))+
  geom_ribbon(aes(date,ymin=CO2_inj,ymax=CO2_ref,fill=as.factor(tiefe)),alpha=0.3)+
  geom_vline(xintercept = Pumpzeiten$start)+xlim(ymd_hms(c("2020-06-08 09:00:00 UTC","2020-06-09 13:00:00 UTC")))
  
  ggplot(data)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"),lwd=1.2)+
  geom_line(aes(date,CO2_roll_ref + offset,col=as.factor(tiefe),linetype="ref + offset"),lwd=0.8)+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=CO2_ref_offst,fill=as.factor(tiefe)),alpha=0.3)+
    labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)+
    ggsave(paste0(plotpfad,"Einspeisung1_diff.pdf"),width=10,height = 7)
  

  data$monthday <- format(data$date,"%m.%d")
  data_month_day <- aggregate(data[,grep("CO2",colnames(data))],list(monthday = format(data$date,"%m.%d"),tiefe = data$tiefe),mean)
  inj_ref_plot <- ggplot(data)+geom_point(aes(CO2_inj,CO2_ref,col=as.factor(Pumpstufe)))+geom_abline(slope=1,intercept = 0)
#offset


datelim_1.5 <- ymd_h("2020.05.23 10")
  ref_profil <- ggplot(data_month_day)+geom_path(aes(CO2_ref,tiefe,col=monthday))
  inj_profil <- ggplot(data_month_day)+geom_path(aes(CO2_inj,tiefe,col=monthday))
egg::ggarrange(ref_profil,inj_profil,ncol=2)

###################
#tracer
ggplot(data)+
  geom_line(aes(date,CO2_tracer,col=as.factor(tiefe)))



ggplot(subset(data, Pumpstufe == 1.5 & date %in% round_date(date,"hours") & date < datelim_1.5))+ 
  geom_path(aes(CO2_tracer,tiefe,col=as.factor(date)))+
  geom_path(aes(CO2_ref + offset,tiefe,col=as.factor(date)))+
  geom_path(aes(CO2_inj,tiefe,col=as.factor(date)))
###########
#export data


paste("tiefe",rev(unique(data$tiefenstufe)),"=",rev(unique(data$tiefe)),"cm",collapse = ", ")

ggplot(data_wide)+geom_line(aes(date,injection_ml_per_min))

write.csv(data_wide,file=paste0(datapfad_harth,"co2_profil_",paste(format(range(data_wide$date),"%j"),collapse = "-"),".txt"),row.names = F)
strptime()
