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

#smp1_plt <- ggplot(smp1)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

#T_plt <- ggplot(smp1)+geom_line(aes(date,T_C))
#egg::ggarrange(smp1_plt,T_plt,heights=c(4,1))
#smp1_plt+geom_vline(xintercept = Pumpstufen$start)
#ggplot(smp2)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

data <- merge(data,klima,by="date",all.x = T)
#Pumpstufe und Versuch aus metadaten auf dataframe übetragen


#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
stunden_bis_steadystate <- rep(10,nrow(Pumpzeiten))
stunden_cut_off <- rep(0,nrow(Pumpzeiten))


#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
cols2data <- c("Pumpstufe","Position")
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

data <- merge(data,soil_wide,by="date",all.x = T)




data <- variable_to_depths("VWC_min")
data <- variable_to_depths("VWC_max")
data <- variable_to_depths("VWC")
data <- variable_to_depths("T_min")
data <- variable_to_depths("T_max")
data <- variable_to_depths("T_soil")

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





# ggplot()+
#   geom_line(data=data,aes(date,VWC,col=as.factor(tiefe)))+
#   geom_point(data=soil_agg,aes(date,VWC,col=as.factor(tiefe)))

###############
#tiefen Offset


data$hour <- hour(data$date) 


data_PSt0 <- lapply(na.omit(unique(data$Position)),function(x) subset(data, Pumpstufe == 0 & Position == x))

#data$tiefe_inj <- data$tiefe +tiefen_offset$offset[1]
#data$tiefe_ref <- data$tiefe +tiefen_offset$offset[2] - 3.5


# ggplot(data)+
#   geom_line(aes(date,CO2_ref_adj,col=as.factor(tiefe_inj)))+
#   geom_line(aes(date,CO2_ref,col=as.factor(tiefe_ref)))
# ggplot(data)+
#   geom_line(aes(date,CO2_ref_adj-CO2_ref,col=as.factor(tiefe_inj)))
##################
#mit glm oder gam
glmgam <- F
if(glmgam == T){
for(i in (1:7)*-3.5){
  #fm <- glm(CO2_roll_inj ~ CO2_roll_ref + hour + CO2_roll_ref * hour,data=subset(data_PSt0,tiefe==i))
  fm <- glm(CO2_roll_inj ~ CO2_roll_ref,data=subset(data_PSt0[[1]],tiefe==i))
  
  fm2 <- mgcv::gam(CO2_roll_inj ~ s(CO2_roll_ref) + s(hour),data=subset(data_PSt0[[1]],tiefe==i))
  
  data$preds[data$tiefe==i] <- predict(fm,newdata = subset(data,tiefe==i))
  data$preds2[data$tiefe==i] <- predict(fm2,newdata = subset(data,tiefe==i))
}
}

# ggplot(subset(data_PSt0[[1]]))+
#   geom_abline(slope = 1,intercept = 0)+
#   geom_point(aes(preds,CO2_inj,col="glm"))+
#   geom_point(aes(preds2,CO2_inj,col="gam"))



########################
for(i in seq_along(data_PSt0)){
  data_PSt0[[i]]$offset <-  data_PSt0[[i]]$CO2_inj - data_PSt0[[i]]$CO2_ref
}

data_kal <- lapply(data_PSt0, function(x) aggregate(x[,grep("CO2|offset",colnames(x))] ,list(tiefe = x$tiefe), mean, na.rm=T))
#for(i in seq_along(data_kal)){
#data_kal[[i]]$offset <-  data_kal[[i]]$CO2_inj - data_kal[[i]]$CO2_ref
#}


# ggplot(data_kal)+
#   geom_path(aes(CO2_ref,tiefe,col="ref"))+
#   geom_path(aes(CO2_inj,tiefe,col="inj"))+
#   geom_ribbon(aes(xmin=CO2_ref, xmax= CO2_ref + offset,y=tiefe,fill="offset"),alpha = 0.3)
data$offset <- NA
for(i in seq_along(data_kal)){
  pos <- na.omit(unique(data$Position))[i]
  posID <- which(data$Position == pos)
  data$offset[posID] <- as.numeric(as.character(factor(data$tiefe[posID], levels=data_kal[[i]]$tiefe,labels=data_kal[[i]]$offset)))
}
#data$CO2_tracer <- data$CO2_roll_inj - (data$CO2_roll_ref + data$offset)
data$CO2_tracer <- data$CO2_inj - (data$CO2_ref + data$offset)
data$tracer_pos <- data$CO2_tracer > 0
data$CO2_ref_offst <- ifelse(data$tracer_pos, data$CO2_ref + data$offset, data$CO2_inj)
#data$CO2_ref_offst <- ifelse(data$tracer_pos, data$CO2_ref + data$offset, data$CO2_roll_inj)
#data$CO2_tracer[data$CO2_tracer < 0 | data$Pumpstufe == 0| is.na(data$Pumpstufe)] <- NA


###############
#plots glm gam offset
plotgam <- F
if(plotgam==T){
ggplot(data)+
  geom_line(aes(date,CO2_roll_ref,col="ref"))+
  geom_line(aes(date,preds,col="glm"))+
  geom_line(aes(date,preds2,col="gam"))+
  geom_line(aes(date,CO2_roll_ref+offset,col="ref+offset"))+
  facet_wrap(~tiefe,scales="free")
ggplot(subset(data,Pumpstufe==0&date > Pumpzeiten$ende[2]& tiefe < 0))+
  geom_line(aes(date,CO2_roll_inj),lwd=1)+
  geom_line(aes(date,preds,col="glm"))+
  geom_line(aes(date,preds2,col="gam"))+
  geom_line(aes(date,CO2_roll_ref+offset,col="ref+offset"))+
  facet_wrap(~tiefe,scales="free")+ggsave(paste0(plotpfad,"hartheim/modellvergleich_per2_fit1u2.pdf"),height = 8,width=15)
ggplot(subset(data,Pumpstufe==0&date < Pumpzeiten$ende[2] & tiefe < 0))+
  geom_line(aes(date,CO2_roll_inj),lwd=1)+
  geom_line(aes(date,preds,col="glm"))+
  geom_line(aes(date,preds2,col="gam"))+
  geom_line(aes(date,CO2_roll_ref+offset,col="ref+offset"))+
  facet_wrap(~tiefe,scales="free")+ggsave(paste0(plotpfad,"hartheim/modellvergleich_per1_fit1u2.pdf"),height = 8,width=15)
ggplot(subset(data,Pumpstufe==0))+
  geom_line(aes(date,CO2_roll_inj - preds,col="glm"))+
  geom_line(aes(date,CO2_roll_inj - preds2,col="gam"))+
  geom_line(aes(date,CO2_roll_inj - (CO2_roll_ref+offset),col="ref+offset"))+
  facet_wrap(~tiefe,scales="free")
}
##############
#data_wide
data_wide <- tidyr::pivot_wider(data, id_cols = date, names_from = tiefenstufe,values_from = c(CO2_inj,CO2_ref,CO2_tracer,Fz),names_prefix = "tiefe")

data_wide <- data_wide[,-grep("CO2_(inj|tracer)_tiefe0|Fz_tiefe[1-7]",colnames(data_wide))]
colnames(data_wide) <- str_replace(colnames(data_wide),"Fz_tiefe0","injection_ml_per_min")



######################
#data_agg

data_agg <- aggregate(data[grep("date|CO2|Fz|Pumpstufe|offset|Ta_2m|Pressure",colnames(data))],list(hour=round_date(data$date,"hours"),tiefe=data$tiefe),mean,na.rm=T)
data_agg$date <- with_tz(data_agg$date,"UTC")
data_agg <- subset(data_agg, Pumpstufe == 1.5)
save(data,data_agg,file=paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))

##################################################################################################
#plots
################################################################################################
########################
#offset bei Pumpstufe 0
offset_plot1 <- ggplot(subset(data,Pumpstufe==0&Position == 1))+
  geom_line(aes(date,CO2_roll_ref+offset,col=as.factor(tiefe),linetype="ref + offset"),lwd=1)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"),lwd=1)+
  geom_ribbon(aes(x=date,ymin=CO2_ref + offset,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.3)+
  labs(title="keine Injektion",col="tiefe",fill="tiefe",linetype="sampler")

offset_plot2 <- ggplot(subset(data,Pumpstufe==0&Position == 7))+
  geom_line(aes(date,CO2_roll_ref+offset,col=as.factor(tiefe),linetype="ref + offset"),lwd=1)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"),lwd=1)+
  geom_ribbon(aes(x=date,ymin=CO2_ref + offset,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.3)+
  labs(title="keine Injektion",col="tiefe",fill="tiefe",linetype="sampler")

###################
#inj_ref
inj <- ggplot(data)+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  annotate("text",x=Pumpzeiten$start[Pumpzeiten$Pumpstufe !=0],y=Inf,label="injection",vjust=1,hjust=-0.1)+
  labs(col="tiefe [cm]",x="",y=expression(CO[2]*" [ppm]"),title="injection sampler")

ref <- ggplot(data)+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  guides(col=F)+labs(y=expression(CO[2]*" [ppm]"),title="reference sampler")
inj_ref <- egg::ggarrange(inj,ref+
                            annotate("text",x=Pumpzeiten$start,y=max(data$CO2_ref,na.rm=T)+nrow(Pumpzeiten):1*400,label=Pumpzeiten$bemerkung,vjust=1,hjust="left"),ncol=1)


range1 <- range(data$date[data$Position ==1],na.rm = T)
inj_1 <- ggplot(subset(data,date > range1[1] & date < range1[2]))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  annotate("text",x=Pumpzeiten$start[2],y=Inf,label="injection",vjust=1,hjust=-0.1)+
  labs(col="tiefe [cm]",x="",y=expression(CO[2]*" [ppm]"),title="injection sampler")
ref_1 <- ggplot(subset(data,date > range1[1] & date < range1[2]))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  guides(col=F)+labs(y=expression(CO[2]*" [ppm]"),title="reference sampler")

inj_ref1 <- egg::ggarrange(inj_1,ref_1,ncol=1)

range2 <- range(data$date[data$Position ==7],na.rm = T)
inj_2 <- ggplot(subset(data,date > range2[1] & date < range2[2]))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  annotate("text",x=Pumpzeiten$start[11],y=Inf,label="injection",vjust=1,hjust=-0.1)+
  labs(col="tiefe [cm]",x="",y=expression(CO[2]*" [ppm]"),title="injection sampler")

ref_2 <- ggplot(subset(data,date > range2[1] & date < range2[2]))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  guides(col=F)+labs(y=expression(CO[2]*" [ppm]"),title="reference sampler")

inj_ref2 <- egg::ggarrange(inj_2,ref_2,ncol=1)

range3 <- range(data$date[data$Position ==8],na.rm = T)
inj_3 <- ggplot(subset(data,date > range3[1] & date < range3[2]))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  annotate("text",x=Pumpzeiten$start[11],y=Inf,label="injection",vjust=1,hjust=-0.1)+
  labs(col="tiefe [cm]",x="",y=expression(CO[2]*" [ppm]"),title="injection sampler")

ref_3 <- ggplot(subset(data,date > range3[1] & date < range3[2]))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  guides(col=F)+labs(y=expression(CO[2]*" [ppm]"),title="reference sampler")

inj_ref3 <- egg::ggarrange(inj_3,ref_3,ncol=1)


##########################
#klimadaten dazu
p_plot <- ggplot(data)+geom_ribbon(aes(x=date,ymin=0,ymax=Precip_Intensity_mmhr),col="blue")+labs(y="Precip Intensity mm/h")

wind_plot <- ggplot(data)+geom_line(aes(x=date,y=WindVel_30m_ms))

Ta_plot <- ggplot(data)+geom_line(aes(x=date,y=Ta_2m))

VWC_plot <-  ggplot(data)+
  geom_line(aes(date,VWC,col=as.factor(tiefe)))+
  labs(x="",y="Soil VWC [%]",col="tiefe [cm]")
VWC_plot_agg <-  ggplot(soil_agg)+
  geom_line(aes(date,mean_VWC,col=as.factor(tiefe)))+
  labs(x="",y="Soil VWC [%]",col="tiefe [cm]")
VWC_plot_alle_plots <-  ggplot(subset(soil_long,unit=="VWC"))+
  geom_line(aes(date,value,col=as.factor(tiefe),linetype=plot))+
  labs(x="",y="Soil VWC [%]",col="tiefe [cm]")

CO2_p_VWC <- egg::ggarrange(inj,ref,VWC_plot_agg+xlim(range(data$date)),p_plot,heights = c(2,2,1,1))
CO2_p_VWC1 <- egg::ggarrange(inj_1,ref_1,VWC_plot_agg+xlim(range1),p_plot+xlim(range1),heights = c(2,2,1,1))
CO2_p_VWC2 <- egg::ggarrange(inj_2,ref_2,VWC_plot_agg+xlim(range2),p_plot+xlim(range2),heights = c(2,2,1,1))

########################
#export
pdf(paste0(plotpfad,"inj_ref.pdf"),width=11,height=9)
inj_ref
dev.off()
pdf(paste0(plotpfad,"inj_ref1.pdf"),width=11,height=9)
inj_ref1
dev.off()
pdf(paste0(plotpfad,"inj_ref2.pdf"),width=11,height=9)
inj_ref2
dev.off()
pdf(paste0(plotpfad,"CO2_p_VWC.pdf"),width=10,height=10)
CO2_p_VWC
dev.off()
pdf(paste0(plotpfad,"CO2_p_VWC1.pdf"),width=10,height=10)
CO2_p_VWC1
dev.off()
pdf(paste0(plotpfad,"CO2_p_VWC2.pdf"),width=10,height=10)
CO2_p_VWC2
dev.off()


#######################################
#tracer ribbon

#erster Versuch
range1 <- range(data$date[data$Position ==1],na.rm = T)
ggplot(subset(data,date > range1[1] & date < range1[2]))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"),lwd=1.2)+
  geom_line(aes(date,CO2_roll_ref + offset,col=as.factor(tiefe),linetype="ref + offset"),lwd=0.8)+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=CO2_ref_offst,fill=as.factor(tiefe)),alpha=0.3)+
    labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)+
  ggsave(paste0(plotpfad,"Einspeisung1_diff.pdf"),width=10,height = 7)
  
range2 <- range(data$date[data$Position ==7],na.rm = T)

ggplot(subset(data,date > range2[1] & date < range2[2]))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"),lwd=1.2)+
  geom_line(aes(date,CO2_roll_ref + offset,col=as.factor(tiefe),linetype="ref + offset"),lwd=0.8)+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=CO2_ref_offst,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)+
  ggsave(paste0(plotpfad,"Einspeisung2_diff.pdf"),width=10,height = 7)


data_sub <- subset(data,date== ymd_h("2020-07-06 12"))
  ggplot(data_sub)+geom_line(aes(CO2_tracer,tiefe),orientation = "y")
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

##################
#eps DS_PTF plot

ggplot(data)+geom_line(aes(date,eps,col=as.factor(tiefe)))
ggplot(data)+geom_line(aes(date,DSD0_PTF*D0_T_p(12)*10^-4,col=as.factor(tiefe)))
colnames(data)
###########
#export data


paste("tiefe",rev(unique(data$tiefenstufe)),"=",rev(unique(data$tiefe)),"cm",collapse = ", ")

ggplot(data_wide)+geom_line(aes(date,injection_ml_per_min))

write.csv(data_wide,file=paste0(datapfad_harth,"co2_profil_",paste(format(range(data_wide$date),"%j"),collapse = "-"),".txt"),row.names = F)
strptime()
