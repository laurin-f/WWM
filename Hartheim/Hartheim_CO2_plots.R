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

load(file=paste0(klimapfad,"klima_data.RData"))

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))


##################################################################################
#plots
##################################################################################

#############
#flux
ggplot(data)+geom_line(aes(date,Fz))

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

range2 <- range(data$date[data$Position ==7],na.rm = T)
offset_plot_glm <- ggplot(subset(data,date > range2[1] & date < range2[2] &tiefe==-24.5))+
  geom_line(aes(date,CO2_roll_ref,col="ref"),lwd=1)+
  geom_line(aes(date,preds,col="ref glm"),lwd=1)+
  geom_line(aes(date,CO2_roll_inj,col="inj"),lwd=1)+
  geom_ribbon(aes(x=date,ymin=preds,ymax=CO2_inj,fill="tracer"),alpha=0.3)+
  facet_wrap(~tiefe,scales="free_y",ncol=2)+
  labs(col="tiefe",fill="tiefe",linetype="sampler")


offset_plot_glm+ggsave(paste0(plotpfad,"inj_ref_tracer2.png"),width=9,height=5)
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
  annotate("text",x=Pumpzeiten$start[c(11,13)],y=Inf,label="injection",vjust=1,hjust=-0.1)+
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
  annotate("text",x=Pumpzeiten$start[Pumpzeiten$Position == 8 & Pumpzeiten$Pumpstufe!= 0],y=Inf,label="injection",vjust=1,hjust=-0.1)+
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

T_plot <-  ggplot(data)+
  geom_line(aes(date,T_soil,col=as.factor(tiefe)))+
  labs(x="",y="Soil T [°C]",col="tiefe [cm]")
T_plot_agg <-  ggplot(soil_agg)+
  geom_line(aes(date,mean_T,col=as.factor(tiefe)))+
  labs(x="",y="Soil T [°C]",col="tiefe [cm]")

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

png(paste0(plotpfad,"inj_ref.png"),width=11,height=9,units = "in",res=500)
inj_ref
dev.off()
png(paste0(plotpfad,"inj_ref1.png"),width=11,height=9,units = "in",res=500)
inj_ref1
dev.off()
png(paste0(plotpfad,"inj_ref2.png"),width=11,height=9,units = "in",res=500)
inj_ref2
dev.off()
png(paste0(plotpfad,"CO2_p_VWC.png"),width=10,height=10,units = "in",res=500)
CO2_p_VWC
dev.off()
png(paste0(plotpfad,"CO2_p_VWC1.png"),width=10,height=10,units = "in",res=500)
CO2_p_VWC1
dev.off()
png(paste0(plotpfad,"CO2_p_VWC2.png"),width=10,height=10,units = "in",res=500)
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

############
#range 2
range2 <- range(data$date[data$Position ==7],na.rm = T)

#tracer glm
Tracer_glm <- ggplot(subset(data,date > range2[1] & date < range2[2]))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,preds,col=as.factor(tiefe),linetype="ref + offset"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)+
  ggsave(paste0(plotpfad,"Einspeisung2_glm.png"),width=10,height=7)
Tracer_glm+geom_vline(xintercept = kammer_dates[1])+xlim(ymd_h(c("2020.07.08 06","2020.07.08 14")))

Tracer_gam <- ggplot(subset(data,date > range2[1] & date < range2[2]))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,preds2,col=as.factor(tiefe),linetype="ref + offset"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds2,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)+
  ggsave(paste0(plotpfad,"Einspeisung2_gam.png"),width=10,height=7)

#tracer offset
Tracer_offset <- ggplot(subset(data,date > range2[1] & date < range2[2]))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,CO2_roll_ref + offset,col=as.factor(tiefe),linetype="ref + offset"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=CO2_ref_offst,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)+
  ggsave(paste0(plotpfad,"Einspeisung2_diff.pdf"),width=10,height = 7)

#drift
ggplot(subset(data,date > range2[1] & date < range2[2]))+
  geom_line(aes(date,preds_drift,col=as.factor(tiefe),linetype="inj"))

#tracer drift
Tracer_drift <- ggplot(subset(data,date > range2[1] & date < range2[2]))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,CO2_roll_ref + preds_drift,col=as.factor(tiefe),linetype="ref + preds_drift"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=CO2_ref+ preds_drift,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)+
  ggsave(paste0(plotpfad,"Einspeisung2_drift.png"),width=10,height=7)

Tracer_glm
Tracer_offset
Tracer_offset
######
#range 3

#offset  drift glm

ggplot(subset(data,date > range3[1] & date < range3[2]))+
  geom_line(aes(date,CO2_roll_ref + preds_drift,col=as.factor(tiefe),linetype="offset_drift"))+
  geom_line(aes(date,preds,col=as.factor(tiefe),linetype="glm"))+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")

#tracer glm
ggplot(subset(data,date > range3[1] & date < range3[2]))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,preds,col=as.factor(tiefe),linetype="ref + offset"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds,fill=as.factor(tiefe)),alpha=0.3)+
  labs(title="glm",y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)+
ggsave(paste0(plotpfad,"Einspeisung3_glm.png"),width=10,height=7)

#gam
sub3 <- subset(data,date > range3[1] & date < range3[2]& tiefe !=0)
sub3_calib <- sub3
sub3_calib[which(sub3_calib$Pumpstufe != 0 | is.na(sub3_calib$Pumpstufe)),grep("CO2|preds",colnames(sub3_calib))] <- NA
ggplot(subset(sub3_calib,tiefenstufe %in% c(1,3,6)))+
  #annotate("text",x = Pumpzeiten$start[c(16,19)],y=Inf,label="calibration",col="grey")+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,CO2_roll_inj,col="inj"),lwd=1)+
  geom_line(aes(date,CO2_roll_ref,col="ref"))+
  geom_line(aes(date,preds2,col="offset model"))+
  labs(y=expression(CO[2]*" [ppm]"),col="")+
  facet_grid(tiefe~.,scales="free")+
  #scale_color_viridis_d()
  scale_color_brewer(type="qual",palette = 6)+
  ggsave(paste0(plotpfad,"Einspeisung3_gam_calib.png"),width=7,height=6)
  
ggplot(sub3)+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  #geom_line(aes(date,CO2_roll_ref,col=as.factor(tiefe),linetype="ref",alpha=0.2))+
  geom_line(aes(date,preds2,col=as.factor(tiefe),linetype="ref offset model"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds2,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tiefe [cm]")+
  ggsave(paste0(plotpfad,"Einspeisung3_gam.png"),width=7,height=4)

#tracer drift
ggplot(subset(data,date > range3[1] & date < range3[2]))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,CO2_roll_ref + preds_drift,col=as.factor(tiefe),linetype="ref + offset"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=CO2_roll_ref + preds_drift,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)#+
ggsave(paste0(plotpfad,"Einspeisung3_offset.png"),width=10,height=7)


#egg::ggarrange(Tracer_offset,Tracer_glm+guides(col=F,fill=F,linetype=F))

##################
#profil
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

timesecs <- as.numeric(difftime(max(Kammer_flux$date),min(Kammer_flux$date),unit="secs")/length(Kammer_flux$date))

F_z_PTF  <- ggplot(subset(F_PTF_agg,tiefe %in% c("0-3.5","3.5-7")))+
  geom_ribbon(aes(x=date,ymin=min,ymax=max,fill=as.factor(tiefe)),alpha=0.3)+
  geom_line(aes(date,mean,col=as.factor(tiefe)))+
  labs(col="DS PTF\ntiefe cm",fill="DS PTF\ntiefe cm")+
  ggnewscale::new_scale_color()+
  geom_errorbar(data=Kammer_flux,aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,col=kammer), width=0.4 *timesecs)+
  geom_point(data=Kammer_flux,aes(date,CO2flux,col=kammer),size=2)+
  scale_color_brewer(type="qual",palette = 6)+
  labs(col="Kammermessungen",
       y=expression(CO[2]*"flux ["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))
F_z_PTF+ggsave(paste0(plotpfad,"Vergleich_Flux_PTF_Kammer.png"),width=9,height=5)

###############
#plots glm gam offset


  range2 <- range(data$date[data$Position ==7],na.rm = T)
  data_sub <- subset(data,date>range2[1] & date < range2[2]& tiefe==-21)
  ggplot(data_sub)+
    #geom_line(aes(date,CO2_roll_ref,col="ref"))+
    geom_line(aes(date,preds,col="glm"))+
    geom_line(aes(date,preds2,col="gam"))+
    geom_line(aes(date,CO2_roll_ref+preds_drift,col="drift"))+
    #geom_line(aes(date,CO2_roll_ref+offset,col="ref+offset"))+
    geom_line(data=subset(data_sub,Pumpstufe==0),aes(date,CO2_roll_inj,col="inj"))+
    facet_wrap(~tiefe,scales="free")
  
  ggplot(subset(data,Pumpstufe==0&date > Pumpzeiten$ende[2]& tiefe < 0))+
    geom_line(aes(date,CO2_roll_inj),lwd=1)+
    geom_line(aes(date,preds,col="glm"))+
    #geom_line(aes(date,preds2,col="gam"))+
    geom_line(aes(date,CO2_roll_ref+offset,col="ref+offset"))+
    facet_wrap(~tiefe,scales="free")#+ggsave(paste0(plotpfad,"modellvergleich_per2_fit1u2.pdf"),height = 8,width=15)
  ggplot(subset(data,Pumpstufe==0&date < Pumpzeiten$ende[2] & tiefe < 0))+
    geom_line(aes(date,CO2_roll_inj),lwd=1)+
    geom_line(aes(date,preds,col="glm"))+
    geom_line(aes(date,preds2,col="gam"))+
    geom_line(aes(date,CO2_roll_ref+preds_drift,col="ref+drift"))+
    geom_line(aes(date,CO2_roll_ref+offset,col="ref+offset"))+
    facet_wrap(~tiefe,scales="free")#+ggsave(paste0(plotpfad,"hartheim/modellvergleich_per1_fit1u2.pdf"),height = 8,width=15)
  ggplot(subset(data,Pumpstufe==0))+
    geom_line(aes(date,CO2_roll_inj - preds,col="glm"))+
    geom_line(aes(date,CO2_roll_inj - preds2,col="gam"))+
    geom_line(aes(date,CO2_roll_inj - (CO2_roll_ref+offset),col="ref+offset"))+
    facet_wrap(~tiefe,scales="free")


