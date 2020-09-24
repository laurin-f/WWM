#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Methodenpaper/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
aufbereitete_ds<-paste0(hauptpfad,"Daten/aufbereiteteDaten/DS_Labor/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr")
check.packages(packages)
theme_set(theme_classic())

#daten laden
load(file=paste0(klimapfad,"klima_data.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(aufbereitete_ds,"Labor_Vergleich.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper.RData"))


range1 <- range(data$date[data$Position ==1],na.rm = T)
range2 <- range(data$date[data$Position ==7],na.rm = T)
range3 <- range(data$date[data$Position ==8],na.rm = T)
range2u3 <- range(data$date[data$Position %in% 7:8],na.rm = T)
data$inj
data %>% group_by(Position,Pumpstufe) %>% summarise(injectionrate=mean(Fz))#ml per min

##################################################################################
#plots
##################################################################################

#############################
#Figure 3 DS Labor und sandkiste
##############################
ggplot()+
  geom_point(data=subset(results,material!="leer"),aes(factor(material,levels=c("sand","splitt und sand","splitt"),labels=c("sand","mixture","grit")),DSD0,col="Lab",shape="Lab"))+
  geom_point(data=comsol,aes(factor(material,levels=c("sand","splitt und sand","splitt"),labels=c("sand","mixture","grit")),DS_D0_mod,col="in situ",shape="in situ"))+labs(y=expression(D[S]/D[0]),x="",col="",shape="")+scale_color_manual(values=1:2)+ylim(c(0,0.3))+ggsave(paste0(plotpfad,"Labor_Vergleich.png"),width=5,height=3)

##########################
#Figure 4 Rawdata
##########################
#inj ref
all_dpths <- sort(c(unique(data$tiefe),unique(-soil_agg$tiefe)))[-1]
inj_2u3 <- ggplot(subset(data,date > range2u3[1] & date < range2u3[2]))+
  #geom_rect(data=subset(Pumpzeiten,Pumpstufe==0 & (is.na(bemerkung)|bemerkung=="zurück getauscht") & Position %in% 8),aes(xmin=min(start),xmax=max(ende),ymin=-Inf,ymax=Inf,col="Zoom extend",fill="Zoom extend"),alpha=0.2)+
  geom_rect(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="injection period",col="injection period"),alpha=0.2)+
  scale_fill_manual("",values=c(1,NA))+
  scale_color_manual("",values=c(NA,1))+
  ggnewscale::new_scale_color()+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  labs(col="depth [cm]",x="",y=expression(CO[2]*" [ppm]"),title="injection sampler",fill="")+
  theme(axis.text.x = element_blank(),
        legend.title = element_text(color=1))+
  scale_color_discrete(limits=all_dpths)+
  guides(colour = guide_legend(override.aes = list(size = 1.5)), 
         fill = guide_legend())

ref_2u3 <- ggplot(subset(data,date > range2u3[1] & date < range2u3[2]))+
  #geom_rect(data=subset(Pumpzeiten,Pumpstufe==0 & (is.na(bemerkung)|bemerkung=="zurück getauscht") & Position %in% 8),aes(xmin=min(start),xmax=max(ende),ymin=-Inf,ymax=Inf,col="Zoom extend",fill="Zoom extend"),alpha=0.2)+
  geom_rect(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="injection period",col="injection period"),alpha=0.2)+
  scale_fill_manual("",values=c(1,NA))+
  scale_color_manual("",values=c(NA,1))+
  ggnewscale::new_scale_color()+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  guides(col=F,fill=F)+labs(y=expression(CO[2]*" [ppm]"),x="",title="reference sampler")+
  theme(axis.text.x = element_blank())+
  scale_color_discrete(limits=all_dpths)


##########################
#klimadaten dazu
data$Precip_Last24hrs_mm[data$Precip_Last24hrs_mm < 0] <- 0

###########
#T
T_plot_2u3 <-  ggplot(subset(soil_agg,date > range2u3[1] & date < range2u3[2] & tiefe %in% c(2,5,10,20,50)))+
  geom_line(aes(date,mean_T,col=as.factor(-tiefe)))+
  guides(col=F)+
  labs(x="date",y="Soil T [°C]",col="depth [cm]")+
  scale_color_discrete(limits=all_dpths)

################
#VWC P
sec_ax_fac <- 1.25
VWC_plot_2u3 <-  ggplot(subset(soil_agg,date > range2u3[1] & date < range2u3[2] & tiefe %in% c(2,5,10,20,50)))+
  geom_ribbon(data=subset(data,date > range2u3[1] & date < range2u3[2]),aes(x=date,ymin=0,ymax=Precip_Last24hrs_mm/sec_ax_fac),fill="blue",alpha=0.8)+
  geom_line(aes(date,mean_VWC,col=as.factor(-tiefe)))+
  scale_y_continuous(sec.axis = sec_axis(~.*sec_ax_fac,name=expression(P["24h"]*" [mm]")))+
  theme(
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"),
    axis.text.x = element_blank()
  )+
  guides()+
  labs(x="",y="Soil VWC [%]",col="depth [cm]")+
  scale_color_discrete(limits=all_dpths)


CO2_p_VWC2u3 <- ggpubr::ggarrange(inj_2u3,ref_2u3,VWC_plot_2u3,T_plot_2u3,ncol=1,heights = c(1.5,1.5,1,1),common.legend = T,legend="right",align="v")+ggsave(paste0(plotpfad,"CO2_p_VWC_t.png"),width=7,height=7)

###########################################
#Figure 5 gam calib
############################################

ggplot(subset(sub3_calib,tiefenstufe %in% c(3)))+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,CO2_roll_inj,col="inj"),lwd=1)+
  geom_line(aes(date,CO2_roll_ref,col="ref"))+
  geom_line(aes(date,preds2,col="ref adj"))+
  labs(y=expression(CO[2]*" [ppm]"),col="")+
  facet_grid(.~paste("depth = ",tiefe," cm"),scales="free")+
  scale_color_discrete(l=55)+
  ggsave(paste0(plotpfad,"Einspeisung3_gam_calib.png"),width=7,height=3)

########################################
#Figure 6 a) Co2 tracer Einspeisung
#######################################

ggplot(sub2u3)+
  geom_vline(xintercept = Pumpzeiten$start[c(11:14,17:18)],col="grey")+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,preds2,col=as.factor(tiefe),linetype="ref adj"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds2,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="depth [cm]",linetype="sampler",fill="depth [cm]")+
  ggsave(paste0(plotpfad,"Einspeisung2u3_gam.png"),width=7,height=3)

########################################
#Figure 6 b) Co2 tracer adj grob size
#######################################

plt_data <- leave_NAtime_plot(y="CO2_tracer_gam",data=subset(data,date > range2u3[1] & date < range2u3[2]&Pumpstufe!=0&tiefe!=0),breaks="2 day",timestep = 60*24,plot=F)
tracer_plot <- ggplot(plt_data)+
  
  #geom_vline(xintercept = Pumpzeiten$start[-1])+
  #geom_vline(xintercept = Pumpzeiten$start[c(11:14,17,18)],col="grey")+
  #annotate("text",x=Pumpzeiten$start[c(11,13,17)],y=Inf,label=paste("injection",1:3),vjust=1,hjust=-0.1)+
  geom_line(aes(date,CO2_tracer_gam,col=as.factor(tiefe)))+
  labs(col="tiefe [cm]",x="",y=expression(CO[2]*"tracer [ppm]"))+
  facet_wrap(~paste("injection",period),scales="free_x",nrow=1)+theme_bw()
png(paste0(plotpfad,"CO2_tracer_gam.png"),width=7,height=3,units="in",res=300)
adj_grob_size(tracer_plot,plt_data,breaks="1 day",date_label="%b %d")
dev.off()

######################################
#Figure 7 Ds profile over time
######################################

ggplot(subset(soil_agg_plot))+
  geom_ribbon(aes(x=date,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill=as.factor(range)),alpha=0.15)+
  geom_line(aes(date,DSD0_PTF,col=as.factor(range),linetype="f(eps)"))+
  geom_line(data=DS_long_roll,aes(date,DSD0_roll,col=range,linetype="in situ"))+
  geom_line(data=DS_long_roll,aes(date,DS_roll/D0_T_p(unit="m2/s"),col=range,linetype="in situ"))+
  labs(y=expression(D[S]/D[0]),col="depth [cm]",fill="depth [cm]",linetype="")+
  xlim(range(DS_long$date))+
  scale_linetype_manual(values=2:1,labels=c(expression(f~(epsilon),"in situ")))+
  ggsave(file=paste0(plotpfad,"DS_plot_gam_feps.png"),width=7,height = 3.5)
######################################
#Figure 8 Flux
######################################
ggplot(subset(Kammer_flux))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,col=kammer),width=10000)+
  geom_point(aes(date,CO2flux,col=kammer))+
  labs(col="chamber")+
  ggnewscale::new_scale_color()+
  geom_line(data=subset(F_df),aes(date,Fz,col=""),alpha=0.2)+
  geom_line(data=subset(F_df),aes(date,Fz_roll,col=""))+
  scale_color_manual("gradient method",values=1:2)+
  xlim(ymd_hms(c("2020-07-06 13:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  labs(y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))+
  #theme(legend.position = "top")#+
ggsave(paste0(plotpfad,"Flux_Kammer_Comsol_gam_3DS.png"),width=7,height = 3.5)
