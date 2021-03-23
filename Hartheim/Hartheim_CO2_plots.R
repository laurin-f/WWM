#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr")
check.packages(packages)
theme_set(theme_classic())

load(file=paste0(klimapfad,"klima_data.RData"))

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))

range1 <- range(data$date[data$Position ==1],na.rm = T)
range2 <- range(data$date[data$Position ==7],na.rm = T)
range3 <- range(data$date[data$Position ==8],na.rm = T)
range2u3 <- range(data$date[data$Position %in% 7:8],na.rm = T)
data$inj
data %>% group_by(Position,Pumpstufe) %>% summarise(injectionrate=mean(Fz))#ml per min

##################################################################################
#plots
##################################################################################

#############
#injectionsrate
ggplot(data)+geom_line(aes(date,Fz))

##########################
#Co2 tracer adj grob size
plt_data <- leave_NAtime_plot(y="CO2_tracer_gam",data=subset(data,date > range2u3[1] & date < range2u3[2]&Pumpstufe!=0&tiefe!=0),breaks="2 day",timestep = 60*24,plot=F)
tracer_plot <- ggplot(plt_data)+
  
  #geom_vline(xintercept = Pumpzeiten$start[-1])+
  #geom_vline(xintercept = Pumpzeiten$start[c(11:14,17,18)],col="grey")+
  #annotate("text",x=Pumpzeiten$start[c(11,13,17)],y=Inf,label=paste("injection",1:3),vjust=1,hjust=-0.1)+
  geom_line(aes(date,CO2_tracer_gam,col=as.factor(tiefe)))+
  labs(col="tiefe [cm]",x="",y=expression(CO[2]*"tracer [ppm]"))+
  facet_wrap(~paste("injection",period),scales="free_x",nrow=1)+theme_bw()
png(paste0(plotpfad_harth,"CO2_tracer_gam.png"),width=7,height=3,units="in",res=300)
adj_grob_size(tracer_plot,plt_data,breaks="1 day",date_label="%b %d")
dev.off()

########
#inj ref
all_dpths <- sort(c(unique(data$tiefe),unique(-soil_agg$tiefe)))[-1]
inj_2u3 <- ggplot(subset(data,date > range2u3[1] & date < range2u3[2]))+
  geom_rect(data=subset(Pumpzeiten,Pumpstufe==0 & (is.na(bemerkung)|bemerkung=="zurück getauscht") & Position %in% 8),aes(xmin=min(start),xmax=max(ende),ymin=-Inf,ymax=Inf,col="Zoom extend",fill="Zoom extend"),alpha=0.2)+
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
  geom_rect(data=subset(Pumpzeiten,Pumpstufe==0 & (is.na(bemerkung)|bemerkung=="zurück getauscht") & Position %in% 8),aes(xmin=min(start),xmax=max(ende),ymin=-Inf,ymax=Inf,col="Zoom extend",fill="Zoom extend"),alpha=0.2)+
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
sub3 <- subset(data,date > range3[1] & date < range3[2]& tiefe !=0)
sub3_calib <- sub3
sub3_calib[which(sub3_calib$Pumpstufe != 0 | is.na(sub3_calib$Pumpstufe)),grep("CO2|preds",colnames(sub3_calib))] <- NA

data$Precip_Intensity_mmhr[data$Precip_Intensity_mmhr< 0] <- NA
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


CO2_p_VWC2u3 <- ggpubr::ggarrange(inj_2u3,ref_2u3,VWC_plot_2u3,T_plot_2u3,ncol=1,heights = c(1.5,1.5,1,1),common.legend = T,legend="right",align="v")+ggsave(paste0(plotpfad_harth,"CO2_p_VWC_t.png"),width=7,height=7)


#########################
#tracer

sub3 <- subset(data,date > range3[1] & date < range3[2]& tiefe !=0)
sub2u3 <- subset(data,date > range2u3[1] & date < range2u3[2]& tiefe !=0)
sub3_calib <- sub3
sub3_calib[which(sub3_calib$Pumpstufe != 0 | is.na(sub3_calib$Pumpstufe)),grep("CO2|preds",colnames(sub3_calib))] <- NA



#######################
#tagesgang
Co2_plot <- ggplot(subset(sub3,tiefe < -21))+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,CO2_roll_ref,col=hour,linetype="inj"))+
  scale_x_datetime(date_labels="%m.%d %H:%M",date_breaks = "1 day",limits=c(range3[2]-3600*24*6,range3[2]))+ylim(c(2500,3500))+theme_bw()
T_plot <- ggplot(sub3)+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,T_soil,col=as.factor(tiefe),linetype="ref offset model"))+
  theme_bw()+scale_x_datetime(date_labels="%m.%d %H:%M",date_breaks = "1 day",minor_breaks = "1 day",limits=c(range3[2]-3600*24*6,range3[2]))
  
sub3_agg <- sub3 %>% group_by(hour,tiefe)%>% summarise_at(c("T_soil","CO2_roll_ref"),mean,na.rm=T)
sub3_agg$CO2_korr <- NA
for(i in 1:7*-3.5){
sub3_agg$CO2_korr[sub3_agg$tiefe == i] <- sub3_agg$CO2_roll_ref[sub3_agg$tiefe == i] / max(sub3_agg$CO2_roll_ref[sub3_agg$tiefe == i])
}

diurnal<- data.frame(a=NA)
for(i in 1:7*-3.5){
diurnal[,paste0("Tmax_",i)] <- sub3_agg$hour[sub3_agg$tiefe==i][which.max(sub3_agg$T_soil[sub3_agg$tiefe==i])]
diurnal[,paste0("Tmin_",i)] <- sub3_agg$hour[sub3_agg$tiefe==i][which.min(sub3_agg$T_soil[sub3_agg$tiefe==i])]
diurnal[,paste0("CO2max_",i)] <- sub3_agg$hour[sub3_agg$tiefe==i][which.max(sub3_agg$CO2_roll_ref[sub3_agg$tiefe==i])]
diurnal[,paste0("CO2min_",i)] <- sub3_agg$hour[sub3_agg$tiefe==i][which.min(sub3_agg$CO2_roll_ref[sub3_agg$tiefe==i])]
}
diurnal_agg<-tidyr::pivot_longer(diurnal[,-1],everything(),names_pattern = "(.*)_(.*)",names_to=c(".value","tiefe"))
  sub3_agg %>% group_by(tiefe) %>%summarise_at(c("T_soil","CO2_roll_ref"),max)
ggplot(sub3_agg)+
  geom_line(aes(hour,T_soil/max(T_soil),col=as.factor(tiefe)))+
  geom_line(aes(hour,CO2_korr,col=as.factor(tiefe)))
  facet_wrap(~tiefe,scales="free")
ggplot(subset(sub3,Pumpstufe==0))+
  geom_point(aes(hour,T_soil,col=as.factor(tiefe)))+
  facet_wrap(~tiefe,scales="free")

#############################
#tracer glm
ggplot(sub3)+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,preds,col=as.factor(tiefe),linetype="ref offset model"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tiefe [cm]")+
  xlim(c(range3[1],range3[2]-3600*24*4))+
  ggsave(paste0(plotpfad_harth,"Einspeisung3_glm.png"),width=7,height=4)

##################
#gam calib
ggplot(subset(sub3_calib,tiefenstufe %in% c(3)))+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,CO2_roll_inj,col="inj"),lwd=1)+
  geom_line(aes(date,CO2_roll_ref,col="ref"))+
  geom_line(aes(date,preds2,col="ref adj"))+
  labs(y=expression(CO[2]*" [ppm]"),col="")+
  facet_grid(.~paste("depth = ",tiefe," cm"),scales="free")+
  scale_color_discrete(l=55)+
  ggsave(paste0(plotpfad_harth,"Einspeisung3_gam_calib.png"),width=7,height=3)

####################
#glm calib
ggplot(subset(sub3_calib,tiefenstufe %in% c(3)))+
  #annotate("text",x = Pumpzeiten$start[c(16,19)],y=Inf,label="calibration",col="grey")+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,CO2_roll_inj,col="inj"),lwd=1)+
  geom_line(aes(date,CO2_roll_ref,col="ref"))+
  geom_line(aes(date,preds,col="ref adj"))+
  labs(y=expression(CO[2]*" [ppm]"),col="")+
  facet_grid(.~paste("depth = ",tiefe," cm"),scales="free")+
  #scale_color_viridis_d()
  scale_color_brewer(type="qual",palette = 6)+
  ggsave(paste0(plotpfad_harth,"Einspeisung3_glm_calib.png"),width=7,height=3)
  
##################
#tracer gam
calib_plt$data
einspeisung_plot <- ggplot(sub3)+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(-tiefe),linetype="inj"))+
  #geom_line(aes(date,CO2_roll_ref,col=as.factor(tiefe),linetype="ref",alpha=0.2))+
  geom_line(aes(date,preds2,col=as.factor(-tiefe),linetype="ref adj"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds2,fill=as.factor(-tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="depth [cm]",linetype="sampler",fill="depth [cm]")+
  xlim(c(range3[1],range3[2]-3600*24*4))+
  ggsave(paste0(plotpfad_harth,"Einspeisung3_gam.png"),width=7,height=4)

#####################
#tracer gam 
einspeisung2u3_plot <- ggplot(sub2u3)+
  geom_vline(xintercept = Pumpzeiten$start[c(11:14,17:18)],col="grey")+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,preds2,col=as.factor(tiefe),linetype="ref adj"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds2,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="depth [cm]",linetype="sampler",fill="depth [cm]")+
  ggsave(paste0(plotpfad_harth,"Einspeisung2u3_gam.png"),width=7,height=3)


tracer_plot <- ggplot(subset(sub3,Pumpstufe!=0))+
  geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
  geom_line(aes(date,CO2_tracer_gam,col=as.factor(tiefe)))+
  labs(y=expression(CO[2]*"tracer [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tiefe [cm]")+
  guides(col=F)

sub3$CO2_tracer_gam[sub3$CO2_tracer_gam < 0] <- 0
tracer_profile <- subset(sub3,Pumpstufe!=0 & date > ymd_h("2020.07.22 15")) %>% 
  group_by(tiefe) %>%
  summarise_at("CO2_tracer_gam",list(min=min,max=max,tracer_mean=mean),na.rm=T)
tiefe_plot <- ggplot(tracer_profile)+
  
  geom_ribbon(aes(xmin=min,xmax=max,y=tiefe),col="grey",alpha=0.3)+
  geom_point(aes(tracer_mean,tiefe))+
  labs(x=expression(CO[2]~"tracer [ppm]"),y="depth [cm]")

tracer_tiefe_plot <- ggarrange(tracer_plot+labs(title="b)"),tiefe_plot+labs(title="c)"),widths=2:1)  
ggpubr::ggarrange(einspeisung_plot+labs(title="a)"),tracer_tiefe_plot,ncol=1,heights = 4:3)
ggpubr::ggarrange(einspeisung_plot+labs(title="a)"),tracer_tiefe_plot,ncol=1,heights = 4:3,common.legend = T,legend="right")+ggsave(paste0(plotpfad_harth,"Einspeisung3_gam_tiefenprofil.jpg"),width=7,height=5)

#tracer drift
ggplot(subset(data,date > range3[1] & date < range3[2]))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,CO2_roll_ref + preds_drift,col=as.factor(tiefe),linetype="ref + offset"))+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=CO2_roll_ref + preds_drift,fill=as.factor(tiefe)),alpha=0.3)+
  labs(y=expression(CO[2]*" [ppm]"),col="tiefe [cm]",linetype="sampler",fill="tracer signal")+
  geom_vline(xintercept = Pumpzeiten$start)#+
ggsave(paste0(plotpfad_harth,"Einspeisung3_offset.png"),width=10,height=7)


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
F_z_PTF+ggsave(paste0(plotpfad_harth,"Vergleich_Flux_PTF_Kammer.png"),width=9,height=5)

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
    facet_wrap(~tiefe,scales="free")#+ggsave(paste0(plotpfad_harth,"modellvergleich_per2_fit1u2.pdf"),height = 8,width=15)
  ggplot(subset(data,Pumpstufe==0&date < Pumpzeiten$ende[2] & tiefe < 0))+
    geom_line(aes(date,CO2_roll_inj),lwd=1)+
    geom_line(aes(date,preds,col="glm"))+
    geom_line(aes(date,preds2,col="gam"))+
    geom_line(aes(date,CO2_roll_ref+preds_drift,col="ref+drift"))+
    geom_line(aes(date,CO2_roll_ref+offset,col="ref+offset"))+
    facet_wrap(~tiefe,scales="free")#+ggsave(paste0(plotpfad_harth,"hartheim/modellvergleich_per1_fit1u2.pdf"),height = 8,width=15)
  ggplot(subset(data,Pumpstufe==0))+
    geom_line(aes(date,CO2_roll_inj - preds,col="glm"))+
    geom_line(aes(date,CO2_roll_inj - preds2,col="gam"))+
    geom_line(aes(date,CO2_roll_inj - (CO2_roll_ref+offset),col="ref+offset"))+
    facet_wrap(~tiefe,scales="free")


sub <-  subset(data,tiefe == -3.5 & date > min(range2u3))
sub2 <- subset(data,tiefe == -3.5& date > min(range2u3) & Pumpstufe != 0)
range(sub$preds2,na.rm = T)
range(sub2$CO2_tracer_gam,na.rm = T)
ggplot(subset(data,tiefe == -3.5 & date > min(range2u3)))+
  geom_line(aes(date,preds2,col=as.factor(tiefe)))
  
  #geom_line(aes(date,CO2_inj,col="inj"))
ggplot(subset(data,tiefe == -3.5& date > min(range2u3) & Pumpstufe != 0))+geom_line(aes(date,CO2_tracer_gam,col=as.factor(tiefe)))


###########################
#heterogeneity
colnames(data)
ggplot(subset(data,tiefe==-24.5 & !is.na(Position)))+
  geom_vline(data=subset(Pumpzeiten,!is.na(bemerkung)),aes(xintercept=start))+
  geom_text(data=subset(Pumpzeiten,!is.na(bemerkung)),aes(x=start,y=7000+300*1:9,label=bemerkung),hjust=0)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(Position)))+
  geom_line(aes(date,CO2_roll_ref,col="ref"))+
  geom_vline(xintercept = ymd_h("2020.07.09 00"),col=2)+
  xlim(ymd_h("2020.07.09 00")+c(-10,10)*3600*24)
 dates <- ymd_h(c("2020.05.20 00","2020.06.20 00","2020.06.25 00","2020.07.09 15"))
 
 Pump_sub <- subset(Pumpzeiten,Pumpstufe==0 & !grepl("regen|tauscht",Pumpzeiten$bemerkung))
 
 data_sub_ls <- vector("list",nrow(Pump_sub))
 for (i in 1:nrow(Pump_sub)) {
   data_sub_ls[[i]] <-subset(data,date > Pump_sub$start[i] & date < (Pump_sub$ende[i]-3600*3))
   data_sub_ls[[i]]$ID <- i
 }
 
 data_sub <- do.call(rbind,data_sub_ls)

  
 data_sub2 <- subset(data_sub,ID %in% c(1,3,7) & !is.na(Position) & tiefe < 0)
data_sub_agg <- data_sub2 %>% group_by(ID,tiefe) %>% summarise_at(paste0("CO2_roll_",c("inj","ref")),list(min=min,max=max,mean=mean),na.rm=T)
data_sub_agg[data_sub_agg == Inf |data_sub_agg == -Inf] <- NA
tiefe_plt <-  ggplot(data_sub_agg)+
  geom_ribbon(aes(xmin=CO2_roll_ref_min,xmax=CO2_roll_ref_max,y=tiefe,fill="ref"),alpha=0.2)+
  geom_ribbon(aes(xmin=CO2_roll_inj_min,xmax=CO2_roll_inj_max,y=tiefe,fill="inj"),alpha=0.2)+
  geom_line(aes(CO2_roll_ref_mean,tiefe,col="ref"))+
  geom_line(aes(CO2_roll_inj_mean,tiefe,col="inj"))+
  facet_wrap(~factor(ID,levels = c(1,3,7),labels=paste("Position",1:3)))+
  labs(x=expression(CO[2]~"[ppm]"),fill="",col="",y="depth [cm]")+theme_bw()

Sys.setlocale("LC_ALL","English")
time_plt <- ggplot(subset(data_sub,ID %in% c(1,3,7) & !is.na(Position)& tiefe < 0))+
  geom_line(aes(date,CO2_roll_ref,col=as.factor(-tiefe),linetype="ref"))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(-tiefe),linetype="inj"))+
  facet_wrap(~factor(ID,levels = c(1,3,7),labels=paste("Position",1:3)),scales="free")+
  labs(x="",y=expression(CO[2]~"[ppm]"),col="depth [cm]",linetype="")+
  scale_x_datetime(date_breaks = "2 days",date_labels = "%b %d")+
  theme_bw()
time_plt_adj <- ggplot(subset(data_sub,ID %in% c(1,3,7) & !is.na(Position)& tiefe < 0))+
  geom_line(aes(date,preds_drift,col=as.factor(-tiefe),linetype="ref adj"))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(-tiefe),linetype="inj"))+
  facet_wrap(~factor(ID,levels = c(1,3,7),labels=paste("Position",1:3)),scales="free")+
  labs(x="",y=expression(CO[2]~"[ppm]"),col="depth [cm]",linetype="")+
  scale_x_datetime(date_breaks = "2 days",date_labels = "%b %d")+
  theme_bw()

leg1 <- ggpubr::get_legend(tiefe_plt)
legend1 <- ggpubr::as_ggplot(leg1)
leg2 <- ggpubr::get_legend(time_plt)
legend2 <- ggpubr::as_ggplot(leg2)
leg <- ggpubr::ggarrange(legend1,legend2,heights = c(1,1.5),ncol=1,align = "v")

leg2_adj <- ggpubr::get_legend(time_plt_adj)
legend2_adj <- ggpubr::as_ggplot(leg2_adj)
leg_adj <- ggpubr::ggarrange(legend1,legend2_adj,heights = c(1,1.5),ncol=1,align = "v")


no_leg <- ggpubr::ggarrange(tiefe_plt+theme(legend.position = "none"),time_plt+theme(legend.position = "none"),ncol=1,align="v") 
no_leg_adj <- ggpubr::ggarrange(tiefe_plt+theme(legend.position = "none"),time_plt_adj+theme(legend.position = "none"),ncol=1,align="v") 
ggpubr::ggarrange(no_leg,leg,widths = c(8,2))+ggsave(paste0(plotpfad_harth,"heterogeneity.jpg"),width=7,height=5)
ggpubr::ggarrange(no_leg_adj,leg_adj,widths = c(8,2))+ggsave(paste0(plotpfad_harth,"heterogeneity_adj.jpg"),width=7,height=5)
Sys.setlocale("LC_ALL","")


ggplot(subset(data,Position %in% 7:8))+geom_line(aes(date,inj_mol_m2_s))
