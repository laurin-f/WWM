Hi Martin,

ich habe nochmal weiter in die Detailed comments geschaut. Und weitere Kritik am Vergleich mit Bodenfeuchtemessungen die 20m entfernt sind. Ich bin selber nicht ganz zufrieden mit den Messergebnissen aus Hartheim da ich keine einzige Kammermessung während der Injektionszeiträume habe nur davor und danach und davon leider keine bei der einzigen injektion die ein stabiles tracerprofil geliefert hat.

Was hälst du davon ich Versuche nochmal in meinem Vorgarten ein paar tests dann kann ich mehrere Kammermessungen während der injektion machen die Methansache mal testen (Methan muss a nicht ins Paper aber würde sich dann anbieten). Die Wege wären auf jeden Fall kurz. Ich könnte außerdem mal die SWC Profilsonden testen.

LG

Laurin


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
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr","png","cowplot","magick")
check.packages(packages)
theme_set(theme_classic())

#daten laden
load(file=paste0(klimapfad,"klima_data.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(aufbereitete_ds,"Labor_Vergleich.RData"))
#aus "plots_COMSOL_out_Hartheim_sweep.R"
load(paste0(comsolpfad,"plotdata_Methodenpaper.RData"))
load(paste0(comsolpfad,"sandkiste_sweep_data_sub.RData"))
load(paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))

pos8_date <- min(data$date[which(data$Position ==8 & data$Pumpstufe != 0)])
range1 <- range(data$date[data$Position ==1],na.rm = T)
range2 <- range(data$date[data$Position ==7],na.rm = T)
range3 <- range(data$date[data$Position ==8],na.rm = T)
range2u3 <- range(data$date[data$Position %in% 7:8],na.rm = T)

F_df[which(F_df$date>pos8_date & F_df$date < (pos8_date + 20*3600)),c("DSD0_1","Fz","Fz_roll")] <- NA
DS_long_roll$DSD0_roll[which(DS_long_roll$date>pos8_date & DS_long_roll$date < (pos8_date + 20*3600) & DS_long_roll$id == 1)] <- NA

#anteil FCO2 10-17 an gesamt respiration
F_2u3 <- subset(F_df,Versuch!="1")
mean(F_2u3$Fz_roll2_10_17 / F_2u3$Fz_roll2,na.rm=T)

data %>% group_by(Position,Pumpstufe) %>% summarise(injectionrate=mean(Fz))#ml per min
range(F_df$Fz_roll,na.rm=T)
##################################################################################
#plots
##################################################################################
#############################
#Figure 2 COMSOL und DS sandkiste
##############################
img <- png::readPNG("../../Dokumentation/Berichte/Methodenpaper_sampler/Abbildungen/Sandboxplot3D.png")
plot_minimal <- ggplot()+theme_minimal()+labs(title="a)")
dim(img)
img_plot <- ggdraw()+draw_image(img,height=1,width=2,x=-0.3)#+draw_plot(plot_minimal)


colnames(data_sub)
mod_obs_plot <- 
  ggplot(subset(data_sub, Versuch %in% c(5,6,9)))+
  #geom_ribbon(aes(xmin=min_mod,xmax=max_mod,y=tiefe,fill="sweep"),alpha=0.3)+
  geom_line(aes(CO2_mod,tiefe,col="mod"))+
  geom_point(aes(CO2,tiefe,fill="obs"))+
  facet_wrap(~factor(material,levels=c("Sand","Sand & Splitt","Splitt"),labels=c("sand","mixture","grit")))+
  geom_text(data=R2_mat,aes(y= -1,x=6000,label = paste0("R² = ",round(R2,3))),hjust="right")+
  #geom_text(data=subset(DS_D0_label,material%in% data_sub$material),aes(y= -1,x=6000,label = label),hjust="right")+
  scale_color_manual(values=2)+
  #annotate("text",y= c(-1,-1,-1),x=c(6000,6000,6000),label=c(label1,label2,label3),hjust="right")+
  labs(x=expression(CO[2]*" [ppm]"),y="depth [cm]",col="",fill="")+theme_bw()#+
  #coord_cartesian(clip = "off") +geom_text(x=-500,y=3.5,size=5,label=c("b)",rep(NA,23)))#+
  #labs(title="b)")+
  #theme(plot.title = element_text(hjust=-0.05,vjust=-8),plot.margin = unit(c(0,1,1,1), "lines"))
  #ggsave(paste0(plotpfad,"comsol_mod_obs.png"),width = 7,height=3)

ggpubr::ggarrange(img_plot,mod_obs_plot,widths=c(1,3))+
  ggsave(paste0(plotpfad,"Fig_2_COMSOL_mod_obs.jpg"),width = 7,height=2.7)
    #############################
#Figure 3 DS Labor und sandkiste
##############################
ggplot()+
  geom_point(data=subset(results,material!="leer"),aes(factor(material,levels=c("sand","splitt und sand","splitt"),labels=c("sand","mixture","grit")),DSD0,col="Lab",shape="Lab"))+
  geom_point(data=comsol,aes(factor(material,levels=c("sand","splitt und sand","splitt"),labels=c("sand","mixture","grit")),DS_D0_mod,col="in situ",shape="in situ"))+labs(y=expression(D[S]/D[0]),x="",col="",shape="")+scale_color_manual(values=1:2)+ylim(c(0,0.3))+ggsave(paste0(plotpfad,"Labor_Vergleich.tiff"),width=5,height=3)

##########################
#Figure 4 Rawdata
##########################
#inj ref
all_dpths <- sort(c(unique(data$tiefe),unique(-soil_agg$tiefe)))[-1]
inj_2u3 <- ggplot(subset(data,date > range2u3[1] & date < range2u3[2]))+
  #geom_rect(data=subset(Pumpzeiten,Pumpstufe==0 & (is.na(bemerkung)|bemerkung=="zurück getauscht") & Position %in% 8),aes(xmin=min(start),xmax=max(ende),ymin=-Inf,ymax=Inf,col="Zoom extend",fill="Zoom extend"),alpha=0.2)+
  geom_rect(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="injection\nperiod",col="injection\nperiod"),alpha=0.2)+
  scale_fill_manual("",values=c(1,NA))+
  scale_color_manual("",values=c(NA,1))+
  ggnewscale::new_scale_color()+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  labs(col="depth [cm]",x="",y=expression(CO[2]*" [ppm]"),title="a) injection sampler",fill="")+
  theme(axis.text.x = element_blank(),
        legend.title = element_text(color=1))+
  #scale_color_discrete(limits=all_dpths)+
  guides(colour = guide_legend(override.aes = list(size = 1.5)))


ref_2u3 <- ggplot(subset(data,date > range2u3[1] & date < range2u3[2]))+
  #geom_rect(data=subset(Pumpzeiten,Pumpstufe==0 & (is.na(bemerkung)|bemerkung=="zurück getauscht") & Position %in% 8),aes(xmin=min(start),xmax=max(ende),ymin=-Inf,ymax=Inf,col="Zoom extend",fill="Zoom extend"),alpha=0.2)+
  geom_rect(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="injection period",col="injection period"),alpha=0.2)+
  scale_fill_manual("",values=c(1,NA))+
  scale_color_manual("",values=c(NA,1))+
  ggnewscale::new_scale_color()+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  guides(col=F,fill=F)+labs(y=expression(CO[2]*" [ppm]"),x="",title="b) reference sampler")+
  theme(axis.text.x = element_blank())#+
  #scale_color_discrete(limits=all_dpths)


##########################
#klimadaten dazu
data$Precip_Last24hrs_mm[data$Precip_Last24hrs_mm < 0] <- 0

###########
#T
T_plot_2u3 <-  ggplot(subset(soil_agg,date > range2u3[1] & date < range2u3[2] & tiefe %in% c(2,5,10,20,50)))+
  geom_line(aes(date,mean_T,col=as.factor(-tiefe)))+
  guides(col=F)+
  labs(title="d)",x="date",y="Soil T [°C]",col="depth [cm]")#+
  #scale_color_discrete(limits=all_dpths)

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
  labs(title="c)",x="",y="SWC [Vol. %]",col="depth [cm]")+
  guides(colour = guide_legend(override.aes = list(size = 1.5)))
  #scale_color_discrete(limits=all_dpths)

leg1 <- get_legend(inj_2u3)
legend1 <- as_ggplot(leg1)
leg2 <- get_legend(VWC_plot_2u3)
legend2 <- as_ggplot(leg2)
leg <- ggpubr::ggarrange(legend1,legend2,ncol=1,align = "v")


rawdata_no_leg <- egg::ggarrange(inj_2u3+theme(legend.position = "none"),ref_2u3+theme(legend.position = "none"),VWC_plot_2u3+guides(col=F),T_plot_2u3,ncol=1,heights = c(1.5,1.5,1,1),draw=F)
ggpubr::ggarrange(rawdata_no_leg,leg,widths=c(5,1))+ggsave(paste0(plotpfad,"Fig_3_Rawdata.jpg"),width=7,height=7)


#CO2_p_VWC2u3 <- ggpubr::ggarrange(inj_2u3,ref_2u3,VWC_plot_2u3,T_plot_2u3,ncol=1,heights = c(1.5,1.5,1,1),common.legend = T,legend="right",align="v")+ggsave(paste0(plotpfad,"CO2_p_VWC_t.tiff"),width=7,height=7)

###########################################
#Figure 5 gam calib
############################################
# 
# ggplot(subset(sub3_calib,tiefenstufe %in% c(3)))+
#   geom_vline(xintercept = Pumpzeiten$start[17:18],col="grey")+
#   geom_line(aes(date,CO2_roll_inj,col="inj"),lwd=1)+
#   geom_line(aes(date,CO2_roll_ref,col="ref"))+
#   geom_line(aes(date,preds2,col="ref adj"))+
#   labs(y=expression(CO[2]*" [ppm]"),col="")+
#   facet_grid(.~paste("depth = ",tiefe," cm"),scales="free")+
#   scale_color_discrete(l=55)+
#   ggsave(paste0(plotpfad,"Einspeisung3_gam_calib.tiff"),width=7,height=3)

########################################
#Figure 6 a) Co2 tracer Einspeisung
#######################################

# ggplot(sub2u3)+
#   geom_vline(xintercept = Pumpzeiten$start[c(11:14,17:18)],col="grey")+
#   geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
#   geom_line(aes(date,preds2,col=as.factor(tiefe),linetype="ref adj"))+
#   geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds2,fill=as.factor(tiefe)),alpha=0.3)+
#   labs(y=expression(CO[2]*" [ppm]"),col="depth [cm]",linetype="sampler",fill="depth [cm]")+
#   ggsave(paste0(plotpfad,"Einspeisung2u3_gam.tiff"),width=7,height=3)

########################################
#Figure 6 b) Co2 tracer adj grob size
#######################################

plt_data <- leave_NAtime_plot(y="CO2_tracer_gam",data=subset(data,date > range2u3[1] & date < range2u3[2]&Pumpstufe!=0&tiefe!=0),breaks="2 day",timestep = 60*24,plot=F)
tracer_plot <- ggplot(plt_data)+
  
  #geom_vline(xintercept = Pumpzeiten$start[-1])+
  #geom_vline(xintercept = Pumpzeiten$start[c(11:14,17,18)],col="grey")+
  #annotate("text",x=Pumpzeiten$start[c(11,13,17)],y=Inf,label=paste("injection",1:3),vjust=1,hjust=-0.1)+
  geom_line(aes(date,CO2_tracer_gam,col=as.factor(tiefe)))+
  labs(col="depth [cm]",x="",y=expression(CO[2]*"tracer [ppm]"))+
  facet_wrap(~factor(period,levels=1:3,labels=paste0("injection ",1:3,c("\nhigh fluctuations in topsoil","\nbetter but still no stable signal","\nclear tracer profile"))),scales="free_x",nrow=1)+theme_bw()+
  guides(colour = guide_legend(override.aes = list(size = 1.5)))
jpeg(paste0(plotpfad,"CO2_tracer_gam.jpg"),width=7,height=3,units="in",res=300)
adj_grob_size(tracer_plot,plt_data,breaks="1 day",date_label="%b %d")
dev.off()

######################################
#Figure 7 Ds profile over time
######################################

DS_long_roll$range2 <- factor(DS_long_roll$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))
soil_agg_plot$range2 <- factor(soil_agg_plot$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))

DS_plot <- ggplot(subset(soil_agg_plot))+
  geom_ribbon(aes(x=date,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill=as.factor(range2)),alpha=0.15)+
  geom_line(aes(date,DSD0_PTF,col=as.factor(range2),linetype="f(eps)"))+
  geom_line(data=subset(DS_long_roll, date > pos8_date),aes(date,DSD0_roll,col=range2,linetype="in situ"))+
  geom_line(data=subset(DS_long_roll, Versuch == "2"),aes(date,DSD0_roll2,col=range2,linetype="in situ"))+
  #geom_line(data=subset(DS_long_roll, Versuch == "1" & id == 1),aes(date,DSD0_roll2,col=range,linetype="in situ"),alpha=0.3)+
  geom_line(data=subset(DS_long_roll, Versuch == "1" & id != 1),aes(date,DSD0_roll2,col=range2,linetype="in situ"))+
  labs(y=expression(D[S]/D[0]),col="depth [cm]",fill="depth [cm]",linetype="")+
  #xlim(range(DS_long$date))+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = range(DS_long$date))+
  scale_linetype_manual(values=2:1,labels=c(expression(f~(epsilon),"in situ")))+
  ggsave(file=paste0(plotpfad,"DS_plot_gam_feps.jpg"),width=7,height = 3.5)
######################################
#Figure 8 Flux
######################################
#ggplot(subset(F_df,Versuch!="1"))+geom_line(aes(date,Fz_roll2_10_17 / Fz_roll2))

Kammer_flux_agg <- Kammer_flux %>% group_by(day) %>% summarise(CO2flux = mean(CO2flux),CO2flux_max=max(CO2flux_max),CO2flux_min=min(CO2flux_min),date=mean(date))

mindate <- ymd_h("2020-07-06 16")
flux_plot <- ggplot(subset(Kammer_flux_agg,date < ymd_h("2020.07.17 00")))+
  geom_linerange(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,col=""))+
  geom_point(aes(date,CO2flux,col=""))+
  labs(col="chamber")+
  scale_color_manual(values=c(1))+
  ggnewscale::new_scale_color()+
  geom_line(data=subset(soil_wide,date<= "2020-07-24 08:20:00 UTC"),aes(date,zoo::rollapply(R_soil,20,mean,fill=NA),col=""),linetype=2)+
  geom_ribbon(data=subset(soil_wide,date<= "2020-07-24 08:20:00 UTC"),aes(x=date,ymin=R_min,ymax=R_max,fill=""),alpha=0.15)+
  scale_color_manual("transfer function\n(Maier et al., 2011)",values=grey(0.3))+
  scale_fill_manual("transfer function\n(Maier et al., 2011)",values=grey(0.3))+
  ggnewscale::new_scale_color()+
  #geom_line(data=subset(F_df,Versuch != "1"),aes(date,Fz,col="0-10 cm"),alpha=0.2)+
  #geom_line(data=subset(F_df),aes(date,Fz_10_17,col="10-20 cm"),alpha=0.2)+
  geom_line(data=subset(F_df,date > pos8_date),aes(date,Fz_roll,col="0-10 cm"))+
  geom_line(data=subset(F_df,date > pos8_date),aes(date,Fz_roll_10_17,col="10-20 cm"))+
  geom_line(data=subset(F_df,Versuch %in% 1:3 & date > mindate),aes(date,Fz_roll_ab20,col="below 20 cm"))+
  geom_line(data=subset(F_df,Versuch %in% 2) ,aes(date,Fz_roll2,col="0-10 cm"))+
  geom_line(data=subset(F_df,Versuch %in% 1:2),aes(date,Fz_roll2_10_17,col="10-20 cm"))+
    #scale_color_brewer("gradient method",type="qual",palette=6)+
  guides(col=F)+
  #scale_color_manual("gradient method",values=1:2)+
  #xlim(ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  #xlim(range(DS_long$date))+
  labs(y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max),col=1,width=10000)


flux_plot
Fig5u6 <- egg::ggarrange(DS_plot+ labs(x="")  +scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC"))),flux_plot,ncol=1,draw=F)
jpeg(file=paste0(plotpfad,"Fig5u6.jpg"),width=7,height = 5,units="in",res=300)
Fig5u6
dev.off()

ggplot(subset(Kammer_flux,date < ymd_h("2020.07.17 00")))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,col=kammer),width=10000)+
  geom_point(aes(date,CO2flux,col=kammer))+
  labs(col="chamber")+
  ggnewscale::new_scale_color()+
  geom_line(data=subset(soil_wide,date<= "2020-07-24 08:20:00 UTC"),aes(date,zoo::rollapply(R_soil,20,mean,fill=NA),col=""),linetype=2)+
  geom_ribbon(data=subset(soil_wide,date<= "2020-07-24 08:20:00 UTC"),aes(x=date,ymin=R_min,ymax=R_max,fill=""),alpha=0.15)+
  scale_color_manual("transfer function\n(Maier et al., 2011)",values=grey(0.3))+
  scale_fill_manual("transfer function\n(Maier et al., 2011)",values=grey(0.3))+
  ggnewscale::new_scale_color()+
  geom_line(data=subset(F_df,Versuch != "1"),aes(date,Fz,col="0-10 cm"),alpha=0.2)+
  geom_line(data=subset(F_df),aes(date,Fz_10_17,col="10-20 cm"),alpha=0.2)+
  geom_line(data=subset(F_df,date > pos8_date),aes(date,Fz_roll,col="0-10 cm"))+
  geom_line(data=subset(F_df,date > pos8_date),aes(date,Fz_roll_10_17,col="10-20 cm"))+
  geom_line(data=subset(F_df,Versuch == "2") ,aes(date,Fz_roll2,col="0-10 cm"))+
  geom_line(data=subset(F_df,date < pos8_date),aes(date,Fz_roll2_10_17,col="10-20 cm"))+
    scale_color_brewer("gradient method",type="qual",palette=6)+
  #scale_color_manual("gradient method",values=1:2)+
  #xlim(ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  #xlim(range(DS_long$date))+
  labs(y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))+
    #theme(legend.position = "top")#+
ggsave(paste0(plotpfad,"Flux_Kammer_Comsol_gam_3DS.jpg"),width=7,height = 3.5)
