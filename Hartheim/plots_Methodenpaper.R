


#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_ms <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Methodenpaper/")
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
#load(paste0(comsolpfad,"plotdata_Methodenpaper.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper_drift.RData"))
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

data %>% group_by(Position,Pumpstufe) %>% summarise(injectionrate=mean(Fz))#ml per min

##################################################################################
#plots
##################################################################################
#############################
#Figure 2 COMSOL und DS sandkiste
##############################
img <- png::readPNG("../../Dokumentation/Berichte/Methodenpaper_sampler/Abbildungen/Sandboxplot3D.png")
plot_minimal <- ggplot()+theme_minimal()+labs(title="a)")
dim(img)
img_plot <- ggdraw()+draw_image(img,height=1,width=2,x=-0.3,y=0.15)+
  #draw_text("a)",0.1,0.9)
  draw_plot(plot_minimal)


col<-scales::hue_pal()

mod_obs_plot <- 
  ggplot(subset(data_sub, Versuch %in% c(5,6,9)))+
  #geom_ribbon(aes(xmin=min_mod,xmax=max_mod,y=tiefe,fill="sweep"),alpha=0.3)+
  geom_line(aes(CO2_mod,tiefe,col=""))+
  geom_point(aes(CO2,tiefe,shape="no Psim"))+
  geom_point(data=subset(respi_wide,Versuch%in%c(5:6,9)),aes(CO2_tracer_calc,tiefe,shape="with Psim"),col=col(3)[2])+
  facet_wrap(~factor(material,levels=c("Sand","Splitt","Sand & Splitt"),labels=c("sand","grit","mixture")))+
#  geom_text(data=R2_mat,aes(y= -1,x=6000,label = paste0("R² = ",round(R2,3))),hjust="right")+
  scale_shape_manual("obs",labels=c(expression("no "*P[sim]),expression("with "*P[sim])),values=c(19,20))+
  guides(shape=guide_legend(override.aes = list(color=c(1,col(3)[2]))))+
  #geom_text(data=subset(DS_D0_label,material%in% data_sub$material),aes(y= -1,x=6000,label = label),hjust="right")+
  #scale_color_manual(values=2)+
  #annotate("text",y= c(-1,-1,-1),x=c(6000,6000,6000),label=c(label1,label2,label3),hjust="right")+
  labs(x=expression(CO[2]*" [ppm]"),y="depth [cm]",col="mod",fill="")+theme_bw()#+
  #coord_cartesian(clip = "off") +geom_text(x=-500,y=3.5,size=5,label=c("b)",rep(NA,23)))#+
  #labs(title="b)")+
  #theme(plot.title = element_text(hjust=-0.05,vjust=-8),plot.margin = unit(c(0,1,1,1), "lines"))
  #ggsave(paste0(plotpfad_ms,"comsol_mod_obs.png"),width = 7,height=3)
mod_obs_plot 

ggplot(respi_wide)+geom_point(aes(CO2_tracer,CO2_tracer_calc))
R2(respi_wide$CO2_tracer,respi_wide$CO2_tracer_calc)

  ggpubr::ggarrange(img_plot,mod_obs_plot,widths=c(1,3))+
  ggsave(paste0(plotpfad_ms,"Fig_2_COMSOL_mod_obs.jpg"),width = 7,height=2.7)



lims <- c("Psim","tracer","tracer + prod")
lims2 <- c(expression(P[sim]),"tracer",expression("tracer + "*P[sim]))
# respi_wide[nrow(respi_wide)+1,] <- NA
# respi_wide[nrow(respi_wide),"material"] <- "Sand & Splitt"
# respi_wide[nrow(respi_wide),"Versuch"] <- 9

resp_plot <- ggplot(subset(respi_wide,Versuch%in%c(5:6)))+
  geom_ribbon(aes(xmin=CO2_atm,xmax=CO2_respi,y=tiefe,fill=lims[1]),alpha=0.2)+
  geom_ribbon(aes(xmin=CO2_respi,xmax=CO2_ges,y=tiefe,fill=lims[2]),alpha=0.3)+
  geom_point(aes(x=CO2_respi,y=tiefe,col=lims[1]))+
  geom_point(aes(x=CO2_ges,y=tiefe,col=lims[3]))+
  #  geom_line(aes(x=CO2_tracer_calc,y=tiefe,col="CO2tracer"))+
  labs(fill=expression(CO[2]), x="", y="depth [cm]",col=expression(CO[2]))+
  guides(col=guide_legend(override.aes = list(shape=c(19,NA,19),fill=c(NA,col(3)[2],NA))))+
  scale_fill_manual(limits=c(lims),labels=lims2,values=col(3)[c(1,2,3)])+
  scale_color_manual(limits=c(lims),labels=lims2,values=col(3)[c(1,2,3)])+
  theme_bw()+
  theme(legend.text.align = 0)+
  #xlim(c(min(respi_wide$CO2_atm),max(respi_wide$CO2_ges)))+
  facet_wrap(~factor(material,levels=c("Sand","Splitt"),labels=c("sand","grit")))
    

#img_respi <- ggpubr::ggarrange(img_plot,resp_plot,widths=c(1,3))
#egg::ggarrange(resp_plot,mod_obs_plot,ncol = 1,widths=c(2,3))

cowplt <- cowplot::ggdraw()+cowplot::draw_plot(resp_plot,x=0,y=0.5,height=0.5,width=0.82)+cowplot::draw_plot(mod_obs_plot,height=0.5)+
  cowplot::draw_text("b)",x=0.,y=0.95,hjust=0)+
  cowplot::draw_text("c)",x=0.,y=0.45,hjust=0)
#cowplt
ggpubr::ggarrange(img_plot,cowplt,widths = c(1,3))+
  ggsave(paste0(plotpfad_ms,"Fig_2_COMSOL_mod_obs_prod.jpg"),width = 7,height=4)
#############################
#Figure 3 DS Labor und sandkiste
##############################
ggplot()+
  geom_point(data=subset(results,material!="leer"),aes(factor(material,levels=c("sand","splitt und sand","splitt"),labels=c("sand","mixture","grit")),DSD0,col="Lab",shape="Lab"))+
  geom_point(data=comsol,aes(factor(material,levels=c("sand","splitt und sand","splitt"),labels=c("sand","mixture","grit")),DS_D0_mod,col="in situ",shape="in situ"))+labs(y=expression(D[S]/D[0]),x="",col="",shape="")+scale_color_manual(values=1:2)+ylim(c(0,0.3))+ggsave(paste0(plotpfad_ms,"Labor_Vergleich.tiff"),width=5,height=3)

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
  geom_line(aes(date,CO2_inj,col=as.factor(-tiefe)))+
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
  geom_line(aes(date,CO2_ref,col=as.factor(-tiefe)))+
  guides(col=F,fill=F)+labs(y=expression(CO[2]*" [ppm]"),x="",title="b) reference sampler")+
  theme(axis.text.x = element_blank())#+
  #scale_color_discrete(limits=all_dpths)


##########################
#klimadaten dazu
data$Precip_Last24hrs_mm[data$Precip_Last24hrs_mm < 0] <- 0

###########
#T
T_plot_2u3 <-  ggplot(subset(soil_agg,date > range2u3[1] & date < range2u3[2] & tiefe %in% c(2,5,10,20,50)))+
  geom_line(aes(date,mean_T,col=as.factor(tiefe)))+
  guides(col=F)+
  labs(title="d)",x="date",y="Soil T [°C]",col="depth [cm]")#+
  #scale_color_discrete(limits=all_dpths)

################
#VWC P
sec_ax_fac <- 1.25
VWC_plot_2u3 <-  ggplot(subset(soil_agg,date > range2u3[1] & date < range2u3[2] & tiefe %in% c(2,5,10,20,50)))+
  geom_ribbon(data=subset(data,date > range2u3[1] & date < range2u3[2]),aes(x=date,ymin=0,ymax=Precip_Last24hrs_mm/sec_ax_fac),fill="blue",alpha=0.8)+
  geom_line(aes(date,mean_VWC,col=as.factor(tiefe)))+
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
ggpubr::ggarrange(rawdata_no_leg,leg,widths=c(5,1))+ggsave(paste0(plotpfad_ms,"Fig_3_Rawdata.jpg"),width=7,height=7)


#CO2_p_VWC2u3 <- ggpubr::ggarrange(inj_2u3,ref_2u3,VWC_plot_2u3,T_plot_2u3,ncol=1,heights = c(1.5,1.5,1,1),common.legend = T,legend="right",align="v")+ggsave(paste0(plotpfad_ms,"CO2_p_VWC_t.tiff"),width=7,height=7)

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
#   ggsave(paste0(plotpfad_ms,"Einspeisung3_gam_calib.tiff"),width=7,height=3)

########################################
#Figure 6 a) Co2 tracer Einspeisung
#######################################

# ggplot(sub2u3)+
#   geom_vline(xintercept = Pumpzeiten$start[c(11:14,17:18)],col="grey")+
#   geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
#   geom_line(aes(date,preds2,col=as.factor(tiefe),linetype="ref adj"))+
#   geom_ribbon(aes(date,ymax=CO2_inj,ymin=preds2,fill=as.factor(tiefe)),alpha=0.3)+
#   labs(y=expression(CO[2]*" [ppm]"),col="depth [cm]",linetype="sampler",fill="depth [cm]")+
#   ggsave(paste0(plotpfad_ms,"Einspeisung2u3_gam.tiff"),width=7,height=3)

########################################
#Figure 6 b) Co2 tracer adj grob size
#######################################

plt_data <- leave_NAtime_plot(y="CO2_tracer_gam",data=subset(data,date > range2u3[1] & date < range2u3[2]&Pumpstufe!=0&tiefe!=0),breaks="2 day",timestep = 60*24,plot=F)
tracer_plot <- ggplot(plt_data)+
  
  #geom_vline(xintercept = Pumpzeiten$start[-1])+
  #geom_vline(xintercept = Pumpzeiten$start[c(11:14,17,18)],col="grey")+
  #annotate("text",x=Pumpzeiten$start[c(11,13,17)],y=Inf,label=paste("injection",1:3),vjust=1,hjust=-0.1)+
  geom_line(aes(date,CO2_tracer_gam,col=as.factor(-tiefe)))+
  labs(col="depth [cm]",x="",y=expression(CO[2]*"tracer [ppm]"))+
  facet_wrap(~factor(period,levels=1:3,labels=paste0("injection ",1:3,c("\nhigh fluctuations in topsoil","\nbetter but still no stable signal","\nclear tracer profile"))),scales="free_x",nrow=1)+theme_bw()+
  guides(colour = guide_legend(override.aes = list(size = 1.5)))
jpeg(paste0(plotpfad_ms,"Fig_4_CO2_tracer.jpg"),width=7,height=3,units="in",res=300)
adj_grob_size(tracer_plot,plt_data,breaks="1 day",date_label="%b %d")
dev.off()

######################################
#Figure 7 Ds profile over time
######################################

h_steady <- 24
DS_long_roll$range2 <- factor(DS_long_roll$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))
soil_agg_plot$range2 <- factor(soil_agg_plot$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))

ds_sub <- subset(DS_long_roll, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))
F_sub <- subset(F_df, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))


DS_plot <- ggplot(subset(soil_agg_plot))+
  geom_ribbon(aes(x=date,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill=as.factor(range2)),alpha=0.15)+
  geom_line(aes(date,DSD0_PTF,col=as.factor(range2),linetype="f(eps)"))+
  #hours till steady state
  #geom_line(data=subset(DS_long_roll, date > pos8_date ),aes(date,DSD0_roll,col=range2,linetype="in situ"),alpha=0.3)+
  #geom_line(data=subset(DS_long_roll, Versuch == "2"),aes(date,DSD0_roll,col=range2,linetype="in situ"),alpha=0.3)+
  geom_line(data=ds_sub,aes(date,DSD0_roll,col=range2,linetype="in situ"))+
  labs(y=expression(D[S]/D[0]),col="depth [cm]",fill="depth [cm]",linetype="")+
  #xlim(range(DS_long$date))+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = range(DS_long$date))+
  scale_linetype_manual(values=2:1,labels=c(expression(f~(epsilon),"in situ")))#+
#  ggsave(file=paste0(plotpfad_ms,"DS_plot_gam_feps.jpg"),width=7,height = 3.5)
DS_plot
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
  scale_color_manual("simple T and SWC model",values=grey(0.3))+
  scale_fill_manual("simple T and SWC model",values=grey(0.3))+
  ggnewscale::new_scale_color()+
  #geom_line(data=subset(F_df,Versuch != "1"),aes(date,Fz,col="0-10 cm"),alpha=0.3)+
  #geom_line(data=subset(F_df),aes(date,Fz_10_17,col="10-20 cm"),alpha=0.3)+
  geom_line(data=F_sub,aes(date,Fz_roll_0_10_adj,col="0-10 cm"))+
  #geom_line(data=F_sub,aes(date,Fz_roll2,col="0-10 cm"))+

  geom_line(data=F_sub,aes(date,Fz_roll_10_17,col="10-20 cm"))+
  guides(col=F)+
  #scale_color_manual("gradient method",values=1:2)+
  #xlim(ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  #xlim(range(DS_long$date))+
  labs(y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max),col=1,width=10000)


flux_plot
#flux_plot
Fig5u6 <- egg::ggarrange(DS_plot+ labs(x="")  +scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC"))),flux_plot,ncol=1,draw=F)
jpeg(file=paste0(plotpfad_ms,"Fig5u6_drift_adj.jpg"),width=7,height = 5,units="in",res=300)
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
ggsave(paste0(plotpfad_ms,"Flux_Kammer_Comsol_gam_3DS.jpg"),width=7,height = 3.5)


#######################
#

efflux_plot <- ggplot(subset(Kammer_flux_agg,date < ymd_h("2020.07.17 00")))+
  geom_linerange(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,col=""))+
  geom_point(aes(date,CO2flux,col=""))+
  labs(col="chamber")+
  scale_color_manual(values=c(1))+
  ggnewscale::new_scale_color()+
  geom_line(data=subset(soil_wide,date<= "2020-07-24 08:20:00 UTC"),aes(date,zoo::rollapply(R_soil,20,mean,fill=NA),col=""),linetype=2)+
  geom_ribbon(data=subset(soil_wide,date<= "2020-07-24 08:20:00 UTC"),aes(x=date,ymin=R_min,ymax=R_max,fill=""),alpha=0.15)+
  scale_color_manual("simple T and SWC model",values=grey(0.3))+
  scale_fill_manual("simple T and SWC model",values=grey(0.3))+
  ggnewscale::new_scale_color()+
  
  geom_line(data=subset(F_df,date > (Pumpzeiten$start[17] + 18*3600)),aes(date,Fz_roll,col="0-7 cm"))+
  geom_line(data=subset(F_df,date > (Pumpzeiten$start[17] + 18*3600)),aes(date,Fz_roll_0_10,col="3-10 cm"))+
  geom_line(data=subset(F_df,date > (Pumpzeiten$start[17] + 18*3600)),aes(date,Fz_roll_0_10_adj,col="3-10 cm adj"))+

  geom_line(data=subset(F_df,Versuch %in% 2 & date > (Pumpzeiten$start[13] + 18*3600)) ,aes(date,Fz_roll,col="0-7 cm"))+
  geom_line(data=subset(F_df,Versuch %in% 2 & date > (Pumpzeiten$start[13] + 18*3600)) ,aes(date,Fz_roll_0_10,col="3-10 cm"))+
  geom_line(data=subset(F_df,Versuch %in% 2 & date > (Pumpzeiten$start[13] + 18*3600)) ,aes(date,Fz_roll_0_10_adj,col="3-10 cm adj"))+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  labs(y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max),col=1,width=10000)


efflux_plot+ggsave(paste0(plotpfad_ms,"Flux_calc.jpg"),width=7,height = 5)

colnames(F_df)
ggplot(F_df)+
  geom_line(aes(date, CO2_ref_0,col="0"))+
  geom_line(aes(date, CO2_ref_1,col="1"))+
  geom_line(aes(date, CO2_ref_2,col="2"))+
  geom_line(aes(date, CO2_ref_3,col="3"))
ggplot(F_df)+
  geom_line(aes(date, CO2_ref_adj_0,col="0"))+
  geom_line(aes(date, CO2_ref_adj_1,col="1"))+
  geom_line(aes(date, CO2_ref_adj_2,col="2"))+
  geom_line(aes(date, CO2_ref_adj_3,col="3"))

ggplot(data.frame(x=seq_along(dC_dz_mol_0_10),y=dC_dz_mol_0_10_adj))+geom_line(aes(x,y))
test <- F_df[1,]
sub <- subset(data,date== test$date)
ggplot(subset(sub))+
  geom_line(aes(preds_drift,tiefe,col="ref_adj"))+
  geom_line(aes(CO2_roll_ref,tiefe,col="ref"))
  
fm <- glm(as.numeric(CO2_ref_mol_m3_adj)~tiefe,data=subset(sub,tiefe > -12))
fm$coefficients

rowMeans(cbind(test$CO2_ref_adj_1 - test$CO2_ref_adj_2,test$CO2_ref_adj_2 - test$CO2_ref_adj_3)/-3.5)
##########################
#ranges
data_sub
data$Wind <- zoo::rollapply(data$WindVel_30m_ms,60*6,mean,fill=NA)
axis_fac <- 10
wind_plot <- ggplot(subset(data,date > min(F_df$date) & date < max(F_df$date)))+
  geom_line(aes(date,Wind,linetype="windspeed"),col=grey(0.5))+
  geom_line(data=ds_sub,aes(date,DSD0_roll*axis_fac,col=range2,linetype="DSD0"))+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = range(DS_long$date))+
  scale_y_continuous(sec.axis = sec_axis(trans=~./axis_fac,name="DSD0"))+
  labs(y="Windspeed [m/s]",x="")


wind_plot+ggsave(paste0(plotpfad_ms,"DS_Wind.jpg"),width=7,height = 5)
ggpubr::ggarrange(DS_plot,wind_plot,ncol=1,align = "v")
egg::ggarrange(DS_plot+ labs(x="") ,wind_plot,ncol=1)
ds_sub %>% group_by(range) %>% summarise(min(DSD0_roll,na.rm=T),max(DSD0_roll,na.rm=T))
F_sub %>% summarise(min(Fz_roll_0_10_adj,na.rm=T),max(Fz_roll_0_10_adj,na.rm=T), mean(Fz_roll_10_17/Fz_roll_0_10,na.rm = T))
F_sub %>% summarise(min(Fz_roll_0_10_adj,na.rm=T),max(Fz_roll_0_10_adj,na.rm=T))
