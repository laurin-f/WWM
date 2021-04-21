


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
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr","png","cowplot","magick","ggforce")
check.packages(packages)
theme_set(theme_classic())

#daten laden
load(file=paste0(klimapfad,"klima_data.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(aufbereitete_ds,"Labor_Vergleich.RData"))
#aus "plots_COMSOL_out_Hartheim_sweep.R"
#load(paste0(comsolpfad,"plotdata_Methodenpaper.RData"))
#load(paste0(comsolpfad,"plotdata_Methodenpaper_drift.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper_roll.RData"))
load(paste0(comsolpfad,"sandkiste_sweep_data_sub.RData"))
load(paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))
load(paste0(datapfad_harth,"PPC_DS.RData"))

pos8_date <- min(data$date[which(data$Position ==8 & data$Pumpstufe != 0)])
range1 <- range(data$date[data$Position ==1],na.rm = T)
range2 <- range(data$date[data$Position ==7],na.rm = T)
range3 <- range(data$date[data$Position ==8],na.rm = T)
range2u3 <- range(data$date[data$Position %in% 7:8],na.rm = T)

F_df[which(F_df$date>pos8_date & F_df$date < (pos8_date + 20*3600)),c("DSD0_1","Fz","Fz_roll")] <- NA
DS_long_roll$DSD0_roll[which(DS_long_roll$date>pos8_date & DS_long_roll$date < (pos8_date + 20*3600) & DS_long_roll$id == 1)] <- NA


data$preds_drift[data$tiefe == 0] <- data$CO2_roll_ref[data$tiefe == 0]
#anteil FCO2 10-17 an gesamt respiration
F_2u3 <- subset(F_df,Versuch!="1")

data %>% group_by(Position,Pumpstufe) %>% summarise(injectionrate=mean(Fz))#ml per min

h_steady <- 18

# DS_long_roll <- DS_anisotrop_long_roll
# DS_long_roll$DSD0_roll <- DS_long_roll$DSD0
# ranges <- c("0 to -10","-10 to -20","> -20")
# 
# DS_long_roll$range <- factor(DS_long_roll$tiefe,levels=1:3,labels=ranges)
# 
DS_long_roll$range2 <- factor(DS_long_roll$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))
soil_agg_plot$range2 <- factor(soil_agg_plot$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))
pos8_date <- min(data$date[which(data$Position ==8 & data$Pumpstufe != 0)])
#Versuch2_date <- ymd_h("2020.07.10 00")
#DS_long_roll$Versuch <- ifelse(DS_long_roll$date < pos8_date,ifelse(DS_long_roll$date < Versuch2_date,"1","2"),"3")
# 
# ds_sub <- subset(DS_long_roll)
ds_sub <- subset(DS_long_roll, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))
F_sub <- subset(F_df, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))

data$Wind <- RcppRoll::roll_mean(data$WindVel_30m_ms,60*4,fill=NA)

Pumpzeiten$Position[10:11] <- NA

# for(i in 1:7){
#   F_df[,c(paste0("Fz_",i))] <- as.numeric((F_df[,c(paste0("CO2_ref_",i-1))] - F_df[,c(paste0("CO2_ref_",i))])/-3.5 *
#     ifelse(i < 3,F_df$DS_1,ifelse(i < 6,F_df$DS_2, F_df$DS_3)) * 100 * 10^6)
# }
# 
# F_long <- tidyr::pivot_longer(F_df,matches("Fz_\\d$"),names_to = "tiefe",values_to = "flux",names_prefix = "Fz_")
# ggplot(F_long)+
#   geom_line(aes(date,flux,col=tiefe))
# ggplot(subset(F_long,date %in% calm_dates))+geom_path(aes(flux,as.numeric(tiefe),col=as.factor(date)))+guides(col=F)

F_sub$P_0_10 <- F_sub$Fz_roll_0_10 - F_sub$Fz_roll_10_17
F_sub$P_10_20 <- F_sub$Fz_10_17
F_long <- tidyr::pivot_longer(F_sub,matches("P_\\d+"),names_to = "tiefe",values_to = "P",names_prefix = "P_")
F_long$tiefe2 <- factor(F_long$tiefe,levels=c("0_10","10_20"),labels=c("0-10 cm","10-20 cm"))
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

cowplt <- cowplot::ggdraw()+cowplot::draw_plot(resp_plot,x=0,y=0.5,height=0.5,width=0.82)+cowplot::draw_plot(mod_obs_plot,height=0.5)+
  cowplot::draw_text("a)",x=0.01,y=0.95,hjust=0)+
  cowplot::draw_text("b)",x=0.01,y=0.45,hjust=0)+
  ggsave(paste0(plotpfad_ms,"Fig_2_COMSOL_mod_obs_prod.jpg"),width = 7,height=5)

###########################
# Figure 4 heterogeneity
#############################

#ggplot(subset(data,tiefe==-24.5 & !is.na(Position)))+
#  geom_vline(data=subset(Pumpzeiten,!is.na(bemerkung)),aes(xintercept=start))+
#  geom_text(data=subset(Pumpzeiten,!is.na(bemerkung)),aes(x=start,y=7000+300*1:9,label=bemerkung),hjust=0)+
#  geom_line(aes(date,CO2_roll_inj,col=as.factor(Position)))+
#  geom_line(aes(date,CO2_roll_ref,col="ref"))+
#  geom_vline(xintercept = ymd_h("2020.07.09 00"),col=2)+
#  xlim(ymd_h("2020.07.09 00")+c(-10,10)*3600*24)
#dates <- ymd_h(c("2020.05.20 00","2020.06.20 00","2020.06.25 00","2020.07.09 15"))

Pump_sub <- subset(Pumpzeiten,Pumpstufe==0 & !grepl("regen|tauscht",Pumpzeiten$bemerkung))

data_sub_ls <- vector("list",nrow(Pump_sub))
for (i in 1:nrow(Pump_sub)) {
  #data_sub_ls[[i]] <-subset(data,date > Pump_sub$start[i] & date < (Pump_sub$ende[i]-3600*3))
  #data_sub_ls[[i]] <-subset(data,date > Pump_sub$start[i] & date < Pump_sub$start[i] + 3.8*24*3600)
  data_sub_ls[[i]] <-subset(data,date > (Pump_sub$ende[i]-3600*3-3.8*3600*24) & date < (Pump_sub$ende[i]-3600*3))
  
  data_sub_ls[[i]]$ID <- i
  
}

data_sub <- do.call(rbind,data_sub_ls)


data_sub2 <- subset(data_sub,ID %in% c(1,3,7) & !is.na(Position) & tiefe < 0)
data_sub_agg <- data_sub2 %>% group_by(ID,tiefe) %>% summarise_at(paste0("CO2_roll_",c("inj","ref")),list(min=min,max=max,mean=mean),na.rm=T)
data_sub_agg[data_sub_agg == Inf |data_sub_agg == -Inf] <- NA

tiefe_plt <-  ggplot(data_sub_agg)+
  geom_ribbon(aes(xmin=CO2_roll_ref_min,xmax=CO2_roll_ref_max,y=tiefe,fill=factor(ID,levels = c(1,3,7),labels=c("B","C","D"))),alpha=0.2)+
  geom_ribbon(aes(xmin=CO2_roll_inj_min,xmax=CO2_roll_inj_max,y=tiefe,fill="A"),alpha=0.2)+
  geom_line(aes(CO2_roll_ref_mean,tiefe,col=factor(ID,levels = c(1,3,7),labels=c("B","C","D"))))+
  geom_line(aes(CO2_roll_inj_mean,tiefe,col="A"))+
  facet_wrap(~factor(ID,levels = c(1,3,7),labels=paste("profile",c("A + B","A + C","A + D"))))+
  labs(x=expression(CO[2]~"[ppm]"),fill="profile",col="profile",y="depth [cm]")+theme_bw()


Sys.setlocale("LC_ALL","English")

time_plt_adj <- ggplot(subset(data_sub,ID %in% c(1,3,7) & !is.na(Position)& tiefe < 0))+
  geom_line(aes(date,preds_drift,col=as.factor(-tiefe),linetype="B/C/D  adj"))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(-tiefe),linetype="A"))+
  #facet_wrap(~factor(ID,levels = c(1,3,7),labels=paste("location",1:3)),scales="free")+
  facet_wrap(~factor(ID,levels = c(1,3,7),labels=paste("profile",c("A + B","A + C","A + D"))),scales="free")+
  labs(x="",y=expression(CO[2]~"[ppm]"),col="depth [cm]",linetype="profile")+
  scale_x_datetime(date_breaks = "2 days",date_labels = "%b %d")+
  theme_bw()

leg1 <- ggpubr::get_legend(tiefe_plt)
legend1 <- ggpubr::as_ggplot(leg1)

leg2_adj <- ggpubr::get_legend(time_plt_adj)
legend2_adj <- ggpubr::as_ggplot(leg2_adj)
leg_adj <- ggpubr::ggarrange(legend1,legend2_adj,heights = c(1,1.5),ncol=1,align = "v")


no_leg_adj <- ggpubr::ggarrange(tiefe_plt+theme(legend.position = "none"),time_plt_adj+theme(legend.position = "none"),ncol=1,align="v") 
#ggpubr::ggarrange(no_leg_adj,leg_adj,widths = c(8,2))
ggpubr::ggarrange(no_leg_adj,leg_adj,widths = c(8,2))+ggsave(paste0(plotpfad_ms,"heterogeneity_adj.jpg"),width=7,height=5)
Sys.setlocale("LC_ALL","")


##########################
#Figure 5 Rawdata
##########################
#inj ref
#all_dpths <- sort(c(unique(data$tiefe),unique(-soil_agg$tiefe)))[-1]
data$date_hour <- round_date(data$date,"hours")

starts <- as.data.frame(Pumpzeiten)[c(1,5,12),"start"]
enden <- as.data.frame(Pumpzeiten)[c(1,5,18),"ende"]
Pumpzeiten$period <- NA
Pumpzeiten$period[12:18] <- 3
data$period <- NA
soil_agg$period <- NA
for(i in seq_along(starts)){
  data$period[data$date >= starts[i]+3*3600 & data$date <= enden[i]-3*3600] <- i
  soil_agg$period[soil_agg$date >= starts[i]+3*3600 & soil_agg$date <= enden[i]-3*3600] <- i
  
}
data_agg <- data %>% 
  filter(!is.na(period)) %>% 
  group_by(date_hour,tiefe) %>% summarise_all(mean)


#inj_2u3 <- ggplot(subset(data,date > range2u3[1] & date < range2u3[2]))+
inj_2u3 <- ggplot(data_agg)+
  #geom_rect(data=subset(Pumpzeiten,Pumpstufe==0 & (is.na(bemerkung)|bemerkung=="zurück getauscht") & Position %in% 8),aes(xmin=min(start),xmax=max(ende),ymin=-Inf,ymax=Inf,col="Zoom extend",fill="Zoom extend"),alpha=0.2)+
  geom_rect(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="soil gas \ntransport \nmeasurements",col="soil gas \ntransport \nmeasurements"),alpha=0.15)+
  geom_text(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(x=start,y=7700,label=paste("injection")),hjust = 0,vjust = 0.5,size=3)+
  scale_fill_manual("",values=c(1,NA))+
  scale_color_manual("",values=c(NA,1))+
  ggnewscale::new_scale_color()+
  geom_line(aes(date,CO2_inj,col=as.factor(-tiefe)))+
  labs(col="depth [cm]",x="",y=expression(CO[2]*" [ppm]"),title="a) profile A",fill="")+
  theme(axis.text.x = element_blank(),
        legend.title = element_text(color=1))+
  #scale_color_discrete(limits=all_dpths)+
  guides(colour = guide_legend(override.aes = list(size = 1.5)))+
  facet_wrap(~period,scales="free_x")+
  scale_x_datetime(date_breaks = "2 days")+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )



inj_adj <- adj_grob_size(inj_2u3+theme(legend.position = "none"),breaks = "4 days",data_agg,date_labels=" ",plot=F)

#inj_plot <- cowplot::ggdraw()+cowplot::draw_plot(inj_adj,x=-0.005,width=0.94)
#inj_plot

ref_2u3 <- ggplot(data_agg)+
  #geom_rect(data=subset(Pumpzeiten,Pumpstufe==0 & (is.na(bemerkung)|bemerkung=="zurück getauscht") & Position %in% 8),aes(xmin=min(start),xmax=max(ende),ymin=-Inf,ymax=Inf,col="Zoom extend",fill="Zoom extend"),alpha=0.2)+
  geom_rect(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="soil gas \ntransport \nmeasurements"),alpha=0.15)+
  geom_text(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(x=start,y=Inf,label="reference"),hjust = 0,vjust = 1.1,size=3)+
  scale_fill_manual("",values=c(1,NA))+
  scale_color_manual("",values=c(NA,1))+
  ggnewscale::new_scale_color()+
  geom_line(aes(date,CO2_ref,col=as.factor(-tiefe)))+
  guides(col=F,fill=F)+labs(y=expression(CO[2]*" [ppm]"),x="",title="b) profile B/C/D")+
  theme(axis.text.x = element_blank())+
  facet_wrap(~factor(period,levels=1:3,labels = paste("profile",c("B","C","D"))),scales="free_x")+
  scale_x_datetime(date_breaks = "2 days")+
  theme_bw()
  #scale_color_discrete(limits=all_dpths)

ref_adj <- adj_grob_size(ref_2u3+theme(legend.position = "none"),data_agg,breaks = "4 days",date_labels=" ",plot=F)

##########################
#klimadaten dazu
data$Precip_Last24hrs_mm[data$Precip_Last24hrs_mm < 0] <- 0

###########
#T
PPC$PPC[PPC$date < ymd_h("2020.07.10 09")] <- NA
y_fac <- 2
PPC_fac <- 50
PPC_offset <- 0

T_plot_2u3 <-  ggplot(subset(soil_agg,!is.na(period)& tiefe %in% c(2,5,10,20,50)))+
  geom_rect(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf),alpha=0.15)+
  geom_line(aes(date,mean_T,col=as.factor(tiefe)))+
  guides(col=F)+
  labs(x="",y="Soil T [°C]",col="depth [cm]")+
  geom_line(data=subset(data,!is.na(period)),
            aes(date,WindVel_30m_ms*y_fac),alpha=0.2)+
  geom_line(data=subset(data,!is.na(period)),
            aes(date,Wind*y_fac),col=grey(0.3))+
  
  #geom_line(data=subset(PPC,date > range2u3[1] & date < range2u3[2]),
  #          aes(date,PPC*PPC_fac+PPC_offset),col=grey(0.3))+
  
  scale_y_continuous(breaks=c(10,15,20),
                     sec.axis = sec_axis(~./y_fac,name=expression("wind velocity [m s"^{-1}*"]"),breaks=c(0,3,6)))+
  #scale_y_continuous(breaks=c(10,15,20),
  #                   sec.axis = sec_axis(~(.-PPC_offset)/PPC_fac,name=expression("PPC [Pa s"^{-1}*"]"),breaks=seq(0,0.4,by=0.2)))+
  facet_wrap(~period,scales="free_x")+
  scale_x_datetime(date_breaks = "2 days",date_labels = "%b %d")+
  theme_bw()+
theme(
  strip.background = element_blank(),
  strip.text.x = element_blank()
)
  
Sys.setlocale("LC_ALL","English")
T_plot_adj <- adj_grob_size(T_plot_2u3,subset(data,!is.na(period)),breaks="4 days",date_labels = "%b %d",plot=F)



################
#VWC P
sec_ax_fac <- 1.25
VWC_plot_2u3 <-  ggplot(subset(soil_agg,!is.na(period) & tiefe %in% c(2,5,10,20,50)))+
  geom_rect(data=subset(Pumpzeiten,Pumpstufe!=0 & Position %in% 7:8),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf),alpha=0.15)+
  geom_ribbon(data=subset(data,!is.na(period)),aes(x=date,ymin=0,ymax=Precip_Last24hrs_mm/sec_ax_fac),fill="blue",alpha=0.8)+
  geom_line(aes(date,mean_VWC,col=as.factor(tiefe)))+
  scale_y_continuous(sec.axis = sec_axis(~.*sec_ax_fac,name=expression(P["24h"]*" [mm]")))+
  theme(
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"),
    axis.text.x = element_blank()
  )+
  guides()+
  labs(x="",y="SWC [Vol. %]",col="depth [cm]")+
  guides(colour = guide_legend(override.aes = list(size = 1.5)))+
  facet_wrap(~period,scales="free_x")+
  scale_x_datetime(date_breaks = "2 days")+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

  #scale_color_discrete(limits=all_dpths)
VWC_plot_adj <- adj_grob_size(VWC_plot_2u3+theme(legend.position = "none"),subset(data,!is.na(period)),breaks="4 days",date_labels=" ",plot=F)

leg1 <- get_legend(inj_2u3)
legend1 <- as_ggplot(leg1)
leg2 <- get_legend(VWC_plot_2u3)
legend2 <- as_ggplot(leg2)
leg <- ggpubr::ggarrange(legend1,legend2,ncol=1,align = "v")


# rawdata_no_leg <- egg::ggarrange(inj_2u3+theme(legend.position = "none"),ref_2u3+theme(legend.position = "none"),VWC_plot_2u3+guides(col=F),T_plot_2u3,ncol=1,heights = c(1.5,1.5,1,1),draw=F)
# ggpubr::ggarrange(rawdata_no_leg,leg,widths=c(5,1))+ggsave(paste0(plotpfad_ms,"Fig_5_Rawdata.jpg"),width=7,height=7)
# Sys.setlocale("LC_ALL","")

inj_plot <- cowplot::ggdraw()+cowplot::draw_plot(inj_adj,x=-0.005,width=0.94)
ref_plot <- cowplot::ggdraw()+cowplot::draw_plot(ref_adj,x=-0.005,width=0.94)
VWC_plot <- cowplot::ggdraw()+cowplot::draw_plot(VWC_plot_adj,x=0.025,width = 0.98)+cowplot::draw_text("c)",x=0.12,y=1.05)
T_plot <- cowplot::ggdraw()+cowplot::draw_plot(T_plot_adj,x=0.025,width = 0.98)+cowplot::draw_text("d)",x=0.12,y=1.05)

no_leg <- ggpubr::ggarrange(inj_plot,ref_plot,VWC_plot,T_plot,ncol=1,heights = c(1.5,1.7,1,1))#,draw=F)

ggpubr::ggarrange(no_leg,leg,widths=c(5,1))+ggsave(paste0(plotpfad_ms,"Fig_4_Rawdata_bw.jpg"),width=7,height=7)

Sys.setlocale("LC_ALL","")

######################################
#Figure 6 Ds profile over time
######################################
h_steady <- 32
ds_sub <- subset(DS_long_roll, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))
F_sub <- subset(F_df, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))

y_fac_ds <- 1/20
DS_plot <- ggplot(subset(soil_agg_plot,range2 %in% c("0-10","10-20")))+
  geom_ribbon(aes(x=date,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill=as.factor(range2)),alpha=0.15)+
  geom_line(aes(date,DSD0_PTF,col=as.factor(range2),linetype="f(eps)"))+
  #hours till steady state
  #geom_line(data=subset(DS_long_roll, date > pos8_date ),aes(date,DSD0_roll,col=range2,linetype="in situ"),alpha=0.3)+
  #geom_line(data=subset(DS_long_roll, Versuch == "2"),aes(date,DSD0_roll,col=range2,linetype="in situ"),alpha=0.3)+
  geom_line(data=subset(ds_sub,range2  %in% c("0-10","10-20")),aes(date,DSD0_roll,col=range2,linetype="in situ"))+
  labs(y=expression(D[eff]/D[0]),col="depth [cm]",fill="depth [cm]",linetype="")+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = range(DS_long$date))+
  scale_linetype_manual(values=2:1,labels=c(expression(f~(epsilon),"in situ")))#+
#  ggsave(file=paste0(plotpfad_ms,"DS_plot_gam_feps.jpg"),width=7,height = 3.5)
#DS_plot#+facet_zoom(xlim = range7)

soil_agg_plot$date <- round_date(soil_agg_plot$date,"mins")

ds_soil <- merge(ds_sub[,c("date","range2","Versuch","DSD0_roll","id")],soil_agg_plot) 
ds_soil$Versuch <- as.numeric(ds_soil$Versuch)
ds_soil$windy <- factor(ds_soil$Versuch,levels = 2:3,labels=c("windy","calm"))
ds_soil_long <- tidyr::pivot_longer(ds_soil,matches("DSD0_PTF"),values_to = "DSD0_PTF")

DS_boxplot <- 
ggplot(ds_soil)+
  geom_boxplot(data=subset(ds_soil,id=="1"),aes("in situ",DSD0_roll,fill="0-10 cm",col="0-10 cm"),alpha=0.5)+
  geom_boxplot(data=subset(ds_soil,id=="2"),aes("in situ",DSD0_roll,fill="10-20 cm",col="10-20 cm"),alpha=0.5)+
  geom_boxplot(data=subset(ds_soil_long,id=="1"),aes("xfeps",DSD0_PTF,fill="0-10 cm",col="0-10 cm"),col=NA,alpha=0.4)+
  geom_boxplot(data=subset(ds_soil_long,id=="2"),aes("xfeps",DSD0_PTF,fill="10-20 cm"),col=NA,alpha=0.4)+
  geom_boxplot(data=subset(ds_soil,id=="2"),aes("xfeps",DSD0_PTF,fill="10-20 cm",col="10-20 cm"))+
  geom_boxplot(data=subset(ds_soil,id=="1"),aes("xfeps",DSD0_PTF,fill="0-10 cm",col="0-10 cm"))+
  #scale_x_discrete(labels=c(expression(f(epsilon)~"calm"),expression(f(epsilon)~"windy"),paste("in situ",c("calm","windy"))))+
  
  scale_x_discrete(labels=c("in situ",expression(f(epsilon))))+
  
  facet_wrap(~windy)+
  labs(x="",y=expression(D[S]/D[0]),fill="",col="")+
  guides(fill=guide_legend(override.aes = list(alpha=0.1)))

DS_box <- egg::tag_facet(DS_boxplot,tag_pool = c("windy","calm"),open="",close="",fontface=1,hjust=-0.1)
  ######################################
#Flux
######################################
#ggplot(subset(F_df,Versuch!="1"))+geom_line(aes(date,Fz_roll2_10_17 / Fz_roll2))
F_sub$P_0_10 <- F_sub$Fz_roll_0_10 - F_sub$Fz_roll_10_17
F_sub$P_10_20 <- F_sub$Fz_10_17
F_long <- tidyr::pivot_longer(F_sub,matches("P_\\d+"),names_to = "tiefe",values_to = "P",names_prefix = "P_")
F_long$tiefe2 <- factor(F_long$tiefe,levels=c("0_10","10_20"),labels=c("0-10 cm","10-20 cm"))
Kammer_flux_agg <- Kammer_flux %>% group_by(day) %>% summarise(CO2flux = mean(CO2flux),CO2flux_max=max(CO2flux_max),CO2flux_min=min(CO2flux_min),date=mean(date))
cols <- scales::hue_pal()(2)
mindate <- ymd_h("2020-07-06 16")
calm_dates <- PPC$date[which(PPC$PPC < 0.07)]

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
  ggnewscale::new_scale_fill()+
  #geom_line(data=subset(F_df,Versuch != "1"),aes(date,Fz,col="0-10 cm"),alpha=0.3)+
  #geom_line(data=subset(F_df),aes(date,Fz_10_17,col="10-20 cm"),alpha=0.3)+
  #geom_line(data=F_sub,aes(date,Fz_roll_0_10,col="0-10 cm"))+
  #geom_line(data=F_sub,aes(date,Fz_roll2,col="0-10 cm"))+

  #geom_line(data=F_sub,aes(date,Fz_roll_10_17,col="10-20 cm"))+
  geom_col(data=subset(F_long)[-(1:2),],aes(date,P,fill=tiefe2,alpha="windy"),width=3600, show.legend = FALSE)+
  geom_col(data=subset(F_long)[1,],aes(date,P,fill=tiefe2,alpha=""),width=3600, show.legend = FALSE)+
  geom_col(data=subset(F_long)[2,],aes(date,P,fill=tiefe2,alpha=" "),width=3600, show.legend = FALSE)+
  geom_col(data=subset(F_long,date %in% calm_dates),aes(date,P,fill=tiefe2,alpha="calm"),width=3600)+
  geom_line(data=subset(soil_wide,date<= "2020-07-24 08:20:00 UTC"),aes(date,zoo::rollapply(R_soil,20,mean,fill=NA)),col=grey(0.3),linetype=2)+
  
  scale_alpha_manual(values=c(0.6,0.35,0.6,0.35))+
  guides(col=F,fill=F,
         alpha= guide_legend("0-10   10-20 cm",override.aes = list(fill=rep(cols[1:2],each=2)),ncol=2))+
#  scale_color_manual(values=cols[1:2])+
  #scale_color_manual("gradient method",values=1:2)+
  #xlim(ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  #ylim(range(F_sub[,c("Fz_roll_0_10","Fz_roll_10_17")],na.rm=T))+
  ylim(c(0,5.5))+
  labs(x="",y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max),col=1,width=10000)

flux_plot


#ggplot(PPC)+
#  geom_col(data=subset(F_long),aes(date,P/10,fill=tiefe2),width=3600, show.legend = FALSE)+
#  geom_line(aes(date,PPC))+geom_hline(yintercept = 0.071)+ylim(c(0,0.1))


#F_long <- tidyr::pivot_longer(F_sub[,c("date","Fz_roll_0_10","Fz_roll_10_17")],matches("Fz_roll_(0_10|10_17)"),names_to = "range",values_to = "Fz",names_prefix = "Fz_roll_")

#ggplot(F_long)+geom_point(aes(Fz,range,col=as.factor(date)))+guides(col=F)


#Fig5u6 <- egg::ggarrange(DS_plot+ labs(x="")  +scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC"))),flux_plot,ncol=1,draw=F)

# jpeg(file=paste0(plotpfad_ms,"Fig5u6_drift_adj.jpg"),width=7,height = 5,units="in",res=300)
# Fig5u6
# dev.off()


################################
#Wind DS
axis_fac <- 10

range7 <- range(subset(DS_long_roll,Versuch == 2)$date)
range8 <- range(subset(DS_long_roll,Versuch == 3)$date)
data$Versuch <- NA
data$Versuch[data$date > range7[1] & data$date < range7[2]] <- 2
data$Versuch[data$date > range8[1] & data$date < range8[2]] <- 3

# wind_DS <- ggplot(subset(data,!is.na(Versuch)))+
#   geom_line(aes(date,WindVel_30m_ms/axis_fac),col=grey(0.5),alpha=0.2)+
#   geom_line(aes(date,Wind/axis_fac,col="wind velocity"))+
#   geom_line(data=subset(ds_sub,id == "1"),aes(date,DSD0_roll,col="DSD0 1" ))+
#   scale_x_datetime(date_label="%b %d",breaks="1 days",limits = )+
#   scale_y_continuous(sec.axis = sec_axis(trans=~.*axis_fac,name=expression("wind velocity [m s"^{-1}*"]")))+
#   scale_color_manual("",values = c(scales::hue_pal()(1),grey(0.2)),labels=c(expression(D[S]/D[0]~1),"wind velocity"))+
#   facet_wrap(~factor(Versuch,levels=c("2","3"),labels = c("windy period","calm period")),scales="free_x")+
#   labs(y=expression(D[S]/D["0"]),x="")#+
#   #ggsave(paste0(plotpfad_harth,"DS_Wind_Inj1u2.jpg"),width=7,height = 5)
# 
# Fig6windDS <- egg::ggarrange(DS_plot+ labs(x="")  +scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC"))),wind_DS,flux_plot,ncol=1,heights = c(1,1,1),draw=F)
# jpeg(file=paste0(plotpfad_ms,"Fig6WindDS.jpg"),width=7,height = 7.5,units="in",res=300)
# Fig6windDS
# dev.off()

col_labs <- c("DSD0 1" ,"DSD0 1 peak","PPC")
col_exps <- c(expression(D[eff]/D[0]~1),expression(D[PPE]/D[0]~1),"PPC")


PPC_DS_plot <- 
  ggplot(subset(data,!is.na(Versuch)))+
  geom_ribbon(data=subset(PPC_DS,date %in% ds_sub$date),aes(x=date,ymax=DSD0_roll,ymin=base,fill="DSD0 1 peak"),alpha=0.2)+
  geom_line(data=subset(PPC_DS,date %in% ds_sub$date),aes(date,PPC,col="PPC"))+
  geom_line(data=subset(PPC_DS,date %in% ds_sub$date),aes(date,peak,col="DSD0 1 peak"))+
  geom_line(data=subset(ds_sub,id == "1"),aes(date,DSD0_roll,col="DSD0 1" ))+
  scale_x_datetime(date_label="%b %d",breaks="1 days",limits = )+
  scale_y_continuous(limits = c(0,0.6),sec.axis = sec_axis(trans=~.,name=expression("PPC [Pa s"^{-1}*"]")))+
  scale_color_manual("",values = c(scales::hue_pal()(1),2,grey(0.2)),labels=col_exps)+
  scale_fill_manual("",limits=col_labs,values = c(NA,2,NA),labels=col_exps)+
  facet_wrap(~factor(Versuch,levels=c("2","3"),labels = c("windy period","calm period")),scales="free_x")+
  labs(y=expression(D[eff]/D["0"]),x="")+

    theme(legend.text.align = 0)#+
  #ggsave(paste0(plotpfad_harth,"DS_PPC_Inj1u2.jpg"),width=7,height = 5)
PPC_DS_plot

cor <- cor(PPC_DS$peak,PPC_DS$PPC,use = "complete")
DPPE_PPC <- ggplot(subset(PPC_DS,date %in% ds_sub$date))+geom_point(aes(PPC,peak))+annotate("text",x=-Inf,y=0.25,label=paste("r =",round(cor,2)),hjust=-0.1)+
  labs(y=expression(D[PPE]/D[0]),x=expression("PPC [Pa s"^{-1}*"]"))
legbox <- get_legend(DS_boxplot)
legend_box <- as_ggplot(legbox)
boxplt_scatter <- egg::ggarrange(DS_boxplot+theme(legend.position = "none"),DPPE_PPC,legend_box,nrow=1,widths=c(3,3,1))
Fig6PPCDS <- egg::ggarrange(PPC_DS_plot,boxplt_scatter,flux_plot,ncol=1,heights = c(1,1,1),draw=F)
boxgrob <- ggplotGrob(DS_boxplot+theme(legend.position = "none"))
scattergrob <- ggplotGrob(DPPE_PPC)
gtable::gtable_col(boxgrob,scattergrob)
rbind(size="first")
jpeg(file=paste0(plotpfad_ms,"Fig_6_PPC_DS.jpg"),width=7,height = 7.5,units="in",res=300)
Fig6PPCDS
dev.off()

p1 <- PPC_DS_plot
p2 <- DS_box
p3 <- DPPE_PPC
p4 <- flux_plot
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)

fg2 <- egg::gtable_frame(g2, debug = F)
fg3 <- egg::gtable_frame(g3, debug = F)

fg23 <-
  egg::gtable_frame(gridExtra::gtable_cbind(fg3, fg2),
               width = unit(2, "null"),
               height = unit(1, "null"))
fg1 <-
  egg::gtable_frame(
    g1,
    width = unit(1, "null"),
    height = unit(1, "null"),
    debug = F
  )
fg4 <-
  egg::gtable_frame(
    g4,
    width = unit(1, "null"),
    height = unit(1, "null"),
    debug = F
  )
combined <- gridExtra::gtable_rbind(fg1, fg23,fg4) 
jpeg(file=paste0(plotpfad_ms,"Fig_6_PPC_DS.jpg"),width=7,height = 7.5,units="in",res=300)
grid::grid.newpage()
grid::grid.draw(combined)
dev.off()

combined <- gridExtra::gtable_rbind(fg1, fg2,fg4) 
jpeg(file=paste0(plotpfad_ms,"Fig_6_boxplot.jpg"),width=7,height = 7.5,units="in",res=300)
grid::grid.newpage()
grid::grid.draw(combined)
dev.off()
# Fig6PPCDS <- egg::ggarrange(DS_plot+ labs(x="")  +scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC"))),PPC_DS_plot,flux_plot,ncol=1,heights = c(1,1,1),draw=F)
# jpeg(file=paste0(plotpfad_ms,"Fig_6_PPC_DS.jpg"),width=7,height = 7.5,units="in",res=300)
# Fig6PPCDS
# dev.off()

colnames(F_sub)
mean(F_sub$Fz_10_17/F_sub$Fz_0_10,na.rm = T)
#######################################################

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
  
  geom_line(data=F_sub,aes(date,Fz_roll,col="0-7 cm"))+
  geom_line(data=F_sub,aes(date,Fz_roll_3_10,col="3-10 cm"))+
  geom_line(data=F_sub,aes(date,Fz_roll_0_10,col="0-10 cm "))+

  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = ymd_hms(c("2020-07-06 11:00:00 UTC", "2020-07-24 08:20:00 UTC")))+
  labs(y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max),col=1,width=10000)


efflux_plot#+ggsave(paste0(plotpfad_ms,"Flux_calc.jpg"),width=7,height = 5)

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


#data$Wind_sd <- zoo::rollapply(data$WindVel_30m_ms,60*4,sd,fill=NA)



klima$SWin_2m <- (klima$SWin_CNR1_2m_A_Wm2 + klima$SWin_CNR1_2m_B_Wm2)/2
ggplot()+
  geom_line(data= subset(klima,date > min(F_df$date) & date < max(F_df$date)), aes(date, SWin_2m))+
  geom_line(data=ds_sub,aes(date,DSD0_roll*axis_fac_2,col=range2,linetype="DSD0"))+
  scale_y_continuous(sec.axis = sec_axis(trans=~./axis_fac_2,name="DSD0"))+
  ggsave(paste0(plotpfad_harth,"DS_ShortWave_irradiance_2m.jpg"),width=7,height = 5)

axis_fac_2 <- 40
T_DS_plot <- ggplot(subset(data,date > min(F_df$date) & date < max(F_df$date)))+
  geom_line(aes(date,T_soil,col=as.factor(tiefe),linetype="T soil"))+
  
  geom_line(data=ds_sub,aes(date,DSD0_roll*axis_fac_2,col=range2,linetype="DSD0"))+
  scale_x_datetime(date_label="%b %d",breaks="2 days",limits = range(subset(DS_long,Versuch==2:3)$date))+
  scale_y_continuous(sec.axis = sec_axis(trans=~./axis_fac_2,name="DSD0"))+
  labs(y="T [°C]",x="")

T_DS_plot+ggsave(paste0(plotpfad_harth,"DS_Tsoil.jpg"),width=7,height = 5)

axis_fac_3 <- 300
Versuch_i <- 3
ggplot(subset(data,date > min(subset(F_df,Versuch==Versuch_i)$date)-3*3600*24 & date < max(subset(F_df,Versuch==Versuch_i)$date)+2*3600*24 & tiefe > -11))+
  geom_rect(data = subset(Pumpzeiten,Pumpstufe %in% c(3,5)[Versuch_i-1]),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf),alpha=0.1)+
  geom_line(aes(date,WindVel_30m_ms*axis_fac_3),col=grey(0.5),alpha=0.2)+
  geom_line(aes(date,Wind*axis_fac_3),col=grey(0.2))+
  geom_line(aes(date,preds_drift,col=as.factor(-tiefe),linetype="ref adj"))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(-tiefe),linetype="inj"))+
  scale_x_datetime(date_label="%b %d",breaks="2 days")+
  scale_y_continuous(sec.axis = sec_axis(trans=~./axis_fac_3,name="Windspeed [m/s]"))+
  theme_bw()+
  labs(title=paste("Injektion",Versuch_i-1),y="CO2 ppm",col="tiefe [cm]",linetype="")#+
  #ggsave(paste0(plotpfad_harth,"CO2_Wind_Inj_",Versuch_i-1,".jpg"),width=7,height = 5)

ggplot(subset(data,date > min(subset(F_df,Versuch==Versuch_i)$date)-3*3600*24 & date < max(subset(F_df,Versuch==Versuch_i)$date)+2*3600*24 & tiefe > -10))+
  geom_rect(data = subset(Pumpzeiten,Pumpstufe %in% c(3,5)[Versuch_i-1]),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf),alpha=0.1)+
  geom_line(aes(date,Wind_sd*axis_fac_3*3),col=grey(0.2))+
  geom_line(aes(date,preds_drift,col=as.factor(-tiefe),linetype="ref adj"))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(-tiefe),linetype="inj"))+
  scale_x_datetime(date_label="%b %d",breaks="2 days")+
  scale_y_continuous(sec.axis = sec_axis(trans=~./(axis_fac_3*3),name="Windspeed sd [m/s]"))+
  theme_bw()+
  labs(title=paste("Injektion",Versuch_i-1),y="CO2 ppm",col="tiefe [cm]",linetype="")+
  ggsave(paste0(plotpfad_harth,"CO2_Windsd_Inj_",Versuch_i-1,".jpg"),width=7,height = 5)
  
  
ggplot(F_df)+geom_line(aes(date, Fz,col=as.factor(Versuch)))

ggplot(subset(data,date > min(F_df$date) & date < max(F_df$date) & T_C > 12))+
  geom_rect(data = subset(Pumpzeiten,Pumpstufe %in% c(3,5)),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf),alpha=0.1)+
  geom_line(aes(date,T_C,col="T Inj"))+
  labs(y="T Injectionskiste [°C]")+
  scale_y_continuous(sec.axis = sec_axis(trans=~./60,name="DS/D0"))+
  geom_line(data=subset(ds_sub,range2 == "0-10"),aes(date,DSD0_roll*60,col="DSD01"))+
  ggsave(paste0(plotpfad_harth,"DS_T_injektionskiste.jpg"),width=7,height = 5)

#data <- data %>% group_by(tiefe) %>% mutate(CO2_tracer_roll = zoo::rollapply(CO2_tracer_drift,60*12,mean,na.rm=T,fill=NA))

ggplot(subset(data,Position %in% 7:8 & tiefe >= -10.5 & tiefe < 0))+
  geom_rect(data = subset(Pumpzeiten,Pumpstufe %in% c(3,5)),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf),alpha=0.1)+
  geom_hline(yintercept = 0,col="grey")+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(-tiefe)),alpha=0.4)+
  geom_line(aes(date,CO2_tracer_roll4,col=as.factor(-tiefe)))+
  theme_bw()+
scale_x_datetime(date_label="%b %d",breaks="2 days")#+
  #ggsave(paste0(plotpfad_harth,"drift_tracer_tiefe123.jpg"),width=7,height = 4)


for(i in 1:7*3.5){
ggplot(subset(data,Position %in% 7:8 & tiefe == -i))+
  geom_line(aes(date,preds_drift,col="ref adj"))+
  geom_line(aes(date,CO2_roll_ref,col="ref"))+
  geom_line(aes(date,CO2_roll_inj,col="inj"))+
    theme_bw()+
  scale_x_datetime(date_label="%b %d",breaks="2 days")+
  labs(title=paste("tiefe",i,"cm"),x="",y="CO2 [ppm]")+
  ggsave(paste0(plotpfad_harth,"inj_ref_adj_tiefe",floor(i),".jpg"),width=7,height = 5)
}

ggpubr::ggarrange(DS_plot,wind_plot,ncol=1,align = "v")
egg::ggarrange(DS_plot+ labs(x="") ,wind_plot,ncol=1)
ds_sub %>% group_by(range) %>% summarise(min(DSD0_roll,na.rm=T),max(DSD0_roll,na.rm=T))
F_sub %>% summarise(min(Fz_roll_0_10_adj,na.rm=T),max(Fz_roll_0_10_adj,na.rm=T), mean(Fz_roll_10_17/Fz_roll_0_10,na.rm = T))
F_sub %>% summarise(min(Fz_roll_0_10_adj,na.rm=T),max(Fz_roll_0_10_adj,na.rm=T))


