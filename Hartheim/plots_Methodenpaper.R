


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
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr","png","cowplot","magick","ggforce","imputeTS")
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
load(paste0(datapfad_harth,"DS_long_list_withPos1minmax.RData"))

#date ranges
pos8_date <- min(data$date[which(data$Position ==8 & data$Pumpstufe != 0)])
range1 <- range(data$date[data$Position ==1],na.rm = T)
range2 <- range(data$date[data$Position ==7],na.rm = T)
range2u3 <- range(data$date[data$Position %in% 7:8],na.rm = T)


F_df[which(F_df$date>pos8_date & F_df$date < (pos8_date + 20*3600)),c("DSD0_1","Fz","Fz_roll")] <- NA

DS_long_roll$DSD0_roll[which(DS_long_roll$date>pos8_date & DS_long_roll$date < (pos8_date + 20*3600) & DS_long_roll$id == 1)] <- NA


data$preds_drift[data$tiefe == 0] <- data$CO2_roll_ref[data$tiefe == 0]


h_steady <- 32

DS_long_roll$range2 <- factor(DS_long_roll$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))
soil_agg_plot$range2 <- factor(soil_agg_plot$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))


ds_sub <- subset(DS_long_roll, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))
F_sub <- subset(F_df, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))

data$Wind <- RcppRoll::roll_mean(data$WindVel_30m_ms,60*4,fill=NA)

Pumpzeiten$Position[10:11] <- NA


F_sub$P_0_10 <- F_sub$Fz_roll_0_10 - F_sub$Fz_roll_10_17
F_sub$P_10_20 <- F_sub$Fz_10_17
F_long <- tidyr::pivot_longer(F_sub,matches("P_\\d+"),names_to = "tiefe",values_to = "P",names_prefix = "P_")
F_long$tiefe2 <- factor(F_long$tiefe,levels=c("0_10","10_20"),labels=c("0-10 cm","10-20 cm"))

soil_agg$id <- as.numeric(factor(soil_agg$tiefe,levels=c(2,5,10,20,50,100),labels=c(1,1,1,2,3,3)))
soil_agg_plot <- subset(soil_agg, tiefe %in% c(5,10,20) & date > range2u3[1] & date < range2u3[2] ) %>% 
  group_by(date,range) %>% 
  summarise_all(mean)
soil_agg_plot$id <- as.character(soil_agg_plot$id)

# colnames(F_sub)
# PPC_DS_2 <- PPC_DS %>% rename(DPPED0 = peak, DSD0_1 = base,DeffD0_1 = DSD0_1) %>% 
#   select(date,DPPED0,DSD0_1,DeffD0_1,Versuch) %>% subset(., (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))
# F_sub_2 <- F_sub %>% 
#   select(date,DSD0_roll_2,Fz_roll_0_10,Fz_roll_10_17,Versuch) %>% 
#   rename(DSD0_2 = DSD0_roll_2,FCO2_1 = Fz_roll_0_10,FCO2_2 = Fz_roll_10_17)
#   
# 
# 
# 
# PPC_DS_F <- merge(PPC_DS_2,F_sub_2,all=T) %>% 
#   mutate_at(c("DSD0_2","FCO2_1","FCO2_2"),list(~imputeTS::na_interpolation(.,maxgap=1)))
# 
# PPC_DS_F$Versuch <- PPC_DS_F$Versuch -1
# 
# write.csv(PPC_DS_F,paste0(datapfad_harth,"DS_FCO2_Hartheim_2020_07.txt"),row.names = F)

##################################################################################
#plots
##################################################################################
###############################################################################
#Figure 2 COMSOL und DS sandkiste
###############################################################################
###############################################################################

col<-scales::hue_pal()

mod_obs_plot <- 
  ggplot(subset(data_sub, Versuch %in% c(5,6,9)))+
  geom_line(aes(CO2_mod,tiefe,col=""))+
  geom_point(aes(CO2,tiefe,shape="no Part"))+
  geom_point(data=subset(respi_wide,Versuch%in%c(5:6,9)),aes(CO2_tracer_calc,tiefe,shape="with Part"),col=col(3)[2])+
  facet_wrap(~factor(material,levels=c("Sand","Splitt","Sand & Splitt"),labels=c("sand","grit","mixture")))+
  scale_shape_manual("obs",labels=c(expression("no "*P[art]),expression("with "*P[art])),values=c(19,20))+
  guides(shape=guide_legend(override.aes = list(color=c(1,col(3)[2]))))+
  labs(x=expression(CO[2]*" [ppm]"),y="depth [cm]",col="mod",fill="")+theme_bw()
mod_obs_plot 


R2(respi_wide$CO2_tracer,respi_wide$CO2_tracer_calc)




lims <- c("Psim","tracer","tracer + prod")
lims2 <- c(expression(P[art]),"tracer",expression("tracer + "*P[art]))


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


cowplot::ggdraw()+
  cowplot::draw_plot(resp_plot,x=0,y=0.5,height=0.5,width=0.77)+
  cowplot::draw_plot(mod_obs_plot,height=0.5)+
  cowplot::draw_text("a)",x=0.01,y=0.95,hjust=0)+
  cowplot::draw_text("b)",x=0.01,y=0.45,hjust=0)+
  ggsave(paste0(plotpfad_ms,"Fig_2_COMSOL_mod_obs_prod.jpg"),width = 7,height=5)



###############################################################################
###############################################################################
#Figure 3 see script 
#"COMSOL_flux_profile_optim.R"  in folder "Produktionseimer"
###############################################################################
###############################################################################


###############################################################################
###############################################################################
#Figure 4 Rawdata
###############################################################################
###############################################################################
#inj ref

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


inj_2u3 <- ggplot(data_agg)+
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


ref_2u3 <- ggplot(data_agg)+
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
  scale_y_continuous(breaks=c(10,15,20),
                     sec.axis = sec_axis(~./y_fac,name=expression("wind velocity [m s"^{-1}*"]"),breaks=c(0,3,6)))+
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
  theme_bw()+
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


inj_plot <- cowplot::ggdraw()+cowplot::draw_plot(inj_adj,x=-0.005,width=0.94)
ref_plot <- cowplot::ggdraw()+cowplot::draw_plot(ref_adj,x=-0.005,width=0.94)
VWC_plot <- cowplot::ggdraw()+cowplot::draw_plot(VWC_plot_adj,x=0.025,width = 0.98)+cowplot::draw_text("c)",x=0.12,y=1.05)
T_plot <- cowplot::ggdraw()+cowplot::draw_plot(T_plot_adj,x=0.025,width = 0.98)+cowplot::draw_text("d)",x=0.12,y=1.05)

no_leg <- ggpubr::ggarrange(inj_plot,ref_plot,VWC_plot,T_plot,ncol=1,heights = c(1.5,1.7,1,1))#,draw=F)

ggpubr::ggarrange(no_leg,leg,widths=c(5,1))+ggsave(paste0(plotpfad_ms,"Fig_4_Rawdata_bw.jpg"),width=7,height=7)

Sys.setlocale("LC_ALL","")

###############################################################################
###############################################################################
# Figure 5 heterogeneity
###############################################################################
###############################################################################


Pump_sub <- subset(Pumpzeiten,Pumpstufe==0 & !grepl("regen|tauscht",Pumpzeiten$bemerkung))

data_sub_ls <- vector("list",nrow(Pump_sub))
for (i in 1:nrow(Pump_sub)) {
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
  labs(x=expression(CO[2]~"[ppm]"),fill="profile",col="profile",y="depth [cm]")+theme_bw()+
  scale_color_brewer(palette= 2,type="qual")+
  scale_fill_brewer(palette= 2,type="qual")

tiefe_plt

Sys.setlocale("LC_ALL","English")

time_plt_adj <- ggplot(subset(data_sub,ID %in% c(1,3,7) & !is.na(Position)& tiefe < 0))+
  geom_line(aes(date,preds_drift,col=as.factor(-tiefe),linetype="B/C/D  adj"))+
  #geom_line(aes(date,preds_SWC_T,linetype=as.factor(tiefe)))+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(-tiefe),linetype="A"))+
  facet_wrap(~factor(ID,levels = c(1,3,7),labels=paste("profile",c("A + B","A + C","A + D"))),scales="free")+
  labs(x="",y=expression(CO[2]~"[ppm]"),col="depth [cm]",linetype="profile")+
  scale_x_datetime(date_breaks = "2 days",date_labels = "%b %d")+
  theme_bw()

leg1 <- ggpubr::get_legend(tiefe_plt)
legend1 <- ggpubr::as_ggplot(leg1)

leg2_adj <- ggpubr::get_legend(time_plt_adj)
legend2_adj <- ggpubr::as_ggplot(leg2_adj)
leg_adj <- ggpubr::ggarrange(legend1,legend2_adj,heights = c(1,2),ncol=1)
leg_adj <- cowplot::ggdraw()+
  cowplot::draw_plot(legend1,y=0.67,height=0.33,x=-0.15)+
  cowplot::draw_plot(legend2_adj,height=0.67,x=0.04)


no_leg_adj <- ggpubr::ggarrange(tiefe_plt+theme(legend.position = "none"),time_plt_adj+theme(legend.position = "none"),ncol=1,align="v") 

heterogeneity_plot <- ggpubr::ggarrange(no_leg_adj,leg_adj,widths = c(8,2))
cowplot::ggdraw()+
  cowplot::draw_plot(heterogeneity_plot)+
  cowplot::draw_text("a)",x=0.05,y=0.97)+
  cowplot::draw_text("b)",x=0.05,y=0.47)+
  ggsave(paste0(plotpfad_ms,"Fig_5_heterogeneity_adj.jpg"),width=7,height=5)
Sys.setlocale("LC_ALL","")



###############################################################################
###############################################################################
#Figure 6 Ds profile over time
###############################################################################
###############################################################################

h_steady <- 32
ds_sub <- subset(DS_long, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))
#ds_sub <- subset(DS_long_roll, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))
F_sub <- subset(F_df, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))

range(subset(ds_sub,id==1)$DSD0_roll,na.rm = T)
range(subset(ds_sub,id==2)$DSD0_roll,na.rm = T)
range(subset(ds_sub,id==3)$DSD0_roll,na.rm = T)
mean(F_sub$Fz_roll_10_17/F_sub$Fz_roll_0_10,na.rm=T)


soil_agg_plot$date <- round_date(soil_agg_plot$date,"mins")
ds_soil <- merge(ds_sub[,c("date","range2","Versuch","DSD0_roll","id")],soil_agg_plot) 
ds_soil$Versuch <- as.numeric(ds_soil$Versuch)
ds_soil$windy <- factor(ds_soil$Versuch,levels = 2:3,labels=c("windy period","calm period"))
PPC_DS$windy <- factor(PPC_DS$Versuch,levels = 2:3,labels=c("windy period","calm period"))

ds_soil_agg <- ds_soil %>% group_by(id,windy) %>% summarise_all(mean)


DS_boxplot <- 
  ggplot()+
  geom_boxplot(data=subset(ds_soil_agg),aes(x="feps",middle=PTF_median_median,ymin=PTF_min_max,ymax=PTF_max_min,lower=PTF_q25_max,upper=PTF_q75_min,fill=id),stat="identity")+
  geom_boxplot(data=subset(ds_soil),aes("in situ",DSD0_roll,fill=id,col=id),alpha=0.5)+
  #geom_boxplot(data=subset(PPC_DS,date %in% ds_soil$date),aes("in situ DS",base,fill=id,col=id),width=0.4,alpha=0.5)+
  scale_x_discrete(labels=c(expression(f(epsilon)),"in situ"))+#expression(D[eff]/D[0]~"")))+
  facet_wrap(~windy)+
  scale_color_manual(limits=1:2,values=scales::hue_pal()(2),labels=c("0-10 cm","10-20 cm"))+
  scale_fill_manual(limits=1:2,values=scales::hue_pal()(2),labels=c("0-10 cm","10-20 cm"))+
  labs(x="",y="exchange coefficient",fill=expression(f(epsilon)),col="in situ")+
  guides(col = guide_legend(override.aes = list(fill=scales::hue_pal()(2),alpha=0.2)))
  
######################################
#Flux
######################################

F_sub$P_0_10 <- F_sub$Fz_roll_0_10 - F_sub$Fz_roll_10_17
F_sub$P_10_20 <- F_sub$Fz_10_17
F_long <- tidyr::pivot_longer(F_sub,matches("P_\\d+"),names_to = "tiefe",values_to = "P",names_prefix = "P_")
F_long$tiefe2 <- factor(F_long$tiefe,levels=c("0_10","10_20"),labels=c("0-10 cm","10-20 cm"))
Kammer_flux_agg <- Kammer_flux %>% group_by(day) %>% summarise(CO2flux = mean(CO2flux),CO2flux_max=max(CO2flux_max),CO2flux_min=min(CO2flux_min),date=mean(date))
cols <- scales::hue_pal()(2)
mindate <- ymd_h("2020-07-06 16")
calm_dates <- PPC$date[which(PPC$PPC < 0.07)]

Kammer_flux_agg$Versuch <- NA
Kammer_flux_agg$Versuch[Kammer_flux_agg$date > range2[1] - 3600 * 40 & Kammer_flux_agg$date < range2[2]] <- 2


soil_wide$Versuch <- NA
soil_wide$Versuch[soil_wide$date > range2[1] - 3600 * 20 & soil_wide$date < range2[2] - 3600 * 20] <- 2
soil_wide$Versuch[soil_wide$date > Pumpzeiten$start[17] & soil_wide$date < Pumpzeiten$ende[17]] <- 3

flux_plot <- ggplot(subset(Kammer_flux_agg,!is.na(Versuch)))+
  geom_linerange(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,col=""))+
  geom_point(aes(date,CO2flux,col=""))+
  labs(col="chamber")+
  scale_color_manual(values=c(1))+
  ggnewscale::new_scale_color()+
  geom_line(data=subset(soil_wide,!is.na(Versuch)),aes(date,zoo::rollapply(R_soil,20,mean,fill=NA),col=""),linetype=2)+
  geom_ribbon(data=subset(soil_wide,!is.na(Versuch)),aes(x=date,ymin=R_min,ymax=R_max,fill=""),alpha=0.15)+
  scale_color_manual("T & SWC model",values=grey(0.3))+
  scale_fill_manual("T & SWC model",values=grey(0.3))+
  ggnewscale::new_scale_color()+
  ggnewscale::new_scale_fill()+
  geom_col(data=subset(F_long)[-(1:2),],aes(date,P,fill=tiefe2,alpha="windy"),width=3600, show.legend = FALSE)+
  geom_col(data=subset(F_long)[1,],aes(date,P,fill=tiefe2,alpha=""),width=3600, show.legend = FALSE)+
  geom_col(data=subset(F_long)[2,],aes(date,P,fill=tiefe2,alpha=" "),width=3600, show.legend = FALSE)+
  geom_col(data=subset(F_long,date %in% calm_dates),aes(date,P,fill=tiefe2,alpha="calm"),width=3600)+
  geom_line(data=subset(soil_wide,!is.na(Versuch)),aes(date,zoo::rollapply(R_soil,20,mean,fill=NA)),col=grey(0.3),linetype=2)+
  
  scale_alpha_manual(values=c(0.6,0.35,0.6,0.35))+
  guides(col=F,fill=F,
         alpha= guide_legend("gradient method\n ",override.aes = list(fill=rep(cols[1:2],each=2)),ncol=2))+
  scale_x_datetime(date_label="%b %d",breaks="2 days")+
  ylim(c(0,5.5))+
  facet_wrap(~Versuch,scales="free_x")+
  labs(x="",y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max),col=1,width=10000)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  geom_vline(data=data.frame(x=c(max(subset(soil_wide,Versuch==2)$date),min(subset(soil_wide,Versuch==3)$date)),Versuch=2:3),aes(xintercept=x),linetype="dashed")


flux_plot
  #theme(panel.border = element_rect(fill=NA))
  geom_vline(data=data.frame(x=c(max(subset(soil_wide,Versuch==2)$date),min(subset(soil_wide,Versuch==3)$date)),Versuch=2:3),aes(xintercept=x),linetype="dashed")
soil_wide$period <- soil_wide$Versuch-1
flux_adj <- adj_grob_size(flux_plot,subset(soil_wide,!is.na(period)),breaks="2 days",date_labels="%b %d",plot=F)
#cowplot::ggdraw()+cowplot::draw_plot(flux_adj)+cowplot::draw_text("0-10    10-20cm",x=0.9,y=0.7,size=10)





################################
#Wind DS
axis_fac <- 10

range7 <- range(subset(DS_long_roll,Versuch == 2)$date)
range8 <- range(subset(DS_long_roll,Versuch == 3)$date)
data$Versuch <- NA
data$Versuch[data$date > range7[1] & data$date < range7[2]] <- 2
data$Versuch[data$date > range8[1] & data$date < range8[2]] <- 3


col_labs <- c("DSD0" ,"DSD0 peak","PPC")
col_exps <- c(expression(D[eff]/D[0]),expression(D[PPE]/D[0]),"PPC")


PPC_DS_plot <- 
  ggplot(subset(data,!is.na(Versuch)))+
  geom_ribbon(data=subset(PPC_DS,date %in% ds_sub$date),aes(x=date,ymax=DSD0_roll,ymin=base,fill="DSD0 peak"),alpha=0.2)+
  geom_line(data=subset(PPC_DS,date %in% ds_sub$date),aes(date,PPC,col="PPC"))+
  geom_line(data=subset(PPC_DS,date %in% ds_sub$date),aes(date,peak,col="DSD0 peak"))+
  geom_line(data=subset(ds_sub,id == "1"),aes(date,DSD0_roll,col="DSD0" ))+
  scale_x_datetime(date_label="%b %d",breaks="1 days",limits = )+
  scale_y_continuous(limits = c(0,0.6),sec.axis = sec_axis(trans=~.,name=expression("PPC [Pa s"^{-1}*"]")))+
  scale_fill_manual("",limits=col_labs,values = c(NA,2,NA),labels=col_exps)+
  scale_color_manual("",values = c(scales::hue_pal()(1),2,grey(0.2)),labels=col_exps)+
  facet_wrap(~factor(Versuch,levels=c("2","3"),labels = c("windy period","calm period")),scales="free_x")+
  labs(y="exchange coefficient",x="")+

    theme(legend.text.align = 0,panel.border = element_rect(fill=NA))#+
  #ggsave(paste0(plotpfad_harth,"DS_PPC_Inj1u2.jpg"),width=7,height = 5)
PPC_DS_plot

PPC_DS$peak_rel <- PPC_DS$peak#/PPC_DS$base*100

PPC_DS_sub <- subset(PPC_DS,date %in% ds_sub$date)
cor(PPC_DS_sub$peak_rel,PPC_DS_sub$PPC,use = "complete")
fm_PPC_DPPE <- glm(peak_rel~PPC,data=subset(PPC_DS,date %in% ds_sub$date))
R2 <- 1-(fm_PPC_DPPE$deviance/fm_PPC_DPPE$null.deviance)
intercept <- fm_PPC_DPPE$coefficients[1]
slope <- fm_PPC_DPPE$coefficients[2]
DPPE_PPC <- 
  ggplot(subset(PPC_DS,date %in% ds_sub$date))+
  geom_point(aes(PPC,peak_rel,col="0-10 cm",fill="0-10 cm"),alpha=0.8)+
  geom_smooth(aes(PPC,peak_rel),method = "glm",se=F,col=1,lwd=0.6,linetype="dashed")+
  annotate("text",x=-Inf,y=0.25,label=paste("R² =",round(R2,2)),hjust=-0.1)+
  annotate("text",x=-Inf,y=0.29,label=paste("y =",round(slope,2),"x - ",abs(round(intercept,2))),hjust=-0.1)+
  labs(y=expression(D[PPE]/D[0]),x=expression("PPC [Pa s"^{-1}*"]"),col="")+
  scale_color_manual("",limits=c("0-10 cm","10-20 cm"),values=scales::hue_pal()(2))+
  scale_fill_manual("",limits=c("0-10 cm","10-20 cm"),values=scales::hue_pal()(2))+
    guides(col = guide_legend(override.aes = list(shape=15,size=8)))
  
DPPE_PPC
slope
R2
intercept
p1 <- cowplot::ggdraw()+cowplot::draw_plot(PPC_DS_plot)+cowplot::draw_text("a)",x=0.03,y=0.97)
p2 <- cowplot::ggdraw()+cowplot::draw_plot(DS_boxplot+theme(legend.position = "none"))+cowplot::draw_text("b)",x=0.05,y=0.97)
p3 <- cowplot::ggdraw()+cowplot::draw_plot(DPPE_PPC)+cowplot::draw_text("c)",x=0.05,y=0.97)
p4 <- cowplot::ggdraw()+cowplot::draw_plot(flux_adj)+cowplot::draw_text("d)",x=0.03,y=0.97)+cowplot::draw_text("0-10    10-20 cm",x=0.82,y=0.92,size=9,hjust=0,vjust=0)#+
  #rel_grid(0.01,color="grey")+
  #rel_grid(0.1)
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)


fg2 <- egg::gtable_frame(g2, debug = F)
fg3 <- egg::gtable_frame(g3, debug = F)


fg23 <-
  egg::gtable_frame(gridExtra::gtable_cbind(fg2, fg3),
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

#######################################################

PPC_DS_plot+ggsave(file=paste0(plotpfad_ms,"PPC_DS_plot.jpg"),width=7,height = 3.5)
PPC_DS_plot+ggsave(file=paste0(plotpfad_ms,"PPC_DS_plot.jpg"),width=7,height = 3.5)



DS_boxplot+ggsave(file=paste0(plotpfad_ms,"DS_box.jpg"),width=4,height =3)
DS_boxplot+ggsave(file=paste0(plotpfad_ms,"DS_box.jpg"),width=4,height =3)


DPPE_PPC+theme(legend.position = "none")+ggsave(file=paste0(plotpfad_ms,"DS_scatter.jpg"),width=3,height =3)
DPPE_PPC+theme(legend.position = "none")+ggsave(file=paste0(plotpfad_ms,"DS_scatter.jpg"),width=3,height =3)



cowplot::ggdraw()+cowplot::draw_plot(flux_adj)+
  rel_grid(0.01,col="grey",lwd=0.2)+
  rel_grid()+
  cowplot::draw_text("0-10    10-20 cm",x=0.82,y=0.77,size=9,hjust=0,vjust=0)+ggsave(file=paste0(plotpfad_ms,"flux_plot.jpg"),width=7,height = 4)

