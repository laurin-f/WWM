rm(list=ls())
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
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg","dplyr")

check.packages(packages)


ggplot(data)+
  geom_line(aes(date,Fz))
######################################
#data_agg
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(samplerpfad,"Hartheim_CO2_noPos1.RData"))
data_noPos1 <- data
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(datapfad_harth,"PPC_DS.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper_roll.RData"))


#load(paste0(datapfad_harth,"DS_long_list_withPos1.RData"))
load(paste0(datapfad_harth,"DS_long_list_withPos1minmax.RData"))
#load(paste0(datapfad_harth,"DS_long_list_withPos1minmax.RData"))
#DS_long_listPos1 <- DS_long_list
#load(paste0(datapfad_harth,"DS_long_list_poly1.RData"))
load(paste0(datapfad_harth,"DS_long_list.RData"))
#data <- data %>% 
#  mutate(across(matches("CO2_tracer"),~case_when(. < 0 ~ 0,
#                                                T ~ .)))


#DS_long_listWS <- DS_long_list$SWC_WS
DS_long$method[DS_long$method == "drift"] <- "ref adj"

ggplot(DS_long)+
  #geom_ribbon(data=DS_long,aes(x=date,ymin=DSD0_min_roll,ymax=DSD0_max_roll,linetype=paste("DSD0",id),fill=method),alpha=0.2)+
  geom_line(data=PPC_DS,aes(date,DSD0_roll,col="ref (SNOPT)"))+
  #geom_line(data=DS_long_mean_each,aes(date,DSD0,linetype=paste("DSD0",id),col="mean_each"))+
  geom_line(data=subset(DS_long),aes(date,DSD0_roll,linetype=paste("DSD0",id),col=method))+
  scale_linetype_manual("",values = rep(1,3))+
  #scale_color_discrete(limits = c("ref adj","ref (SNOPT)","SWC_T"))+
  #scale_fill_discrete(limits = c("ref adj","ref (SNOPT)","SWC_T"))+
  labs(col="method",y="DSD0")#+
#  ggsave(paste0(plotpfad,"DS_drift_SWC_T_minmax.jpg"),width=7,height = 4)

ggplot()+
  geom_line(data=DS_long_roll,aes(date,DSD0_roll,group=id,col="ref (SNOPT)"))+
  #geom_ribbon(data=DS_long_list$drift,aes(x=date,ymin=DS_min,ymax=DS_max,group=id,fill="drift"),alpha=0.2)+
#  geom_ribbon(data=DS_long_list$SWC_T,aes(x=date,ymin=DSD0_min_roll,ymax=DSD0_max_roll,group=id,fill="SWC_T"),alpha=0.2)+
  geom_line(data=DS_long_list$drift,aes(date,DSD0_roll,group=id,col="drift"))+
  geom_line(data=DS_long_list$SWC_T,aes(date,DSD0,group=id,col="SWC_T"),linetype=2)

DS_ceil <- DS_long_list$drift
ggplot()+
  geom_line(data=DS_floor,aes(date,DSD0,group=id,col="floor"))+
  geom_line(data=DS_ceil,aes(date,DSD0,group=id,col="ceil"))+
  geom_line(data=DS_long_list$drift,aes(date,DSD0,group=id,col="signif"))
ggplot(DS_long_listPos1$SWC)+
  geom_line(aes(date,DSD0,group=id,col="SWC"))+
  geom_line(data=DS_long_list$drift,aes(date,DSD0,group=id,col="drift"))+
  geom_line(data=DS_long_listPos1$drift,aes(date,DSD0,group=id,col="driftPo2"),linetype=2)

data_sub <- subset(data,Position == 7 & tiefe < 0)
data_sub_nop <- subset(data_noPos1,Position == 7 & tiefe < 0)
data_sub <- subset(data,date >ymd_h("2020-06-30 02")& date <ymd_h("2020-07-29 12")  )

data_uncert
ggplot(data_sub)+
#  geom_hline(yintercept = 0,linetype=2)+
  geom_line(aes(date,CO2_roll_inj,group=tiefe))+
  geom_line(data=subset(data_sub,Pumpstufe != 0),aes(date,CO2_roll_inj,group=tiefe),col="grey")+
  geom_line(aes(date,preds_SWC_T,group=tiefe,col="SWC_T"))+
  #geom_line(aes(date,preds_SWC,group=tiefe,col="SWC"))+
  #geom_line(aes(date,preds_SWC_WS,group=tiefe,col="SWC_WS"))+
  geom_ribbon(data=subset(data_uncert,Position==7),aes(x=date,ymin=preds_drift_min,ymax=preds_drift_max,group=tiefe,fill="drift"),alpha=0.2)+
  geom_line(aes(date,preds_drift,group=tiefe,col="drift"))+
  #geom_vline(xintercept = ymd_h("2020.07.01 18"))
  #facet_grid(tiefe~.,scales="free")+
  labs(x="",y=expression(CO[2]~"[ppm]"))

    
colnames(data_uncert)
ggplot(subset(data,Position == 8 & tiefe > -17))+
 geom_hline(yintercept = 0,linetype=2)+

  geom_line(aes(date,CO2_tracer_drift,group=tiefe,col="drift"))+
  geom_line(aes(date,CO2_mol_per_m3,group=tiefe,col="SWC"))#+
  

ggplot(subset(data,Position == 7:8 & tiefe == 0))+
#  geom_hline(yintercept = 0,linetype=2)+
  
  geom_line(aes(date,Wind,group=tiefe))



ggplot(subset(data_sub,tiefe==-10.5& Pumpstufe==0))+
  geom_point(aes(CO2_roll_ref,Wind,col=VWC))+
  geom_smooth(aes(CO2_roll_ref,Wind),method="lm")
    

CO2_plot <- ggplot(subset(data_sub,tiefe > -15))+
  
  geom_line(data=subset(data_sub,tiefe > -15 & Pumpstufe == 0),aes(date,CO2_roll_inj,group=tiefe,linetype="calibration"))+
  geom_line(data=subset(data_sub,tiefe > -15 & Pumpstufe != 0),aes(date,CO2_roll_inj,group=tiefe,linetype="injection"))+
  geom_line(data=subset(data_sub_nop, tiefe > -15),aes(date,preds_SWC_T,group=tiefe,col="short calibration"))+
  #geom_line(data=subset(data_sub_nop, tiefe > -15),aes(date,preds_SWC,group=tiefe,col="no T effekt short"))+
  geom_line(aes(date,preds_SWC,group=tiefe,col="no T effekt"))+
  geom_line(aes(date,preds_SWC_T,group=tiefe,col="long calibration"))+
  labs(y="CO2 [ppm]",col="SWC T model",linetype="CO2 inj",x="")


T_plot <- ggplot(subset(data,date < max(data_sub$date) & date > min(data_sub$date) & tiefe == -10.5))+
  geom_line(aes(date,T_soil,col=as.factor(tiefe)))+
  labs(col="tiefe [cm]")


ggplot(subset(data_sub,tiefe==-10.5))+
  geom_line(aes(date,preds_drift,col="CO2"))+
  geom_line(data=subset(data_sub,tiefe==-10.5& Pumpstufe==0),aes(date,CO2_roll_inj,col="inj"))+
  #geom_line(aes(date,VWC_roll*200,col="VWC"))+
  geom_line(aes(date,T_daymean*100,col="T_roll"))+
  geom_line(aes(date,T_soil*100,col="T"))

png(paste0(plotpfad,"SWC_T_calibration_period.png"),width = 7,height = 6,res=300,units = "in")
egg::ggarrange(CO2_plot,T_plot,heights = c(2,1))
dev.off()
