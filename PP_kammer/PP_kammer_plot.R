hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 


klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)
theme_set(theme_classic())
##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

Versuch <- nrow(pp_chamber)
Versuch <- 1
for(Versuch in 20:nrow(pp_chamber)){
datelim <- c(pp_chamber$Start[Versuch]-3600*24*1,pp_chamber$Ende[Versuch]+3600*24*1)
plot <-  T
#datelim <- c(ymd_h("2022-04-13 18"),ymd_h("2022-04-25 18"))
#datelim <- ymd_hm("2022.05.02 00:00","2022.05.02 01:20")
#datelim <- ymd_hm("2022.05.08 18:00","2022.05.10 13:20")
#datelim <- ymd_hm("2022.05.12 10:00","2022.05.16 16:00")

plot_ls <- list()

load(file = paste(datapfad_PP_Kammer,"klima_DWD.RData"))
if(as.numeric(difftime(now(),max(klima$date),unit="hours")) > 24){
  source("./PP_kammer/klima_dwd.R")
}
klima_sub <- sub_daterange(klima,datelim)
############
#probe 1 u 2
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")


wechsel_date <- ymd_h("22.03.23 14","22.03.28 10")
#CO2_wechsel <- sub_daterange(data_probe1u2,wechsel_date)
#names(data_probe1u2)
data_probe1u2[daterange_id(data_probe1u2,wechsel_date),paste0("CO2_smp",1:2)] <- data_probe1u2[daterange_id(data_probe1u2,wechsel_date),paste0("CO2_smp",2:1)]

plot_ls[["probe1"]] <- ggplot(data_probe1u2)+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  #geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe),linetype="probe 2"))+
  geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe)))+
  scale_fill_manual(values = "black")+
  coord_cartesian(xlim=datelim)+
  guides(fill=F)+
  labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe",title=pp_chamber$Bemerkung[Versuch],subtitle = "probe 1")
plot_ls[["probe2"]] <- ggplot(data_probe1u2)+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe)))+
  #geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe),linetype="probe 1"))+
  scale_fill_manual(values = "black")+
  coord_cartesian(xlim=datelim)+
  guides(col=F)+
  labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe",subtitle = "probe 2")

############################
#kammermessungen
if(exists("flux")){
  rm(flux)
  rm(flux_data)
}

gga_data_T <- !is.na(pp_chamber$GGA_kammermessung[Versuch])
flux_ls <- chamber_arduino(datelim=datelim,gga_data = gga_data_T,return_ls = T,t_init=2,plot="",t_offset = 60,t_min=4,gga=pp_chamber$GGA_kammermessung[Versuch])
flux <- flux_ls[[1]]
flux_data <- flux_ls[[2]]


if(!is.null(flux_data)){
flux_data$atm <- ifelse(minute(flux_data$date) %% 30 > 5,1,0)
  plot_ls[["probe2"]] <- 
    plot_ls[["probe2"]]+
    geom_line(data=subset(flux_data,atm == 1),aes(date,RcppRoll::roll_mean(CO2,5,fill=NA),col="0"))
}

if(!is.null(flux)){
  flux_plot <- ggplot(flux)+
    geom_point(aes(date,CO2_mumol_per_s_m2,col="Dynament"),alpha=0.5)+
    geom_line(aes(date,RcppRoll::roll_mean(CO2_mumol_per_s_m2,3,fill=NA),col="Dynament"),lwd=1)+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
    scale_fill_grey()+
    guides(fill  = F)+
    coord_cartesian(xlim=datelim)
  if("CO2_GGA_mumol_per_s_m2" %in% names(flux)){
    flux_plot <- flux_plot+
      geom_point(aes(date,CO2_GGA_mumol_per_s_m2,col="GGA"))+
      geom_line(aes(date,CO2_GGA_mumol_per_s_m2,col="GGA"))
  }
  plot_ls[["flux"]] <- flux_plot
  
}

############
#swc
source("./PP_kammer/Bodenfeuchte_PP_Kammer.R")

swc_sub <- sub_daterange(swc_long,datelim)
if(nrow(swc_sub) > 0){
  sec_ax_fac <- 0.7
  swc_min <- min(swc_sub$swc,na.rm = T)/1.1
  plot_ls[["swc"]] <- ggplot(swc_sub)+
    geom_line(aes(date,swc,col=as.factor(tiefe)))+
    xlim(datelim)+
    labs(x="",y="SWC (Vol. %)",col="tiefe (cm)")
  if(nrow(klima_sub) > 0){
  plot_ls[["swc"]] <- plot_ls[["swc"]]+
    scale_y_continuous(sec.axis = sec_axis(~(. - swc_min)*sec_ax_fac,name=expression(italic(P)["24h"]*" (mm)")))+
    theme(
      axis.title.y.right = element_text(color = "blue"),
      axis.text.y.right = element_text(color = "blue"),
      axis.text.x = element_blank()
    )+
    geom_ribbon(data=klima_sub,aes(x=date,ymin=swc_min,ymax=P24tot/sec_ax_fac + swc_min),fill="blue",alpha=0.8)
    
  }
}

###############
#PPC

data_PPC <- read_PP(datelim = datelim)

if(nrow(data_PPC) > 0){
  data_PPC <- subset(data_PPC,id %in% c(1:4,5))
  dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)
  
  data_PPC <- data_PPC %>% 
    group_by(id) %>%
    mutate(dt = diff_time(date,"secs"),
           P_diff = abs(c(NA,diff(P_filter)))/!!dt,
           PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
           P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA))
  
  data_PPC$id[data_PPC$id == 6] <- "outside"
  data_PPC$id[data_PPC$id == 5] <- "soil"
  data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
  
  
  
  plot_ls[["PPC"]] <- 
    ggplot(data_PPC)+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,PPC5,col=id))+
    coord_cartesian(xlim=datelim)+
    scale_fill_grey()+
    guides(fill=F)+
    labs(x="",y="PPC (Pa/s)")
  
  plot_ls[["P_roll"]] <- ggplot(subset(data_PPC,id %in% 1:4) )+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,P_roll,col=id),alpha=0.5, linetype=2)+
    coord_cartesian(xlim=datelim)+
    scale_fill_grey()+
    guides(fill=F,col=F)+
    labs(x="",y=expression(P["moving average"]))
}

#################
#ws

if(nrow(klima_sub) > 0){
plot_ls[["ws"]] <- ggplot(klima_sub)+
  geom_line(aes(date,wind,col="WS"))+
  labs(x="",y="windspeed (m/s)")
}else{
plot_ls[["ws"]] <- ggplot()
}
if(!is.null(flux_data)){
  sec_ax_T <- 4
  plot_ls[["ws"]] <- plot_ls[["ws"]]+
    geom_line(data=flux_data,aes(date,T_C/sec_ax_T,col="T"))+
    scale_y_continuous(sec.axis = sec_axis(~.*sec_ax_T,name=expression(T["atm"]~"(°C)")))+
    coord_cartesian(xlim=datelim)
}

# load(file = paste(datapfad_PP_Kammer,"data_ws.RData"))
# 
# ws_sub <- sub_daterange(data_ws,datelim)
# 
# if(nrow(ws_sub)>0){
#   plot_ls[["ws"]] <- plot_ls[["ws"]]+
#     geom_line(data = ws_sub,aes(date,RcppRoll::roll_median(WS,60,fill=NA),col="WS chamber"))+
#     geom_point(data = ws_sub,aes(date,RcppRoll::roll_median(WS,60,fill=NA),col="WS chamber"))+
#     xlim(datelim)
#   
#   
# }
if(plot){
  png(paste0(plotpfad_PPchamber,"PP_Versuch",Versuch,".png"),width = 9,height = 10,units = "in",res=300)
}
egg::ggarrange(plots=plot_ls,ncol=1,heights = c(2,2,rep(1,length(plot_ls)-2)))

if(plot){
  dev.off()
}
}


CO2_GGA_flux <- ggplot(flux)+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_point(aes(date,CO2_GGA_mumol_per_s_m2,col="CO2"))+
  geom_line(aes(date,CO2_GGA_mumol_per_s_m2,col="CO2"),alpha=0.3)+
  geom_line(aes(date,RcppRoll::roll_mean(CO2_GGA_mumol_per_s_m2,5,fill=NA),col="CO2"))+
    coord_cartesian(xlim=datelim)+
  labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
    scale_fill_grey()

CH4_GGA_flux <- ggplot(flux)+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_point(aes(date,CH4_mumol_per_s_m2*10^3,col="CH4"))+
  geom_line(aes(date,(CH4_mumol_per_s_m2*10^3),col="CH4"),alpha=0.3)+
  geom_line(aes(date,RcppRoll::roll_mean(CH4_mumol_per_s_m2*10^3,5,fill=NA),col="CH4"))+
    coord_cartesian(xlim=datelim)+
  labs(x="",y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
    scale_fill_grey()+
  guides(fill=F)+
  scale_color_manual(values = scales::hue_pal()(2)[2])

if(plot & "CH4_R2" %in% names(flux)){
  png(paste0(plotpfad_PPchamber,"GGA_Versuch",Versuch,".png"),width = 9,height = 10,units = "in",res=300)
}
#egg::ggarrange(CO2_GGA_flux,CH4_GGA_flux,plot_ls$PPC,ncol=1,heights=c(2,2,1))
egg::ggarrange(CO2_GGA_flux,CH4_GGA_flux,ncol=1)

if(plot & "CH4_R2" %in% names(flux)){
  dev.off()
}
