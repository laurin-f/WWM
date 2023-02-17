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
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

injections <- read_ods(paste0(metapfad_PP,"injektionen_hartheim.ods"))
injections$Start <- dmy_hm(injections$Start)
injections$Ende <- dmy_hm(injections$Ende)

Versuch <- nrow(pp_chamber)
Versuch <- 24
#for(Versuch in 1:nrow(pp_chamber)){
datelim <- c(pp_chamber$Start[Versuch]-3600*24*0.5,pp_chamber$Ende[Versuch]+3600*24*0.5)
plot <-  T
if(is.na(datelim[2])){
  datelim[2] <- now()
}
#datelim <- c(ymd_h("2022-04-13 18"),ymd_h("2022-04-25 18"))
#datelim <- ymd_hm("2022.05.02 00:00","2022.05.02 01:20")
#datelim <- ymd_hm("2022.05.08 18:00","2022.05.10 13:20")
#datelim <- ymd_hm("2022.05.12 10:00","2022.05.16 16:00")

plot_ls <- list()


###############
#load PPC

data_PPC <- read_PP(datelim = datelim,table="PP_1min")

if(nrow(data_PPC) > 0){
  data_PPC <- subset(data_PPC,id %in% c(1:6))
  dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)
  
  data_PPC <- data_PPC %>% 
    group_by(id) %>%
    mutate(dt = diff_time(date,"secs"),
           PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
           P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA))
  
  data_PPC$id[data_PPC$id == 6] <- "outside"
  #data_PPC$id[data_PPC$id == 5] <- "reference"
  #data_PPC$P_roll[abs(data_PPC$P_roll) > 100] <- NA
  data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
  names(data_PPC)
  data_Proll_wide <- tidyr::pivot_wider(data_PPC,date,values_from = P_roll,names_from = id,names_prefix = "id_")
  data_Proll_wide[paste0("id_",1:4)] <- data_Proll_wide[paste0("id_",1:4)] - data_Proll_wide$id_5
  data_Proll <- tidyr::pivot_longer(data_Proll_wide[,1:6],paste0("id_",1:5),names_prefix = "id_",names_to = "id",values_to="P_roll")
  
  step_thr <- 0.03
  data_PPC <- data_PPC[order(data_PPC$date),]
  PPC_steps <- data_PPC %>%
    filter(id %in% 1:4) %>%
    mutate(date = round_date(date,"10 min")) %>%
    group_by(id,date) %>%
    summarise(across(everything(),mean)) %>%
    mutate(PPC_diff = abs(c(NA,diff(PPC5))),
           step = ifelse(PPC_diff > step_thr,1,0))
  
  
  step_date <- sort(unique(PPC_steps$date[PPC_steps$step == 1]))
  step_date <- step_date[c(as.numeric(diff(step_date)),100) > 60]
  step_date <- step_date[!is.na(step_date)]
  
}

step_df <- PPC_steps %>% 
  mutate(step = ifelse(date %in% !!step_date,1,0),
         step_id=cumsum(step)) %>% 
  group_by(step_id) %>% 
  summarise(PPC = mean(PPC,na.rm=T),
            Start = min(date),
            End = max(date))

# ggplot(PPC_steps)+
#   geom_line(aes(date,PPC_diff))+
#   geom_hline(yintercept = step_thr)
############
#probe 1 u 2
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")
data_probe3 <- read_sampler("sampler3",datelim = datelim, format = "long")
if(!is.null(nrow(data_probe3))){
  if(nrow(data_probe3) > 1){
    data_probe3 <- data_probe3 %>% 
      filter(!is.na(CO2)) %>%
      mutate(date = round_date(date,"5 mins")) %>% 
      group_by(date,tiefe) %>% 
      summarise(CO2 = mean(CO2,na.rm=T)) %>% 
      ungroup() %>% 
      group_by(tiefe) %>% 
      mutate(CO2_roll = RcppRoll::roll_mean(CO2,5,fill=NA),
             tiefe = tiefe + 3.5)
  }else{
  data_probe3 <- NULL
  }
}
data_probe1u2 <- data_probe1u2 %>% 
  group_by(tiefe) %>% 
  mutate(CO2_smp1_roll = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
         CO2_smp2_roll = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
  )

plot_ls[["probe1"]] <- ggplot(data_probe1u2)+
  geom_vline(xintercept = step_date,linetype=2,color="grey")+
  geom_rect(data=injections,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_line(aes(date,CO2_smp1_roll,col=as.factor(-tiefe)))+
  scale_fill_manual(values = "black")+
  scale_color_discrete(limits = factor(0:7*3.5))+
  coord_cartesian(xlim=datelim)+
  guides(fill=F)+
  labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe",title=paste("Versuch",Versuch,"Modus",pp_chamber$Modus[Versuch]),subtitle = "probe 1")
plot_ls[["probe2"]] <- ggplot(data_probe1u2)+
  geom_vline(xintercept = step_date,linetype=2,color="grey")+
  geom_rect(data=injections,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="injection"),alpha=0.1)+
  #geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="injection"),alpha=0.1)+
  geom_line(aes(date,CO2_smp2_roll,col=as.factor(-tiefe)))+
  #geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe),linetype="probe 1"))+
  scale_fill_manual(values = "black")+
  scale_color_discrete(limits = factor(0:7*3.5))+
  coord_cartesian(xlim=datelim)+
  guides(col=F)+
  labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe",subtitle = "probe 2")

if(!is.null(data_probe3)){
  
  plot_ls[["probe3"]] <- ggplot(data_probe3)+
    geom_vline(xintercept = step_date,linetype=2,color="grey")+
    geom_rect(data=injections,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="injection"),alpha=0.1)+
    #geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="injection"),alpha=0.1)+
    geom_line(aes(date,CO2_roll,col=as.factor(-tiefe)))+
    #geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe),linetype="probe 1"))+
    scale_fill_manual(values = "black")+
    scale_color_discrete(limits = factor(0:7*3.5))+
    coord_cartesian(xlim=datelim)+
    guides(col=F)+
    labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe",subtitle = "probe 3")
}  
############################
#kammermessungen
if(exists("flux")){
  rm(flux)
  rm(flux_data)
}

gga_data_T <- !is.na(pp_chamber$GGA_kammermessung[Versuch])
gga <- "gga"
#datelim <- ymd_hm("22.09.28 11:20","22.09.28 11:40")
flux_ls <- chamber_arduino(datelim=datelim,
                           gga_data = T,
                           return_ls = T,
                           t_init=1,
                           plot="timeline",
                           t_offset = "from_df",
                           t_min=2,
                           t_max=2)
flux <- flux_ls[[1]]
flux_data <- flux_ls[[2]]
range(flux_data$date)

if(!is.null(flux_data)){
  flux_data$atm <- ifelse(minute(flux_data$date) %% 30 > 5 & minute(flux_data$date) %% 30 < 29,1,0)
  CO2_atm_plot <- ggplot()+
    geom_line(data=subset(flux_data,atm == 1 & !is.na(CO2_GGA)),aes(date,RcppRoll::roll_mean(CO2_GGA,5,fill=NA),col="0"))
  plot_ls[["probe2"]] <- 
    plot_ls[["probe2"]]+
    geom_line(data=subset(flux_data,atm == 1 & !is.na(CO2_GGA)),aes(date,RcppRoll::roll_mean(CO2_GGA,5,fill=NA),col="0"))
}
# egg::ggarrange(CO2_atm_plot+coord_cartesian(xlim=datelim)+geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
#                  scale_alpha(range = c(0,0.3))+
#                  guides(alpha = F),plot_ls$PPC)
names(flux)
if(!is.null(flux)){
  flux_plot <- ggplot(flux)+
    geom_vline(xintercept = step_date,linetype=2,color="grey")+
    #geom_point(aes(date,CO2_mumol_per_s_m2,col="Dynament"),alpha=0.5)+
    #geom_line(aes(date,RcppRoll::roll_mean(CO2_mumol_per_s_m2,3,fill=NA),col="Dynament"),lwd=1)+
    geom_rect(data=injections,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    #geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
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
source("./PP_kammer_hartheim/SWC_hartheim.R")

swc_sub <- sub_daterange(swc_long,datelim)
if(nrow(swc_sub) > 0){
  sec_ax_fac <- 0.7
  swc_min <- min(swc_sub$swc,na.rm = T)/1.1
  plot_ls[["swc"]] <- ggplot(swc_sub)+
    geom_line(aes(date,swc,col=as.factor(tiefe)))+
    xlim(datelim)+
    labs(x="",y="SWC (Vol. %)",col="tiefe (cm)")
}


###############
#plot PPC
if(nrow(data_PPC) > 0){
  plot_ls[["PPC"]] <- 
    ggplot(subset(data_PPC,date %in% round_date(date,"mins")))+
    geom_vline(xintercept = step_date,linetype=2,color="grey")+
    geom_rect(data=injections,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    #geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,PPC,col=id))+
    coord_cartesian(xlim=datelim)+
    scale_fill_grey()+
    guides(fill=F)+
    labs(x="",y="PPC (Pa/s)")
  
  plot_ls[["P_roll"]] <- ggplot(subset(data_Proll,id %in% c(1:4) & date %in% round_date(date,"mins")) )+
    geom_vline(xintercept = step_date,linetype=2,color="grey")+
    geom_rect(data=injections,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    #geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,P_roll,col=id))+
    coord_cartesian(xlim=datelim)+
    scale_fill_grey()+
    scale_color_discrete(limits=c(1:5,"outside"))+
    guides(fill=F,col=F)+
    labs(x="",y=expression(P["moving average"]~"(Pa)"))
}

#################
#ws


if(!is.null(flux_data)){
  plot_ls[["T_C"]] <- 
    ggplot()+
    geom_line(data=subset(flux,!is.na(T_C)),aes(date,RcppRoll::roll_mean(T_C,10,fill=NA)))+
    labs(y=expression(T["atm"]~"(°C)"))+
    xlim(datelim)
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
  png(paste0(plotpfad_PPchamber,"PP_hartheim",Versuch,".png"),width = 9,height = 10,units = "in",res=300)
}
egg::ggarrange(plots=plot_ls,ncol=1,heights = c(2,2,rep(1,length(plot_ls)-2)))

if(plot){
  dev.off()
}


if(plot){
  png(paste0(plotpfad_PPchamber,"CO2_profile_hartheim",Versuch,".png"),width = 9,height = 10,units = "in",res=300)
}
if(!is.null(data_probe3)){
  egg::ggarrange(plot_ls$probe1+
                   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
                   scale_alpha(range = c(0,0.3))+
                   guides(alpha = F),
                 plot_ls$probe2+
                   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
                   scale_alpha(range = c(0,0.3))+
                   guides(alpha = F),
                 plot_ls$probe3+
                   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
                   scale_alpha(range = c(0,0.3))+
                   guides(alpha = F),
                 plot_ls$PPC+
                   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
                   scale_alpha(range = c(0,0.3))+
                   guides(alpha = F),
                 plot_ls$P_roll+
                   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
                   scale_alpha(range = c(0,0.3))+
                   guides(alpha = F),
                 ncol=1,heights = c(2,2,2,1,1))
}else{
  egg::ggarrange(plot_ls$probe1+
                   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
                   scale_alpha(range = c(0,0.3))+
                   guides(alpha = F),
                 plot_ls$probe2+
                   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
                   scale_alpha(range = c(0,0.3))+
                   guides(alpha = F),
                 plot_ls$PPC+
                   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
                   scale_alpha(range = c(0,0.3))+
                   guides(alpha = F),
                 plot_ls$P_roll+
                   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
                   scale_alpha(range = c(0,0.3))+
                   guides(alpha = F),
                 ncol=1,heights = c(2,2,1,1))
}
if(plot){
  dev.off()
}
#}

if(plot & "CH4_R2" %in% names(flux)){
  flux_gga <- subset(flux,!is.na(CO2_GGA_mumol_per_s_m2) & CO2_GGA_R2 > 0.9)
  
  CO2_GGA_flux <- ggplot(flux_gga)+
    geom_vline(xintercept = step_date,linetype=2,col="grey")+
    #geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_point(aes(date,CO2_GGA_mumol_per_s_m2,col="CO2"))+
    geom_line(aes(date,CO2_GGA_mumol_per_s_m2,col="CO2"),alpha=0.3)+
    geom_line(aes(date,RcppRoll::roll_mean(CO2_GGA_mumol_per_s_m2,5,fill=NA),col="CO2"))+
    #coord_cartesian(xlim=datelim)+
    labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="",title=paste("Versuch",Versuch,"Modus",pp_chamber$Modus[Versuch]))+
    scale_fill_grey()
  
  CH4_GGA_flux <- ggplot(flux_gga)+
    geom_vline(xintercept = step_date,linetype=2,col="grey")+
    #geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_point(aes(date,CH4_mumol_per_s_m2*10^3,col="CH4"))+
    geom_line(aes(date,(CH4_mumol_per_s_m2*10^3),col="CH4"),alpha=0.3)+
    geom_line(aes(date,RcppRoll::roll_mean(CH4_mumol_per_s_m2*10^3,5,fill=NA),col="CH4"))+
    #coord_cartesian(xlim=datelim)+
    labs(x="",y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
    scale_fill_grey()+
    guides(fill=F)+
    scale_color_manual(values = scales::hue_pal()(2)[2])
  
  ppc_plot <- 
    ggplot(subset(data_PPC,date %in% round_date(date,"mins")))+
    geom_vline(xintercept = step_date,linetype=2,color="grey")+
    geom_rect(data=injections,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    #geom_rect(data=pp_chamber[Versuch,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,PPC5,col=id))+
    xlim(range(flux_gga$date))+
    scale_fill_grey()+
    guides(fill=F)+
    labs(x="",y="PPC (Pa/s)")
  
  png(paste0(plotpfad_PPchamber,"GGA_hartheim",Versuch,".png"),width = 9,height = 10,units = "in",res=300)
  egg::ggarrange(CO2_GGA_flux,CH4_GGA_flux,ppc_plot,plot_ls$T_C + xlim(range(flux_gga$date)),ncol=1)
  #egg::ggarrange(CO2_GGA_flux,CH4_GGA_flux,plot_ls$PPC,ncol=1,heights=c(2,2,1))
  
  
  dev.off()
  
}
#}#for i in Versuch
names(flux)
CO2_gga_plot <- ggplot(subset(flux,!is.na(CO2_GGA_mumol_per_s_m2)))+geom_line(aes(date,CO2_GGA_mumol_per_s_m2))+
  geom_vline(xintercept = step_date,linetype=2,col="grey")

CH4_plot <- ggplot(subset(flux,!is.na(CO2_GGA_mumol_per_s_m2)))+geom_line(aes(date,CH4_mumol_per_s_m2))+
  geom_vline(xintercept = step_date,linetype=2,col="grey")
egg::ggarrange(CH4_plot,CO2_gga_plot,ppc_plot,ncol=1)

# zoom_date <- ymd_hm("22.10.02 14:00","22.10.02 16:00")
# zoom_date_2 <- ymd_hm("22.10.02 15:18","22.10.02 15:20")
# PPC_outside <- subset(data_PPC,id=="outside")
# PPC_plot_zoom <- ggplot(sub_daterange(PPC_outside,zoom_date))+
#   geom_line(aes(date,PPC,col=factor(id)))+theme_bw()
# P_plot <- ggplot(sub_daterange(PPC_outside,zoom_date))+
#   geom_line(aes(date,P,col=factor(id)))+theme_bw()
# P_filter_plot <- ggplot(sub_daterange(PPC_outside,zoom_date))+
#   geom_line(aes(date,P_filter,col=factor(id)))+
#   ggforce::facet_zoom(xlim=zoom_date_2)+theme_bw()
# 
# ggpubr::ggarrange(P_plot,P_filter_plot,ncol=1,heights = c(1,2))
