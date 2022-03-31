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


for(i in 1:nrow(pp_chamber)){
  datelim <- c(pp_chamber$Start[i]-3600*24*2,pp_chamber$Ende[i]+3600*24*2)
  
  plot_ls <- list()
  
  
  ############
  #probe 1 u 2
  data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")
  
  wechsel_date <- ymd_h("22.03.23 14","22.03.28 10")
  #CO2_wechsel <- sub_daterange(data_probe1u2,wechsel_date)
  #names(data_probe1u2)
  data_probe1u2[daterange_id(data_probe1u2,wechsel_date),paste0("CO2_smp",1:2)] <- data_probe1u2[daterange_id(data_probe1u2,wechsel_date),paste0("CO2_smp",2:1)]
  
  plot_ls[["probe1"]] <- ggplot(data_probe1u2)+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    #geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe),linetype="probe 2"))+
    geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe)))+
    scale_fill_manual(values = "black")+
    xlim(datelim)+
    guides(fill=F)+
    labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe",title=pp_chamber$Bemerkung[i],subtitle = "probe 1")
  plot_ls[["probe2"]] <- ggplot(data_probe1u2)+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe)))+
    #geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe),linetype="probe 1"))+
    scale_fill_manual(values = "black")+
    xlim(datelim)+
    guides(fill=F,col=F)+
    labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe",subtitle = "probe 2")
  
  ############################
  #kammermessungen
  flux <- chamber_arduino(datelim,gga_data = T,return_ls = F,t_init=2,plot="",t_offset = 60,t_min=4)
  if(!is.null(flux)){
    flux_plot <- ggplot(flux)+
      geom_point(aes(date,CO2_mumol_per_s_m2,col="Dynament"))+
      geom_line(aes(date,CO2_mumol_per_s_m2,col="Dynament"))+
      geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
      geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
      labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
      scale_fill_grey()+
      xlim(datelim)
    if("CO2_GGA_mumol_per_s_m2" %in% names(flux)){
      flux_plot <- flux_plot+
        geom_point(aes(date,CO2_GGA_mumol_per_s_m2,col="GGA"))+
        geom_line(aes(date,CO2_GGA_mumol_per_s_m2,col="GGA"))
    }
    plot_ls[["flux"]] <- flux_plot
  }
  
  ############
  #swc
  load(file = paste(datapfad_FVAgarten,"swc_long.RData"))
  plot_ls[["swc"]] <- ggplot(sub_daterange(swc_long,datelim))+
    geom_line(aes(date,swc,col=as.factor(tiefe)))+
    xlim(datelim)+
    labs(x="",y="SWC (Vol. %)",col="tiefe (cm)")
  
  
  
  #################
  #ws
  load(file = paste(datapfad_PP_Kammer,"data_ws.RData"))
  ws_sub <- sub_daterange(data_ws,datelim)
  
  if(nrow(ws_sub)>0){
    plot_ls[["ws"]] <- ggplot(sub_daterange(data_ws,datelim))+
      geom_line(aes(date,WS))+
      xlim(datelim)+
      labs(x="",y="windspeed (m/s)")
    
    
  }
  png(paste0(plotpfad_PPchamber,"PP_Versuch",i,".png"),width = 9,height = 9,units = "in",res=300)
  egg::ggarrange(plots=plot_ls,ncol=1,heights = c(2,2,rep(1,length(plot_ls)-2)))
  dev.off()
}
