hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_EGU <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Dokumentation/Praesentationen/EGU_22/"
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


i <- 11
datelim <- c(pp_chamber$Start[i]-3600*12*1,pp_chamber$Ende[i]+3600*12*1)
plot_ls <- list()

##################################
#flux
####################################

gga_data_T <- !is.na(pp_chamber$GGA_kammermessung[i])
flux_ls <- chamber_arduino(datelim=datelim,gga_data = gga_data_T,return_ls = T,t_init=2,plot="",t_offset = 60,t_min=4,gga=pp_chamber$GGA_kammermessung[i])
flux <- flux_ls[[1]]
flux_data <- flux_ls[[2]]
0:59%% 30 > 5
flux_data$atm <- ifelse(minute(flux_data$date) %% 30 > 5,1,0)

RColorBrewer::brewer.pal(n=2,name="dark2",type="qual")
cols <- RColorBrewer::brewer.pal(3,"Dark2")
col1 <- cols[1]
col2 <- cols[2]


if(!is.null(flux)){
  flux_plot <- ggplot(flux)+
    geom_point(aes(date,CO2_mumol_per_s_m2),col=col1,alpha=0.5)+
    geom_line(aes(date,RcppRoll::roll_mean(CO2_mumol_per_s_m2,1,fill=NA)),col=col1,lwd=1)+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    labs(subtitle=expression(CO[2]~"efflux"),x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
    scale_fill_grey()+
    guides(fill  = F)+
    theme(axis.text.x = element_blank())+
    coord_cartesian(xlim=datelim)
  if("CO2_GGA_mumol_per_s_m2" %in% names(flux)){
    flux_plot <- flux_plot+
      geom_point(aes(date,CO2_GGA_mumol_per_s_m2,col="GGA"))+
      geom_line(aes(date,CO2_GGA_mumol_per_s_m2,col="GGA"))
  }
  plot_ls[["flux"]] <- flux_plot
  
}
plot_ls[["flux"]]

####################################################################
#probe 1 u 2
####################################################################
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")

data_probe1u2$tiefe <- data_probe1u2$tiefe - 1


plot_ls[["probe1"]] <- 
  ggplot(data_probe1u2)+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  #geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe),linetype="probe 2"))+
  geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe)))+
  scale_fill_manual(values = "black")+
  coord_cartesian(xlim=datelim)+
  theme(axis.text.x = element_blank())+
  guides(fill=F)+
  labs(x="", y = expression(CO[2]~"(ppm)"),fill="",col="depth (cm)",subtitle = "profile 1")
plot_ls[["probe2"]] <- ggplot(data_probe1u2)+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP chamber"),alpha=0.1)+
  geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP chamber"),alpha=0.1)+
  geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe)))+
  #geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe),linetype="probe 1"))+
  scale_fill_manual(values = "black")+
  coord_cartesian(xlim=datelim)+
  guides(col=F)+
  theme(axis.text.x = element_blank())+
  labs(x="", y = expression(CO[2]~"(ppm)"),fill="",col="depth",subtitle = "profile 2")


if(!is.null(flux_data)){
  plot_ls[["probe2"]] <- 
    plot_ls[["probe2"]]+
    geom_line(data=subset(flux_data,atm == 1),aes(date,RcppRoll::roll_mean(CO2,50,fill=NA),col=factor("0",levels = c("0",unique(data_probe1u2$tiefe)))))+
      scale_color_discrete(limits = as.character(c(0,unique(-data_probe1u2$tiefe))))
  plot_ls[["probe1"]] <- 
    plot_ls[["probe1"]]+
    geom_line(data=subset(flux_data,atm == 1),aes(date,RcppRoll::roll_mean(CO2,50,fill=NA),col="0"))+
      scale_color_discrete(limits = as.character(c(0,unique(-data_probe1u2$tiefe))))
}

###########################
# PPC
############################

data_PPC <- read_PP(datelim = datelim)

range(data_PPC$date[data_PPC$PPC5 != 0])
if(nrow(data_PPC) > 0){
  data_PPC <- subset(data_PPC,!id %in% 5:6)
  dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)
  
  
  data_PPC <- data_PPC %>% 
    group_by(id) %>%
    mutate(dt = diff_time(date,"secs"),
           P_diff = abs(c(NA,diff(P_filter)))/!!dt,
           PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
           P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA),
           PPC5 = case_when(is.na(PPC5) ~ 0,
                            T ~ PPC5))
  
  data_PPC <- data_PPC %>% 
    filter(id %in% 1:4) %>% 
    group_by(date) %>% 
    summarise(PPC5 = mean(PPC5),
              PPC = mean(PPC),
              P_roll = mean(P_roll),
              dt = mean(dt),
              )
  
#  data_PPC$id[data_PPC$id == 6] <- "outside"
  data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
  
  
  
  plot_ls[["PPC"]] <- 
    ggplot(data_PPC)+
    geom_line(aes(date,PPC5),col=col2)+
    geom_ribbon(aes(date,ymax=PPC5,ymin=0),fill=col2,alpha=0.2)+
    coord_cartesian(xlim=datelim)+
    guides(fill=F)+
    scale_y_continuous(breaks = c(0.1,0.3,0.5))+
    labs(x="",y="PPC (Pa/s)")
  
}

png(paste0(plotpfad_EGU,"EGU_plot.png"),width = 5,height=5,unit="in",res=300)
egg::ggarrange(plots=plot_ls[c("flux", "PPC")],heights = 2:1)
dev.off()

png(paste0(plotpfad_EGU,"EGU_plot2.png"),width = 7,height=6,unit="in",res=300)
egg::ggarrange(plots=plot_ls,heights = c(2,2,2,1))
dev.off()

plot_ls[["PPC"]]
names(plot_ls)
