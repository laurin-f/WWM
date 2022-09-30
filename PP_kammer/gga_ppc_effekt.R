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
pp_chamber <- read_ods(paste0(metapfad_PP,"injektionen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

Versuch <- nrow(pp_chamber)
Versuch <- 2
#for(Versuch in 20:nrow(pp_chamber)){
datelim <- c(pp_chamber$Start[Versuch]-3600*24*2,pp_chamber$Ende[Versuch]+3600*24*2)
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

data_PPC <- read_PP(datelim = datelim)


  data_PPC <- subset(data_PPC,id %in% c(1:6))
  dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)
  
  data_PPC <- data_PPC %>% 
    group_by(id) %>%
    mutate(dt = diff_time(date,"secs"),
           P_diff = abs(c(NA,diff(P_filter)))/!!dt,
           PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
           P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA))
  
  data_PPC$id[data_PPC$id == 6] <- "outside"
  data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
names(data_PPC)
data_PPC_wide <- tidyr::pivot_wider(data_PPC,date,names_from = id,values_from = PPC5,names_prefix = "PPC_")  

data_PPC_wide$PPC_sum <- data_PPC_wide$PPC_2 + data_PPC_wide$PPC_outside



gga <- "gga"
#datelim <- ymd_hm("22.09.28 11:20","22.09.28 11:40")
flux_ls <- chamber_arduino(datelim=datelim,gga_data = T,return_ls = T,t_init=1,plot="flux",t_offset = -70,t_min=3,t_max=3)
flux <- flux_ls[[1]]
flux_data <- flux_ls[[2]]

flux_sub <- subset(flux,!is.na(CH4_R2))
data_merge <- merge(flux_sub,data_PPC_wide)

CO2_GGA_flux <- ggplot(data_merge)+
  geom_point(aes(date,CO2_GGA_mumol_per_s_m2,col="CO2"))+
  geom_line(aes(date,CO2_GGA_mumol_per_s_m2,col="CO2"))+
  guides(col=F)+
  labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")

CH4_GGA_flux <- ggplot(data_merge)+
  geom_point(aes(date,CH4_mumol_per_s_m2*10^3,col="CH4"))+
  geom_line(aes(date,(CH4_mumol_per_s_m2*10^3),col="CH4"))+
  labs(x="",y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
  guides(col=F)+
  scale_color_manual(values = scales::hue_pal()(2)[2])

PPC_sum_plot <- ggplot(data_merge)+
  geom_line(aes(date,PPC_2,col="artificial"))+
  geom_line(aes(date,PPC_outside,col="natural"))+
  geom_line(aes(date,PPC_sum,col="sum"))+
  scale_color_manual(values=c(2,4,1))+
  theme(legend.position = "top")+
  labs(x = "",y="PPC (Pa/s)",col="")


T_plot <- 
  ggplot(subset(data_merge,!is.na(T_C)))+
  geom_line(aes(date,RcppRoll::roll_mean(T_C,10,fill=NA)))+
  labs(y=expression(T["atm"]~"(°C)"))+
  xlim(range(data_merge$date))

timelines <- ggpubr::ggarrange(CO2_GGA_flux,CH4_GGA_flux,PPC_sum_plot,ncol=1,align = "v")

CO2_scatter <- ggplot(data_merge,aes(PPC_sum,CO2_GGA_mumol_per_s_m2))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[1])+
  ggpubr::stat_regline_equation(label.y = 2.49,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y = 2.4,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),x= expression(PPC[sum]~"(Pa/s)"))

CH4_scatter <- 
  ggplot(data_merge,aes(PPC_sum,CH4_mumol_per_s_m2*10^3))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[2])+
  ggpubr::stat_regline_equation(label.y=-1.1,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y=-1.13,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),x= expression(PPC[sum]~"(Pa/s)"))

CO2_T <- ggplot(data_merge,aes(T_C,CO2_GGA_mumol_per_s_m2))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[1])+
  ggpubr::stat_regline_equation(label.y = 2.49,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y = 2.4,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),x= "T (°C)")

CH4_T <- 
  ggplot(data_merge,aes(T_C,CH4_mumol_per_s_m2*10^3))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[2])+
  ggpubr::stat_regline_equation(label.y=-1.1,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y=-1.13,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),x= "T (°C)")
#ggpubr::ggarrange(CO2_scatter,CH4_scatter)

ggpubr::ggarrange(CO2_scatter,CO2_T,CH4_scatter,CH4_T)+
  ggsave(paste0(plotpfad_PPchamber,"flux_scatterplots.png"),width = 7,height = 6)

png(paste0(plotpfad_PPchamber,"GGA_PPC_effect.png"),width=7,height = 7,unit="in",res=300)
grid.arrange(timelines,
             CO2_scatter+theme(axis.title = element_blank()),
             CH4_scatter+theme(axis.title.y = element_blank()),
             layout_matrix = rbind(c(1,1,4),
                                   c(1,1,5),
                                   c(1,1,NA)))
dev.off()
