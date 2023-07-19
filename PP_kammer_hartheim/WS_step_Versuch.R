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
Versuch <- 9

start_date <- pp_chamber$Start[Versuch]
start_date2 <- start_date+step_hours*3600*3+8*3600
end_date <- pp_chamber$Ende[Versuch]

step_hours <- pp_chamber$step_hours[Versuch]


datelim <- c(pp_chamber$Start[Versuch]-3600*24*0.5,pp_chamber$Ende[Versuch]+3600*24*0.5)
plot <-  T
if(is.na(datelim[2])){
  datelim[2] <- now()
  #datelim[2] <- ymd_h("23.02.28 04")
}
#datelim <- c(ymd_h("2022-04-13 18"),ymd_h("2022-04-25 18"))
#datelim <- ymd_hm("2022.05.02 00:00","2022.05.02 01:20")
#datelim <- ymd_hm("2022.05.08 18:00","2022.05.10 13:20")
#datelim <- ymd_hm("2022.05.12 10:00","2022.05.16 16:00")

plot_ls <- list()

############
#probe 1 u 2
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")

data_probe1u2 <- data_probe1u2 %>% 
  group_by(tiefe) %>% 
  mutate(CO2_smp1_roll = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
         CO2_smp2_roll = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
  )


plot_ls[["probe2"]] <- ggplot(data_probe1u2)+
  geom_line(aes(date,CO2_smp2_roll,col=as.factor(-tiefe)))+
  scale_fill_manual(values = "blue")+
  scale_color_discrete(limits = factor(0:7*3.5))+
  coord_cartesian(xlim=datelim)+
  guides(fill=F)+
  labs(y = expression(CO[2]~"(ppm)"),fill="",col="depth (cm)",subtitle = "profile 1")
plot_ls[["probe1"]] <- ggplot(data_probe1u2)+
  
  geom_line(aes(date,CO2_smp1_roll,col=as.factor(-tiefe)))+
  #geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe),linetype="probe 1"))+
  scale_fill_manual(values = "blue")+
  scale_color_discrete(limits = factor(0:7*3.5))+
  coord_cartesian(xlim=datelim)+
  guides(col=F)+
  labs(y = expression(CO[2]~"(ppm)"),fill="",col="depth (cm)",subtitle = "profile 2")

##############################
WS_df <- data.frame(start = c(start_date+step_hours*3600*c(0:2),start_date2),
                    stop = c(start_date+step_hours*3600*c(1:3),end_date),
                    pwm = c(100,66,32,100)
)
ggpubr::ggarrange(
  plot_ls$probe2+
    geom_rect(data=WS_df,aes(ymin = -Inf,ymax = Inf, xmin = start, xmax = stop, alpha = factor(pwm)))+
    scale_alpha_discrete("fanspeed (%)",range=c(0.1,0.4))+
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank()),
  plot_ls$probe1+
    geom_rect(data=WS_df,aes(ymin = -Inf,ymax = Inf, xmin = start, xmax = stop, alpha = factor(pwm)))+
    scale_alpha_discrete("pwm",range=c(0.1,0.4))+
    theme(axis.title.x = element_blank()),
  # plot_ls$probe3+
  #   geom_rect(data=WS_df,aes(ymin = -Inf,ymax = Inf, xmin = start, xmax = stop, alpha = factor(pwm)))+
  #   scale_alpha_discrete("pwm",range=c(0.1,0.4)),
  common.legend = T,ncol=1,legend = "right",heights = c(5,5))+
  ggsave(paste0(plotpfad_PPchamber,"WS_Versuch.png"),width=7,height = 5)

CO2_GGA_flux+
  geom_rect(data=WS_df,aes(ymin = -Inf,ymax = Inf, xmin = start, xmax = stop, alpha = factor(pwm)))+
  scale_alpha_discrete("pwm",range=c(0.1,0.4))
CH4_GGA_flux+
  geom_rect(data=WS_df,aes(ymin = -Inf,ymax = Inf, xmin = start, xmax = stop, alpha = factor(pwm)))+
  scale_alpha_discrete("pwm",range=c(0.1,0.4))
