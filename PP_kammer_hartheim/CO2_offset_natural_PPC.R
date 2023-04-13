hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 


klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
klimapfad_CR1000<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/Hartheim CR1000/")
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

#for(Versuch in PP_Versuche){
datelim <- c(ymd_h("22.12.14 10","23.01.10 10"))

###################
#PPC data

data_PPC <- read_PP(datelim = datelim,table="PP_1min")

names(data_PPC)

dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)

data_PPC <- data_PPC %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
         P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA))

P_ref <- subset(data_PPC,id==5)
P_ref$P_ref <- P_ref$P_roll
data_PPC <- subset(data_PPC,id!=5)
data_PPC <- merge(data_PPC,P_ref[,c("date","P_ref")])
data_PPC$P_roll <- data_PPC$P_roll - data_PPC$P_ref

####################
#probe 1 and 2
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")

data_probe1u2 <- data_probe1u2 %>% 
  group_by(tiefe) %>% 
  mutate(CO2_smp1_roll = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
         CO2_smp2_roll = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
  )

###########
#CO2atm

chamber_data <- read_arduino_chamber(datelim=datelim)


chamber_data$atm <- ifelse(minute(chamber_data$date) %% 30 > 5 & minute(chamber_data$date) %% 30 < 29,1,0)

CO2_atm <- chamber_data %>% 
  filter(atm == 1) %>% 
  select(date,CO2,T_C) %>% 
  mutate(date = round_date(date,"10 mins")) %>% 
  group_by(date) %>% 
  summarise(CO2_atm = mean(CO2,na.rm=T),
            T_atm = mean(T_C,na.rm=T)) %>% 
  mutate(CO2_atm = imputeTS::na_interpolation(CO2_atm),
         T_atm = imputeTS::na_interpolation(T_atm))


#########
#swc
source("./PP_kammer_hartheim/SWC_hartheim.R")

swc_sub <- sub_daterange(swc_wide,datelim)
########################


#############
#klima_data

load(paste0(klimapfad_CR1000,"klima_data_PP_kammer.RData"))
#############################################

names(data_PPC)
data_PPC_wide <- tidyr::pivot_wider(data_PPC[,c("date","id","P_roll","PPC")],names_from = id,values_from =c(P_roll,PPC))
names(data_PPC_wide) <- str_replace_all(names(data_PPC_wide),c("P_roll"="P","PPC5"="PPC"))

data_probes_wide <- tidyr::pivot_wider(data_probe1u2,id_cols=c(date,T_C),names_from = tiefenstufe,values_from = matches("CO2_smp"))


data <- merge(data_PPC_wide,data_probes_wide)
data <- merge(data,swc_sub)
data <- merge(data,CO2_atm,all.x = T)
data <- merge(data,klima,all.x = T)
data <- data %>% mutate(zeit = as.numeric(difftime(date,min(date)))) %>% 
  mutate(CO2_atm = imputeTS::na_interpolation(CO2_atm),
         T_atm = imputeTS::na_interpolation(T_atm))

data[,paste0("CO2_smp1_roll_",1:2)] <- NA
data$PPC_roll <- RcppRoll::roll_mean(data$PPC_6,60,fill=NA)

data$calm <- ifelse(data$PPC_roll < 0.15,1,0)
data_long <- tidyr::pivot_longer(data,matches("CO2_smp\\d_roll_\\d"),names_pattern = "CO2_smp(\\d)_roll_(\\d)",values_to = "CO2",names_to = c("probe","tiefe"))
data_long$smp_depth <- paste(data_long$probe,data_long$tiefe,sep="_")
#data <- data %>% mutate(zeit = as.numeric(difftime(date,min(date))))





data_long$CO2_preds <- NA 

for(i in unique(data_long$smp_depth)){
  cal_df <- subset(data_long, smp_depth == i & calm == 1 & !is.na(CO2))
  cal_datelim <- range(cal_df$date)
  if(length(!is.na(cal_df$CO2)) > 10){
    fm <- mgcv::gam(CO2 ~ s(zeit),data = cal_df)
    tiefenID <- which(data_long$smp_depth == i & daterange_id(data_long,cal_datelim))
    data_long$CO2_preds[tiefenID] <- predict(fm,newdata = data_long[tiefenID,])
  }
}
data_long$CO2_offset <- data_long$CO2 - data_long$CO2_preds
change <- c(NA,diff(data$calm))
PPC_dates <- data.frame(start = data$date[which(change == -1)],end = data$date[c(which(change == 1))])

datelim_plot <- ymd_h("22.12.20 10","23.01.10 10")
datelim_plot <- range(data$date)

PPC_plot <- ggplot(data)+
  #geom_line(aes(date,RcppRoll::roll_mean(PPC_2,60,fill=NA)),col=4)+
  geom_rect(data = PPC_dates,aes(xmin = start,xmax = end,ymin = -Inf,ymax = Inf),alpha=0.2)+
  geom_line(aes(date,(PPC_6),col = "PPC"))+
  geom_line(aes(date,PPC_roll,col = "PPC_10h"))+
  xlim(datelim_plot)+
  #geom_hline(yintercept = 0.1)+
  scale_color_manual(values = c(grey(0.5),1))+
  labs(y = "PPC (Pa/s)",col="",x ="")

T_plot <- ggplot(data)+
  #geom_line(aes(date,RcppRoll::roll_mean(PPC_2,60,fill=NA)),col=4)+
  geom_rect(data = PPC_dates,aes(xmin = start,xmax = end,ymin = -Inf,ymax = Inf),alpha=0.2)+
  xlim(datelim_plot)+
  geom_line(aes(date,T_C,col="soil"))+
  geom_line(aes(date,T_atm,col="atm"))+
  labs(y = "T (°C)",col="")

names(data)
p_plot <- ggplot(data)+
  #geom_line(aes(date,RcppRoll::roll_mean(PPC_2,60,fill=NA)),col=4)+
  geom_rect(data = PPC_dates,aes(xmin = start,xmax = end,ymin = -Inf,ymax = Inf),alpha=0.2)+
  xlim(datelim_plot)+
  geom_line(aes(date,P_hPa,col="P (hPa)"))+
  labs(y = "P (hPa)",col="")


CO2_plot <- 
  ggplot(data_long)+
  geom_rect(data = PPC_dates,aes(xmin = start,xmax = end,ymin = -Inf,ymax = Inf),alpha=0.2)+
  #geom_point(data = subset(data_long,calm == 1),aes(date,CO2,col=factor(tiefe)))+
  #geom_line(aes(date,CO2_preds,col=factor(tiefe)),linetype=2)+
  geom_line(aes(date,CO2,col=factor(tiefe)))+
  xlim(datelim_plot)+
  facet_wrap(~probe,ncol=1)+
  labs(x = "",y="CO2 (ppm)",col="tiefe")
CO2_shift_plot <- 
  ggplot(data_long)+
  geom_rect(data = PPC_dates,aes(xmin = start,xmax = end,ymin = -Inf,ymax = Inf),alpha=0.2)+
  geom_line(aes(date,CO2_offset,col=factor(tiefe)))+
  facet_wrap(~probe,ncol=1)

#ggpubr::ggarrange(CO2_plot,CO2_shift_plot,PPC_plot,T_plot,ncol=1,common.legend = T,legend = "right",align = "v",heights = c(3,2,1,1))  
#ggpubr::ggarrange(CO2_plot,PPC_plot,T_plot,ncol=1,common.legend = T,legend = "right",align = "v",heights = c(3,1,1))  
ggpubr::ggarrange(CO2_plot,PPC_plot,T_plot,p_plot,ncol=1,common.legend = F,legend = "right",align = "v",heights = c(4,1,1,1))#+
  ggsave(paste0(plotpfad_PPchamber,"natural_PP.png"),width = 7,height = 7)

ggplot(data_long)+
  geom_point(aes(PPC_roll,CO2_offset))+
  facet_wrap(~probe)
ggplot(data_long)+
  geom_point(aes(CO2,T_C,col=probe))+
  facet_wrap(~tiefe,scales = "free")
  