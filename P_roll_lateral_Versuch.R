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


Versuch <- 20
#for(Versuch in 1:nrow(pp_chamber)){
datelim <- c(pp_chamber$Start[Versuch],pp_chamber$Ende[Versuch]+3600*24*0.5)

data_PPC <- read_PP(datelim = datelim,table="PP_1min")

data_PPC <- subset(data_PPC,id %in% c(1:5))
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

step_thr <- 0.25
data_PPC <- data_PPC[order(data_PPC$date),]
PPC_steps <- data_PPC %>%
  filter(id %in% 1:4) %>%
  mutate(date = round_date(date,"10 min")) %>%
  group_by(id,date) %>%
  summarise(across(everything(),mean)) %>%
  mutate(PPC_diff = abs(c(NA,diff(P_roll))),
         step = ifelse(PPC_diff > step_thr,1,0))


step_date <- sort(unique(PPC_steps$date[PPC_steps$step == 1]))
step_date <- step_date[c(as.numeric(diff(step_date)),100) > 60]
step_date <- step_date[!is.na(step_date)]

PPC_steps_wide <- tidyr::pivot_wider(PPC_steps,id_cols = date,names_from = id,values_from = P_roll,names_prefix = "P_roll_")

step_df <- PPC_steps_wide %>% 
  mutate(step = ifelse(date %in% !!step_date,1,0),
         step_id=cumsum(step)) %>% 
  group_by(step_id) %>% 
  summarise(across(matches("P_roll"),mean,na.rm=T),
            Start = min(date),
            End = max(date))

ggplot(PPC_steps)+
  geom_vline(xintercept = step_date,col="grey",linetype=2)+
  geom_hline(yintercept = step_thr,col="grey",linetype=2)+
  geom_line(aes(date,PPC_diff))

modes <- c(rep(c("0,0","1,1","1,-1","-1,1","-1,-1"),3),"0,0")
step_df$mode <- modes

data_PPC$step_id <- NA
data_PPC$mode <- NA
for(i in 1:nrow(step_df)){
  id <- which(daterange_id(data_PPC,c(step_df$Start[i],step_df$End[i])))
  data_PPC$mode[id] <- step_df$mode[i]
  data_PPC$step_id[id] <- step_df$step_id[i]
}




cal_period <- data_PPC %>% 
  
  filter(mode == "0,0") %>% 
  filter(!is.na(P_roll)) %>% 
  group_by(id) %>% 
  mutate(zeit = as.numeric(difftime(date,min(date))),
         sensor_drift = predict(glm(P_roll ~ zeit)),
         P_roll_cal = P_roll - sensor_drift)
data_PPC <- merge(data_PPC,cal_period[,c("date","sensor_drift","id")],all = T)
data_PPC <- data_PPC %>% 
  group_by(id) %>% 
  mutate(sensor_drift = imputeTS::na_interpolation(sensor_drift),
         P_roll_cal = P_roll - sensor_drift)

ggplot(data_PPC)+
  geom_line(aes(date,P_roll,col=id))+
  geom_line(aes(date,sensor_drift,col=id))
ggplot(data_PPC)+
  geom_line(aes(date,P_roll_cal,col=id))
############
#probe 1 u 2
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")

data_probe1u2 <- data_probe1u2 %>% 
  group_by(tiefe) %>% 
  mutate(CO2_smp1_roll = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
         CO2_smp2_roll = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
  )

names(data_PPC)
data_PPC_wide <- tidyr::pivot_wider(data_PPC[,c("date","id","P_roll_cal","mode","step_id")],names_from = id,values_from =P_roll_cal,names_prefix = "P_")
data_probes_wide <- tidyr::pivot_wider(data_probe1u2,id_cols=date,names_from = tiefenstufe,values_from = matches("CO2_smp"))
names(data_probes_wide)


data <- merge(data_PPC_wide,data_probes_wide)
names(data)
ggplot(data)+
  geom_point(aes(P_3,CO2_smp2_6,col=P_2))
ggplot(data)+
  geom_point(aes(P_3,CO2_smp1_6,col=P_2))
probe1_plot <- 
  ggplot(subset(data_probe1u2,tiefenstufe %in% c(3:7)))+
  geom_vline(xintercept = step_date,col="grey",linetype=2)+
  geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,fill=mode),alpha=0.4)+
  geom_line(aes(date, CO2_smp1_roll,col=factor(abs(tiefe))))+
  labs(col="tiefe (cm)",y = "CO2 profile 1 (ppm)",x="")+
    scale_color_manual(values = scales::hue_pal()(7),limits = factor(1:7 * 3.5))+
  theme(axis.title.y = element_text(colour = "blue"))+
    scale_fill_manual(values = c(0,scales::hue_pal()(4)),limits = unique(modes))

probe2_plot <- ggplot(subset(data_probe1u2,tiefenstufe %in% c(1:7)))+
  geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,fill=mode),alpha=0.4)+
  geom_vline(xintercept = step_date,col="grey",linetype=2)+
  geom_line(aes(date, CO2_smp2_roll,col=factor(abs(tiefe))))+
  labs(y = "CO2 profile 2 (ppm)",x="")+
    scale_fill_manual(values = c(0,scales::hue_pal()(4)),limits = unique(modes))

P_roll_plot <- ggplot(subset(data_PPC,id%in%1:4))+
  geom_vline(xintercept = step_date,col="grey",linetype=2)+
  geom_hline(yintercept = 0,col="grey",linetype=2)+
  geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,fill=mode),alpha=0.4)+
  geom_line(aes(date, P_roll_cal,col=factor(id),group=id),alpha=0.8)+
  labs(y = expression(P[rollmean]~"(Pa)"))+
  scale_color_manual(values = c("black","black","blue","blue"))+
    scale_fill_manual(values = c(0,scales::hue_pal()(4)),limits = unique(modes))

ggpubr::ggarrange(probe1_plot,probe2_plot,P_roll_plot,ncol=1,align = "v",common.legend = T,legend = "right")+
  ggsave(paste0(plotpfad_PPchamber,"P_roll_lateral_Versuch.png"),width = 7,height = 6)

