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

Versuch <- 31

datelim <- c(pp_chamber$Start[Versuch]-3600 * 12,pp_chamber$Ende[Versuch]+3600*12)
###################
#PPC data

data_PPC <- read_PP(datelim = datelim,table="PP_1min")

data_PPC <- data_PPC %>% 
  filter(id != 6) %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         PPC5 = RcppRoll::roll_mean(P_diff,10,fill=NA),
         P_roll = RcppRoll::roll_mean(P,20,fill=NA))

P_ref <- subset(data_PPC,id==5)
P_ref$P_ref <- P_ref$P_roll
data_PPC <- subset(data_PPC,id!=5)
data_PPC <- merge(data_PPC,P_ref[,c("date","P_ref")])
data_PPC$P_roll <- data_PPC$P_roll - data_PPC$P_ref

ggplot(data_PPC)+
  geom_line(aes(date, P_roll, col=factor(id)))
data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA

# data_Proll_wide <- tidyr::pivot_wider(data_PPC,date,values_from = P_roll,names_from = id,names_prefix = "id_")
# data_Proll_wide[paste0("id_",1:4)] <- data_Proll_wide[paste0("id_",1:4)] - data_Proll_wide$id_5
# data_Proll <- tidyr::pivot_longer(data_Proll_wide[,1:6],paste0("id_",1:5),names_prefix = "id_",names_to = "id",values_to="P_roll")

step_thr <- 0.04
data_PPC <- data_PPC[order(data_PPC$date),]
PPC_steps <- data_PPC %>%
  filter(id %in% c(1,3,4)) %>%
  mutate(date = round_date(date,"10 min")) %>%
  group_by(id,date) %>%
  summarise(across(everything(),mean)) %>%
  mutate(PPC_diff = abs(c(NA,diff(PPC5))),
         step = ifelse(PPC_diff > step_thr,1,0)
  )

step_date <- sort(unique(PPC_steps$date[PPC_steps$step == 1]))
step_date <- step_date[c(as.numeric(diff(step_date)),100) > 60]
step_date <- step_date[!is.na(step_date)]

PPC_steps_wide <- tidyr::pivot_wider(PPC_steps,id_cols = date,names_from = id,values_from = P_roll,names_prefix = "P_roll_")

step_df <- data_PPC %>%
  group_by(id) %>% 
  mutate(step = ifelse(date %in% !!step_date,1,0),
         step_id=cumsum(step)) %>% 
  group_by(step_id) %>% 
  summarise(across(PPC,mean,na.rm=T),
            Start = min(date),
            End = max(date)) %>% 
  mutate(cal = ifelse(PPC > 0.1,0,1))

ggplot(PPC_steps)+
  geom_vline(xintercept = step_date,col="grey",linetype=2)+
  geom_hline(yintercept = step_thr,col="grey",linetype=2)+
  geom_line(aes(date,PPC_diff))+
  geom_line(aes(date,PPC/2,col=factor(id)))


data_PPC$step_id <- NA


for(i in 1:nrow(step_df)){
  id <- which(daterange_id(data_PPC,c(step_df$Start[i]+1800,step_df$End[i]-1800)))
  data_PPC$step_id[id] <- step_df$step_id[i]
}
ggplot(data_PPC)+
  geom_line(aes(date,PPC,col=factor(step_id),group = id))

data_PPC$cal <- factor(data_PPC$step_id,levels = step_df$step_id,labels = step_df$cal)

cal_period <- data_PPC %>% 
  filter(cal == 1) %>% 
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
step_df_sub <- subset(step_df,PPC > 0.1)
#data_PP_1 <- read_PP(datelim = c(step_df_sub$Start[1],step_df_sub$End[1])) # Versuch 3 Plateral
#data_PP_2 <- read_PP(datelim = c(step_df_subStart[4],step_df_sub$End[4])) # Versuch 3 PP
data_PP_1 <- read_PP(datelim = c(step_df_sub$Start[1],step_df_sub$End[1])) # Versuch 31 Plateral
data_PP_2 <- read_PP(datelim = c(step_df_sub$Start[8],step_df_sub$End[8]))#Versuch 31 PP
data_PP_1$mode <- "PP & P-lateral"
data_PP_2$mode <- "PP"
data_PP <- rbind(data_PP_1,data_PP_2)
data <- merge(data_PP,cal_period[,c("date","sensor_drift","id")],all = T)
data <- data %>% 
  filter(id %in% 1:4) %>% 
  group_by(id) %>% 
  mutate(sensor_drift = imputeTS::na_interpolation(sensor_drift),
         P_cal = P - sensor_drift) %>% 
  filter(!is.na(mode)) %>% 
  group_by(mode,id) %>% 
  mutate(zeit = as.numeric(date - min(date))-3600,
         P_roll1 = RcppRoll::roll_mean(P_cal,10,fill=NA),
         P_roll2 = RcppRoll::roll_mean(P_cal,3600,fill=NA))

names(data)
cols <- RColorBrewer::brewer.pal(4,"PuOr")[c(1,4)]
ggplot(subset(data,zeit < 180 & zeit > 0 & id %in% c(1,3)))+
  geom_hline(yintercept = 0,col="grey",linetype = 2)+
  geom_line(aes(zeit,P_roll1,col=factor(id,labels = c("1 & 2","3 & 4"))))+
  geom_line(aes(zeit,P_roll2,col=factor(id,labels = c("1 & 2","3 & 4")),linetype="Pmean"))+
  facet_wrap(~factor(mode))+
  scale_color_manual(values = cols)+
  scale_linetype_manual(labels = expression(P[mean]),values=2)+
  labs(x = "time (s)", y = "P (Pa)",col="subspace",linetype="")+
  ggsave(paste0(plotpfad_PPchamber,"P_oscillation_plot.png"),width=7,height = 3)
  
