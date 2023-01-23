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
#Metadata
pp_chamber_Sand <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber_Sand$Modus <- str_replace(pp_chamber_Sand$Modus,"PWM.+","PP")

files <- list.files(datapfad_PP_Kammer,pattern="CO2_offset_(Sand)?\\d+.RData",full.names = T)
data_ls <- list()
for(i in seq_along(files)){
  load(files[i])
  data_long$Versuch <- as.character(data_long$Versuch)
  data_ls[[i]] <- data_long
}


data <- do.call(rbind,data_ls)

Modus_2d_1d <- which(pp_chamber$Modus == "2D, 1D, 2D, 1D")

ggplot(subset(data,Versuch == 18))+
  geom_line(aes(date,PPC_2,col=factor(modus),group=1))


data <- data[,!grepl("P_5",names(data))]
Sand_Versuch <- unique(grep("Sand",data$Versuch,value = T))
harth_Versuch <- sort(as.numeric(unique(grep("^\\d",data$Versuch,value = T))))

data$soil <- ifelse(grepl("Sand",data$Versuch),"Sand","Soil")
data$Versuch <- factor(data$Versuch,levels = c(harth_Versuch,Sand_Versuch))

data$modus <- factor(data$Versuch,levels = c(harth_Versuch,Sand_Versuch),labels = c(pp_chamber$Modus[harth_Versuch],pp_chamber_Sand$Modus[as.numeric(str_extract(Sand_Versuch,"\\d+"))]),)

ID_2d_1d <- data$Versuch %in% Modus_2d_1d 
data$modus[ID_2d_1d] <- ifelse(data$step_id[ID_2d_1d] %in% c(5:7,13:15),"2D PP","1D PP")

data$P_sub <- NA
data$P_sub[data$probe == 1] <- data$P_3[data$probe == 1]
data$P_sub[data$probe == 2] <- data$P_2[data$probe == 2]

data$P_mean <- rowMeans(data[,grep("P_\\d",names(data))])


data$P_lateral <- data$P_horiz
data$P_lateral[data$probe == 2] <- -data$P_lateral[data$probe == 2]


data_agg <- data %>% 
  group_by(tiefe,probe,step_id,Versuch,soil,modus) %>% 
  summarise(across(everything(),mean))

data_Sand <- subset(data_agg,grepl("Sand",Versuch))

Versuche_sel <- c(2:3,11:14,17:21)
data_harth <- subset(data_agg, Versuch %in% Versuche_sel)

##########################
#Hartheim plots

#P_mean
ggplot(subset(data_harth,tiefe%in%1:7 ),aes(P_mean,CO2_offset))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))

#PPC
ggplot(subset(data_harth,tiefe%in%1:7 ),aes(PPC_2,CO2_offset))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))

#P lateral
ggplot(subset(data_harth,tiefe%in%1:7 ),aes(P_lateral,CO2_offset))+
  geom_point(aes(col=modus))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))

ggplot(subset(data_harth,tiefe%in%1:7 ),aes(P_horiz,CO2_offset))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))
facet_grid(paste("tiefe" ,tiefe)~paste("probe",probe))

############################
#Sand
#P_mean
ggplot(subset(data_Sand,tiefe%in%1:7 ),aes(P_mean,CO2_offset))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))

#PPC
ggplot(subset(data_Sand,tiefe%in%1:7 ),aes(PPC_2,CO2_offset))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))

#P lateral
ggplot(subset(data_Sand,tiefe%in%1:7 ),aes(P_lateral,CO2_offset))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))#+
#  facet_grid(~paste("probe",probe))

facet_grid(paste("tiefe" ,tiefe)~paste("probe",probe))


#############
#Beide

ggplot(subset(data_agg,Versuch %in% c(Sand_Versuch,Versuche_sel)) ,aes(P_lateral,CO2_offset))+
  geom_point(aes(col=modus))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))

ggplot(subset(data_agg,Versuch %in% c(Versuche_sel,unique(data_Sand$Versuch)) ),aes(P_mean,CO2_offset))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))

ggplot(subset(data_agg,Versuch %in% c(Versuche_sel,unique(data_Sand$Versuch)) ),aes(P_horiz,CO2_offset))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))
facet_grid(paste("tiefe" ,tiefe)~paste("probe",probe))


ggplot(subset(data_agg,Versuch %in% c(Versuche_sel,unique(data_Sand$Versuch)) ),aes(PPC_2,CO2_offset))+
  geom_point(aes(col=soil),alpha=1)+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))


ggplot(subset(data_agg,Versuch %in% Versuche_sel ),aes(P_horiz,CO2_offset))+
  geom_point(aes(col=factor(Versuch,levels = Versuche_sel)),alpha=1)+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))
facet_grid(paste("tiefe" ,tiefe)~paste("probe",probe))



ggplot(subset(data_agg,tiefe%in%3:5 & Versuch %in% c(2:3,11:14,17:18,20)))+
  scale_color_viridis_c()+
  geom_point(aes(P_mean,CO2_offset,col=P_horiz),alpha=0.5)+
  #  geom_smooth(aes(P_horiz,CO2_offset,col=factor(Versuch)),method="glm")+
  facet_grid(paste("tiefe" ,tiefe)~paste("probe",probe))
ggplot(subset(data,tiefe%in%3:5 & Versuch %in% c(3,11:14,17:18,20)))+
  geom_point(aes(P_horiz,CO2_offset,col=P_mean),alpha=0.5)+
  scale_color_viridis_c(limits=c(-0.5,0.5))+
  facet_grid(paste("tiefe" ,tiefe)~paste("probe",probe))

ggplot(subset(data,tiefe==4))+
  geom_point(aes(P_horiz,CO2_offset,col=factor(Versuch)))+
  facet_grid(~probe)

ggplot(subset(data,tiefe==6))+
  geom_point(aes(PPC_1,CO2_offset,col=(P_horiz)))+
  scale_color_viridis_c()+
  facet_grid(~probe)

ggplot(subset(data,tiefe==4))+
  geom_point(aes(PPC_1,CO2_offset,col=factor(Versuch)))+
  facet_grid(~probe)

ggplot(subset(data,Versuch == 20))+
  geom_point(aes(P_horiz,CO2_offset,col=factor(tiefe)))+
  facet_grid(~probe)
