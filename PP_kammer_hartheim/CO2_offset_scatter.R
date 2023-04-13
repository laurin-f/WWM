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
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS","wesanderson")
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
data$CO2_shift <-  data$CO2_offset / data$CO2_preds * 100


Modus_2d_1d <- grep("2D, 1D, 2D, 1D",pp_chamber$Modus)
names(data)

data$profile <- factor(data$probe,levels = 2:1,labels = 1:2)

data <- data[,!grepl("P_5",names(data))]
Sand_Versuch <- unique(grep("Sand",data$Versuch,value = T))
harth_Versuch <- sort(as.numeric(unique(grep("^\\d",data$Versuch,value = T))))

data$soil <- ifelse(grepl("Sand",data$Versuch),"Sand","Soil")
data$Versuch <- factor(data$Versuch,levels = c(harth_Versuch,Sand_Versuch))

pp_chamber$Modus <- str_replace(pp_chamber$Modus,"Über- Unterdruck lateraler gradient","P-lateral")
data$modus <- factor(data$Versuch,levels = c(harth_Versuch,Sand_Versuch),labels = c(pp_chamber$Modus[harth_Versuch],pp_chamber_Sand$Modus[as.numeric(str_extract(Sand_Versuch,"\\d+"))]),)

ID_2d_1d <- data$Versuch %in% Modus_2d_1d 
PP2D_stepids <- 0:3 + rep((0:4)*8,each = 4)
data$modus[ID_2d_1d] <- ifelse(data$step_id[ID_2d_1d] %in% PP2D_stepids,"2D PP","1D PP")
ggplot(subset(data,Versuch == 23))+
  geom_line(aes(date,PPC_2,col=factor(modus),group=1))


data$P_sub <- NA
data$P_sub[data$probe == 1] <- data$P_3[data$probe == 1]
data$P_lateral[data$probe == 1] <- data$P_3[data$probe == 1] - data$P_1[data$probe == 1]
data$P_sub[data$probe == 2] <- data$P_2[data$probe == 2]
data$P_lateral[data$probe == 2] <- data$P_1[data$probe == 2] - data$P_3[data$probe == 2]

data$P_mean <- rowMeans(data[,grep("P_\\d",names(data))])
data$PPC <- rowMeans(data[,grep("PPC_\\d",names(data))],na.rm = T)


names(data)

data_agg <- data %>% 
  group_by(tiefe,probe,profile,step_id,Versuch,soil,modus) %>% 
  summarise(across(everything(),mean))
data_agg$modus2 <- as.character(data_agg$modus)
data_agg$modus2[grep("PP",data_agg$modus)] <- "PP"
data_agg$modus2[which(grepl("PP",data_agg$modus) & abs(data_agg$P_lateral) > 0.1)] <- "PP & P-lateral"

data_Sand <- subset(data_agg,grepl("Sand",Versuch))

Versuche_sel <- c(2:3,11:14,17:22)
#2-3 6 PP & P-lateral

#11-12 3 PP und 1 PP & P-lateral
#13-14 2 PP & P-lateral
#17 2 PP und 2 PP & P-lateral
#18 4 PP 
#20-22 9 static pressure and lateral
#PP = 9 Versuche
#PP & P-lateral = 11 Versuche 

data_harth <- subset(data_agg, Versuch %in% Versuche_sel)
names(data_harth)
ggplot(subset(data_harth,modus2 != "P-lateral"))+
  geom_point(aes(date,PPC_2,group=1,col=modus2))+
  facet_wrap(~Versuch,scales = "free")
tiefen_sel <- 3:5
########################
#überblick
Versuch_x <- 3
ggplot(subset(data,Versuch %in% Versuch_x & probe ==1))+
  geom_line(aes(date, CO2_shift,col=factor(tiefe)))+
  geom_point(data = subset(data_agg,Versuch %in% Versuch_x & probe ==1),aes(date, CO2_shift))+
  facet_wrap(~Versuch,scales = "free_x")
names(data)

ggplot(subset(data,Versuch %in% Versuch_x & step_id %in% 9:11))+
         geom_col(aes(CO2_shift,tiefe,fill=factor(probe)))+
  geom_vline(xintercept = 0,linetype = 2,col="grey")+
        scale_y_discrete(limits = factor(7:1),labels = 7:1 * -3.5)+
  labs(x = expression(CO[2]*shift~("%")),y = "tiefe (cm)",fill = "profil")+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_tiefenprofile.png"),width = 3, height = 3)


ggplot(subset(data,Versuch %in% Versuch_x & probe ==1))+
  geom_line(aes(date, P_1),col="grey")+
  geom_line(aes(date, P_3),col="grey")+
  geom_line(aes(date, P_lateral))+
  geom_point(data = subset(data_agg,Versuch %in% Versuch_x & probe ==1),aes(date, P_lateral,col=modus2))+
  facet_wrap(~Versuch,scales = "free_x")
names(data)
##########################
#Hartheim plots

#P_mean
ggplot(subset(data_harth,tiefe%in%tiefen_sel & abs(P_sub) < 1 ),aes(P_sub,CO2_shift))+
  geom_point(aes(col=P_lateral))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  #facet_grid(~paste("probe",probe))+
  theme(legend.position = "top")+
  scale_color_viridis_c()+
  labs(x = expression(P[mean]~"(Pa)"),y = CO[2]~shift~"(%)",col="P_lateral (Pa)")
#  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_scatter_hartheim_Pmean.png"),width = 7, height = 6)
ggplot(subset(data_harth,tiefe%in%tiefen_sel ),aes(P_sub,CO2_shift))+
  geom_point(aes(col=P_lateral))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  #ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  ggpubr::stat_regline_equation(label.y = -15,aes(label= ..rr.label..))+
  facet_grid(~paste("profile",profile))+
#  theme(legend.position = "top")+
  scale_color_viridis_c()+
  labs(x = expression(P[subchamber]~"(Pa)"),y = CO[2]~shift~"(%)",col="P_lateral (Pa)")+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_scatter_hartheim_Pmean.png"),width = 7, height = 4)

#PPC
data_harth %>% 
  group_by(Versuch,modus2) %>%
  summarise(PPC = mean(PPC,na.rm=T))
  names(data_harth)
  
ggplot(subset(data_harth,tiefe%in%tiefen_sel & modus2 == "PP"),aes(PPC,CO2_shift))+
    geom_point(aes(,col=P_lateral,shape = modus2))+
    geom_smooth(method = "glm",col=1,linetype = 2)+
    #ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
    ggpubr::stat_cor(aes(label= ..rr.label..))+
    facet_grid(~paste("profile",profile))+
    #theme(legend.position = "top")+
    labs(x = "PPC (Pa/s)",y = CO[2]~shift~"(%)",col=expression(P[lateral]),shape = "mode")+
    scale_color_viridis_c()+
  ggsave(paste0(plotpfad_PPchamber,"CO2_shift_PPC.png"),width = 7, height = 4)
  

 brewer <- RColorBrewer::brewer.pal(5,"RdYlBu")

ggplot(subset(data_harth,tiefe%in%tiefen_sel & modus != "P-lateral"),aes(PPC,CO2_shift))+
  geom_point(aes(col=P_lateral))+
  geom_point(aes(shape = modus2))+
  #geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  #ggpubr::stat_cor(aes(label= ..rr.label..))+
  facet_grid(~paste("profile",profile))+
  labs(x = "PPC (Pa/s)",y = CO[2]~shift~"(%)",col=expression(P[lateral]~"(Pa)"),alpha = "mode")+
  scale_color_gradientn(colours = c(brewer[5:4],"Yellow",brewer[1]))+
  #scale_color_viridis_c()+
  scale_shape_manual(expression("|P"["lateral"]*"|"<0.1~Pa),values = c(21,NA),labels = c("",""))+
  #scale_color_discrete(labels = c("PP","PP &\nP-lateral"))+
  ggsave(paste0(plotpfad_PPchamber,"CO2_shift_PPC_P_lateral.png"),width = 7, height = 4)
  #ggsave(paste0(plotpfad_PPchamber,"CO2_shift_PPC_P_lateral_viridis.png"),width = 7, height = 4)

ggplot(subset(data_harth,tiefe%in%tiefen_sel & modus2 == "PP"),aes(PPC,CO2_shift))+
  geom_point(aes(col=factor(modus)))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("profile",profile))+
  theme(legend.position = "top")+
  labs(x = "PPC (Pa/s)",y = CO[2]~shift~"(%)",col="mode")+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_scatter_hartheim_PPC_sub.png"),width = 7, height = 5)


legend_df <- data.frame(x = c(rep(-0.72,2),rep(-0.6,2)),
                        y = c(-16,-18,-16,-18),
                        profile = as.character(c(1,2,1,2)))
#P lateral
Versuche_sel
n_Versuche <- length(unique(data_harth$Versuch))
ggplot(subset(data_harth,tiefe%in%tiefen_sel ),aes(P_lateral,CO2_shift,shape = profile,linetype = profile))+
  geom_point(aes(col=tiefe))+
  geom_smooth(method = "glm",col=1,lwd = 0.8,se = F)+
  ggpubr::stat_regline_equation(label.y = c(-16,-18)+0.4,label.x = -1.1,aes(label=..rr.label..))+
  geom_line(data = legend_df,aes(x,y),lwd = 0.8)+
#  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  #theme(legend.position = "top")+
#  scale_color_viridis_c()+
  #facet_wrap(~probe)
  labs(x = expression(P[lateral]~"(Pa)"),y = CO[2]~shift~"(%)",col="depth")+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_scatter_hartheim_lateral.png"),width = 7, height = 5)

ggplot(subset(data_harth,tiefe%in%tiefen_sel ),aes(P_horiz,CO2_shift))+
  geom_point(aes(col=modus))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))+
  theme(legend.position = "top")+
#facet_grid(paste("tiefe" ,tiefe)~paste("probe",probe))+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_scatter_hartheim_lateral_facets.png"),width = 7, height = 6)


ggplot(subset(data_agg,Versuch %in% c(Versuche_sel) & tiefe %in% tiefen_sel) ,aes(P_lateral,CO2_shift))+
  geom_point(aes(col=probe))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))#+
  #ggsave(paste0(plotpfad_PPchamber,"CO2_offset_lateral_scatter_hartheim_probes.png"),width = 7, height = 6)


#############################
#PPC
############################

data_agg$modus

ggplot(subset(data_harth,tiefe%in%tiefen_sel & modus2 == "PP"  ),aes(PPC,CO2_shift))+
  geom_point(aes(col=factor(Versuch)))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))+
  theme(legend.position = "top")+
  
  labs(x = "PPC (Pa/s)",y = CO[2]~shift~"(%)",col="mode")
#  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_scatter_hartheim_PPC.png"),width = 7, height = 6)


ggplot(subset(data_agg,tiefe%in%3:6 & Versuch %in% Versuche_sel),aes(P_sub,CO2_shift))+
  geom_point(aes(col=P_lateral))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(label.y = -15,aes(label= ..rr.label..))+
  #ggpubr::stat_regline_equation(aes(label= ..eq.label..))+
  #theme(legend.position = "top")+
  facet_wrap(~probe)+
  scale_color_viridis_c()
ggplot(subset(data_agg,tiefe%in%3:6 & Versuch %in% Versuche_sel),aes(P_lateral,CO2_shift))+
  geom_point(aes(col=P_sub))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(label.y = -15,aes(label= ..rr.label..))+
  #ggpubr::stat_regline_equation(aes(label= ..eq.label..))+
  #theme(legend.position = "top")+
  facet_wrap(~probe)+
  scale_color_viridis_c()
ggplot(subset(data_agg,tiefe%in%3:6 & Versuch %in% c(20:22)),aes(P_3,CO2_shift))+
  geom_point(aes(col=P_2))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(label.y = -15,aes(label= ..rr.label..))+
  #ggpubr::stat_regline_equation(aes(label= ..eq.label..))+
  theme(legend.position = "top")+
  facet_wrap(~probe)+
  scale_color_viridis_c()
#  labs(x = expression(P[lateral]~"(Pa)"),y = CO[2]~shift~"(%)",col="P")


corrplot::corrplot.mixed(data_agg[,c("CO2_shift","P_sub","P_lateral")])
############################
#Sand
#P_mean
ggplot(subset(data_Sand,tiefe%in%1:7 ),aes(P_mean,CO2_shift))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))

#PPC
ggplot(subset(data_Sand,tiefe%in%1:7 ),aes(PPC_2,CO2_shift))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  facet_grid(~paste("probe",probe))

#P lateral
ggplot(subset(data_Sand,tiefe%in%1:7 ),aes(P_lateral,CO2_shift))+
  geom_point(aes(col=Versuch))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))#+
#  facet_grid(~paste("probe",probe))

facet_grid(paste("tiefe" ,tiefe)~paste("probe",probe))


#############
#Beide

ggplot(subset(data_agg,Versuch %in% c(Sand_Versuch,Versuche_sel)& tiefe %in% tiefen_sel) ,aes(P_lateral,CO2_shift))+
  geom_point(aes(col=soil))+
  geom_smooth(method = "glm",col=1,linetype = 2)+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  labs(x = expression(P[lateral]~"(Pa)"),y = CO[2]~shift~"(%)",col="")+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_scatter_lateral_soil.png"),width = 7, height = 6)


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



ggplot(subset(data_agg,tiefe%in%tiefen_sel & Versuch %in% c(2:3,11:14,17:18,20)))+
  scale_color_viridis_c()+
  geom_point(aes(P_mean,CO2_offset,col=P_horiz),alpha=0.5)+
  #  geom_smooth(aes(P_horiz,CO2_offset,col=factor(Versuch)),method="glm")+
  facet_grid(paste("tiefe" ,tiefe)~paste("probe",probe))
ggplot(subset(data,tiefe%in%tiefen_sel & Versuch %in% c(3,11:14,17:18,20)))+
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




###########################################

##PPC model
#######################################

PerformanceAnalytics::chart.Correlation(data_harth[,c("CO2_shift","P_sub","P_lateral","PPC_2")])

preds <- data_agg %>%
  filter(Versuch %in% Versuche_sel) %>% 
  group_by(probe,tiefe) %>% 
  mutate(CO2_pred = predict(glm(CO2_shift ~ P_lateral),newdata = list(.)),
         CO2_shift_cor = CO2_shift - CO2_pred)
ggplot(subset(preds, Versuch == 3))+
  geom_line(aes(date,CO2_shift,col=factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_pred,col=factor(tiefe)),linetype =2)+
  facet_wrap(~probe,scales = "free")
range(subset(data_harth,tiefe%in%2:6 & modus2 == "PP")$P_lateral,na.rm = T)

sub <- subset(data_harth,probe == 2 & tiefe == 2)
test <- glm(CO2_shift ~ P_lateral,data = sub)
test$coefficients
R2_fm(test)
ggplot(subset(data_harth,tiefe%in%2:5),aes(P_lateral,CO2_shift,col=factor(tiefe)))+
  geom_point()+
  geom_smooth(method = "glm")+
  ggpubr::stat_regline_equation(label.y.npc = "bottom",aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  facet_wrap(~paste("probe",probe))+
  #ylim(c(-7,17))+
  labs(col="tiefe")+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_scatter_Plateral_regressions.png"),width = 7, height = 5)


CO2_ppc <- ggplot(subset(data_harth,tiefe%in%2:5 & modus2 == "PP"),aes(PPC,CO2_shift,col=factor(tiefe)))+
  geom_point()+
  geom_smooth(method = "glm")+
  ggpubr::stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  facet_wrap(~paste("probe",probe),scales = "free_x")+
  ylim(c(-7,17))+
  labs(title = "PPC without P-lateral: -0.1 < P-lateral < 0.1 Pa",col="tiefe")
unique(preds$modus)
CO2_ppc_cor <- ggplot(subset(preds,tiefe %in% 2:5 & modus != "P-lateral"),aes(PPC,CO2_shift_cor,col=factor(tiefe)))+
  geom_point()+
  geom_smooth(method = "glm")+
  ggpubr::stat_regline_equation(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  facet_wrap(~paste("probe",probe),scales = "free_x")+
  ylim(c(-7,17))+
  labs(title = "PPC with P-lateral effect corrected")

ggpubr::ggarrange(CO2_ppc,CO2_ppc_cor,ncol = 1,common.legend = T,legend = "right")+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_scatter_PPC_cor.png"),width = 7, height = 8)

fms <- data_harth %>% 
  ungroup() %>% 
  nest_by(profile,tiefe) %>% 
  mutate(fm = list(glm(CO2_shift ~ P_lateral,data =data)),
         R2 = R2_fm(fm),
         slope = fm$coefficients[2])
r2_plot <- ggplot(fms)+
  geom_point(aes(tiefe,R2,col=profile))+
  theme_bw()
slope_plt <- ggplot(fms)+
  geom_point(aes(tiefe,slope,col=profile))+
  theme_bw()

ggpubr::ggarrange(r2_plot,slope_plt,ncol=1)

fm <- glm(CO2_shift ~ PPC_2 + P_lateral + probe,data = data_harth)
#fm <- glm(CO2_shift ~ PPC_2,data = data_harth)
summary(fm)
