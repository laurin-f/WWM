
#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","ggforce","dplyr","directlabels")
check.packages(packages)
theme_set(theme_classic())

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
data$Wind <- RcppRoll::roll_mean(data$WindVel_30m_ms,60*4,fill=NA)
data_plot <- data %>% filter(date == round_date(date,"30 mins"))
ggplot(subset(data_plot,Position == 7 ))+
  #geom_point(aes(VWC,CO2_ref,col=T_soil))+facet_wrap(~tiefe,scales = "free")+scale_color_viridis_c()
  #geom_point(aes(Wind,CO2_ref,col=VWC))+facet_wrap(~tiefe,scales = "free")+scale_color_viridis_c()
  geom_point(aes(T_soil,CO2_ref,col=Wind))+facet_wrap(~tiefe,scales = "free")+scale_color_viridis_c()
ggplot(subset(data_plot,Position == 7:8 & Pumpstufe==0))+
  geom_point(aes(VWC,CO2_inj,col=T_soil))+facet_wrap(~tiefe,scales = "free")+scale_color_viridis_c()
#  geom_point(aes(T_soil,CO2_inj,col=as.factor(tiefe)))
colnames(data)
data$CO2_inj[data$Pumpstufe !=0] <- NA
cor_mat <- subset(data,Position %in% 7:8) %>% 
  group_by(tiefe) %>% 
  summarise_at(c("T_soil","VWC","Wind"),~cor(.,CO2_ref,"na.or.complete"))

ggplot(cor_mat)+
  geom_point(aes(tiefe,Wind,col="Wind"))+
  geom_point(aes(tiefe,VWC,col="VWC"))+
  geom_point(aes(tiefe,T_soil,col="T_soil"))+
  geom_hline(yintercept = c(1,0,-1))

test <- subset(data_plot,Position==8&tiefe==-10.5)$CO2_ref[1:100]

cor(test,test)
