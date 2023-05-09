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

#############
#klima_data

load(paste0(klimapfad_CR1000,"klima_data_PP_kammer.RData"))
names(klima)
##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

PP_Versuche <- grep("PP|\\dD",pp_chamber$Modus)
Versuch <- 3
datelim <- c(min(pp_chamber$Start),max(pp_chamber$Ende))
data_PPC <- read_PP(datelim = datelim,table="PP_1min")
data_PPC$Versuch <- NA
for(i in 1:nrow(pp_chamber)){
  id_i <- daterange_id(data_PPC,c(pp_chamber$Start[i],pp_chamber$Ende[i]))
  data_PPC$Versuch[id_i] <- i
}
names(data_PPC)
#data_PPC <- subset(data_PPC,id %in% c(1:5))
dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)

data_PPC$P[which(abs(data_PPC$P) > 10 & data_PPC$id != 6)] <- NA
data_PPC <- data_PPC %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
         P_roll = RcppRoll::roll_mean(P,20,fill=NA))

P_ref <- subset(data_PPC,id==5)
P_ref$P_ref <- P_ref$P_roll
data_PPC <- subset(data_PPC,id!=5)
data_PPC <- merge(data_PPC,P_ref[,c("date","P_ref")])
data_PPC$P_roll <- data_PPC$P_roll - data_PPC$P_ref
data_wide <- tidyr::pivot_wider(data_PPC,id_cols = c(date,Versuch),names_from = id,values_from = P_roll,names_prefix = "P_")
names(data_wide)
data_PPC$P_horiz <- (data_PPC$P_1 + data_PPC$P_2)/2 - (data_PPC$P_3 + data_PPC$P_4)/2
#data_PPC$P_horiz <- ((data_PPC$P_1 - data_PPC$P_4) + (data_PPC$P_2 - data_PPC$P_3))/2
data_sub <- subset(data_PPC,id %in% 1:4)
data_PPC$id[data_PPC$id == 6] <- "ambient"

peak_df <- data_PPC %>% 
  filter(id == "ambient") %>% 
  mutate(ymd = as.Date(date)) %>% 
  group_by(ymd) %>% 
  summarise(peak = date[which.max(P_roll)])

datelim <- ymd_h("22.09.20 01","22.10.25 23")
#datelim <- ymd_h("22.11.20 01","22.12.25 23")
data_PPC$PPC_01 <- ifelse(data_PPC$PPC > 0.1,T,F)
p_plot <- 
  ggplot(sub_daterange(subset(data_PPC,!is.na(PPC)),datelim))+
  geom_hline(yintercept = 0,col="grey",linetype=2)+
    geom_vline(data = peak_df,aes(xintercept = peak),col="grey",linetype=2)+
  geom_line(aes(date,P_roll,col=PPC > 0.1,group = 1))+
    facet_grid(id~.,scales = "free_y")+
    xlim(datelim)+
    theme_bw()#+
#    ggsave(paste0(plotpfad_PPchamber,"Pmean_subspaces.png"),width = 9,height = 8)
  
data_wide$P_6  
  names(klima)
wind_plot <- ggplot(sub_daterange(klima,datelim))+
  geom_vline(data = peak_df,aes(xintercept = peak),col="grey",linetype=2)+
    geom_line(aes(date, RcppRoll::roll_mean(Wind_ms,30,fill = NA)))+
  labs(y = "Wind (m/s)")
p_hPa_plot <- ggplot(sub_daterange(klima,datelim))+
    geom_line(aes(date,P_hPa))
  
  p_plot2 <- 
    ggplot(sub_daterange(subset(data_PPC,id %in% c(2,"ambient") & !is.na(PPC)),datelim))+
  geom_hline(yintercept = 0,col="grey",linetype=2)+
  geom_vline(data = peak_df,aes(xintercept = peak),col="grey",linetype=2)+
  geom_line(aes(date,P_roll,col=factor(PPC_01),group = 1))+
    facet_grid(id~.,scales = "free_y")+
    labs(x="",y=P[mean]~(Pa),col="PPC > 0.1")+
    xlim(datelim)
  
  png(paste0(plotpfad_PPchamber,"Pmean_Wind.png"),width = 9,height = 8,units = "in",res=300)
  egg::ggarrange(p_plot2,wind_plot,heights = c(3,1))
  dev.off()
  
  geom_hline(yintercept = 0,col="grey",linetype=2)+
  geom_line(aes(date,P_2,col="2"))+
  geom_line(aes(date,-P_6/100,col="6"))

  ggplot()
  #P_plt <- 
  ggplot(data_PPC)+
  geom_hline(yintercept = 0,col="grey",linetype=2)+
  geom_line(aes(date,P_horiz,col="lateral",linetype=""))+
  geom_line(aes(date,P_1,col="1"))+
  geom_line(aes(date,P_2,col="2"))+
  geom_line(aes(date,P_3,col="3"))+
  geom_line(aes(date,P_4,col="4"))+
  scale_color_manual(values = c(cols,1))+
  guides(col=F)+
  labs(linetype="lateral gradient",y = expression(P[mean]~"(Pa)"))
