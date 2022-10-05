#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
datapfad_bf<- paste0(hauptpfad,"Daten/Urdaten/Bodenfeuchte_FVA_Garten/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","svMisc")
check.packages(packages)
##########################################################
###########################################################

datelim <- ymd_hms("2022-09-30 08:00:00 UTC", "2022-09-30 09:20:00 UTC")
datelim2 <- ymd_hms("2022-09-30 09:10:00 UTC", "2022-09-30 09:15:00 UTC")

#data_wide <- read_PP(datelim = datelim,format = "wide")
data_long <- read_PP(datelim = datelim,corfac = F)
data <- subset(data_long, id != 6)


dt <- 1


P_corfac <- data %>% 
  sub_daterange(datelim2) %>% 
  #mutate(group = 1) %>% 
  group_by(id) %>% 
  summarise(P_mean = mean(P))

save(P_corfac,file=paste0(datapfad_PP_Kammer,"P_corfac_2.RData"))
data$cal <- as.numeric(as.character(factor(data$id,levels = 1:5,labels = P_corfac$P_mean)))

data <- data %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         P_diff = abs(c(NA,diff(P_filter)))/!!dt,
         PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
         P_cal = P -cal,
         P_roll_cal = RcppRoll::roll_mean(P_cal,3*60/!!dt,fill=NA),
         P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA))

ggplot(sub_daterange(data,datelim2))+
  geom_line(aes(date, P_cal,col=id),alpha=0.3)+
  geom_line(aes(date, P_roll_cal,col=id))
ggplot(sub_daterange(data,datelim2))+
  geom_line(aes(date, P,col=id),alpha=0.3)+
  geom_line(aes(date, cal,col=id),alpha=0.3)+
  geom_line(aes(date, P_roll,col=id))
ggplot(data)+
  geom_line(aes(date, PPC5,col=id))

#range(data_PPC$date)
names(data_PPC)
PPC_plot <- ggplot(subset(data_PPC,id%in%c(1:4) & date %in% round_date(date,"1 mins")))+
  geom_line(aes(date,PPC5,col=factor(id)))

Prollplt <- 
  ggplot(subset(data_PPC,id%in%c(1:4) & date %in% round_date(date,"1 secs")))+
  #  geom_vline(xintercept = ymd_hm(paste("22.04.27",c("13:45","13:50","13:55","14:00","14:05"))))+
  #geom_line(aes(date,P,col=factor(id)),alpha=0.4)+
  geom_line(aes(date,P_roll,col=factor(id)))

Prollplt
datelim2 <- ymd_hms("2022-09-30 09:10:00 UTC", "2022-09-30 09:20:15:00 UTC")

egg::ggarrange(PPC_plot+xlim(datelim2),
               Prollplt+xlim(datelim2),ncol=1)

