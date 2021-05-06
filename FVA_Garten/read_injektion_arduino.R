hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Injektionsrate_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(inj_pfad,full.names = T)

data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F)
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2")

data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 9000] <- NA


range(data$date,na.rm=T)
daterange <- ymd_h(c("21/04/22 12", "21/05/04 12"))
#daterange <- ymd_h(c("21/05/04 10", "21/05/04 12"))

smp1u2 <- read_sampler(datelim=daterange,format="wide",cols="T_C") 

data$min10 <- round_date(data$date,"10mins")
T_C_df <- smp1u2 %>% 
  mutate(min10 = round_date(date,"10mins")) %>% 
  group_by(min10) %>% 
  summarise(T_C = mean(T_C,na.rm=T))

data_merge <- merge(data,T_C_df,all.x = T)
data_sub <- subset(data_merge, date >= min(daterange) & date <= max(daterange)) 



ggplot(data_sub)+geom_line(aes(date,CO2))
ggplot(data_sub)+geom_point(aes(date,T_C))+
  geom_line(data=smp1u2,aes(date,T_C))

inj <- injectionrate(data=data_sub,closing_lim = 20,t_min=2,t_init = 1,Pumpstufen = 1,T_C="T_C")

colnames(inj)
inj_plot <- ggplot(inj)+
  geom_line(aes(date,CO2_ml_per_min))
inj_plot <- ggplot(inj)+
  geom_line(aes(date,CO2_mumol_per_s))
T_plt <- ggplot()+
    geom_line(data=smp1u2,aes(date,T_C))+
  xlim(range(inj$date))
egg::ggarrange(inj_plot,T_plt)
save(inj,file = paste(datapfad_FVAgarten,"injectionrates.RData"))
