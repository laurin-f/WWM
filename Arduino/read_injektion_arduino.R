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
daterange <- ymd_h(c("21/04/20 10", "21/04/26 16"))
#daterange <- ymd_h(c("21/05/04 10", "21/05/04 12"))

smp1u2 <- read_sampler(datelim=daterange) 


data_sub <- subset(data, date >= min(daterange) & date <= max(daterange))

ggplot(data_sub)+geom_line(aes(date,CO2))
data$CO2_raw <- data$CO2
inj <- injectionrate(data=data_sub,t_min=2,t_init = 1,Pumpstufen = 1,T_C=13)
#split_chamber(data_sub,closing_lim = 20,t_min=2)
inj_plot <- ggplot(inj)+
  geom_line(aes(date,CO2_ml_per_min))
T_plt <- ggplot()+
    geom_line(data=smp1u2,aes(date,T_C))+
  xlim(range(inj$date))
egg::ggarrange(inj_plot,T_plt)
