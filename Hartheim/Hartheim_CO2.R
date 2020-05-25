#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

datelim <- c("2020.05.18 10:00:00","2020.06.18 10:00:00")
smp1 <- read_sampler("sampler1",datelim = datelim, format = "long")
smp2 <- read_sampler("sampler2",datelim = datelim, format = "long")

smp1_plt <- ggplot(smp1)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

T_plt <- ggplot(smp1)+geom_line(aes(date,T_C))
egg::ggarrange(smp1_plt,T_plt,heights=c(4,1))
ggplot(smp2)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

data <- merge(smp1,smp2,by=c("date","tiefe","tiefenstufe"),all=T,suffixes = c(".smp1",".smp2"))

ggplot(data)+geom_point(aes(CO2.smp1,CO2.smp2,col=as.factor(tiefenstufe)))+geom_abline(slope=1,intercept = 0)
