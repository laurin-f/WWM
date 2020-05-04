#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)


datelim <- c("2020.04.28 18:00:00","2020.04.28 20:00:00")
data <- read_sampler("sampler1",datelim = datelim, format = "long")
ggplot(data)+geom_line(aes(date,CO2,col=as.factor(tiefe)))
data_agg <- aggregate(list(CO2=data[,c("CO2")]),list(tiefe=data$tiefe),mean)
ggplot(data_agg)+geom_ribbon(aes(xmin=0,xmax=CO2,y=tiefe,fill=""))+
  guides(fill=F)+
  labs(x=expression(CO[2]*" [ppm]"),y="depth [cm]")+
  ggsave(paste0(plotpfad,"tiefenprofil_garten1.png"),width = 2,height=2)
