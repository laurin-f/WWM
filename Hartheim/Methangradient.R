#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units")
check.packages(packages)
datelim <- ymd_hm(c("2020.06.09 12:15","2020.06.09 13:00"))
datelim2 <- ymd_hm(c("2020.06.18 11:00","2020.06.18 13:00"))
micro <- read_db("GGA.db","micro",datelim)
smp2 <- read_sampler("sampler2",datelim=datelim)
gga_co2 <- ggplot(micro)+geom_line(aes(date,CO2))+labs(x="",title="GGA")
gga_ch4 <- ggplot(micro)+geom_line(aes(date,CH4))
smp2_plot <- ggplot(smp2)+geom_line(aes(date,CO2,col=as.factor(tiefe)))+xlim(range(micro$date))+labs(x="",title="gradient sampler",col="tiefe")


p <- egg::ggarrange(smp2_plot,gga_co2,gga_ch4,heights=c(2,1,1))

pdf(paste0(plotpfad,"Methangradient_test.pdf"),width=6,height = 7)
p
dev.off()
ggplot()+
  geom_line(data=micro,aes(date,CO2,col="GGA"))+
  geom_line(data=subset(smp2),aes(date,CO2,col=as.factor(tiefe)))
