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
datelim_harth1 <- ymd_hm(c("2020.06.09 12:15","2020.06.09 13:00"))
datelim_harth2 <- ymd_hm(c("2020.06.18 12:00","2020.06.18 13:00"))
datelim_glovebox_smp <- ymd_hm(c("2020.06.25 11:37","2020.06.25 11:50"))
datelim_glovebox_accurel <- ymd_hm(c("2020.06.25 16:51","2020.06.25 17:04"))
glovebox_smp <- gga_co2+xlim(datelim_glovebox_smp)+ylim(c(400,6000))
glovebox_accurel <- gga_co2+xlim(datelim_glovebox_accurel)
egg::ggarrange(glovebox_smp,glovebox_accurel)
datelim <- range(c(datelim_harth1,datelim_glovebox_accurel))
micro <- read_db("GGA.db","micro",datelim)
micro$p_kPa <- as.numeric(set_units(set_units(micro$GasP_torr,"torr"),"kPa"))
smp2 <- read_sampler("sampler2",datelim=datelim)
gga_co2 <- ggplot(micro)+geom_line(aes(date,CO2))+labs(x="",title="GGA")
gga_ch4 <- ggplot(micro)+geom_line(aes(date,CH4))
gga_GasT <- ggplot(micro)+geom_line(aes(date,GasT_C))
gga_AmbT <- ggplot(micro)+geom_line(aes(date,AmbT_C))
gga_p <- ggplot(micro)+geom_line(aes(date,p_kPa))
smp2_plot <- ggplot(smp2)+geom_line(aes(date,CO2,col=as.factor(tiefe)))+xlim(range(micro$date))+labs(x="",title="gradient sampler",col="tiefe")
library(units)

p_harth1 <- egg::ggarrange(smp2_plot+xlim(datelim_harth1),gga_co2+xlim(datelim_harth1),gga_ch4+xlim(datelim_harth1),heights=c(2,1,1))

p_T_harth1 <- egg::ggarrange(gga_co2+xlim(datelim_harth1),gga_AmbT+xlim(datelim_harth1),gga_GasT+xlim(datelim_harth1),gga_p,ncol=1)

p_harth2 <- egg::ggarrange(smp2_plot+xlim(datelim_harth2),gga_co2+xlim(datelim_harth2),gga_ch4+xlim(datelim_harth2),heights=c(2,1,1))

p_T_harth2 <- egg::ggarrange(gga_co2+xlim(datelim_harth2),gga_AmbT+xlim(datelim_harth2),gga_GasT+xlim(datelim_harth2),gga_p,ncol=1)

p_T_glovebox_smp <- egg::ggarrange(gga_co2+xlim(datelim_glovebox_smp),gga_AmbT+xlim(datelim_glovebox_smp),gga_GasT+xlim(datelim_glovebox_smp),gga_p,ncol=1)

pdf(paste0(plotpfad,"Methangradient_test.pdf"),width=6,height = 7)
p
dev.off()
ggplot()+
  geom_line(data=micro,aes(date,CO2,col="GGA"))+
  geom_line(data=subset(smp2),aes(date,CO2,col=as.factor(tiefe)))
