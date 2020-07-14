#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)


###################
#metadaten

load(file=paste0(klimapfad,"klima_data.RData"))

#Pumpzeiten
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad_harth,"Tracereinspeisung_Hartheim.xlsx"))
Pumpzeiten$ende <- as_datetime(Pumpzeiten$ende)
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]

#tiefenoffset
tiefen_offset <- read.table(paste0(metapfad_harth,"sampler_tiefen_offset.txt"),header = T)

#Metadata Pumpstufen flux
flux <- read.csv(paste0(metapfad,"Tracereinspeisung/Pumpstufen_flux.txt"))



datelim <- min(c(Pumpzeiten$start,Pumpzeiten$ende))

data <- read_sampler("sampler1u2",datelim = datelim, format = "long")

colnames(data) <- str_replace_all(colnames(data),c("smp1" = "inj", "smp2" = "ref"))


inj <- ggplot(data)+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  annotate("text",x=Pumpzeiten$start[2],y=Inf,label="injection",vjust=1,hjust=-0.1)+
  labs(col="tiefe [cm]",x="",y=expression(CO[2]*" [ppm]"),title="injection sampler")+
  xlim(ymd_h(c("2020.06.12 0","2020.06.26 13")))

ref <- ggplot(data)+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  annotate("text",x=Pumpzeiten$start,y=Inf,label=str_replace(Pumpzeiten$bemerkung,"sampler umgesetzt","sampler\numgesetzt"),vjust=1,hjust="left")+
  guides(col=F)+labs(y=expression(CO[2]*" [ppm]"),title="reference sampler")+
  xlim(ymd_h(c("2020.06.12 0","2020.06.26 13")))

p <- egg::ggarrange(inj,ref,ncol=1)  
pdf(paste0(plotpfad,"sampler_umgesetzt.pdf"),width=11,height=9)
p
dev.off()
