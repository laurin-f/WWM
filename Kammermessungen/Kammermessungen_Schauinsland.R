
#pfade definieren

rm(list=ls())
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<-paste0(hauptpfad,"Daten/Metadaten/")
plotpfad_schaui <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Schauinsland/")

#flux.kammer<-function(ort="Schauinsland",
#                      messnr){

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg")
check.packages(packages)

doku <- readODS::read_ods(paste0(metapfad,"Schauinsland/Schnee_Doku.ods"))

flux <- chamber_flux(1:5,mess_dir = "Schauinsland",aggregate = F,
                            closing_lim=1,
                            opening_lim=0,
                            t_init=1,
                            t_max=4,
                            t_min=2,adj_openings=T,return_data = F)

flux$day <- as.Date(flux$date)
flux$Versuch <- factor(flux$day,labels = seq_along(unique(flux$day)))


flux$schnee <- ifelse(grepl("Schnee",flux$kammer),1,0)
flux$plot <- stringr::str_extract(flux$kammer,"^A|B")
sub <- subset(flux,Versuch %in% 2:5 )

t.test(sub$CO2_ppm_per_min~sub$schnee)
t.test(sub$CH4_ppm_per_min~sub$schnee)

ggplot(sub)+
   geom_boxplot(aes(factor(schnee),CO2_ppm_per_min,fill=factor(schnee)))
ggplot(sub)+
   geom_boxplot(aes(factor(schnee),CH4_ppm_per_min,fill=factor(schnee)))

ggplot(flux)+
   geom_point(aes(date,CO2_ppm_per_min,col=factor(schnee),shape=plot))+
   geom_line(aes(date,CO2_ppm_per_min,col=factor(schnee),group=kammer,linetype=""))+
  facet_wrap(~day,scales="free_x")+
  labs(linetype="same chamber",col="schnee",y=expression(CO[2]~flux~"(ppm/s)"))+
  ggsave(paste0(plotpfad_schaui,"schneeversuche_CO2_flux.png"),width=7,height = 5)
ggplot(flux)+
   geom_point(aes(date,CH4_ppm_per_min,col=factor(schnee),shape=plot))+
   geom_line(aes(date,CH4_ppm_per_min,col=factor(schnee),group=kammer,linetype=""))+
  labs(linetype="same chamber",col="schnee",y=expression(CH[4]~flux~"(ppm/s)"))+
  facet_wrap(~day,scales="free_x")+
  ggsave(paste0(plotpfad_schaui,"schneeversuche_CH4_flux.png"),width=7,height = 5)


ggplot(flux)+
  geom_point(aes(date,CO2_ppm_per_min,col=factor(kammer)))+
  geom_line(aes(date,CO2_ppm_per_min,col=factor(kammer)))+
  facet_wrap(~day,scales="free_x")
doku$Kommentar
ggplot(flux_schaui)+
  geom_point(aes(date,CH4_ppm_per_min,col=kammer))+
  geom_line(aes(date,CH4_ppm_per_min,col=kammer))
