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

####################################
#load data
####################################
#Metadata xlsx
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad,"Tracereinspeisung_Sandkiste.xlsx"))
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]
Versuch_x <- unique(Pumpzeiten$Versuch)

Versuch_sub <- subset(Pumpzeiten,Versuch %in% Versuch_x)
Versuch_sub$period <-Versuch_sub$Versuch 
Versuch_sub$PSt_Nr <- paste("PSt",Versuch_sub$Pumpstufe,"Nr",Versuch_sub$Versuch,sep="_")

#samplerdata
datelim <- range(c(Versuch_sub$start, Versuch_sub$ende),na.rm = T)
datelim[2] <- datelim[2]+3600*12

data <- read_sampler("sampler1",datelim = datelim, format = "long")

#Metadata Pumpstufen flux
flux <- read.csv(paste0(metapfad,"Pumpstufen_flux.txt"))

###############################
#postprocesing
#############################

#Pumpstufe und Versuch aus metadaten auf dataframe übetragen
data$Pumpstufe <- NA
data$Versuch <- NA

#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
stunden_bis_steadystate <- 10
stunden_cut_off <- 2

#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
for(i in seq_along(Pumpzeiten$Pumpstufe)){
  Pumpzeiten_lim <- data$date > (Pumpzeiten$start[i] + stunden_bis_steadystate*60*60) & 
    data$date < (Pumpzeiten$ende[i] - stunden_cut_off * 60 * 60)
data$Pumpstufe[Pumpzeiten_lim] <- Pumpzeiten$Pumpstufe[i]
data$Versuch[Pumpzeiten_lim] <- Pumpzeiten$Versuch[i]
}

#Spalte PSt_Nr anhängen
data$PSt_Nr <- paste("PSt",data$Pumpstufe,"Nr",data$Versuch,sep="_")
data$PSt_Nr[is.na(data$Pumpstufe)]<-NA

#kurven glätten mit rollapply
data$CO2_rollapply <- NA
for(i in unique(data$tiefe)){
  tiefe.i <- data$tiefe == i
  data$CO2_rollapply[tiefe.i] <- zoo::rollapply(data$CO2[tiefe.i],width=10,mean,fill=NA)
}


###################
#Aggregate Data
data_agg <- aggregate(list(CO2=data$CO2),list(Pumpstufe=data$Pumpstufe,tiefe=data$tiefe,tiefenstufe = data$tiefenstufe,Versuch= data$Versuch,PSt_Nr = data$PSt_Nr),mean)

###################
#gradienten berechnen
data_agg$gradient <- NA
data_agg$dz <- NA
data_agg$dC <- NA
for(i in unique(data_agg$PSt_Nr)){

  data_agg$gradient[data_agg$PSt_Nr==i] <- c(diff(data_agg$CO2[data_agg$PSt_Nr==i])/diff(-data_agg$tiefe[data_agg$PSt_Nr==i]),NA) #ppm/cm

  data_agg$dC[data_agg$PSt_Nr==i] <- c(diff(data_agg$CO2[data_agg$PSt_Nr==i]),NA) #cm3/cm3
  data_agg$dz[data_agg$PSt_Nr==i] <- c(diff(-data_agg$tiefe[data_agg$PSt_Nr==i]),NA)
}

##############################
#Fick's Law
#Fz = -DS * (dC / dz)
D0_CO2 <- 0.159#cm2/s
#Fläche
A <-20^2*pi#cm2

#flux für jeweilige Pumpstufe aus metadaten übertragen
data_agg$Fz <- sapply(data_agg$Pumpstufe, function(x) flux$tracer_ml_per_min[flux$Pumpstufe == x])#ml/min

#Ficks law anwenden
data_agg$DS <- data_agg$Fz/60/A * data_agg$dz / (data_agg$dC/10^6) #cm3 s-1 cm-2 * cm = cm2 s-1

mean(data_agg$DS[data_agg$Versuch==3],na.rm=T)#cm2/s

data_agg$DS_D0_CO2 <- data_agg$DS/D0_CO2
data_agg$tiefenmittel <- rowMeans(cbind(c(data_agg$tiefe[-nrow(data_agg)],NA),c(data_agg$tiefe[-1],NA)))
data_agg$tiefenmittel[data_agg$tiefe == -24.5] <- NA


############################
#künstliche respiration
respi <- subset(data_agg,PSt_Nr %in% Versuch_sub$PSt_Nr[Versuch_sub$respiration_simul == "ja"])
respi <- respi[!colnames(respi)[] %in% c("PSt_Nr")]
respi_wide <- tidyr::pivot_wider(respi,names_from = Pumpstufe,values_from = which(!colnames(respi)[] %in% c("tiefe","tiefenstufe","Versuch","Pumpstufe","tiefenmittel","dz")))

colnames(respi_wide)
respi_wide <- respi_wide[!colnames(respi_wide)[] %in% c("Fz_0","DS_0","DS_D0_CO2_0")]
colnames(respi_wide) <- str_replace_all(colnames(respi_wide),c("0"="respi","3"="ges"))
respi_wide$CO2_tracer <- respi_wide$CO2_ges - respi_wide$CO2_respi

respi$respiration <- respi$CO2

respi_long <- tidyr::pivot_longer(respi_wide,grep("^CO2",colnames(respi_wide)),names_to="source",values_to = "CO2")
respi_long$source <- str_remove(respi_long$source,"CO2_")
#############################
#Datensatz speichern
save(data_agg,file=paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))
##################
#plots

#CO2 ~ Zeit mit Pumpstufen als rect
ggplot(data)+
  annotate("rect", xmin=Versuch_sub$start,xmax=Versuch_sub$ende,ymin=-Inf,ymax=Inf, alpha=Versuch_sub$Pumpstufe/max(Versuch_sub$Pumpstufe)*0.3, fill="red")+
  geom_line(aes(date,CO2,col=as.factor(tiefenstufe)))+labs(col="tiefe")


#######
#CO2 ~ Zeit leave NAtime
plt <- leave_NAtime_plot(data=data,group="CO2",plot=T,adj_grob_size=F,breaks="1 day",date_labels= "%b %d")
plt_data <- leave_NAtime_plot(data=data,group="CO2",plot=F)

plt2 <- plt+
  geom_rect(data=Versuch_sub,aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf, fill=as.character(Pumpstufe)))+
  scale_fill_manual(values = alpha("red",seq(0,0.3,len=length(Versuch_x))))+
  labs(fill="Pumpstufe")+
  theme(strip.text.x = element_blank())


ggplot(subset(plt_data,period %in% 4:5))+
  geom_point(aes(date,CO2,col=as.factor(tiefenstufe)),size=0.4)+labs(col="tiefe")+
  geom_smooth(data=subset(plt_data,period %in% 4:5 & Pumpstufe == 3),aes(date,CO2,col=as.factor(tiefenstufe)),method="glm",linetype=2)+
  ggsave(paste0(plotpfad,"Sandkiste_Versuch_4u5.pdf"),width=11,height=7)
  

pdf(paste0(plotpfad,"Tracer_Versuche_sandkiste.pdf"),width=11,height = 5)
adj_grob_size(plt2,plt_data,"1 day",date_labels= "%b %d")
dev.off()

#CO2_rollapply ~ Zeit 
leave_NAtime_plot(y="CO2_rollapply",col="PSt_Nr",data=data,group="CO2",geom="point",breaks="1 day",date_labels= "%b %d")
ggplot(subset(data, !is.na(Pumpstufe)))+
  geom_point(aes(date, CO2_rollapply,col=as.factor(tiefenstufe)))

#CO2 ~ tiefe ohne glm
ggplot(subset(data,!is.na(Pumpstufe)))+
  geom_point(aes(CO2_rollapply,tiefe,col=PSt_Nr))+labs(col="Pumpstufe Versuch-Nr")
#CO2 ~ tiefe mit respi


ggplot(respi_long)+
  geom_path(aes(CO2,tiefe,col=source,linetype=as.factor(Versuch)))+labs(shape="Versuch")+
  ggsave(paste0(plotpfad,"respi_tiefenprofil_sandkiste.pdf"),width=6,height=6)
  


#CO2 ~ tiefe mit glm
ggplot(subset(data,!is.na(Pumpstufe)), aes(CO2_rollapply, tiefe, col=PSt_Nr))+
  geom_point()+
  geom_smooth(method="glm",formula= y ~ poly(x,2,raw=T))+
  labs(col="Pumpstufe Versuch-Nr")+labs(x="CO2 [ppm]",y="tiefe [cm]")+ggsave(paste0(plotpfad,"CO2_Tiefe_sandkiste.pdf"),width=8,height=5)


#CO2 ~ tiefe aggregierte Punkte 
ggplot(subset(data,Versuch==4),aes(CO2,tiefe,col=PSt_Nr))+
  geom_point()+
  geom_smooth(method = "glm")
ggplot(subset(data_agg,Versuch==4),aes(CO2,tiefe,col=PSt_Nr))+
  geom_point()+
  geom_smooth(method = "glm")

#gradient ~ tiefe
ggplot(data_agg)+geom_path(aes(gradient,tiefenmittel,col=PSt_Nr))
#gradient ~ Pumpstufe
ggplot(data_agg,aes(Pumpstufe,gradient,col=as.character(tiefenmittel),shape=as.character(Versuch)))+
  geom_point()#+
  

ggplot(data_agg)+geom_path(aes(DS_D0_CO2,tiefe,col=PSt_Nr))


