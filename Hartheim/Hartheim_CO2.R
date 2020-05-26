#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Hartheim/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)


###################
#metadaten
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad,"Tracereinspeisung_Hartheim.xlsx"))
Pumpzeiten$ende <- as_datetime(Pumpzeiten$ende)
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]

datelim <- c("2020.05.18 10:00:00")
smp1 <- read_sampler("sampler1",datelim = datelim, format = "long")
smp2 <- read_sampler("sampler2",datelim = datelim, format = "long")

#smp1_plt <- ggplot(smp1)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

#T_plt <- ggplot(smp1)+geom_line(aes(date,T_C))
#egg::ggarrange(smp1_plt,T_plt,heights=c(4,1))
#smp1_plt+geom_vline(xintercept = Pumpstufen$start)
#ggplot(smp2)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

data <- merge(smp1,smp2,by=c("date","tiefe","tiefenstufe","variable"),all=T,suffixes = c("_inj","_ref"))

#Pumpstufe und Versuch aus metadaten auf dataframe übetragen


#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
stunden_bis_steadystate <- rep(10,nrow(Pumpzeiten))
stunden_cut_off <- rep(2,nrow(Pumpzeiten))


#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
cols2data <- c("Pumpstufe")
data[,cols2data] <- NA

for(i in seq_along(Pumpzeiten$Pumpstufe)){
  Pumpzeiten_lim <- data$date > (Pumpzeiten$start[i] + stunden_bis_steadystate[i] * 60 * 60) & if(!is.na(Pumpzeiten$ende[i])) {data$date < (Pumpzeiten$ende[i] - stunden_cut_off[i] * 60 * 60)}else{ T }
  for(j in cols2data){
    data[Pumpzeiten_lim,j] <- Pumpzeiten[i,j]
  }
}




#kurven glätten mit rollapply
for(j in c("_inj","_ref")){
 data[,paste0("CO2_roll",j)] <- NA
for(i in unique(data$tiefe)){
  tiefe.i <- data$tiefe == i
  data[tiefe.i,paste0("CO2_roll",j)] <- zoo::rollapply(data[tiefe.i,paste0("CO2",j)],width=10,mean,fill=NA)
}
}


inj <- ggplot(data)+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  geom_vline(xintercept = Pumpzeiten$start[2])+
  annotate("text",x=Pumpzeiten$start[2],y=Inf,label="injection",vjust=1.9,hjust=-0.3)+labs(col="tiefe [cm]",x="",y=expression(CO[2]*" [ppm]"),title="injection sampler")

ref <- ggplot(data)+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  geom_vline(xintercept = Pumpzeiten$start[2])+
  guides(col=F)+labs(y=expression(CO[2]*" [ppm]"),title="reference sampler")
ref  
p <- egg::ggarrange(inj,ref,ncol=1)

pdf(paste0(plotpfad,"hartheim_einspeisung1.pdf"),width=9,height=7)
p
dev.off()
  ggplot(data)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_roll_ref,col=as.factor(tiefe)))+
  geom_ribbon(aes(date,ymin=CO2_inj,ymax=CO2_ref,fill=as.factor(tiefe)),alpha=0.3)+
  geom_vline(xintercept = Pumpzeiten$start)+
  ggsave(paste0(plotpfad,"hartheim_einspeisung1.pdf"),width=5,height=14)

ggplot(data)

ggplot(data)+geom_point(aes(CO2_inj,CO2_ref,col=as.factor(Pumpstufe)))+geom_abline(slope=1,intercept = 0)