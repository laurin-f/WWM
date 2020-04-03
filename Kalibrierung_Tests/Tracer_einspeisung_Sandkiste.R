#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

#Metadata Backpulverversuch
# Pumpstufe <- 4 
# Pumpzeitraum<-ymd_hm("2020.02.19 13:05","2020.02.20 08:35")
# 
# datelim<-c("2020.02.19 12:05:00","2020.02.20 09:35:00")
# 
# data <- read_sampler("sampler1",datelim = datelim)
# 
# #plot Backpulver versuch
# ggplot(data)+
#   annotate("rect", xmin=Pumpzeitraum[1],xmax=Pumpzeitraum[2],ymin=0,ymax=10000, alpha=0.2, fill="red")+
#   geom_line(aes(date,CO2,col=tiefenstufe))

#Metadata xlsx
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad,"Tracereinspeisung_Sandkiste.xlsx"))
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]
Versuch_x <- unique(Pumpzeiten$Versuch)
#Versuch_x <- 3
Versuch_sub <- subset(Pumpzeiten,Versuch %in% Versuch_x)
Versuch_sub$period <-Versuch_sub$Versuch 
datelim <- range(c(Versuch_sub$start, Versuch_sub$ende),na.rm = T)
datelim[2] <- datelim[2]+3600*12
data <- read_sampler("sampler1",datelim = datelim, format = "long")

#Metadata Pumpstufen flux
flux <- read.csv(paste0(metapfad,"Pumpstufen_flux.txt"))

#Pumpstufe und Versuch aus metadaten auf dataframe übetragen
data$Pumpstufe <- NA
data$Versuch <- NA
for(i in seq_along(Pumpzeiten$Pumpstufe)){
  Pumpzeiten_lim <- data$date > Pumpzeiten$start[i] + 9*60*60 & data$date < Pumpzeiten$ende[i] - 2*60*60
data$Pumpstufe[Pumpzeiten_lim] <- Pumpzeiten$Pumpstufe[i]
data$Versuch[Pumpzeiten_lim] <- Pumpzeiten$Versuch[i]
}
data$PSt_Nr <- paste("PSt",data$Pumpstufe,"Nr",data$Versuch,sep="_")

#kurven glätten mit rollapply
data$CO2_rollapply <- zoo::rollapply(data$CO2,width=10,mean,fill=NA)


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
A <-20^2*pi

#flux für jeweilige Pumpstufe aus metadaten übertragen
data_agg$Fz <- sapply(data_agg$Pumpstufe, function(x) flux$tracer_ml_per_min[flux$Pumpstufe == x])

#Ficks law anwenden
data_agg$DS <- data_agg$Fz/60/A * data_agg$dz / (data_agg$dC/10^6) #cm3 s-1 cm-2 * cm = cm2 s-1

mean(data_agg$DS[data_agg$Versuch==3],na.rm=T)#cm2/s

data_agg$DS_D0_CO2 <- data_agg$DS/D0_CO2
data_agg$tiefenmittel <- rowMeans(cbind(c(data_agg$tiefe[-nrow(data_agg)],NA),c(data_agg$tiefe[-1],NA)))
data_agg$tiefenmittel[data_agg$tiefe == -24.5] <- NA

data_agg$Z_mod <- data_agg$tiefe +40
data_agg$R_mod <- 0
# data_agg$R_mod <- 10.5


data_agg$CO2_mol_per_m3 <- ppm_to_mol(data_agg$CO2)

# meas_depths <- (40-(1:7*3.5))
# 
# mesh <- read.table(paste0(comsolpfad,"conc_test.txt"),skip=9,col.names = c("r","z","CO2_mol_per_m3"))
# id<-rep(NA,length(meas_depths))
# for(i in seq_along(meas_depths)){
#   id[i]<-which( mesh$r < 0.1 & mesh$r > 0 & abs((mesh$z) - meas_depths[i]-0.1)< 0.2)
# }
# mesh[id,]
# data_agg$Z_adj <- mesh[id,"z"]
# data_agg$R_adj <- mesh[id,"r"]

# data_agg$Z_mod <- data_agg$Z_mod - 0.1
# data_agg$R_mod <- data_agg$R_mod + 0.1
write.table(data_agg[data_agg$PSt_Nr == "PSt_5_Nr_3",c("Z_mod","CO2_mol_per_m3")],file = paste0(comsolpfad,"PSt_5_Nr_3.txt"),row.names = F,col.names = F)
write.table(data_agg[data_agg$PSt_Nr == "PSt_5_Nr_3",c("Z_mod","R_mod","CO2_mol_per_m3")],file = paste0(comsolpfad,"PSt_5_Nr_3.csv"),row.names = F,col.names = F,sep=",")
# write.table(data_agg[data_agg$PSt_Nr == "PSt_5_Nr_3",c("Z_adj","R_adj","CO2_mol_per_m3")],file = paste0(comsolpfad,"PSt_5_Nr_3.csv"),row.names = F,col.names = F,sep=",")

##################
#plots

#CO2 ~ Zeit mit Pumpstufen als rect
ggplot(data)+
  annotate("rect", xmin=Versuch_sub$start,xmax=Versuch_sub$ende,ymin=-Inf,ymax=Inf, alpha=Versuch_sub$Pumpstufe/max(Versuch_sub$Pumpstufe)*0.3, fill="red")+
  geom_line(aes(date,CO2,col=as.factor(tiefenstufe)))+labs(col="tiefe")

#######
#CO2 ~ Zeit leave NAtime
plt <- leave_NAtime_plot(data=data,group="CO2",plot=T)
plt_data <- leave_NAtime_plot(data=data,group="CO2",plot=F)

plt2 <- plt+
  geom_rect(data=Versuch_sub,aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf, fill=as.character(Pumpstufe)))+
  scale_fill_manual(values = alpha("red",seq(0.1,0.3,len=3)))+
  labs(fill="Pumpstufe")+
  theme(strip.text.x = element_blank())

pdf(paste0(plotpfad,"Tracer_Versuche_sandkiste.pdf"),width=11,height = 5)
adj_grob_size(plt2,plt_data,"1 day",date_labels= "%b %d")
dev.off()

#CO2_rollapply ~ Zeit 
ggplot(subset(data, !is.na(Pumpstufe)))+
  geom_point(aes(date, CO2_rollapply,col=as.factor(tiefenstufe)))

#CO2 ~ tiefe ohne glm
ggplot(subset(data,!is.na(Pumpstufe)))+
  geom_point(aes(CO2_rollapply,tiefe,col=PSt_Nr))+labs(col="Pumpstufe Versuch-Nr")

#CO2 ~ tiefe mit glm
ggplot(subset(data,!is.na(Pumpstufe)), aes(CO2_rollapply, tiefe, col=PSt_Nr))+
  geom_point()+
  geom_smooth(method="glm",formula= y ~ poly(x,2,raw=T))+
  labs(col="Pumpstufe Versuch-Nr")+labs(x="CO2 [ppm]",y="tiefe [cm]")+ggsave(paste0(plotpfad,"CO2_Tiefe_sandkiste.pdf"),width=8,height=5)


#CO2 ~ tiefe aggregierte Punkte 
ggplot(data_agg,aes(CO2,tiefe,col=PSt_Nr))+
  geom_point()+
  geom_smooth(method = "glm")

#gradient ~ tiefe
ggplot(data_agg)+geom_point(aes(gradient,tiefenmittel,col=PSt_Nr))
#gradient ~ Pumpstufe
ggplot(data_agg,aes(Pumpstufe,gradient,col=as.character(tiefenmittel),shape=as.character(Versuch)))+
  geom_point()#+
  

ggplot(data_agg)+geom_point(aes(DS_D0_CO2,tiefe,col=PSt_Nr))


