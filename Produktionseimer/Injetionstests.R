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

datelim <- ymd_h(c("2020.12.22 00","2020.12.22 04"))
#datelim <- ymd_h(c("2021.02.09 10","2021.02.09 17"))

test <- injectionrate(datelim = datelim,Pumpstufen = 1,group="Pumpstufe",t_init = 1,aggregate = T,closing_before = 50,closing_after = 50)


par(mfrow = c(2,1),mar=c(1,3,1,1))
plot(data.agg$date, data.agg[,gas], pch=20,xlab="")

before_afters <- c(closing_before,closing_after,opening_before,opening_after)
plot(before,xlab="",ylim = c(min(before_afters)-10,2*max(before_afters)))
abline(h=closing_before,col=3,lty=2)
abline(h=closing_after,col=3)
abline(h=opening_before,col=2,lty=2)
abline(h=opening_after,col=2)
points(after,pch=3,col=4)

legend("bottomleft",c("before","after","closing","opening"),col = c(1,4,3,2),pch=c(1,3,NA,NA),lty=c(NA,NA,1,1), bty = "n")
par(mfrow = c(1,1))

####################################
#load data
####################################
#Metadata xlsx
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad,"Tracereinspeisung_Sandkiste.xlsx"))
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]

Pumpzeiten$period <-Pumpzeiten$Versuch 
Pumpzeiten$PSt_Nr <- paste("PSt",Pumpzeiten$Pumpstufe,"Nr",Pumpzeiten$Versuch,sep="_")

#samplerdata
datelim <- range(c(Pumpzeiten$start, Pumpzeiten$ende),na.rm = T)
datelim[2] <- datelim[2]+3600*12

data <- read_sampler("sampler1",datelim = datelim, format = "long")


#Metadata Pumpstufen flux
flux <- read.csv(paste0(metapfad,"Pumpstufen_flux.txt"))

###############################
#postprocesing
#############################

#Pumpstufe und Versuch aus metadaten auf dataframe übetragen

#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
stunden_bis_steadystate <- rep(10,nrow(Pumpzeiten))
stunden_cut_off <- rep(2,nrow(Pumpzeiten))

#Veruch 7 war kürzer
stunden_cut_off[17:18]<-0.5
stunden_bis_steadystate[18] <- 9

#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
cols2data <- c("Pumpstufe","Versuch","respiration_simul","material")
data[,cols2data] <- NA

for(i in seq_along(Pumpzeiten$Pumpstufe)){
  Pumpzeiten_lim <- data$date > (Pumpzeiten$start[i] + stunden_bis_steadystate[i] * 60 * 60) & 
    data$date < (Pumpzeiten$ende[i] - stunden_cut_off[i] * 60 * 60)
  for(j in cols2data){
    data[Pumpzeiten_lim,j] <- Pumpzeiten[i,j]
  }
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

############################
#Fz mit zeitlichem drift bestimmen
flux$date <- ymd_hms(flux$date)
data$Fz <- NA
for(i in na.omit(unique(data$Pumpstufe))){
  flux_i <- subset(flux, Pumpstufe == i)
  id <- which(data$Pumpstufe == i)
  if(nrow(flux_i) > 1){
    data[id,"Fz"] <- approx(x=(flux_i$date),y=flux_i$tracer_ml_per_min,xout=(data$date[id]),rule=2)$y
  }else{
    data[id,"Fz"] <- flux_i$tracer_ml_per_min
  }
}

#ggplot(data)+geom_point(aes(date,Fz,col=as.factor(Pumpstufe)))
###################
#Aggregate Data
data_agg <- aggregate(list(CO2=data$CO2,Fz=data$Fz),list(Pumpstufe=data$Pumpstufe,tiefe=data$tiefe,tiefenstufe = data$tiefenstufe,Versuch= data$Versuch,PSt_Nr = data$PSt_Nr,respi_sim = data$respiration_simul,material = data$material),mean)

#Spalte ID angängen
data_agg$ID <- paste0(data_agg$PSt_Nr,data_agg$respi_sim)

###################
#gradienten berechnen


fm_list <- lapply(unique(data_agg$ID),function(x) glm(CO2~tiefe,data=subset(data_agg, ID == x)))
dC_dz <- sapply(fm_list,"[[","coefficients")[2,]
names(dC_dz) <-  unique(data_agg$ID)
data_agg$dC_dz <- NA
for (i in names(dC_dz)){
  data_agg$dC_dz[data_agg$ID == i] <- abs(dC_dz[i])
}
##############################
#Fick's Law
#Fz = -DS * (dC / dz)
D0_CO2 <- D0_T_p(15)#cm/s

#Fläche
A <-15^2*pi#cm2
