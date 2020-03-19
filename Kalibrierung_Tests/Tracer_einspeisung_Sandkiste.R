#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
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
Versuch_sub <- subset(Pumpzeiten,Versuch %in% Versuch_x)
datelim <- range(c(Versuch_sub$start, Versuch_sub$ende),na.rm = T)
datelim[2] <- datelim[2]+3600*12
data <- read_sampler("sampler1",datelim = datelim, format = "long")

data$Pumpstufe <- NA
for(i in seq_along(Pumpzeiten$Pumpstufe)){
data$Pumpstufe[data$date > Pumpzeiten$start[i] + 7*60*60 & data$date < Pumpzeiten$ende[i] - 2*60*60] <- Pumpzeiten$Pumpstufe[i]
}

ggplot(data)+
  annotate("rect", xmin=Versuch_sub$start,xmax=Versuch_sub$ende,ymin=-Inf,ymax=Inf, alpha=Versuch_sub$Pumpstufe/max(Versuch_sub$Pumpstufe)*0.3, fill="red")+
  geom_line(aes(date,CO2,col=as.factor(tiefenstufe)))

data$CO2_rollapply <- zoo::rollapply(data$CO2,width=10,mean,fill=NA)

ggplot(subset(data, !is.na(Pumpstufe)))+
  geom_point(aes(date, CO2_rollapply,col=as.factor(tiefenstufe)))

ggplot(subset(data,!is.na(Pumpstufe)))+
  geom_point(aes(CO2_rollapply,tiefe,col=as.factor(Pumpstufe)))

ggplot(subset(data,!is.na(Pumpstufe) & tiefe < -3.5), aes(CO2_rollapply, tiefe, col=as.factor(Pumpstufe)))+
  geom_point()+
  geom_smooth(method="glm",formula= y ~ poly(x,2,raw=T))


data_agg <- aggregate(list(CO2=data$CO2),list(Pumpstufe=data$Pumpstufe,tiefe=data$tiefe,tiefenstufe = data$tiefenstufe),mean)

data_agg$gradient <- NA
for(i in unique(data_agg$Pumpstufe)){
data_agg$gradient[data_agg$Pumpstufe==i] <- c(diff(data_agg$CO2[data_agg$Pumpstufe==i])/diff(-data_agg$tiefe[data_agg$Pumpstufe==i]),NA) #ppm/cm
}
ggplot(data_agg)+geom_point(aes(gradient,tiefe,col=as.character(Pumpstufe)))
ggplot(data_agg)+
  geom_point(aes(Pumpstufe,gradient,col=as.character(tiefenstufe)))+
  geom_smooth(aes(Pumpstufe,gradient,col=as.character(tiefenstufe)),method="glm")

