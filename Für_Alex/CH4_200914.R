#packages
library(pkg.WWM)
check.packages(c("lubridate","stringr","ggplot2"))

#daten laden
datelim <- ymd_h(c("2020.09.14 13","2020.09.14 15"))
data <- read_db("GGA.db","micro",datelim)

#subset erstes und zweites Prüfgas
data1 <- subset(data, CH4 < 1.8 & CH4 > 1.7)
data2 <- subset(data, CH4 < 0.6 & CH4 > 0)

#anfang der ansaugzeiträume
begins1 <- (c(1,which(diff(data1$date)>10)+1))
begins2 <- (c(1,which(diff(data2$date)>10)+1))

#zeitraum der abgeschnitten wird
cutoff <- -1:2


trim1 <- c(rep(begins1,each=length(cutoff))+cutoff,nrow(data1))
trim2 <- c(rep(begins2,each=length(cutoff))+cutoff,nrow(data2))
#datensatz zuscheiden
data1_trim <- data1[-trim1,]
data2_trim <- data2[-trim2,]

ggplot(data)+
  geom_line(aes(date,CH4))+
  geom_point(data=data1_trim,aes(date,CH4,col="Prüfgas 1"))+
  geom_point(data=data2_trim,aes(date,CH4,col="Prüfgas 2"))

ggplot(data)+
  geom_line(aes(date,CO2))+
  geom_point(data=data1_trim,aes(date,CO2,col="Prüfgas 1"))+
  geom_point(data=data2_trim,aes(date,CO2,col="Prüfgas 2"))

ggplot(data1)+
  geom_point(aes(date,CH4))+
  geom_point(data=data1_trim,aes(date,CH4,col="trim"))
ggplot(data2)+
  geom_point(aes(date,CH4))+
  geom_point(data=data2_trim,aes(date,CH4,col="trim"))

df <- data.frame(Prüfgas=1:2)
#mittelwert Prüfgas erste Verdünnung
df$CH4mean[1] <- mean(data1$CH4dry)
#sd Prüfgas erste Verdünnung
df$CH4sd[1] <- sd(data1$CH4dry)
#mittelwert Prüfgas zweite Verdünnung
df$CH4mean[2] <- mean(data2$CH4dry)
#sd Prüfgas zweite Verdünnung
df$CH4sd[2] <- sd(data2$CH4dry)
#mittelwert Prüfgas erste Verdünnung
df$CO2mean[1] <- mean(data1$CO2dry)
#sd Prüfgas erste Verdünnung
df$CO2sd[1] <- sd(data1$CO2dry)
#mittelwert Prüfgas zweite Verdünnung
df$CO2mean[2] <- mean(data2$CO2dry)
#sd Prüfgas zweite Verdünnung
df$CO2sd[2] <- sd(data2$CO2dry)

write.csv(df,file="Prüfgase.csv",row.names = F)
write.csv(data[,c("date","CO2dry","CH4dry")],file="Zeitreihe_CO2uCH4.csv",row.names = F)
