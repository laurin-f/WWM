#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","svMisc")
check.packages(packages)

datelim <- c("2022-03-16 12:30:00","2022-03-16 16:30:00")
data <- read_PP(datelim = datelim)
range(data$date)
ggplot(subset(data,id==6))+geom_line(aes(date,p_Pa,col=id))

datelim <- c("2022-01-31 10:55:00","2022-01-31 11:00:00")
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,p_Pa,col=id))
datelim <- "2022-02-01 13:40:00"
datelim <- c("2022-03-08 11:40:00","2022-03-08 12:30:00")
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,p_Pa,col=id))
datelim <- "2022-03-09 10:40:00"
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,p_Pa,col=id))
datelim <- "2022-03-09 16:00:00"
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,p_Pa,col=id))
datelim <- "2022-03-14 10:00:00"
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,p_Pa,col=id))





plot((0:59 -5 - 2 )%% 10)
lines((0:59 +2) %% 10)
mins <- 0:20
off <- ifelse((0:59 -5 - 2 )%% 30 == 0,1,0)
on <- ifelse((0:59 +2) %% 30 == 0,1,0)
cbind(mins,on,off)
ifelse((0:20 -5 - 2 )%% 30== 0 |(0:20 +2) %% 30 == 0,1,0)
