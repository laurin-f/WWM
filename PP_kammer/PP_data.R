#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","svMisc")
check.packages(packages)

pp_bemerkungen <-  readODS::read_ods(paste0(metapfad_PP,"PP_Kammer_Bemerkungen.ods"))
pp_bemerkungen$Start <- dmy_hm(pp_bemerkungen$Start)

pp_chamber <- readODS::read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)
i<-9
i<-nrow(pp_chamber)
datelim <- t(pp_chamber[i,c("Start","Ende")]+(3600*2*c(-1,1)))
pp_bemerkungen$Start <- pp_bemerkungen$Start - 3600
pp_bemerkungen <- sub_daterange(pp_bemerkungen,datelim,"Start")

data <- read_PP(datelim = datelim,format="long")
data <- subset(data, id != 5)

dt <- round(median(diff_time(data$date[data$id == 1]),na.rm=T),2)
data <- data %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,3*60/dt,fill=NA))


ggplot(data)+
  geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.2)+
  #geom_text(data=pp_bemerkungen,aes(x=Start,y=Inf,label=Bemerkung),hjust=0,vjust=1)+
  #geom_vline(data=pp_bemerkungen,aes(xintercept=Start))+
  geom_line(aes(date,PPC,col=id))

data_sub <- sub_daterange(data,ymd_hms("2022-04-04 12:00:00","2022-04-04 12:02:00"))
data_sub <- sub_daterange(data,ymd_hms("2022-04-06 14:30:00","2022-04-06 14:35:00"))
data_sub <- sub_daterange(data,ymd_hms("2022-04-11 15:00:00","2022-04-11 16:27:00"))
data_sub <- sub_daterange(data,ymd_hms("2022-04-12 13:00:00","2022-04-13 16:27:00"))
ggplot(subset(data_sub,id %in% 1:4 ))+
  geom_line(aes(date,P,col=id),alpha=0.4)+
  geom_line(aes(date,P_roll,col=id))+
  geom_line(aes(date,P_filter,col=id))


#######################################################
#######################################################
pp_chamber
datelim <- c("2022-03-30 13:00:00","2022-03-30 14:40:00")
#datelim <- c("2022-03-30 11:00:00","2022-03-30 13:00:00")
data <- read_PP(datelim = datelim)
range(data$date)
ggplot(subset(data,id!=6))+geom_line(aes(date,P,col=id))

datelim <- c("2022-01-31 10:55:00","2022-01-31 11:00:00")
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,P,col=id))
datelim <- "2022-02-01 13:40:00"
datelim <- c("2022-03-08 11:40:00","2022-03-08 12:30:00")
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,P,col=id))
datelim <- "2022-03-09 10:40:00"
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,P,col=id))
datelim <- "2022-03-09 16:00:00"
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,P,col=id))
datelim <- "2022-03-14 10:00:00"
data <- read_PP(datelim = datelim)
ggplot(subset(data))+geom_line(aes(date,P,col=id))





plot((0:59 -5 - 2 )%% 10)
lines((0:59 +2) %% 10)
mins <- 0:20
off <- ifelse((0:59 -5 - 2 )%% 30 == 0,1,0)
on <- ifelse((0:59 +2) %% 30 == 0,1,0)
cbind(mins,on,off)
ifelse((0:20 -5 - 2 )%% 30== 0 |(0:20 +2) %% 30 == 0,1,0)


