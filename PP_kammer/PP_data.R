#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","svMisc")
check.packages(packages)
##########################################################
###########################################################


datelim <- ymd_hms("2022-06-28 16:00:00 UTC", "2022-07-01 12:00:00 UTC")
datelim <- ymd_hms("2022-07-05 10:00:00 UTC", "2022-07-05 14:00:00 UTC")
datelim <- ymd_hms("2022-07-12 10:00:00 UTC", "2022-07-13 14:00:00 UTC")
datelim <- ymd_hms("2022-07-13 14:00:00 UTC", "2022-07-14 14:00:00 UTC")
datelim <- ymd_hms("2022-07-18 11:00:00 UTC", "2022-07-18 12:00:00 UTC")
datelim <- ymd_hms("2022-07-19 10:00:00 UTC", "2022-07-19 15:00:00 UTC")
datelim <- ymd_hms("2022-07-19 14:00:00 UTC", "2022-07-20 15:00:00 UTC")
datelim <- ymd_hms("2022-07-20 10:00:00 UTC", "2022-07-20 17:00:00 UTC")
datelim <- ymd_hms("2022-07-26 10:00:00 UTC", "2022-07-26 17:00:00 UTC")
datelim <- ymd_hms("2022-07-26 10:00:00 UTC", "2022-07-27 17:00:00 UTC")
datelim <- ymd_hms("2022-07-27 10:00:00 UTC", "2022-07-28 12:00:00 UTC")
datelim <- ymd_hms("2022-08-01 10:00:00 UTC", "2022-08-01 18:00:00 UTC")
datelim <- ymd_hms("2022-08-02 10:00:00 UTC", "2022-08-03 12:00:00 UTC")
datelim <- ymd_hms("2022-08-03 15:00:00 UTC", "2022-08-03 17:00:00 UTC")
datelim <- ymd_hms("2022-08-06 15:00:00 UTC", "2022-08-10 17:00:00 UTC")
datelim <- ymd_hms("2022-08-10 15:00:00 UTC", "2022-08-11 17:00:00 UTC")

data_PPC <- read_PP(datelim = datelim)

dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)

data_PPC <- data_PPC %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         P_diff = abs(c(NA,diff(P_filter)))/!!dt,
         PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
         P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA))

#range(data_PPC$date)


ggplot(subset(data_PPC,id%in%c(1,2,3,4,5,6)))+
  #xlim(ymd_hms("2022-07-12 14:58:00 UTC", "2022-07-12 16:30:00 UTC"))+
  #geom_line(aes(date,P,col=as.factor(id)))
  geom_line(aes(date,P_roll,col=as.factor(id)))#+



ggplot(subset(data_PPC,id%in%c(1,2,3,4,5,6)))+
  #xlim(ymd_hms("2022-07-12 10:00:00 UTC", "2022-07-12 15:30:00 UTC"))+
  geom_line(aes(date,PPC5,col=as.factor(id)))#+
#  geom_vline(xintercept = ymd_hm("2022.07.28 09:20"))

ggplot(subset(data_PPC,id%in%c(1,2,3,4,5,6)))+
  geom_line(aes(date,P,col=as.factor(id)))+

  #geom_vline(xintercept = ymd_hm("2022.06.29 16:08","2022.06.29 16:44","2022.06.29 16:51","2022.06.29 17:23"))+
  xlim(ymd_hm("2022.07.26 13:45","2022.07.26 13:50"))
xlim(ymd_hm("2022.06.30 12:40","2022.06.30 13:05"))
xlim(ymd_hm("2022.06.29 13:40","2022.06.29 13:45"))
#xlim(ymd_hm("2022.06.30 00:40","2022.06.30 06:50"))
#xlim(ymd_hm("2022.06.29 16:05","2022.06.29 16:15"))
#xlim(ymd_hm("2022.06.29 15:40","2022.06.29 17:50"))




##############################
#ALT
###############################


pp_bemerkungen <-  readODS::read_ods(paste0(metapfad_PP,"PP_Kammer_Bemerkungen.ods"))
pp_bemerkungen$Start <- dmy_hm(pp_bemerkungen$Start)
pp_bemerkungen$Ende <- dmy_hm(pp_bemerkungen$Ende)

pp_chamber <- readODS::read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)
i<-10
i<-nrow(pp_chamber)
datelim <- t(pp_chamber[i,c("Start","Ende")]+(3600*2*c(-1,1)))
pp_bemerkungen$Start <- pp_bemerkungen$Start - 3600
pp_bemerkungen <- sub_daterange(pp_bemerkungen,datelim,"Start")

data <- read_PP(datelim = datelim,format="long")
data <- subset(data, id != 6)

dt <- round(median(diff_time(data$date[data$id == 1]),na.rm=T),2)
data <- data %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,3*60/dt,fill=NA))

#messung i=9 am Anfang alle P Sonden zusammen
ggplot(subset(data[1:50000,],id %in% 1:4 ))+
  geom_line(aes(date,P,col=id))


255*0.99

ggplot(data)+
  geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.2)+
  #geom_text(data=pp_bemerkungen,aes(x=Start,y=Inf,label=Bemerkung),hjust=0,vjust=1)+
  #geom_vline(data=pp_bemerkungen,aes(xintercept=Start))+
  geom_line(aes(date,P_roll,col=id))

data_sub <- sub_daterange(data,ymd_hms("2022-04-04 12:00:00","2022-04-04 12:02:00"))
data_sub <- sub_daterange(data,ymd_hms("2022-04-06 14:30:00","2022-04-06 14:35:00"))
data_sub <- sub_daterange(data,ymd_hms("2022-04-11 15:00:00","2022-04-11 16:27:00"))
data_sub <- sub_daterange(data,ymd_hms("2022-04-12 13:00:00","2022-04-13 16:27:00"))
ggplot(subset(data_sub,id %in% 1:4 ))+
  geom_line(aes(date,P,col=id),alpha=0.4)+
  geom_line(aes(date,P_roll,col=id))+
  geom_line(aes(date,P_filter,col=id))

#############################
#Kalibrierung Messung 14

i<-14
datelim <- ymd_hms(t(pp_chamber[i,c("Start","Ende")]+(3600*5*c(-1,1))))
pp_bemerkungen$Start <- pp_bemerkungen$Start - 3600
pp_bemerkungen <- sub_daterange(pp_bemerkungen,datelim,"Start")

data <- read_PP(datelim = datelim,format="long",corfac = F)
data <- data %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,3*60,fill=NA))
data_sub <- subset(data,id!=6)
start_datelim <- c(min(data$date),min(data$date)+2*60)
end_datelim <- c(max(data$date)-3*60,max(data$date))
ggplot(sub_daterange(data_sub,start_datelim))+
  geom_line(aes(date,P,col=id),alpha=.2)+
  geom_line(aes(date,P_roll,col=id))
ggplot(sub_daterange(data_sub,end_datelim))+
  geom_line(aes(date,P,col=id),alpha=.2)+
  geom_line(aes(date,P_roll,col=id))
ggplot(data_sub)+
  #geom_line(aes(date,P,col=id))+
  geom_line(aes(date,P_roll,col=id))
#  xlim(ymd_hm("2022.05.03 10:00","2022.05.03 10:30"))

P_cor_start <- sub_daterange(data_sub,start_datelim) %>% 
  group_by(id) %>%
  summarise(P_mean = mean(P))
P_cor_end <- sub_daterange(data_sub,end_datelim) %>% 
  group_by(id) %>%
  summarise(P_mean = mean(P))

ggplot()+
  geom_point(data=P_cor_start,aes(id,P_mean,col="start"))+
  geom_point(data=P_cor_end,aes(id,P_mean,col="end"))+
  geom_point(data=P_corfac,aes(id,P_mean,col="corfac"))


for(i in 1:5){
  data_sub$P[data_sub$id == i] <- data_sub$P[data_sub$id == i] - P_cor_start$P_mean[i]
  data_sub$P_roll[data_sub$id == i] <- data_sub$P_roll[data_sub$id == i] - P_cor_start$P_mean[i]
}

ggplot(data_sub)+
  #geom_line(aes(date,P,col=id))+
  geom_line(aes(date,P_roll,col=id))

!(1==2&2==2)
((2:23)-2) %% 3
#######################################################
#######################################################
pp_chamber
 
datelim <- c("2022-03-30 13:38:42","2022-03-30 13:38:50")
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


###################
#kalibriermessung in der Werkstatt
datelim <- c("2022-04-28 14:00:00","2022-04-28 15:00:00")
data1 <- read_PP(datelim = datelim,corfac = F)
datelim <- c("2022-04-27 13:40:00","2022-04-27 13:45:00")
data <- read_PP(datelim = datelim,corfac = F)
data <- data %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,10,fill=NA))
ggplot(subset(data,id %in% 1:4))+geom_line(aes(date,P_roll,col=id))

P_corfac <- data1 %>% 
  filter(id != 6) %>% 
  group_by(id) %>%
  summarise(P_mean = mean(P))
means2 <- data %>% 
  group_by(id) %>%
  summarise(P_mean = mean(P))
ggplot(means[1:4,])+
  geom_point(aes(id,P_mean))+
  geom_point(data=means2[1:4,],aes(id,P_mean,col="2"))

  
for(i in 1:5){
  data_sub[,paste0("P_",i)] <- data_sub[,paste0("P_",i)]-P_cor_start$P_mean[i]
}
#save(P_corfac,file=paste0(datapfad_PP_Kammer,"P_corfac.RData"))
#####################
#test von unterdruck in der Kammer

datelim <- c("2022-04-27 10:55:00","2022-04-27 15:00:00")
data <- read_PP(datelim = datelim)
dt <- round(median(diff_time(data$date[data$id == 1]),na.rm=T),2)
data <- data %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,1*60/dt,fill=NA))

pp_bemerk <- pp_bemerkungen[c(9:10,12:17),]

ggplot(subset(data, id %in% 1:4))+
  #geom_line(aes(date,P,col=id))+
  geom_line(aes(date,P_roll,col=id))+
  geom_vline(data = pp_bemerk,aes(xintercept=Start))+
  geom_text(data = pp_bemerk,aes(Start,6+(1:8)*0.3,label=str_remove(Bemerkung,"Kammer ")),hjust=0,vjust=1)+
  xlim(range(c(pp_bemerk$Start[5:8],pp_bemerk$Ende[5:8]),na.rm=T))



plot((0:59 -5 - 2 )%% 10)
lines((0:59 +2) %% 10)
mins <- 0:20
off <- ifelse((0:59 -5 - 2 )%% 30 == 0,1,0)
on <- ifelse((0:59 +2) %% 30 == 0,1,0)
cbind(mins,on,off)
ifelse((0:20 -5 - 2 )%% 30== 0 |(0:20 +2) %% 30 == 0,1,0)


