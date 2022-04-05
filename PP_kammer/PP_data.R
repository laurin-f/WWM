#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","svMisc")
check.packages(packages)


pp_chamber <- readODS::read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)
datelim <- t(pp_chamber[8,c("Start","Ende")])
data <- read_PP(datelim = t(pp_chamber[8,c("Start","Ende")]),format="wide")
range(data_long$date)
#data <- data[data$date %in% round_date(data$date,"secs"),]
#fqz <- 10#1/s
fs <- 10#Hz
fpass <- c(0.1,0.01)
wpass <- fpass / (fs / 2)



bpfilter <- gsignal::butter(n=4,w=fpass,type="pass",plane="s")

  data[,paste0("Pbp_",i)] <- gsignal::filtfilt(bpfilter,data[,paste0("P_",i)])

  

datelim <- ymd_hms("2022-04-04 12:00:00","2022-04-04 12:03:00")
ggplot(sub_daterange(data,datelim))+
  geom_line(aes(date,Pbp_1,col="bp"))
  #geom_line(aes(date,P_1,col="P"))
  t_diff <- median(difftime(data$date[-1],data$date[-nrow(data)],"secs")) %>% as.numeric()
fqz <- 1 / round(t_diff,1)#1/s
for(i in 1:6){
  data[,paste0("PPC_",i)] <- fqz*RcppRoll::roll_mean(abs(c(NA,diff(data[,paste0("P_",i)]))),30*60*fqz,fill=NA)
}
data_long <- tidyr::pivot_longer(data,matches("PPC|P"),names_pattern = "(.+)_(\\d)",names_to = c(".value","id"))
ggplot(data_long)+
  geom_rect(data=pp_chamber[8,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.2)+
  geom_line(aes(date,PPC,col=id))
data_sub <- sub_daterange(data_long,ymd_hms("2022-04-04 12:00:00","2022-04-04 12:02:00"))
ggplot(subset(data_sub,id %in% 1:4 ))+
  geom_line(aes(date,P,col=id))
#ppc <- fs*RcppRoll::rollmean(abs(diff(p)))

pp_chamber
datelim <- c("2022-03-30 13:00:00","2022-03-30 14:40:00")
data <- read_PP(datelim = datelim)
range(data$date)
ggplot(subset(data,id!=6))+geom_line(aes(date,p_Pa,col=id))

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
