#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
datapfad_bf<- paste0(hauptpfad,"Daten/Urdaten/Bodenfeuchte_FVA_Garten/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","svMisc")
check.packages(packages)
##########################################################
###########################################################


##################
#Metadata
bemerkungen <- readODS::read_ods(paste0(metapfad_PP,"PP_Kammer_Bemerkungen_hartheim.ods"))
bemerkungen$Start <- dmy_hm(bemerkungen$Start)
bemerkungen$Ende <- dmy_hm(bemerkungen$Ende)
datelims <- bemerkungen[grep("Sensor offset",bemerkungen$Bemerkung),1:2]


# #######
# #start
# datelim1 <- ymd_hm("2023.01.10 10:40", "2023.01.10 11:15")
# datelim1_2 <- ymd_hm("2023.01.10 10:55", "2023.01.10 11:05")
# 
# ######
# #stop
# datelim2 <- ymd_hm("2023.01.19 08:40", "2023.01.19 09:40")
# datelim2_2 <- ymd_hm("2023.01.19 09:00", "2023.01.19 09:20")

plot <- F

#data_wide <- read_PP(datelim = datelim,format = "wide")
corfac_ls <- list()

for(i in 1:nrow(datelims)){

data_long <- read_PP(datelim = c(datelims$Start[i]-600,datelims$Ende[i]+600),corfac = F,table.name = "PP_1min")

data_long <- data_long %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,5,fill=NA))

data <- subset(data_long, id != 6)

P_corfac_i <- data %>% 
  sub_daterange(c(datelims$Start[i],datelims$Ende[i])) %>% 
  group_by(id) %>% 
  summarise(date = mean(date),
            P_mean = mean(P))

corfac_ls[[i]] <- P_corfac_i
}
P_corfac <- do.call(rbind,corfac_ls)
P_corfac

#save(P_corfac,file=paste0(datapfad_PP_Kammer,"P_corfac_date.RData"))

###################################
#############################
#plots
#######################################

data <- read_PP(datelim = ymd_h("23.01.27 09","23.01.27 10"),corfac = T,table.name = "PP_1min")
data_2 <- read_PP(datelim = ymd_h("23.01.27 09","23.01.27 10"),corfac = "P_corfac_date",table.name = "PP_1min")

ggplot()+
  geom_line(data = subset(data,id!=6),aes(date,P,col=factor(id),linetype="1"))+
  geom_line(data = subset(data_2,id!=6),aes(date,P,col=factor(id),linetype="2"))
ggplot(P_corfac)+
  geom_point(aes(id,P_mean,col=factor(date)))
ggplot(data)+
  geom_line(aes(date,P_roll,col=factor(id)))
#load(file=paste0(datapfad_PP_Kammer,"P_corfac_2.RData"))

data$cal <- as.numeric(as.character(factor(data$id,levels = 1:5,labels = P_corfac_i$P_mean)))

data <- data %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         P_diff = abs(c(NA,diff(P_filter)))/!!dt,
         PPC5 = RcppRoll::roll_mean(P_diff,10,fill=NA),
         P_cal = P -cal,
         P_roll_cal = RcppRoll::roll_mean(P_cal,3,fill=NA),
         P_roll = RcppRoll::roll_mean(P,3,fill=NA))

ggplot(subset(data_2,id != 6))+
  geom_line(aes(date,P,col=factor(id),linetype="1"))+
  #geom_line(data = data,aes(date,P,col=factor(id),linetype="2"))+
  geom_line(data = data,aes(date,P_cal,col=factor(id),linetype="3"))

ggplot(data)+
  #  geom_line(aes(date, P_cal,col=id),alpha=0.3)+
  geom_line(aes(date, P_roll_cal,col=id))
ggplot(data)+
  geom_line(aes(date, P,col=id),alpha=0.3)+
  geom_line(aes(date, cal,col=id),alpha=0.3)+
  geom_line(aes(date, P_roll,col=id))
ggplot(data)+
  geom_line(aes(date, PPC5,col=id))

#range(data_PPC$date)
names(data_PPC)
PPC_plot <- ggplot(subset(data_PPC,id%in%c(1:4) & date %in% round_date(date,"1 mins")))+
  geom_line(aes(date,PPC5,col=factor(id)))

Prollplt <- 
  ggplot(subset(data_PPC,id%in%c(1:4) & date %in% round_date(date,"1 secs")))+
  #  geom_vline(xintercept = ymd_hm(paste("22.04.27",c("13:45","13:50","13:55","14:00","14:05"))))+
  #geom_line(aes(date,P,col=factor(id)),alpha=0.4)+
  geom_line(aes(date,P_roll,col=factor(id)))

Prollplt
datelim2 <- ymd_hms("2022-09-30 09:10:00 UTC", "2022-09-30 09:20:15:00 UTC")

egg::ggarrange(PPC_plot+xlim(datelim2),
               Prollplt+xlim(datelim2),ncol=1)

