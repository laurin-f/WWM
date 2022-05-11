hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)


pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

Kammer <-
  readxl::read_xlsx(paste0(metapfad, "Kammermessungen/Kammer_Volumen.xlsx"),
                    sheet = "automatische Kammer")
Vol <- Kammer$Kammer_Volumen_cm3
Grundfl <- Kammer$Kammer_Grundfl_cm2

datelim_ls <- list()

datelim_ls[[1]] <- ymd_hm("2022.03.16 11:00","2022.03.16 18:00")

datelim_ls[[2]] <- ymd_hm("2022.03.21 01:00","2022.03.22 01:00")
datelim_ls[[3]] <- ymd_hm("2022.03.22 06:00","2022.03.24 24:00")
datelim_ls[[4]] <- ymd_hm("2022.03.23 06:00","2022.03.24 23:00")
datelim_ls[[5]] <- ymd_hm("2022.03.24 23:00","2022.03.29 15:00")
datelim_ls[[6]] <- ymd_hm("2022.03.29 15:00","2022.04.04 10:00")
datelim_ls[[7]] <- ymd_hm("2022.04.04 00:00","2022.04.07 10:00")
datelim_ls[[8]] <- ymd_hm("2022-05-10 09:05", "2022-05-10 11:00")

#for(i in 1:length(datelim)){

i <- 3
flux_ls <- chamber_arduino(datelim=datelim_ls[[i]],
                           gga_data = T,
                           gga="gga",
                           return_ls = T,
                           t_init=2,
                           plot="flux",
                           t_offset = 60,
                           t_min=4)




#flux_GGA <- chamber_arduino(datelim,gga_data = T,gas=c("CO2_GGA"),t_init = 0,plot="timeline",t_offset = 100)
#flux_CH4 <- chamber_arduino(datelim,gga_data = T,gas=c("CH4_GGA"),t_init = 2,plot="timeline",t_offset = 100)
flux <- flux_ls[[1]]
data <- flux_ls[[2]]

#######################
#plots
####################################

################
#timelines
#CO2

ggplot(data)+
  geom_line(aes(date,CO2_GGA,col="gga"))+
  geom_line(aes(date,CO2,col="dynament"))+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.2)+
  scale_fill_grey()+
  xlim(range(data$date))+
  ggsave(paste0(plotpfad_PPchamber,"CO2_timeline_",date(min(data$date)),".png"),width=7,height = 4)


#CH4
ggplot(data)+geom_line(aes(date,CH4,col=as.factor(messid),group=1))+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.2)+
  scale_fill_grey()+
  guides(col=F)+
  xlim(range(subset(data,!is.na(CH4))$date))+
  ggsave(paste0(plotpfad_PPchamber,"CH4_timeline_",date(min(data$date)),".png"),width=7,height = 4)

#########
#flux plots
#CO2
ggplot(flux)+
  geom_point(aes(date,CO2_mumol_per_s_m2,col="Dynament"))+
  geom_line(aes(date,CO2_mumol_per_s_m2,col="Dynament"))+
  #geom_smooth(aes(date,CO2_mumol_per_s_m2))+
  geom_point(aes(date,CO2_GGA_mumol_per_s_m2,col="GGA"))+
  geom_line(aes(date,CO2_GGA_mumol_per_s_m2,col="GGA"))+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.2)+
  scale_fill_grey()+
  xlim(range(data$date))+
  ggsave(paste0(plotpfad_PPchamber,"CO2_flux_",date(min(data$date)),".png"),width=7,height = 4)
  #scale_color_distiller(palette = "Spectral")

#}

#CH4
ggplot(flux)+geom_line(aes(date,CH4_mumol_per_s_m2))+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.2)+
  scale_fill_grey()+
  xlim(range(data$date))

#############
##facets

#CO2
ggplot(subset(data,!is.na(messid)))+
  geom_line(aes(zeit,CO2_GGA,col=as.factor(messid)))+
  geom_line(aes(zeit,CO2,col=as.factor(messid)))+
  facet_wrap(~ceiling(messid),scales="free")+
  guides(col=F)

#CH4
ggplot(subset(data,!is.na(messid)))+
  geom_line(aes(zeit,CH4,col=as.factor(messid)))+
  facet_wrap(~ceiling(messid),scales="free")+
  guides(col=F)


##############################
#plots für martin

datelim_pp <- ymd_hm("2022.03.21 12:28","2022.03.21 15:40")
datelim_no_pp <- ymd_hm("2022.03.21 10:20","2022.03.21 11:20")
datelim_3 <- ymd_hm("2022.03.21 11:30","2022.03.21 13:00")

flux_pp <- chamber_arduino(datelim_pp,gga_data = T,return_ls = T,t_init=0,plot="facets",t_offset = 60,t_min=4)
data_pp <- flux_pp[[2]]
flux_no_pp <- chamber_arduino(datelim_no_pp,gga_data = T,return_ls = T,t_init=0,plot="facets",t_offset = 60,t_min=4)
data_no_pp <- flux_no_pp[[2]]
flux_3 <- chamber_arduino(datelim_3,gga_data = T,return_ls = T,t_init=0,plot="timeline",t_offset = 60,t_min=4)
data_3 <- flux_3[[2]]


ggplot(subset(data_pp,!is.na(messid)))+
  geom_smooth(aes(zeit,CO2_tara,col="Dynament"),method="lm",se=F,linetype=2,lwd=0.7)+
  geom_line(aes(zeit,CO2_tara,col="Dynament"))+
  geom_line(aes(zeit,CO2_GGA_tara,col="GGA"))+
  facet_wrap(~paste("",messid))+
  labs(y="CO2 (ppm) tara",col="sensor")+
  ggsave(paste0(plotpfad_PPchamber,"chamber_sensor_vergleich_facets_PP.png"),width = 7,height=5)
ggplot(subset(data_pp,date > ymd_hm("2022-03-21 12:20")&date < ymd_hm("2022-03-21 13:20")))+
  geom_line(aes(date,CO2_GGA,col="GGA"))+
  #xlim(ymd_hm("2022-03-21 12:20","2022-03-21 14:20"))+
  labs(y="CO2 (ppm)",col="sensor",linetype="chamber",x="time")+
  ggsave(paste0(plotpfad_PPchamber,"chamber_gga_PP_fluctuations.png"),width = 7,height=5)

for(i in 4:nrow(pp_chamber)){
datelim_pp_start <- pp_chamber$Start[i]+c(-3600,3600)
flux_start <- chamber_arduino(datelim_pp_start,gga_data = T,return_ls = T,t_init=0,plot="timeline",t_offset = 60,t_min=4)
data_start <- flux_start[[2]]
if("CO2_GGA" %in% names(data_start)){
ggplot(subset(data_start))+
  geom_line(aes(date,CO2_GGA,col=factor(chamber,levels=c(0,1),labels=c("open","closed")),group=1))+
  geom_vline(xintercept = pp_chamber$Start[i],linetype=2)+
  annotate("text",x= pp_chamber$Start[i]+60,y=620,label="PP-chamber start +- 10 min",hjust=0)+
  labs(y="CO2 (ppm)",col="chamber",x="time",title = pp_chamber$Bemerkung[i])+
  ggsave(paste0(plotpfad_PPchamber,"chamber_gga_start",i,".png"),width = 9,height=5)
}
}

ggplot(subset(data_pp))+
  geom_line(aes(date,CO2,col="Dynament",linetype="open"))+
  geom_line(data=subset(data_pp,!is.na(messid)),aes(date,CO2,col="Dynament",linetype="closed",group=messid))+
  #geom_line(aes(date,RcppRoll::roll_mean(CO2,30,fill=NA),col="Dynament"))+
  geom_line(aes(date,CO2_GGA,col="GGA",linetype="open"))+
  geom_line(data=subset(data_pp,!is.na(messid)),aes(date,CO2_GGA,col="GGA",linetype="closed",group=messid))+
  xlim(ymd_hm("2022-03-21 12:20","2022-03-21 14:20"))+
  labs(y="CO2 (ppm)",col="sensor",linetype="chamber",x="time")+
  ggsave(paste0(plotpfad_PPchamber,"chamber_sensor_vergleich_PP.png"),width = 7,height=5)
ggplot(subset(data_no_pp))+
  geom_line(aes(date,CO2,col="Dynament",linetype="open"))+
  geom_line(data=subset(data_no_pp,!is.na(messid)),aes(date,CO2,col="Dynament",linetype="closed",group=messid))+
  #geom_line(aes(date,RcppRoll::roll_mean(CO2,30,fill=NA),col="Dynament"))+
  geom_line(aes(date,CO2_GGA,col="GGA",linetype="open"))+
  geom_line(data=subset(data_no_pp,!is.na(messid)),aes(date,CO2_GGA,col="GGA",linetype="closed",group=messid))+
  labs(y="CO2 (ppm)",col="sensor",linetype="chamber",x="time")+
  ggsave(paste0(plotpfad_PPchamber,"chamber_sensor_vergleich.png"),width = 7,height=5)



####
#scatterplot
ggplot(data)+geom_point(aes(CO2,CO2_GGA))


as.numeric(median(diff(data_GGA$date)))
data_GGA <- read_GGA(datelim =datelim,table.name = "gga")
data_GGA$date <- round_date(data_GGA$date,"5 secs")
names(data_GGA) <- c("date",paste0(names(data_GGA[-1]),"_GGA"))
data_agg <- data %>% 
  mutate(date = round_date(date, "5 secs")) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean,na.rm=T))

data_merge <- merge(data_agg,data_GGA,all=T)

ggplot(data_GGA)+
  geom_line(data=data_GGA,aes(date,CO2,col="GGA"))+
  geom_line(data=data,aes(date,CO2,col="dyn"))#+
  xlim(ymd_h("2022.03.16 12","2022.03.16 15"))

closingID <- which(data_merge == )
openingID <- which((minute(data_GGA$date)-7) %% 20 == 0 & second(data_GGA$date) < 10)

openingID <- openingID[-2]
data_GGA$zeit <- NA
data_GGA$messid <- NA
for (i in 1:length(openingID)) {
  #zeit in minuten nach closing
  data_GGA$zeit[closingID[i]:openingID[i]] <-
    difftime(data_GGA$date[closingID[i]:openingID[i]], data_GGA$date[closingID[i]], unit =
               "mins")
  #messid als durchlaufende Nummer f�r jede closing opening periode
  data_GGA$messid[closingID[i]:openingID[i]] <- i
}

T_GGA <- mean(data$T_C[data$date %in% data_GGA$date])
flux_GGA_ls <- calc_flux(na.omit(data_GGA),group="messid",Vol=Vol,Grundfl = Grundfl,T_deg=T_GGA)
flux_GGA <- flux_GGA_ls[[1]]
data_GGA <- flux_GGA_ls[[2]]
names(flux_GGA)

ggplot(data_GGA)+geom_line(aes(zeit,CO2_tara,col=as.factor(messid)))
ggplot(data_GGA)+
  geom_line(aes(date,CO2_GGA))+
  geom_point(data=subset(data_GGA,(minute(date)-1) %% 20 == 0 & second(date) < 10),aes(date,CO2_GGA,col="closing"))+
  geom_point(data=subset(data_GGA,(minute(date)-7) %% 20 == 0 & second(date) < 10 ),aes(date,CO2_GGA,col="opening"))+
  xlim(ymd_hm("2022.03.15 12:00","2022.03.15 14:30"))+
  ylim(c(450,600))


##################
#merge
names(data_GGA) <- c("date",paste0(names(data_GGA)[-1],"_GGA"))
data_merge <- merge(data,data_GGA,all = T)


ggplot(data_merge)+
  geom_point(data=subset(data_merge,!is.na(CO2)),aes(date,CO2+100,col="dyn"))+
  geom_line(data=subset(data_merge,!is.na(CO2_GGA)),aes(date,CO2_GGA,col="GGA"))+
  geom_line(data=subset(data_merge,!is.na(CO2_GGA)),aes(date,CO2_GGA,col="GGA"))+
  xlim(ymd_hm("2022.03.15 12:00","2022.03.15 14:30"))+
  ylim(c(450,550))
# Kammer <-
#   readxl::read_xlsx(paste0(metapfad, "Kammermessungen/Kammer_Volumen.xlsx"),
#                     sheet = "automatische Kammer")
# Vol <- Kammer$Kammer_Volumen_cm3
# Grundfl <- Kammer$Kammer_Grundfl_cm2
# 
# files <- list.files(inj_pfad,pattern = "_chamber",full.names = T)
# 
# data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F,fill=T)
# data <- do.call(rbind,data_ls)
# data$date <- ymd_hms(data$date)
# colnames(data) <- c("date","CO2","T_C","chamber")
# 
# data <- data[order(data$date),]
# 
# 
# 
# data$CO2 <- as.numeric(data$CO2)
# data$CO2[ data$CO2 < 300| data$CO2 > 9000] <- NA
# data <- data[-which(diff(data$CO2) < -200),]
# data$CO2 <- imputeTS::na_interpolation(data$CO2,maxgap = 10)
# data$CO2_roll <- RcppRoll::roll_mean(data$CO2,60,fill=NA)
# 
# data$T_C <- as.numeric(data$T_C)
# data$T_C[ data$T_C < -10| data$T_C > 60 |data$T_C == 0] <- NA
# data$T_C[which(abs(diff(data$T_C)) > 1)] <- NA
# 
# range(data$T_C,na.rm=T)
# range(data$date,na.rm=T)
# 
# data_sub <- subset(data,date >ymd_hm("2022.03.10 15:00") & date < ymd_hm("2022.03.11 11:00"))
# data_sub <- subset(data,date >ymd_hm("2022.03.14 10:00") & date < ymd_hm("2022.03.15 14:00"))
# ggplot((data_sub))+geom_line(aes(date,CO2,col=as.factor(chamber),group="test"))#+
ggplot(data_GGA)+
  geom_line(aes(date,CO2))+
  geom_line(data=subset(data,date %in% data_GGA$date & chamber==1),aes(date+60,CO2+100))

#  geom_line(data=subset(data,date %in% data_GGA$date),aes(date+60,CO2_roll+100))
# 
#   
#   
# closingID <- which(diff(data_sub$chamber) == 1)+1
# openingID <- which(diff(data_sub$chamber) == -1)+1
# 
# if(closingID[1] > openingID[1]){
#   closingID <- c(1,closingID)
# }
# 
# if(tail(closingID,1) > tail(openingID,1)){
#   openingID <- c(openingID,nrow(data_sub))
# }
# 
# data_sub$zeit <- NA
# data_sub$messid <- NA
# for (i in 1:length(openingID)) {
#   #zeit in minuten nach closing
#   data_sub$zeit[closingID[i]:openingID[i]] <-
#     difftime(data_sub$date[closingID[i]:openingID[i]], data_sub$date[closingID[i]], unit =
#                "mins")
#   #messid als durchlaufende Nummer f�r jede closing opening periode
#   data_sub$messid[closingID[i]:openingID[i]] <- i
# }
# 
# data_sub$messid[which(data_sub$zeit < 0)]
# 

# 
# 
# flux <- calc_flux(na.omit(data_sub),group="messid",Vol=Vol,Grundfl = Grundfl,T_deg = "T_C")[[1]]
# data_flux <- calc_flux(na.omit(data_sub),group="messid",Vol=Vol,Grundfl = Grundfl,T_deg = "T_C")[[2]]

#ggplot(data_flux)+geom_line(aes(zeit,CO2_tara,col=as.factor(messid)))+facet_wrap(~messid)
#data_dyn <- subset(data_flux,date %in% data_GGA$date)

GGA_split <- split_chamber(data_GGA,closing_lim = 5 
                           ,t_max = 7,t_init=0)
T_GGA <- mean(data_dyn$T_C)
flux_GGA <- calc_flux(GGA_split,group="messid",Vol=Vol,T_deg=T_GGA,Grundfl = Grundfl)[[1]]
GGA_data <- calc_flux(GGA_split,group="messid",Vol=Vol,T_deg=T_GGA,Grundfl = Grundfl)[[2]]
ggplot()+
  geom_line(data=data_dyn,aes(zeit,CO2+100))+
  geom_smooth(data=data_dyn,aes(zeit,CO2+100),method="glm")+
  geom_line(data=GGA_data,aes(zeit,CO2))+
  geom_smooth(data=GGA_data,aes(zeit,CO2),method="glm")
ggplot(data)+geom_line(aes(date,T_C))

chamber_flux()

