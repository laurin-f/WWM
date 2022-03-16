hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)


Kammer <-
  readxl::read_xlsx(paste0(metapfad, "Kammermessungen/Kammer_Volumen.xlsx"),
                    sheet = "automatische Kammer")
Vol <- Kammer$Kammer_Volumen_cm3
Grundfl <- Kammer$Kammer_Grundfl_cm2

datelim <- ymd_hm("2022.03.14 10:00","2022.03.16 18:00")
datelim <- ymd_hm("2022.03.16 11:00","2022.03.16 18:00")
flux_ls <- chamber_arduino(datelim,return_ls = T)
flux <- flux_ls[[1]]
data <- flux_ls[[2]]
#flux_GGA_ls <- chamber_flux(datelim=datelim,GGA = "micro",chamber="automatische Kammer",return_data = T,closing_lim=4,t_max=7)


ggplot(data)+
  geom_line(aes(zeit,CO2_tara,col=as.factor(messid)))+
  facet_wrap(~ceiling(messid/10))+
  guides(col=F)
ggplot(data)+
  geom_line(aes(date,CO2,col=as.factor(messid)))+
#  xlim(ymd_h("2022.03.16 14","2022.03.16 18"))+
  guides(col=F)

ggplot(subset(flux,CO2_R2 > 0.5))+
  geom_point(aes(date,CO2_mumol_per_s_m2,col=CO2_R2))+
  geom_line(aes(date,CO2_mumol_per_s_m2))+
  geom_smooth(aes(date,CO2_mumol_per_s_m2))+
#  geom_line(data=subset(flux_GGA,messid %in% 2:11),aes(date,CO2_mumol_per_s_m2),col=2)+
  scale_color_viridis_c()


data_GGA <- read_GGA(datelim =datelim,table.name = "gga")
ggplot(data_GGA)+
  geom_line(data=data_GGA,aes(date,CO2,col="GGA"))+
  geom_line(data=data,aes(date,CO2,col="dyn"))+
  xlim(ymd_h("2022.03.16 12","2022.03.16 15"))

closingID <- which((minute(data_GGA$date)-1) %% 20 == 0 & second(data_GGA$date) < 10)
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

