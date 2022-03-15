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
check.pac
Kammer <-
  readxl::read_xlsx(paste0(metapfad, "Kammermessungen/Kammer_Volumen.xlsx"),
                    sheet = "automatische Kammer")
Vol <- Kammer$Kammer_Volumen_cm3
Grundfl <- Kammer$Kammer_Grundfl_cm2

files <- list.files(inj_pfad,pattern = "_chamber",full.names = T)

data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F,fill=T)
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2","T_C","chamber")


data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 9000] <- NA
data <- data[-which(diff(data$CO2) < -200),]

data$T_C <- as.numeric(data$T_C)
data$T_C[ data$T_C < -10| data$T_C > 60 |data$T_C == 0] <- NA
data$T_C[which(abs(diff(data$T_C)) > 1)] <- NA

range(data$T_C,na.rm=T)
range(data$date,na.rm=T)

data_sub <- subset(data,date >ymd_hm("2022.03.10 15:00") & date < ymd_hm("2022.03.11 11:00"))
data_sub <- subset(data,date >ymd_hm("2022.03.14 10:00") & date < ymd_hm("2022.03.14 14:00"))
ggplot((data_sub))+geom_line(aes(date,CO2,col=as.factor(chamber),group="test"))#+

  
  
closingID <- which(diff(data_sub$chamber) == 1)+1
openingID <- which(diff(data_sub$chamber) == -1)+1

if(closingID[1] > openingID[1]){
  closingID <- c(1,closingID)
}

if(tail(closingID,1) > tail(openingID,1)){
  openingID <- c(openingID,nrow(data_sub))
}

data_sub$zeit <- NA
data_sub$messid <- NA
for (i in 1:length(openingID)) {
  #zeit in minuten nach closing
  data_sub$zeit[closingID[i]:openingID[i]] <-
    difftime(data_sub$date[closingID[i]:openingID[i]], data_sub$date[closingID[i]], unit =
               "mins")
  #messid als durchlaufende Nummer f�r jede closing opening periode
  data_sub$messid[closingID[i]:openingID[i]] <- i
}

ggplot(data_sub)+
  geom_line(aes(zeit,CO2,col=as.factor(messid)))


flux <- calc_flux(na.omit(data_sub),group="messid",Vol=Vol,Grundfl = Grundfl,T_deg = "T_C")[[1]]


ggplot(data)+geom_line(aes(date,T_C))

ggplot(flux)+geom_line(aes(date,CO2_mumol_per_s_m2))


test <- split_chamber(data_sub,closing_lim = 5,opening_lim=3,adj_openings = T)
calc_flux(test,group="messid,Vol=Vol")