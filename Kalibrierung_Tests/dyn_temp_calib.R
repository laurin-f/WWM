#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad_dyn<- paste0(hauptpfad,"Daten/Metadaten/Dynament/")
codepfad<- paste0(hauptpfad,"Programme/Eigenentwicklung/RData/")

###################################################
#Daten laden

#Zeitrahmen festlegen
datelim<-c("2021-02-03 10:00:00","2021-02-03 14:40:00")
sampler <- "sampler3"

# dyn_calib <- function(datelim,
#                       sensor_ids) {

#db funktion

data<-read_sampler("sampler3_raw","wide",datelim,korrektur_dyn=F)

data$temp_mean <- apply(data[,grep("temp",colnames(data))],1,mean,na.rm=F) #%>% 

data_long <-
  tidyr::pivot_longer(data,contains("_tiefe"),names_pattern = "(CO2|temp)_tiefe(\\d)",names_to = c(".value","tiefe"))

ggplot(data_long)+
  geom_line(aes(date,temp,col=tiefe))+
  geom_point(aes(date,temp_mean))
ggplot(data_long)+geom_point(aes(temp,temp_mean,col=tiefe))


tiefen <- 1:7
#listen für regression und Koeffizienten anlegen
offset<-vector("list",length(tiefen))

for (i in tiefen) {
  sub <- subset(data_long,tiefe == tiefen[i])
  offset[[i]] <- mean(sub$temp_mean - sub$temp,na.rm=T)
  #fm[[i]] <- glm(temp_mean~temp,data = subset(data_long,tiefe == tiefen[i]))
  #Werte vorhersagen
  korrs<-data_long$temp[data_long$tiefe==tiefen[i]] + offset[[i]]
    #predict(fm[[i]],newdata = data.frame(temp=data_long$temp[data_long$tiefe==tiefen[i]]))
  data_long[data_long$tiefe==tiefen[i],"temp_korr"] <- korrs
}


ggplot(data_long)+
  geom_point(aes(date,temp,col=tiefe))+
  geom_line(aes(date,temp_korr,col=tiefe))
names(offset)<-paste0("temp_tiefe",tiefen,"_",sampler)

#falls es schon korrekturfaktoren gibt diese
if(file.exists(paste0(metapfad,"korrektur_fm.RData"))){
  load(paste0(metapfad_dyn,"korrektur_fm.RData"),envir = .GlobalEnv)
  fm[names(offset)] <- offset
}
names(fm)

#korrektur fms speichern
#save(fm,file=paste0(metapfad_dyn,"korrektur_fm.RData"))
#save(fm,file=paste0(codepfad,"korrektur_fm_backup.RData"))


