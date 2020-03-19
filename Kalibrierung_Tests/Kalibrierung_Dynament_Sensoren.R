#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Dynament/")

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

###################################################
#Daten laden

#Zeitrahmen festlegen
datelim<-c("2020-01-15 10:00:00","2020-01-21 10:00:00")

#db funktion
dynament_raw<-read_db("dynament.db","dynament_test",datelim,korrektur_dyn=F)
dynament_korr<-read_db("dynament.db","dynament_test",datelim,korrektur_dyn=T)
colnames(dynament_korr)<-str_replace(colnames(dynament_korr),"Dyn","korr")
GGA<-read_db("GGA.db","micro",datelim,"CO2,CO2dry")

#GGA auf 10s runden  damit Dataframes zusammenpassen
GGA$date<-round_date(GGA$date,"10s")

#GGA und dynament mergen
data<-merge(GGA,dynament_raw,all =F)
data<-merge(data,dynament_korr)

####################################
#Kalibrierpunkte identifizieren
#großer CO2 sprung oder Zeit differenz
wechsel<-
  which(abs(diff(data$CO2)) > 100 | as.numeric(difftime(data$date[-1],data$date[-nrow(data)],units = "mins")) > 10)
start<-c(1,wechsel+1)
ende<-c(wechsel,nrow(data))

#start und endpunkte zusammenfügen, punkte die start und endpunkt zugleich sind kommen weg
periode<-cbind(start,ende)[!start %in% ende,]
#von startpunkten jeweils die ersten 5 min wegschneiden
periode[,1]<-periode[,1]+5

#spalte kal_punkt an data anfügen und durchnummerieren
data$kal_punkt<-NA
for(i in 1:nrow(periode)){
  periode.i <- periode[i,1]:periode[i,2]
  data$kal_punkt[periode.i]<-i
}

#testplot ob die einteilung geklappt hat
ggplot(data)+geom_point(aes(date,CO2dry,col=as.factor(kal_punkt)))#+facet_wrap(~kal_punkt,scales="free_x")

#NA werte weglassen
data<-data[!is.na(data$kal_punkt),]

#datensatz ins long-format
#data_long<-reshape2::melt(data,id=c("date","CO2","CO2dry","kal_punkt"),variable.name="sensor",value.name="CO2_Dyn")

#ohne korr spalten
data_long <-
  reshape2::melt(data[,-grep("korr",colnames(data))],id=c("date","CO2","CO2dry","kal_punkt"),variable.name="sensor",value.name="CO2_Dyn")

#ohne dyn spalten
data_long_korr<-reshape2::melt(data[,-grep("Dyn",colnames(data))],id=c("date","CO2","CO2dry","kal_punkt"),variable.name="sensor_korr_db",value.name="CO2_korr_db")

#sensornummer als Spalte
data_long$sensor_nr<-str_extract(data_long$sensor,"\\d+$")
data_long_korr$sensor_nr<-str_extract(data_long_korr$sensor,"\\d+$")

#korr und dyn mergen
data_long <- merge(data_long,data_long_korr)

#vektor mit sensor nummern
sensor_nrs<-sort(unique(data_long$sensor_nr))

#spalte für korrigierte CO2 werte
data_long$CO2_korr<-NA
data_long$CO2_korr_kal<-NA

#nach kal_punkt und sensor aggregieren
kal_punkte<-aggregate(data_long[c("CO2dry","CO2","CO2_Dyn","CO2_korr")],list(kal_punkt=data_long$kal_punkt,sensor_nr=data_long$sensor_nr),mean)
kal_punkte$CO2_korr_5000<-NA

#listen für regression und Koeffizienten anlegen
fm<-vector("list",length(sensor_nrs))
fm_kal<-fm
fm_kal_5000<-fm
coeffs<-data.frame(sensor_nr=sensor_nrs,intercept=rep(NA,length(sensor_nrs)),slope=rep(NA,length(sensor_nrs)),stringsAsFactors = F)
coeffs_kal<-coeffs
coeffs_kal_5000<-coeffs

#Schleife um Regression für jeden sensor zu fitten
for(i in seq_along(sensor_nrs)){
  #regression fitten
  fm[[i]] <- glm(CO2dry~CO2_Dyn,data = subset(data_long,sensor_nr==sensor_nrs[i]))
  fm_kal[[i]] <- glm(CO2dry~CO2_Dyn,data = subset(kal_punkte,sensor_nr==sensor_nrs[i]))
  fm_kal_5000[[i]] <- glm(CO2dry~CO2_Dyn,data = subset(kal_punkte,sensor_nr==sensor_nrs[i] & CO2 <= 5000))
  #Werte vorhersagen
  korrs<-predict(fm[[i]],newdata = data.frame(CO2_Dyn=data_long$CO2_Dyn[data_long$sensor_nr==sensor_nrs[i]]))
  korrs_kal_long<-
    predict(fm_kal_5000[[i]],newdata = data.frame(CO2_Dyn=data_long$CO2_Dyn[data_long$sensor_nr==sensor_nrs[i]]))
  korrs_kal<-
    predict(fm_kal[[i]],newdata = data.frame(CO2_Dyn=kal_punkte$CO2_Dyn[kal_punkte$sensor_nr==sensor_nrs[i]]))
  korrs_kal_5000<-
    predict(fm_kal_5000[[i]],newdata = data.frame(CO2_Dyn=kal_punkte$CO2_Dyn[kal_punkte$sensor_nr==sensor_nrs[i]]))
  #korrigierte Werte in Datensatz schreiben
  data_long[data_long$sensor_nr==sensor_nrs[i],"CO2_korr"] <- korrs
  data_long[data_long$sensor_nr==sensor_nrs[i],"CO2_korr_kal"] <- korrs_kal_long
  kal_punkte[kal_punkte$sensor_nr==sensor_nrs[i],"CO2_korr"] <- korrs_kal
  kal_punkte[kal_punkte$sensor_nr==sensor_nrs[i],"CO2_korr_5000"] <- korrs_kal_5000
  #Koeffizieten in Datensatz schreiben
  coeffs[i,2:3]<-fm[[i]]$coefficients
  coeffs_kal[i,2:3]<-fm_kal[[i]]$coefficients
  coeffs_kal_5000[i,2:3]<-fm_kal_5000[[i]]$coefficients
}

#check ob die korrekturfaktoren in read_db richtig übernommen wurden
which(data_long$CO2_korr_db-data_long$CO2_korr_kal!=0)
#ja
##################################
#präzision der Sensoren
#mittlerer sd pro 10 min intervall jedes kal_punkts und sensors
sd_kal_punkte<-aggregate(data_long[c("CO2dry","CO2","CO2_Dyn")],list(kal_punkt=data_long$kal_punkt,sensor_nr=data_long$sensor_nr),FUN = function(x) mean(zoo::rollapply(x,10,sd)))
#kal_punkte aggregieren
sd_aggregate<-aggregate(sd_kal_punkte[c("CO2dry","CO2","CO2_Dyn")],list(sensor_nr=sd_kal_punkte$sensor_nr),mean,na.rm=T)


############################################
#Plots

plots <- F
if(plots == T){
#Plot übersicht der Kalibrierstufen
ggplot(data_long)+geom_line(aes(date,CO2_Dyn,col=sensor_nr))+geom_line(aes(date,CO2dry))+facet_wrap(~(kal_punkt),scales="free")

#Präzision
ggplot(sd_aggregate)+geom_point(aes(sensor_nr,CO2_Dyn))

#scatter-plot alle kalibrierpunkte
scatterplot<-ggplot(kal_punkte)+geom_abline(slope=1,intercept=0,linetype=2)+
  geom_abline(data=coeffs_kal_5000,aes(slope=slope,intercept=intercept),col=2)+
  geom_abline(data=coeffs_kal,aes(slope=slope,intercept=intercept),col=1)+
  geom_point(aes(CO2_Dyn,CO2dry,col=sensor_nr))+
  ggnewscale::new_scale_color()+
  geom_point(aes(CO2_korr_5000,CO2dry,shape="< 5000 ppm",col="< 5000 ppm"))+
  geom_point(aes(CO2_korr,CO2dry,shape="alle",col="alle"))+
  facet_wrap(~sensor_nr)+
  geom_text(data=coeffs_kal,aes(-Inf,Inf,label=paste(round(slope,2),"* x",ifelse(intercept > 0,"+","-"),
                                                     abs(round(intercept,1)))),hjust=-0.25,vjust=2 )+
  geom_text(data=coeffs_kal_5000,aes(Inf,-Inf,label=paste(round(slope,2),"* x",ifelse(intercept > 0,"+","-"),
                                                          abs(round(intercept,1)))),hjust=1.25,vjust=-2,col=2 )+
  scale_shape_manual("korrigiert",values=c(3,1))+scale_color_manual("korrigiert",values = c(2,1))

scatterplot
#scatter-plot Zoom auf niedrige kalibrierpunkte
scatterplot+
  lims(x=c(750,1500),y=c(750,1500))



#Plot residuenverteilung
ggplot(kal_punkte)+geom_point(aes(CO2dry,CO2_korr_5000-CO2dry,col="< 5000"),alpha=0.7)+geom_point(aes(CO2dry,CO2_korr-CO2dry,col="alle"),alpha=0.7)
#< 5000 passt im Messbereich besser



#scatterplot mit allen Punkten
ggplot(data_long)+geom_abline(slope=1,intercept=0)+
  geom_point(aes(CO2_Dyn,CO2,col=sensor_nr))+
  ggnewscale::new_scale_color()+
  geom_point(aes(CO2_korr,CO2,shape="alle",col="alle"))+
  geom_point(aes(CO2_korr_kal,CO2,shape="kal",col="kal"),size=0.5)+
  facet_wrap(~sensor_nr)+
  geom_text(data=coeffs,aes(-Inf,Inf,label=paste(round(slope,2),"* x",ifelse(intercept > 0,"+","-"),
                                                   abs(round(intercept,1)))),hjust=-0.25,vjust=2 )+
  scale_shape_manual("korrigiert",values=c(1,3))+scale_color_manual("korrigiert",values = c(1,2))
}

#########################################################
#Korrekturfaktoren exportieren
#namen der fm liste festelegen
names(fm_kal_5000)<-paste0("CO2_Dyn_",sensor_nrs)

#falls es schon korrekturfaktoren gibt diese
if(file.exists(paste0(metapfad,"korrektur_fm.RData"))){
  fm_kal_5000_neu<-fm_kal_5000
  load(paste0(metapath,"korrektur_fm.RData"),envir = .GlobalEnv)
  new.names <- !names(fm_kal_5000_neu)%in%names(fm_kal_5000)
  fm_kal_5000[names(fm_kal_5000_neu)[new.names]] <- fm_kal_5000_neu[new.names]
}
#korrektur fms speichern
save(fm_kal_5000,file=paste0(metapfad,"korrektur_fm.RData"))

sensor_liste<-read_excel(paste0(metapfad,"Sensor_liste.xlsx"))
sensor_liste[sensor_liste$Nummer %in% as.numeric(coeffs_kal_5000$sensor_nr),c("intercept","slope")] <- 
  coeffs_kal_5000[c("intercept","slope")]
sensor_liste[sensor_liste$Nummer %in% as.numeric(sd_aggregate$sensor_nr),"sd"] <- 
  sd_aggregate["CO2_Dyn"]
sensor_liste$HerrstellerID<-as.character(sensor_liste$HerrstellerID)
xlsx::write.xlsx(sensor_liste,paste0(metapfad,"Sensor_liste2.xlsx"),showNA = F)
