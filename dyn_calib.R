
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Dynament/")

###################################################
#Daten laden

#Zeitrahmen festlegen
datelim<-c("2020-01-24 13:00:00","2020-01-28 10:00:00")


dyn_calib <- function(datelim,
                      sensor_ids) {

  #db funktion
  dynament_raw<-read_db("dynament.db","dynament_test",datelim,korrektur_dyn=F)

  GGA<-read_db("GGA.db","micro",datelim,"CO2,CO2dry")

  GGA$CO2[GGA$CO2 < 0] <- NA
  GGA$CO2dry[GGA$CO2dry < 0] <- NA
  #GGA auf 10s runden  damit Dataframes zusammenpassen
  GGA$date<-round_date(GGA$date,"10s")

  #GGA und dynament mergen
  data_dyn<-merge(dynament_raw,dynament_korr,all=T)


  ####################################
  #Kalibrierpunkte identifizieren
  #großer CO2 sprung oder Zeit differenz



  #funktion anwenden
  GGA_list <- wechsel_fun(data=GGA)
  sampler1_list <- wechsel_fun(data=data_sampler1,col="CO2_Dyn_14")
  sampler2_list <- wechsel_fun(data=data_sampler2,col="CO2_Dyn_02")

  GGA_wechsel <- GGA_list[[2]]
  sampler1_wechsel <- sampler1_list[[2]]
  sampler2_wechsel <- sampler2_list[[2]]

  t_diff_sampler1 <- median(as.numeric(difftime(GGA_wechsel$ende[-(1:2)], sampler1_wechsel$ende[-1],units = "secs")))
  t_diff_sampler2 <- median(as.numeric(difftime(GGA_wechsel$ende[-(1:2)], sampler2_wechsel$ende[-(1:2)],units = "secs")))

  shifted_sampler1 <- data_sampler1
  shifted_sampler2 <- sampler2_list[[1]]


  shifted_sampler1$date <- sampler1_list[[1]]$date + t_diff_sampler1
  shifted_sampler2$date <- sampler2_list[[1]]$date + t_diff_sampler2

  shifted_sampler1$date <- round_date(shifted_sampler1$date,"mins")
  shifted_sampler2$date <- round_date(shifted_sampler2$date,"mins")


  data_GGA_smpl1<-merge(GGA,shifted_sampler1,all=T)
  data <- merge(data_GGA_smpl1,shifted_sampler2,all=F)
  #data <- merge(data_smpl1u2,GGA,all=T)

  datelim1 <- ymd_hms("2020-04-24 13:00:00", "2020-04-24 16:00:00")
  datelim2 <- ymd_hm(c("2020.04.27 10:00","2020.04.27 16:00"))

  ggplot(data)+geom_point(aes(date,CO2_Dyn_02,col="dyn"))+geom_line(aes(date,CO2dry,col="GGA"))+xlim(datelim1)
  ggplot(data)+geom_point(aes(date,CO2_Dyn_02,col=as.factor(kal_punkt)))+geom_point(aes(date,CO2dry,col=as.factor(kal_punkt)))+xlim(datelim2)


  #NA werte weglassen
  data<-data[!is.na(data$kal_punkt),]

  #datensatz ins long-format
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


  ggplot(data_long)+geom_point(aes(date,CO2_Dyn,col=sensor_nr))+xlim(datelim1)
  ggplot(subset(data_long,sensor_nr %in% sampler1))+geom_point(aes(date,CO2_Dyn,col=sensor_nr))+xlim(datelim2)
  ggplot(subset(data_long,sensor_nr %in% sampler2))+geom_line(aes(date,CO2_Dyn,col=sensor_nr))+xlim(datelim2)

  #vektor mit sensor nummern
  sensor_nrs<-sort(unique(data_long$sensor_nr))

  #spalte für korrigierte CO2 werte
  data_long$CO2_korr<-NA

  #nach kal_punkt und sensor aggregieren
  kal_punkte<-aggregate(data_long[c("CO2dry","CO2","CO2_Dyn","CO2_korr","CO2_korr_db")],list(kal_punkt=data_long$kal_punkt,sensor_nr=data_long$sensor_nr),mean,na.rm=T)


  #listen für regression und Koeffizienten anlegen
  fm<-vector("list",length(sensor_nrs))


  coeffs<-data.frame(sensor_nr=sensor_nrs,intercept=rep(NA,length(sensor_nrs)),slope=rep(NA,length(sensor_nrs)),stringsAsFactors = F)

  #Schleife um Regression für jeden sensor zu fitten
  for(i in seq_along(sensor_nrs)){
    #regression fitten

    #oder doch die gesamte range??
    #fm[[i]] <- glm(CO2dry~CO2_Dyn,data = subset(kal_punkte,sensor_nr==sensor_nrs[i]))
    fm[[i]] <- glm(CO2dry~CO2_Dyn,data = subset(kal_punkte,sensor_nr==sensor_nrs[i] & CO2 <= 5000))
    #Werte vorhersagen
    korrs_long<-
      predict(fm[[i]],newdata = data.frame(CO2_Dyn=data_long$CO2_Dyn[data_long$sensor_nr==sensor_nrs[i]]))
    korrs<-
      predict(fm[[i]],newdata = data.frame(CO2_Dyn=kal_punkte$CO2_Dyn[kal_punkte$sensor_nr==sensor_nrs[i]]))

    #korrigierte Werte in Datensatz schreiben
    data_long[data_long$sensor_nr==sensor_nrs[i],"CO2_korr"] <- korrs_long
    kal_punkte[kal_punkte$sensor_nr==sensor_nrs[i],"CO2_korr"] <- korrs

    #Koeffizieten in Datensatz schreiben
    coeffs[i,2:3]<-fm[[i]]$coefficients
  }

  #check ob die korrekturfaktoren in read_db richtig übernommen wurden
  which(data_long$CO2_korr_db-data_long$CO2_korr!=0)
  #ja
  ##################################
  #präzision der Sensoren
  #mittlerer sd pro 10 min intervall jedes kal_punkts und sensors
  sd_punkte<-aggregate(data_long[c("CO2dry","CO2","CO2_Dyn")],list(kal_punkt=data_long$kal_punkt,sensor_nr=data_long$sensor_nr),FUN = function(x) mean(zoo::rollapply(x,10,sd)))
  #kal_punkte aggregieren
  sd_aggregate<-aggregate(sd_punkte[c("CO2dry","CO2","CO2_Dyn")],list(sensor_nr=sd_punkte$sensor_nr),mean,na.rm=T)


  ############################################
  #Plots
  plots <- F
  if(plots == T){
    #Plot übersicht der Kalibrierstufen
    ggplot(data_long)+geom_line(aes(date,CO2_Dyn,col=sensor_nr))+geom_line(aes(date,CO2dry))+facet_wrap(~(kal_punkt),scales="free")

    ggplot(data_long)+geom_line(aes(date,CO2_korr,col=sensor_nr))+geom_line(aes(date,CO2dry))+facet_wrap(~(kal_punkt),scales="free")

    ##############################
    #sampler 1 und 2 vergleich korr and not korr und dbkorr
    ggplot(subset(data_long,sensor_nr %in% sampler1))+
      geom_line(aes(date,CO2_korr,col="korr",linetype=sensor_nr))+
      geom_line(aes(date,CO2_Dyn,col="Dyn",linetype=sensor_nr))+
      geom_line(aes(date,CO2dry))+
      facet_wrap(~(kal_punkt),scales="free")
    ggplot(subset(data_long,sensor_nr %in% sampler1))+
      geom_line(aes(date,CO2_korr,col="korr",linetype=sensor_nr))+
      geom_line(aes(date,CO2_korr_db,col="db",linetype=sensor_nr))+
      geom_line(aes(date,CO2dry))+
      facet_wrap(~(kal_punkt),scales="free")

    ggplot(subset(data_long,sensor_nr %in% sampler2))+
      geom_line(aes(date,CO2_korr,col="korr",linetype=sensor_nr))+
      geom_line(aes(date,CO2_Dyn,col="Dyn",linetype=sensor_nr))+
      geom_line(aes(date,CO2dry))+
      facet_wrap(~(kal_punkt),scales="free")
    ggplot(subset(data_long,sensor_nr %in% sampler2))+
      geom_line(aes(date,CO2_korr,col="korr",linetype=sensor_nr))+
      geom_line(aes(date,CO2_korr_db,col="db",linetype=sensor_nr))+
      geom_line(aes(date,CO2dry))+
      facet_wrap(~(kal_punkt),scales="free")
    ##########################

    #Präzision
    ggplot(sd_aggregate)+geom_point(aes(sensor_nr,CO2_Dyn))

    #scatter-plot alle kalibrierpunkte
    scatterplot <- vector("list",2)
    for(i in 1:2){
      sampleri <- get(paste0("sampler",i))
      plt_data <- subset(kal_punkte,sensor_nr %in% sampleri)
      plt_data$sensor_nr
      scatterplot[[i]]<-ggplot(plt_data)+geom_abline(slope=1,intercept=0,linetype=2)+
        geom_abline(data=subset(coeffs,sensor_nr %in% sampleri),aes(slope=slope,intercept=intercept),col=1)+
        geom_point(aes(CO2_Dyn,CO2dry,col=sensor_nr))+
        ggnewscale::new_scale_color()+
        geom_point(aes(CO2_korr,CO2dry,col="korrigiert",shape="korrigiert"))+
        facet_wrap(~sensor_nr)+
        geom_text(data=subset(coeffs,sensor_nr %in% sampleri),aes(-Inf,Inf,label=paste(round(slope,2),"* x",ifelse(intercept > 0,"+","-"),
                                                                                       abs(round(intercept,1)))),hjust=-0.25,vjust=2 )+
        scale_shape_manual("",values=c(1))+scale_color_manual("",values = c(1))
    }


    scatterplot[[1]]
    scatterplot[[2]]
    #scatter-plot Zoom auf niedrige kalibrierpunkte
    scatterplot[[1]]+
      lims(x=c(650,5000),y=c(650,5000))


    #vergleich korr und korrdb
    ggplot(subset(kal_punkte,sensor_nr %in% sampler1))+
      geom_abline(slope=1,intercept=0)+
      geom_point(aes(CO2,CO2_korr,col="korr"))+
      geom_point(aes(CO2,CO2_korr_db,col="db"))+
      facet_wrap(~sensor_nr)

    ggplot(subset(kal_punkte,sensor_nr %in% sampler2))+
      geom_abline(slope=1,intercept=0)+
      geom_point(aes(CO2,CO2_korr,col="korr"))+
      geom_point(aes(CO2,CO2_korr_db,col="db"))+
      facet_wrap(~sensor_nr)


    #Plot residuenverteilung
    ggplot(kal_punkte)+geom_point(aes(CO2dry,CO2_korr-CO2dry))
    #< 5000 passt im Messbereich besser


  }
  #########################################################
  #Korrekturfaktoren exportieren
  #namen der fm liste festelegen
  names(fm)<-paste0("CO2_Dyn_",sensor_nrs)

  #falls es schon korrekturfaktoren gibt diese
  if(file.exists(paste0(metapfad,"korrektur_fm.RData"))){
    fm_neu<-fm
    load(paste0(metapfad,"korrektur_fm.RData"),envir = .GlobalEnv)
    fm[names(fm_neu)] <- fm_neu
  }

  #korrektur fms speichern
  #save(fm,file=paste0(metapfad,"korrektur_fm.RData"))

  sensor_liste<-read_excel(paste0(metapfad,"Sensor_liste2.xlsx"))
  sensor_liste <- sensor_liste[,-1]
  sensor_liste[sensor_liste$Nummer %in% as.numeric(coeffs$sensor_nr),c("intercept","slope")] <-
    coeffs[c("intercept","slope")]
  sensor_liste[sensor_liste$Nummer %in% as.numeric(sd_aggregate$sensor_nr),"sd"] <-
    sd_aggregate["CO2_Dyn"]
  sensor_liste$HerrstellerID<-as.character(sensor_liste$HerrstellerID)
  #xlsx::write.xlsx(sensor_liste,paste0(metapfad,"Sensor_liste2.xlsx"),showNA = F)



  ######
  #korrekturkorrektu
  load(paste0(metapfad,"korrektur_fm_alt.RData"),envir = .GlobalEnv)
  load(paste0(metapfad,"korrektur_fm.RData"),envir = .GlobalEnv)
  fm[["CO2_Dyn_18"]]$coefficients
  fm_kal_5000[["CO2_Dyn_18"]]$coefficients

  #unterschiede neue gegen alte fm
  coeff_alt <- t(sapply(fm_kal_5000,function(x) x$coefficients))
  coeff_neu <- t(sapply(fm,function(x) x$coefficients))

  plot(coeff_alt[rownames(coeff_alt),2],coeff_neu[rownames(coeff_alt),2])

  abline(0,1)
  plot(coeff_alt[rownames(coeff_alt),1],coeff_neu[rownames(coeff_alt),1])
  abline(0,1)

  fm[["CO2_Dyn_18"]] <- fm_kal_5000[["CO2_Dyn_18"]]
}
