
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad_dyn <- paste0(hauptpfad,"Daten/Metadaten/Dynament/")
codepfad<- paste0(hauptpfad,"Programme/Eigenentwicklung/RData/")

###################################################
#Daten laden
datelim<-c("2021-02-03 09:00:00","2021-02-03 14:40:00")
datelim2<-c("2021-03-02 10:00:00","2021-03-02 16:20:00")

dyn_calib <- function(datelim, plot= T,kal_lim=6000){
  #Zeitrahmen festlegen


# dyn_calib <- function(datelim,
#                       sensor_ids) {

  #db funktion
  if(!is.list(datelim)){
    datelim <- list(datelim)
  }
    
  dynament_ls <- lapply(datelim, function(x) read_sampler("sampler3_raw","wide",x,korrektur_dyn=F)) 
  
  dynament <- do.call(rbind,dynament_ls)
  #dynament<-read_sampler("sampler3_raw","wide",datelim)
  
  
  GGA_ls<-lapply(datelim,function(x) read_db("GGA.db","micro",x,"CO2,CO2dry"))
  GGA <- do.call(rbind,GGA_ls)
  #GGA<-read_db("GGA.db","micro",datelim,"CO2,CO2dry")

  GGA$CO2[GGA$CO2 < 0] <- NA
  GGA$CO2dry[GGA$CO2dry < 0] <- NA
  #GGA auf 10s runden  damit Dataframes zusammenpassen
  GGA$date<-round_date(GGA$date,"10s")
  dynament$date<-round_date(dynament$date,"10s")

  #GGA und dynament mergen
  data_merge<-merge(dynament,GGA,all.x = T)


  wechsel_list <- wechsel_fun(data_merge,puffer_start = 25,ppm_lim = 30)
  data <- wechsel_list[[1]]
  colnames(data) <- stringr::str_replace(colnames(data),"^CO2_tiefe","CO2_Dyn_tiefe")
  #NA werte weglassen

  #datensatz ins long-format
  #ohne korr spalten
 
  
  data_long <-
    tidyr::pivot_longer(data,contains("_tiefe"),names_pattern = "(CO2_Dyn|temp)_tiefe(\\d)",names_to = c(".value","tiefe"))
    
  data_long_withNA <- data_long
  data_long<-data_long[!is.na(data_long$kal_punkt),]


  
  kal_punkte <- data_long %>% 
    dplyr::group_by(kal_punkt,tiefe) %>% 
    dplyr::summarise_at(vars(tidyselect::contains("CO2")),~mean(tail(.,10),na.rm=T))
  #nach kal_punkt und sensor aggregieren
  

  tiefen <- 1:7
  #listen für regression und Koeffizienten anlegen
  fm<-vector("list",length(tiefen))


  coeffs<-data.frame(tiefe=tiefen,intercept=rep(NA,length(tiefen)),slope=rep(NA,length(tiefen)),stringsAsFactors = F)

  
  #Schleife um Regression für jeden sensor zu fitten
  for(i in seq_along(tiefen)){
    #regression fitten
    fm[[i]] <- glm(CO2dry~CO2_Dyn,data = subset(kal_punkte,tiefe == tiefen[i] & CO2dry < kal_lim))
    #Werte vorhersagen
    korrs_long<-
      predict(fm[[i]],newdata = data.frame(CO2_Dyn=data_long$CO2_Dyn[data_long$tiefe==tiefen[i]]))
    korrs<-
      predict(fm[[i]],newdata = data.frame(CO2_Dyn=kal_punkte$CO2_Dyn[kal_punkte$tiefe==tiefen[i]]))

    #korrigierte Werte in Datensatz schreiben
    data_long[data_long$tiefe==tiefen[i],"CO2_korr"] <- korrs_long
    kal_punkte[kal_punkte$tiefe==tiefen[i],"CO2_korr"] <- korrs

    #Koeffizieten in Datensatz schreiben
    coeffs[i,2:3]<-fm[[i]]$coefficients
  }

  #check ob die korrekturfaktoren in read_db richtig übernommen wurden
  #which(data_long$CO2_korr_db-data_long$CO2_korr!=0)
  #ja
  ##################################
  #präzision der Sensoren
  #mittlerer sd pro 10 min intervall jedes kal_punkts und sensors
  sd_punkte<-aggregate(data_long[c("CO2dry","CO2","CO2_Dyn")],list(kal_punkt=data_long$kal_punkt,tiefe=data_long$tiefe),FUN = function(x) mean(zoo::rollapply(x,10,sd)))
  #kal_punkte aggregieren
  sd_aggregate<-aggregate(sd_punkte[c("CO2dry","CO2","CO2_Dyn")],list(tiefe=sd_punkte$tiefe),mean,na.rm=T)


  ############################################
  #Plots

    
    #dev.new()
    overview_plot <- ggplot(data_long_withNA)+
    geom_point(aes(date,CO2_Dyn,col=tiefe,shape=is.na(kal_punkt)))+
    scale_shape_manual(values=c(19,1))
    # ggplot(data_long)+
    #   geom_point(aes(CO2dry,CO2_Dyn,col=tiefe))+
    #   geom_smooth(aes(CO2dry,CO2_Dyn,col=tiefe))
    # 
    #Plot übersicht der Kalibrierstufen
    
    # ggplot(data_long)+geom_line(aes(date,CO2_Dyn,col=tiefe))+geom_line(aes(date,CO2dry))+facet_wrap(~(kal_punkt),scales="free")
    # 
    # 
    # ggplot(data_long)+geom_line(aes(date,CO2_korr,col=tiefe))+geom_line(aes(date,CO2dry))+facet_wrap(~(kal_punkt),scales="free")

    ##############################
    #sampler 1 und 2 vergleich korr and not korr und dbkorr
    ##########################
    
    
    date_CO2 <- ggplot(data_long)+
      geom_line(aes(date,CO2_Dyn,col=tiefe))+
      geom_point(aes(date,CO2_korr,shape="korr",col=tiefe))+
      scale_shape_manual(values=1)+
      facet_wrap(~(kal_punkt),scales="free")
    
    #Präzision
    precision <- ggplot(sd_aggregate)+geom_point(aes(tiefe,CO2_Dyn))

    #print(precision)
    #scatter-plot alle kalibrierpunkte
      scatterplot<-ggplot(kal_punkte)+
        geom_abline(slope=1,intercept=0,linetype=2)+
        geom_abline(data=coeffs,aes(slope=slope,intercept=intercept),col=1)+
        geom_point(aes(CO2_Dyn,CO2dry,col=tiefe))+
        ggnewscale::new_scale_color()+
        geom_point(aes(CO2_korr,CO2dry,col="korrigiert",shape="korrigiert"))+
        facet_wrap(~tiefe)+
        geom_text(data=coeffs,aes(-Inf,Inf,label=paste(round(slope,2),"* x",ifelse(intercept > 0,"+","-"),
                                                                                       abs(round(intercept,1)))),hjust=-0.25,vjust=2 )+
        scale_shape_manual("",values=c(1))+scale_color_manual("",values = c(1))



  if(plot == T){
    print(overview_plot)
  
    print(date_CO2)
  
    print(scatterplot)
  }#plots == T
  
  return(list(fm,kal_punkte,data_long,scatterplot))
  }
  



 out1 <- dyn_calib(datelim,plot=T)
 out2 <- dyn_calib(datelim2,plot=T)
out3 <- dyn_calib(list(datelim,datelim2),plot=T,kal_lim = 6000)
data_long <- out2[[3]]
# scatterplot1 <- out1[[4]]
# scatterplot2 <- out2[[4]]
# scatterplot3 <- out3[[4]]
# 
# kal_punkte1 <- out1[[2]]
# kal_punkte2 <- out2[[2]]
# kal_punkte3 <- out3[[2]]
# 
# scatterplot2+
#   geom_point(data=kal_punkte1,aes(CO2_Dyn,CO2dry))+
#   ylim(300,1000)+xlim(300,1000)
# scatterplot1+
#   geom_point(data=kal_punkte2,aes(CO2_Dyn,CO2dry))+
#   ylim(300,1000)+xlim(300,1000)
# 
# scatterplot3#+
#   ylim(300,1000)+xlim(300,1000)

fm <- out3[[1]]

#########################################################
  #Korrekturfaktoren exportieren
  #namen der fm liste festelegen
sampler <- "sampler3"
tiefen <- 1:7
  names(fm)<-paste0("CO2_tiefe",tiefen,"_",sampler)

  #falls es schon korrekturfaktoren gibt diese
  if(file.exists(paste0(metapfad_dyn,"korrektur_fm.RData"))){
    fm_neu<-fm
    load(paste0(metapfad_dyn,"korrektur_fm.RData"),envir = .GlobalEnv)
    fm[names(fm_neu)] <- fm_neu
  }
names(fm)
  #korrektur fms speichern
#  save(fm,file=paste0(metapfad_dyn,"korrektur_fm.RData"))

#save(fm,file=paste0(codepfad,"korrektur_fm_backup.RData"))

