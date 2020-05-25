#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

metapfad<-paste0(hauptpfad,"Daten/Metadaten/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

####################################
#load data
####################################
#Metadata xlsx
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad,"Vorgarten/Tracereinspeisung_Vorgarten.xlsx"))

Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]

#samplerdata
datelim <- range(c(Pumpzeiten$start, Pumpzeiten$ende),na.rm = T)


data_tracer <- read_sampler("sampler1",datelim = datelim, format = "long")
data_ref <- read_sampler("sampler2",datelim = datelim, format = "long")


#Metadata Pumpstufen flux
flux <- read.csv(paste0(metapfad,"Tracereinspeisung/Pumpstufen_flux.txt"))

#tiefenoffset
tiefen_offset <- read.table(paste0(metapfad,"Vorgarten/sampler_tiefen_offset.txt"),header = T)


###############################
#postprocesing
#############################

#Pumpstufe aus metadaten auf dataframe übetragen
data_tracer$Pumpstufe <- NA

#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
stunden_bis_steadystate <- 9
stunden_cut_off <- 0.5
  
#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
cols2data <- c("Pumpstufe")

for(i in seq_along(Pumpzeiten$Pumpstufe)){
  Pumpzeiten_lim <- data_tracer$date > (Pumpzeiten$start[i] + stunden_bis_steadystate * 60 * 60) & 
    data_tracer$date < (Pumpzeiten$ende[i] - stunden_cut_off * 60 * 60)
  for(j in cols2data){
    data_tracer[Pumpzeiten_lim,j] <- Pumpzeiten[i,j]
  }
}


#kurven glätten mit rollapply
data_tracer$CO2_roll <- NA
for(i in unique(data_tracer$tiefe)){
  tiefe.i <- data_tracer$tiefe == i
  data_tracer$CO2_roll[tiefe.i] <- zoo::rollapply(data_tracer$CO2[tiefe.i],width=10,mean,fill=NA)
}
data_ref$CO2_roll <- NA
for(i in unique(data_ref$tiefe)){
  tiefe.i <- data_ref$tiefe == i
  data_ref$CO2_roll[tiefe.i] <- zoo::rollapply(data_ref$CO2[tiefe.i],width=10,mean,fill=NA)
}

data <- merge(data_tracer,data_ref,by=c("date","tiefe","tiefenstufe","variable"),suffixes = c("","_ref"),all=T)
############################
#Fz mit zeitlichem drift bestimmen
flux$date <- ymd_hms(flux$date)
data$Fz <- NA
for(i in na.omit(unique(data$Pumpstufe))){
  flux_i <- subset(flux, Pumpstufe == i)
  id <- which(data$Pumpstufe == i)
  if(nrow(flux_i) > 1){
    data[id,"Fz"] <- approx(x=(flux_i$date),y=flux_i$tracer_ml_per_min,xout=(data$date[id]),rule=2)$y
  }else{
    data[id,"Fz"] <- flux_i$tracer_ml_per_min
  }
}

#ggplot(data)+geom_point(aes(date,Fz,col=as.factor(Pumpstufe)))
###################
#Aggregate Data
data_agg <- aggregate(list(CO2_inj=data$CO2,CO2_ref = data$CO2_ref,Fz=data$Fz),list(Pumpstufe=data$Pumpstufe,tiefe=data$tiefe,tiefenstufe = data$tiefenstufe),mean)
unique(data$tiefe)

data_agg$tiefe_inj <- data_agg$tiefe + tiefen_offset[1,2]
data_agg$tiefe_ref <- data_agg$tiefe + tiefen_offset[2,2] - 3.5

profile_list <- vector("list",4)

data_inter <- data.frame(tiefe = unique(sort(c(data_agg$tiefe_ref,data_agg$tiefe_inj))))
for(i in unique(data_agg$Pumpstufe)){
  sub_i <- subset(data_agg, Pumpstufe == i)
  for(j in c("ref", "inj")){
    data_inter[paste("CO2",j,i,sep="_")] <- approx(sub_i[,paste0("tiefe_",j)],sub_i[,paste0("CO2_",j)],xout=data_inter$tiefe)$y
  }
}
  profile_ref_0 <- approx(data_agg$tiefe_ref[data_agg$Pumpstufe == 0],data_agg$CO2_ref[data_agg$Pumpstufe == 0],xout=c(data_agg$tiefe_ref[data_agg$Pumpstufe == 0],data_agg$tiefe_inj[data_agg$Pumpstufe == 0]))
profile_inj_0 <- approx(data_agg$tiefe_inj[data_agg$Pumpstufe == 0],data_agg$CO2[data_agg$Pumpstufe == 0],xout=c(data_agg$tiefe_ref[data_agg$Pumpstufe == 0],data_agg$tiefe_inj[data_agg$Pumpstufe == 0]))

data_inter <- data.frame(CO2_ref= approx(data_agg$tiefe_ref[data_agg$Pumpstufe == 0],data_agg$CO2_ref[data_agg$Pumpstufe == 0]))
###################
#gradienten berechnen


fm <- glm(CO2~tiefe,data=data_agg)
dC_dz <- fm$coefficients[2]
names(dC_dz) <-  unique(data_agg$ID)

##############################
#Fick's Law
#Fz = -DS * (dC / dz)
D0_CO2 <- D0_T_p(15)#cm/s

#Fläche
#A <-15^2*pi#cm2


#Ficks law anwenden
#DS = -FZ * dz / dC
###################################

#data_agg$DS <- data_agg$Fz / 60 / A / (data_agg$dC_dz / 10^6)
#DS <- 0.3 / 60 / A * dz / (dC / 10^6)
#unique(cbind(data_agg$DS/D0_CO2,data_agg$ID))

######################################

# ggplot(subset(data_agg,PSt_Nr == "PSt_3_Nr_8" & respi_sim == "ja"))+
#   geom_point(aes(CO2,tiefe,col=PSt_Nr))+
#   geom_smooth(aes(CO2,tiefe,col=PSt_Nr),method="glm")
# ggplot(subset(data_agg,PSt_Nr == "PSt_3_Nr_8" & respi_sim == "ja"))+geom_path(aes(dC,tiefe,col=PSt_Nr))
# ggplot(subset(data_agg,PSt_Nr == "PSt_3_Nr_8" & respi_sim == "ja"))+geom_path(aes(DS,tiefe,col=PSt_Nr))

#data_agg$DS_D0_CO2 <- data_agg$DS/D0_CO2


############################
#künstliche respiration

#############################
#Datensatz speichern
#data_agg$CO2_mol_per_m3 <- ppm_to_mol(data_agg$CO2)
#save(data_agg,file=paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))
##################
#plots

#CO2 ~ Zeit mit Pumpstufen als rect
# ggplot(data)+
#   annotate("rect", xmin=Pumpzeiten$start,xmax=Pumpzeiten$ende,ymin=-Inf,ymax=Inf, alpha=Pumpzeiten$Pumpstufe/max(Pumpzeiten$Pumpstufe)*0.3, fill="red")+
#   geom_line(aes(date,CO2,col=as.factor(tiefenstufe)))+labs(col="tiefe")

#if(plot == T){
  
  #######
  #CO2 ~ Zeit 
  

  ##########################
  #plot 
  
  smp1 <- ggplot(data)+
    geom_rect(data=Pumpzeiten,aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf, fill=as.factor(Pumpstufe)))+
    geom_line(aes(date, CO2,col=as.factor(tiefe)))+
    scale_fill_manual(values = alpha("red",(Pumpzeiten$Pumpstufe)/1.5*0.3))
  
  smp2 <- ggplot(data)+
    geom_line(aes(date, CO2_ref,col=as.factor(tiefe)))+
    scale_fill_manual(values = alpha("red",(Pumpzeiten$Pumpstufe)/1.5*0.3))
  T_plt <- ggplot(data)+geom_point(aes(date,T_C))
  egg::ggarrange(smp1,smp2)
  
  ggplot(data)+
    geom_vline(data=Pumpzeiten[-1,],aes(xintercept=start))+
    geom_line(aes(date,CO2_roll,linetype="injection_sampler",col=as.factor(tiefenstufe)),lwd=1)+
    geom_line(aes(date,CO2_roll_ref,linetype="ref_sampler",col=as.factor(tiefenstufe)),lwd=1)+
  geom_ribbon(aes(x=date,ymin=CO2_ref,ymax=CO2,fill=as.factor(tiefenstufe)),alpha=0.5)+
    annotate("text",x=mean(c(Pumpzeiten$start[2],Pumpzeiten$ende[2])),y=8200,label="injection")+labs(col="tiefe [cm]",fill="tiefe [cm]",linetype="",y=expression(CO[2]*" [ppm]"))+ggsave(paste0(plotpfad,"vorgarten_injection.pdf"),width=10,height=7)
  
  ggplot(data)+
    geom_rect(data=Pumpzeiten,aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf, fill=as.factor(Pumpstufe)))+
    geom_vline(data=Pumpzeiten,aes(xintercept=start))+
    geom_point(aes(date, CO2_roll,col=as.factor(Pumpstufe)))+
    geom_point(aes(date, CO2_roll_ref,col="ref"))+
    scale_fill_manual(values = alpha("red",(Pumpzeiten$Pumpstufe)/1.5*0.3))
  
  ggplot(subset(data,!is.na(Pumpstufe)))+geom_point(aes(CO2_roll,CO2_roll_ref,col=as.factor(Pumpstufe)))+geom_abline(slope=1,intercept=0)
  #CO2 ~ tiefe ohne glm
  ggplot(subset(data,!is.na(Pumpstufe)))+
    geom_point(aes(CO2_roll,tiefe,col=as.factor(Pumpstufe)))+labs(col="Pumpstufe Versuch-Nr")
  
  
  data_agg$offset <- NA
  data_agg$tracer <- NA
  data_agg$offset[data_agg$Pumpstufe == 0] <- data_agg$CO2_inj[data_agg$Pumpstufe == 0] - data_agg$CO2_ref[data_agg$Pumpstufe == 0]

  data_agg$tracer[data_agg$Pumpstufe == 0] <- data_agg$CO2_inj[data_agg$Pumpstufe == 1.5] - (data_agg$CO2_ref[data_agg$Pumpstufe == 1.5] + data_agg$offset[data_agg$Pumpstufe == 0])


  data_agg$tracer[data_agg$Pumpstufe == 1.5] <- data_agg$tracer[data_agg$Pumpstufe == 0]
  data_agg$offset[data_agg$Pumpstufe == 1.5] <- data_agg$offset[data_agg$Pumpstufe == 0]
  
  data_inter$offset <- data_inter$CO2_inj_0 - data_inter$CO2_ref_0
  data_inter$tracer <- data_inter$CO2_inj_1.5 - (data_inter$CO2_ref_1.5 +data_inter$offset)
  
  col<-scales::hue_pal()
  
  offset_plt <- ggplot(data_inter)+
    geom_ribbon(aes(xmin=CO2_ref_0,xmax=CO2_inj_0, y=tiefe,fill="offset"),alpha=0.5)+
    geom_path(aes(CO2_ref_0,tiefe,col="ref"))+
    geom_path(aes(CO2_inj_0,tiefe,col="inj"))+
    labs(x=expression(CO[2]*" [ppm]"),col="",title="Offset",fill="")
  
  injection_plt <- ggplot(data_inter)+
    geom_ribbon(aes(y=tiefe,xmin=CO2_ref_1.5 + offset,xmax=CO2_inj_1.5,fill="tracer"),alpha=0.5)+
    geom_path(aes(CO2_ref_1.5,tiefe,col="ref"))+
    geom_path(aes(CO2_ref_1.5 + offset,tiefe,col="ref + offset"))+
    geom_path(aes(CO2_inj_1.5,tiefe,col="inj"))+
    labs(x=expression(CO[2]*" [ppm]"),col="",title = "Injection",fill="")+
    scale_color_manual(values = col(4)[c(1,3,4)])+
    scale_fill_manual(values = col(4)[2])
  
    ggplot(data_agg)+
    geom_path(aes(CO2_ref,tiefe_ref,col="ref"))+
    geom_path(aes(CO2_inj,tiefe_inj,col="inj"))+facet_wrap(~Pumpstufe)
  
  offset_plt_not_adj <- ggplot(subset(data_agg,Pumpstufe == 0))+
    geom_ribbon(aes(y=tiefe,xmin=CO2_ref,xmax=CO2_inj,fill="offset"),alpha=0.5)+
    geom_line(aes(CO2_inj,tiefe,col="injection sampler"))+
    geom_path(aes(CO2_ref,tiefe,col="ref sampler"))+labs(x=expression(CO[2]*" [ppm]"),col="",title="Offset")
  
  injection_plt_not_adj <- ggplot(subset(data_agg,Pumpstufe == 1.5))+
    geom_ribbon(aes(y=tiefe,xmin=CO2_ref + offset,xmax=CO2_inj,fill="tracer"),alpha=0.5)+
    geom_line(aes(CO2_inj,tiefe,col="injection sampler"))+
    geom_path(aes(CO2_ref,tiefe,col="ref sampler"))+
    geom_path(aes(CO2_ref + offset,tiefe,col="ref sampler + offset"))+
    labs(x=expression(CO[2]*" [ppm]"),col="",title = "Injection")+
    scale_color_manual(values = col(4)[c(1,3,4)])+
    scale_fill_manual(values = col(4)[2])
  
  p <- egg::ggarrange(offset_plt,injection_plt,ncol=2)
  p2 <- egg::ggarrange(offset_plt_not_adj,injection_plt_not_adj,ncol=2)
  p
  pdf(paste0(plotpfad,"offset_injection_depth_adj.pdf"),width=10,height=4)
  p
  dev.off()
  
  pdf(paste0(plotpfad,"offset_injection.pdf"),width=10,height=4)
  p2
  dev.off()
  
  
  ggplot()+
    geom_path(data=data_inter,aes(tracer,tiefe,col="depth adj"))+
    geom_path(data=data_agg,aes(tracer,tiefe,col="no depth adj"))
  plot(tracer)
  plot(offset)
