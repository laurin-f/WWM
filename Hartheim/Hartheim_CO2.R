#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units")
check.packages(packages)


###################
#metadaten

#Pumpzeiten
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad_harth,"Tracereinspeisung_Hartheim.xlsx"))
Pumpzeiten$ende <- as_datetime(Pumpzeiten$ende)
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]

#tiefenoffset
tiefen_offset <- read.table(paste0(metapfad_harth,"sampler_tiefen_offset.txt"),header = T)

#Metadata Pumpstufen flux
flux <- read.csv(paste0(metapfad,"Tracereinspeisung/Pumpstufen_flux.txt"))



datelim <- c("2020.05.18 10:00:00")
smp1 <- read_sampler("sampler1",datelim = datelim, format = "long")
smp2 <- read_sampler("sampler2",datelim = datelim, format = "long")


#smp1_plt <- ggplot(smp1)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

#T_plt <- ggplot(smp1)+geom_line(aes(date,T_C))
#egg::ggarrange(smp1_plt,T_plt,heights=c(4,1))
#smp1_plt+geom_vline(xintercept = Pumpstufen$start)
#ggplot(smp2)+geom_line(aes(date,CO2,col=as.factor(tiefe)))

data <- merge(smp1,smp2,by=c("date","tiefe","tiefenstufe","variable"),all=T,suffixes = c("_inj","_ref"))

#Pumpstufe und Versuch aus metadaten auf dataframe übetragen


#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
stunden_bis_steadystate <- rep(10,nrow(Pumpzeiten))
stunden_cut_off <- rep(2,nrow(Pumpzeiten))


#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
cols2data <- c("Pumpstufe")
data[,cols2data] <- NA

for(i in seq_along(Pumpzeiten$Pumpstufe)){
  Pumpzeiten_lim <- data$date > (Pumpzeiten$start[i] + stunden_bis_steadystate[i] * 60 * 60) & if(!is.na(Pumpzeiten$ende[i])) {data$date < (Pumpzeiten$ende[i] - stunden_cut_off[i] * 60 * 60)}else{ T }
  for(j in cols2data){
    data[Pumpzeiten_lim,j] <- Pumpzeiten[i,j]
  }
}




#kurven glätten mit rollapply
for(j in c("_inj","_ref")){
 data[,paste0("CO2_roll",j)] <- NA
for(i in unique(data$tiefe)){
  tiefe.i <- data$tiefe == i
  data[tiefe.i,paste0("CO2_roll",j)] <- zoo::rollapply(data[tiefe.i,paste0("CO2",j)],width=50,mean,fill=NA)
}
}

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

###############
#tiefen Offset
data$tiefe_inj <- data$tiefe + tiefen_offset[1,2]
data$tiefe_ref <- data$tiefe + tiefen_offset[2,2] - 3.5

data_PSt0 <- subset(data, Pumpstufe == 0)


data$tiefe_inj <- data$tiefe +tiefen_offset$offset[1]
data$tiefe_ref <- data$tiefe +tiefen_offset$offset[2] - 3.5

# for(i in unique(data$date[!is.na(data$CO2_ref)])){
# data$CO2_ref_adj[data$date == i] <- approx(unique(data$tiefe_ref),data$CO2_ref[data$date == i],unique(data$tiefe_inj),rule=2)$y
# }


# ggplot(data)+
#   geom_line(aes(date,CO2_ref_adj,col=as.factor(tiefe_inj)))+
#   geom_line(aes(date,CO2_ref,col=as.factor(tiefe_ref)))
# ggplot(data)+
#   geom_line(aes(date,CO2_ref_adj-CO2_ref,col=as.factor(tiefe_inj)))
  
data_kal <- aggregate(data_PSt0[,grep("CO2",colnames(data_PSt0))] ,list(tiefe = data_PSt0$tiefe), mean, na.rm=T)
data_kal$offset <-  data_kal$CO2_inj - data_kal$CO2_ref
# ggplot(data_kal)+
#   geom_path(aes(CO2_ref,tiefe,col="ref"))+
#   geom_path(aes(CO2_inj,tiefe,col="inj"))+
#   geom_ribbon(aes(xmin=CO2_ref, xmax= CO2_ref + offset,y=tiefe,fill="offset"),alpha = 0.3)

data$offset <- as.numeric(as.character(factor(data$tiefe, levels=data_kal$tiefe,labels=data_kal$offset)))
data$CO2_tracer <- data$CO2_roll_inj - (data$CO2_roll_ref + data$offset)
data$tracer_pos <- data$CO2_tracer > 0
data$CO2_ref_offst <- ifelse(data$tracer_pos, data$CO2_ref + data$offset, data$CO2_roll_inj)
data$CO2_tracer[data$CO2_tracer < 0 | data$Pumpstufe == 0| is.na(data$Pumpstufe)] <- NA


data_wide <- tidyr::pivot_wider(data, id_cols = date, names_from = tiefenstufe,values_from = c(CO2_inj,CO2_ref,CO2_tracer,Fz),names_prefix = "tiefe")

data_wide <- data_wide[,-grep("CO2_(inj|tracer)_tiefe0|Fz_tiefe[1-7]",colnames(data_wide))]
colnames(data_wide) <- str_replace(colnames(data_wide),"Fz_tiefe0","injection_ml_per_min")

offset_plot <- ggplot(subset(data,Pumpstufe==0))+
  geom_line(aes(date,CO2_roll_ref+offset,col=as.factor(tiefe),linetype="ref + offset"),lwd=1)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"),lwd=1)+
  geom_ribbon(aes(x=date,ymin=CO2_ref + offset,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.3)+
  labs(title="keine Injektion",col="tiefe",fill="tiefe",linetype="sampler")


data_agg <- aggregate(data[grep("date|CO2|Fz|Pumpstufe",colnames(data))],list(hour=round_date(data$date,"hours"),tiefe=data$tiefe),mean)
data_agg$date <- with_tz(data_agg$date,"UTC")
data_agg <- subset(data_agg, Pumpstufe == 1.5)
save(data,data_agg,file=paste0(samplerpfad,"Hartheim_CO2.RData"))
###############################
#COMSOL input
A_inj <- set_units(1^2*pi,"mm^2")
injection_ml_min <- na.omit(unique(round(data_agg$Fz,2)))



injection_rates <- ppm_to_mol(injection_ml_min,unit_in = "cm^3/min",out_class = "units")
inj_mol_m2_s <- sort(set_units(injection_rates,"mol/s")/set_units(A_inj,"m^2"))

#CO2_atm <- ppm_to_mol(data_agg$CO2_ref[data_agg$tiefe == 0 & data_agg$Pumpstufe == 1.5],unit_in = "ppm",out_class = "units",T_C = na.omit(unique(data_agg$T_C[data_agg$Pumpstufe == 1.5])))

CO2_atm <- 0

min_DS <- c(2e-6,1.75e-6,1e-6,6.5e-7)
max_DS <- c(3e-6,2.75e-6,2e-6,1.1e-6)
#step <- c(1e-7,1e-7,1e-7)
step <- (max_DS - min_DS) / 10
schichten <- length(min_DS)
DS_1bis8 <- matrix(paste0("DS_",1:schichten," range(",min_DS,",",step,",",max_DS,")"),schichten,1)

pars_Hartheim <- rbind(paste0("injection_rate ",paste(inj_mol_m2_s,collapse = ", ")),paste0("CO2_atm ",paste(CO2_atm,collapse=", ")),DS_1bis8)



write.table(pars_Hartheim,
            file = paste0(metapfad_comsol,"parameter_Hartheim.txt"),
            row.names = F,col.names = F,quote=F)





#################################
#plots

inj <- ggplot(data)+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  annotate("text",x=Pumpzeiten$start[2],y=Inf,label="injection",vjust=1.9,hjust=-0.3)+labs(col="tiefe [cm]",x="",y=expression(CO[2]*" [ppm]"),title="injection sampler")

ref <- ggplot(data)+
  geom_line(aes(date,CO2_ref,col=as.factor(tiefe)))+
  geom_vline(xintercept = Pumpzeiten$start[-1])+
  guides(col=F)+labs(y=expression(CO[2]*" [ppm]"),title="reference sampler")
ref  
inj
p <- egg::ggarrange(inj,ref,ncol=1)

pdf(paste0(plotpfad,"hartheim_einspeisung1.pdf"),width=9,height=7)
p
dev.off()

  ggplot(data)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"))+
  geom_line(aes(date,CO2_roll_ref,col=as.factor(tiefe),linetype="ref"))+
  geom_ribbon(aes(date,ymin=CO2_inj,ymax=CO2_ref,fill=as.factor(tiefe)),alpha=0.3)+
  geom_vline(xintercept = Pumpzeiten$start)
  
  ggplot(data)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe),linetype="inj"),lwd=1.2)+
  geom_line(aes(date,CO2_roll_ref + offset,col=as.factor(tiefe),linetype="ref + offset"),lwd=0.8)+
  geom_ribbon(aes(date,ymax=CO2_inj,ymin=CO2_ref_offst,fill=as.factor(tiefe)),alpha=0.3)+
  geom_vline(xintercept = Pumpzeiten$start)
  

  data$monthday <- format(data$date,"%m.%d")
  data_month_day <- aggregate(data[,grep("CO2",colnames(data))],list(monthday = format(data$date,"%m.%d"),tiefe = data$tiefe),mean)
  inj_ref_plot <- ggplot(data)+geom_point(aes(CO2_inj,CO2_ref,col=as.factor(Pumpstufe)))+geom_abline(slope=1,intercept = 0)
#offset


datelim_1.5 <- ymd_h("2020.05.23 10")
  ref_profil <- ggplot(data_month_day)+geom_path(aes(CO2_ref,tiefe,col=monthday))
  inj_profil <- ggplot(data_month_day)+geom_path(aes(CO2_inj,tiefe,col=monthday))
egg::ggarrange(ref_profil,inj_profil,ncol=2)

ggplot(data)+
  geom_line(aes(date,CO2_tracer,col=as.factor(tiefe)))



ggplot(subset(data, Pumpstufe == 1.5 & date %in% round_date(date,"hours") & date < datelim_1.5))+ 
  geom_path(aes(CO2_tracer,tiefe,col=as.factor(date)))+
  geom_path(aes(CO2_ref + offset,tiefe,col=as.factor(date)))+
  geom_path(aes(CO2_inj,tiefe,col=as.factor(date)))
###########
#export data


paste("tiefe",rev(unique(data$tiefenstufe)),"=",rev(unique(data$tiefe)),"cm",collapse = ", ")

ggplot(data_wide)+geom_line(aes(date,injection_ml_per_min))

write.csv(data_wide,file=paste0(datapfad_harth,"co2_profil_",paste(format(range(data_wide$date),"%j"),collapse = "-"),".txt"),row.names = F)
strptime()
