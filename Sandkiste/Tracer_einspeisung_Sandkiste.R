#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
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
Pumpzeiten <- readxl::read_xlsx(paste0(metapfad,"Tracereinspeisung_Sandkiste.xlsx"))
Pumpzeiten$ende[is.na(Pumpzeiten$ende)] <- Pumpzeiten$start[which(is.na(Pumpzeiten$ende))+1]

Pumpzeiten$period <-Pumpzeiten$Versuch 
Pumpzeiten$PSt_Nr <- paste("PSt",Pumpzeiten$Pumpstufe,"Nr",Pumpzeiten$Versuch,sep="_")

#samplerdata
datelim <- range(c(Pumpzeiten$start, Pumpzeiten$ende),na.rm = T)
datelim[2] <- datelim[2]+3600*12

data <- read_sampler("sampler1",datelim = datelim, format = "long")


#Metadata Pumpstufen flux
flux <- read.csv(paste0(metapfad,"Pumpstufen_flux.txt"))

###############################
#postprocesing
#############################

#Pumpstufe und Versuch aus metadaten auf dataframe übetragen

#intervalle die am anfang und am ende der Pumpversuche verweorfen werden
stunden_bis_steadystate <- rep(10,nrow(Pumpzeiten))
stunden_cut_off <- rep(2,nrow(Pumpzeiten))

#Veruch 7 war kürzer
stunden_cut_off[17:18]<-0.5
stunden_bis_steadystate[18] <- 9

#Schleife um Zeiträume mit Pumpzeiten von Metadaten zu übernehmen
cols2data <- c("Pumpstufe","Versuch","respiration_simul","material")
data[,cols2data] <- NA

for(i in seq_along(Pumpzeiten$Pumpstufe)){
  Pumpzeiten_lim <- data$date > (Pumpzeiten$start[i] + stunden_bis_steadystate[i] * 60 * 60) & 
    data$date < (Pumpzeiten$ende[i] - stunden_cut_off[i] * 60 * 60)
  for(j in cols2data){
    data[Pumpzeiten_lim,j] <- Pumpzeiten[i,j]
  }
}


#Spalte PSt_Nr anhängen
data$PSt_Nr <- paste("PSt",data$Pumpstufe,"Nr",data$Versuch,sep="_")
data$PSt_Nr[is.na(data$Pumpstufe)]<-NA

#kurven glätten mit rollapply
data$CO2_rollapply <- NA
for(i in unique(data$tiefe)){
  tiefe.i <- data$tiefe == i
  data$CO2_rollapply[tiefe.i] <- zoo::rollapply(data$CO2[tiefe.i],width=10,mean,fill=NA)
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

#ggplot(data)+geom_point(aes(date,Fz,col=as.factor(Pumpstufe)))
###################
#Aggregate Data
data_agg <- aggregate(list(CO2=data$CO2,Fz=data$Fz),list(Pumpstufe=data$Pumpstufe,tiefe=data$tiefe,tiefenstufe = data$tiefenstufe,Versuch= data$Versuch,PSt_Nr = data$PSt_Nr,respi_sim = data$respiration_simul,material = data$material),mean)

#Spalte ID angängen
data_agg$ID <- paste0(data_agg$PSt_Nr,data_agg$respi_sim)

###################
#gradienten berechnen


fm_list <- lapply(unique(data_agg$ID),function(x) glm(CO2~tiefe,data=subset(data_agg, ID == x)))
dC_dz <- sapply(fm_list,"[[","coefficients")[2,]
names(dC_dz) <-  unique(data_agg$ID)
data_agg$dC_dz <- NA
for (i in names(dC_dz)){
  data_agg$dC_dz[data_agg$ID == i] <- abs(dC_dz[i])
}
##############################
#Fick's Law
#Fz = -DS * (dC / dz)
D0_CO2 <- D0_T_p(15)#cm/s

#Fläche
A <-15^2*pi#cm2


#Ficks law anwenden
#DS = -FZ * dz / dC
###################################

data_agg$DS <- data_agg$Fz / 60 / A / (data_agg$dC_dz / 10^6)#cm3/s/cm2 *cm = cm2/s
#DS <- 0.3 / 60 / A * dz / (dC / 10^6)
unique(cbind(data_agg$DS/D0_CO2,data_agg$ID))

######################################

# ggplot(subset(data_agg,PSt_Nr == "PSt_3_Nr_8" & respi_sim == "ja"))+
#   geom_point(aes(CO2,tiefe,col=PSt_Nr))+
#   geom_smooth(aes(CO2,tiefe,col=PSt_Nr),method="glm")
# ggplot(subset(data_agg,PSt_Nr == "PSt_3_Nr_8" & respi_sim == "ja"))+geom_path(aes(dC,tiefe,col=PSt_Nr))
# ggplot(subset(data_agg,PSt_Nr == "PSt_3_Nr_8" & respi_sim == "ja"))+geom_path(aes(DS,tiefe,col=PSt_Nr))

data_agg$DS_D0_glm <- data_agg$DS/D0_CO2
data_agg$tiefenmittel <- rowMeans(cbind(c(data_agg$tiefe[-nrow(data_agg)],NA),c(data_agg$tiefe[-1],NA)))
data_agg$tiefenmittel[data_agg$tiefe == -24.5] <- NA


############################
#künstliche respiration
respi <- subset(data_agg,Versuch %in% c(5,6,7,8))
respi <- respi[!colnames(respi)[] %in% c("PSt_Nr")]
respi_wide <- tidyr::pivot_wider(respi,names_from = c(Pumpstufe,respi_sim),values_from = which(!colnames(respi)[] %in% c("tiefe","tiefenstufe","Versuch","Pumpstufe","tiefenmittel","dz","respi_sim")))

respi_wide <- respi_wide[!colnames(respi_wide)[] %in% paste0(rep(c("Fz_0","DS_0","DS_D0_glm_0","material_3"),2),rep(c("_ja","_nein"),each=4))]
colnames(respi_wide) <- str_replace_all(colnames(respi_wide),c("0_ja"="respi","3_ja"="ges","3_nein"="tracer","material_respi" = "material"))


respi_wide$CO2_atm <- NA
for(i in unique(respi$Versuch)){
  id <- respi_wide$Versuch == i
  id2 <- id & respi_wide$tiefe == 0
  respi_wide$CO2_atm[id] <- mean(respi_wide$CO2_respi[id2], respi_wide$CO2_respi[id2])
}

respi_wide$CO2_tracer_calc <- respi_wide$CO2_ges - respi_wide$CO2_respi + respi_wide$CO2_atm

respi_long <- tidyr::pivot_longer(respi_wide,grep("^CO2",colnames(respi_wide)),names_to="source",values_to = "CO2")
respi_long$source <- str_remove(respi_long$source,"CO2_")
#############################
#Datensatz speichern
data_agg$CO2_mol_per_m3 <- ppm_to_mol(data_agg$CO2)

#save(data_agg,respi_wide,file=paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))




######################
# Plots              #
######################

#CO2 ~ Zeit mit Pumpstufen als rect
# ggplot(data)+
#   annotate("rect", xmin=Pumpzeiten$start,xmax=Pumpzeiten$ende,ymin=-Inf,ymax=Inf, alpha=Pumpzeiten$Pumpstufe/max(Pumpzeiten$Pumpstufe)*0.3, fill="red")+
#   geom_line(aes(date,CO2,col=as.factor(tiefenstufe)))+labs(col="tiefe")
plot <- F
if(plot == T){

#######
#CO2 ~ Zeit leave NAtime
lim1 <- ymd("2020.04.24")
lim2 <- ymd("2020.05.01")

plt <- leave_NAtime_plot(data=data[data$date < lim1 | data$date > lim2,],group="CO2",plot=T,adj_grob_size=F,breaks="1 day",date_labels= "%b %d")
plt_data <- leave_NAtime_plot(data=data[data$date < lim1 | data$date > lim2,],group="CO2",plot=F)


plt2 <- plt+
  geom_rect(data=Pumpzeiten,aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf, fill=as.character(Pumpstufe)))+
  scale_fill_manual(values = alpha("red",seq(0,0.3,len=length(unique(Pumpzeiten$Versuch)))))+
  labs(fill="Pumpstufe")+
  theme(strip.text.x = element_blank())

##########################
#plot period x
period_x <- 7
Pumpzeiten_x <- subset(Pumpzeiten, period == period_x)

ggplot(subset(plt_data,period %in% period_x))+
  geom_rect(data=Pumpzeiten_x,aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf, fill=as.factor(Pumpstufe)))+
  geom_vline(data=Pumpzeiten_x,aes(xintercept=start))+
  geom_point(aes(date,CO2,col=as.factor(tiefenstufe)),size=0.4)+labs(col="tiefe")+
  scale_fill_manual(values = alpha("red",sort(Pumpzeiten_x$Pumpstufe)/5*0.3))+
  geom_vline(data= flux, aes(xintercept =  date),col=2)

colnames(data)
#roll
ggplot(subset(plt_data,period %in% period_x))+
  geom_rect(data=subset(Pumpzeiten, period == period_x),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf, fill=as.character(Pumpstufe)))+
  geom_vline(data=subset(Pumpzeiten, period == period_x),aes(xintercept=start))+
  geom_point(aes(date,CO2_rollapply,col=PSt_Nr),size=0.4)+labs(col="")+
  scale_fill_manual(values = alpha("red",sort(Pumpzeiten_x$Pumpstufe)/5*0.3))
##################################
#geom_smooth(data=subset(plt_data,period %in% 4:5 & Pumpstufe == 3),aes(date,CO2,col=as.factor(tiefenstufe)),method="glm",linetype=2)+
  #ggsave(paste0(plotpfad,"Sandkiste_Versuch_4u5.pdf"),width=11,height=7)
  

#pdf(paste0(plotpfad,"Tracer_Versuche_sandkiste.pdf"),width=11,height = 5)
adj_grob_size(plt2,plt_data,"1 day",date_labels= "%b %d")
#dev.off()

#CO2_rollapply ~ Zeit 
leave_NAtime_plot(y="CO2_rollapply",col="PSt_Nr",data=data,group="CO2",geom="point",breaks="1 day",date_labels= "%b %d")
unique(data_agg[order(data_agg$Versuch),c("Versuch","ID","material")])
#CO2 ~ tiefe ohne glm
ggplot(subset(data,!is.na(Pumpstufe)))+
  geom_point(aes(CO2_rollapply,tiefe,col=PSt_Nr))+labs(col="Pumpstufe Versuch-Nr")


#CO2 ~ tiefe mit respi
ggplot(subset(respi_long,source %in% c("tracer","tracer_calc")))+
  geom_path(aes(CO2,tiefe,col=source,linetype=as.factor(Versuch)))+guides(linetype=F)+
  facet_wrap(~material)+
  scale_linetype_manual(values= rep(1,4))+
  ggsave(paste0(plotpfad,"tracer_tiefenprofil_sandkiste.pdf"),width=6,height=4)

#################
#plot für EGU pico
colnames(respi_wide)

ggplot()+
  geom_area(data= subset(respi_long,Versuch %in% c(5,6) & source %in% c("tracer_calc_raw","respi")),aes(CO2,tiefe,fill=source),position="stack",orientation = "y")+
  labs(shape="Versuch")+
  facet_wrap(~material)

col<-scales::hue_pal()
col(3)[1:2]
lims <- c("CO2resp","CO2tracer","CO2total")
ggplot(subset(respi_wide,Versuch%in%5:6))+
  geom_ribbon(aes(xmin=CO2_atm,xmax=CO2_respi,y=tiefe,fill="CO2resp"),alpha=0.2)+
  geom_point(aes(x=CO2_respi,y=tiefe,col="CO2resp"))+
  geom_ribbon(aes(xmin=CO2_respi,xmax=CO2_ges,y=tiefe,fill="CO2tracer"),alpha=0.3)+
  geom_point(aes(x=CO2_ges,y=tiefe,col="CO2total"))+
#  geom_line(aes(x=CO2_tracer_calc,y=tiefe,col="CO2tracer"))+
  labs(fill="", x="", y="depth [cm]",col="")+
  guides(col=guide_legend(override.aes = list(shape=c(19,NA,19),fill=c(NA,col(3)[2],NA))))+
  scale_fill_manual(limits=c(lims),values=col(3)[c(1,2,3)])+
  scale_color_manual(limits=c(lims),values=col(3)[c(1,2,3)])+
  xlim(c(min(respi_wide$CO2_atm),max(respi_wide$CO2_ges)))+facet_wrap(~material)

ggplot(respi_wide)+geom_point(aes(CO2_tracer,CO2_tracer_calc))
R2(respi_wide$CO2_tracer,respi_wide$CO2_tracer_calc)
RMSE(respi_wide$CO2_tracer,respi_wide$CO2_tracer_calc)

ref<-ggplot(subset(respi_wide,Versuch==5))+
  geom_ribbon(aes(xmin=CO2_atm,xmax=CO2_respi,y=tiefe,fill="reference"))+
  geom_ribbon(aes(xmin=CO2_respi,xmax=CO2_ges,y=tiefe,fill="injection"))+
  labs(fill="", x="", y="depth [cm]")+
  scale_fill_manual(values=col(3)[c(1,3)])+
  xlim(c(min(respi_wide$CO2_atm),max(respi_wide$CO2_ges)))

tracer<-ggplot(subset(respi_wide,Versuch==5))+
  geom_ribbon(aes(xmin=CO2_atm,xmax=CO2_tracer_calc,y=tiefe,fill="tracer"))+
  labs(fill="", x=expression("CO"[2]*" [ppm]"),y="depth [cm]")+
  scale_fill_manual(values=col(3)[2])+
  xlim(c(min(respi_wide$CO2_atm),max(respi_wide$CO2_ges)))


#png(paste0(plotpfad,"ref_tracer.png"),width = 1500,height = 2000,res=450)
egg::ggarrange(ref,tracer)
#dev.off()  



#CO2 ~ tiefe mit glm
ggplot(subset(data,!is.na(Pumpstufe)), aes(CO2_rollapply, tiefe, col=PSt_Nr))+
  geom_point()+
  geom_smooth(method="glm",formula= y ~ poly(x,2,raw=T))+
  labs(col="Pumpstufe Versuch-Nr")+labs(x="CO2 [ppm]",y="tiefe [cm]")+ggsave(paste0(plotpfad,"CO2_Tiefe_sandkiste.pdf"),width=8,height=5)


#CO2 ~ tiefe aggregierte Punkte 
ggplot(subset(data,Versuch==4),aes(CO2,tiefe,col=PSt_Nr))+
  geom_point()+
  geom_smooth(method = "glm")
ggplot(subset(data_agg,Versuch==4),aes(CO2,tiefe,col=PSt_Nr))+
  geom_point()+
  geom_smooth(method = "glm")

#gradient ~ tiefe
ggplot(data_agg)+geom_path(aes(gradient,tiefenmittel,col=PSt_Nr))
#gradient ~ Pumpstufe
ggplot(data_agg,aes(Pumpstufe,gradient,col=as.character(tiefenmittel),shape=as.character(Versuch)))+
  geom_point()#+
  

ggplot(data_agg)+geom_path(aes(DS_D0_glm,tiefe,col=PSt_Nr))
}

