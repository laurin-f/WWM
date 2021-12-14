


#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_ms <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Methodenpaper/")
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
aufbereitete_ds<-paste0(hauptpfad,"Daten/aufbereiteteDaten/DS_Labor/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr","png","cowplot","magick","rmatio","PerformanceAnalytics")
check.packages(packages)
theme_set(theme_classic())

#daten laden
load(file=paste0(klimapfad,"klima_data.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(aufbereitete_ds,"Labor_Vergleich.RData"))
#aus "plots_COMSOL_out_Hartheim_sweep.R"
#load(paste0(comsolpfad,"plotdata_Methodenpaper.RData"))
#load(paste0(comsolpfad,"plotdata_Methodenpaper_drift.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper_roll.RData"))
load(paste0(comsolpfad,"sandkiste_sweep_data_sub.RData"))
load(paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))


load(paste0(datapfad_harth,"DS_long_list_withPos1minmax.RData"))

#injektion 1 weg
data$Position[data$date > Pumpzeiten$start[10] & data$date < Pumpzeiten$start[12]] <- NA


pos8_date <- min(data$date[which(data$Position ==8 & data$Pumpstufe != 0)])
range1 <- range(data$date[data$Position ==1],na.rm = T)
range2 <- range(data$date[data$Position ==7],na.rm = T)
range3 <- range(data$date[data$Position ==8],na.rm = T)
range2u3 <- range(data$date[data$Position %in% 7:8],na.rm = T)

#F_df[which(F_df$date>pos8_date & F_df$date < (pos8_date + 20*3600)),c("DSD0_1","Fz","Fz_roll")] <- NA
#DS_long_roll$DSD0_roll[which(DS_long_roll$date>pos8_date & DS_long_roll$date < (pos8_date + 20*3600) & DS_long_roll$id == 1)] <- NA


data %>% group_by(Position,Pumpstufe) %>% summarise(injectionrate=mean(Fz))#ml per min

h_steady <- 0

DS_long_roll$range2 <- factor(DS_long_roll$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))
soil_agg_plot$range2 <- factor(soil_agg_plot$range,levels=c("0 to -10","-10 to -20","> -20"),labels = c("0-10","10-20","below 20"))
pos8_date <- min(data$date[which(data$Position ==8 & data$Pumpstufe != 0)])
#Versuch2_date <- ymd_h("2020.07.10 00")
#DS_long_roll$Versuch <- ifelse(DS_long_roll$date < pos8_date,ifelse(DS_long_roll$date < Versuch2_date,"1","2"),"3")
# ds_sub <- subset(DS_long_roll)

DS_long_roll[(nrow(DS_long_roll)+1):(nrow(DS_long_roll)+10),] <- NA
DS_long_roll[(nrow(DS_long_roll)-9):(nrow(DS_long_roll)),"date"] <- min(DS_long_roll$date,na.rm = T)-(1:10*3600)
DS_long_roll[(nrow(DS_long_roll)-9):(nrow(DS_long_roll)),"Versuch"] <- "2"
#DS_long_roll[(nrow(DS_long_roll)-19):(nrow(DS_long_roll)),"DSD0_roll"] <- 1
DS_long_roll[(nrow(DS_long_roll)-9):(nrow(DS_long_roll)),"id"] <- "1"
ds_sub <- subset(DS_long_roll, (Versuch %in% 2  | date > Pumpzeiten$start[17]))


data <- data %>% group_by(tiefe) %>% 
  mutate(Wind = RcppRoll::roll_mean(WindVel_30m_ms,60*4,fill=NA))


ds_sub2 <- merge(ds_sub,data[data$tiefe == 0,c("date","WindVel_30m_ms","Wind")])

##################
#PPC
##################
files.mat <- list.files(paste0(klimapfad,"PPC"),"PPC_gnd.+\\.mat$",full.names = T)
PPC_ls <- lapply(files.mat,function(x) as.data.frame(read.mat(x)[[1]]))
for(i in seq_along(files.mat)){
  PPC_ls[[i]]$date <- seq.POSIXt(ymd_hm(paste(str_extract(files.mat[i],"\\d{4}_\\d{1,2}_\\d{1,2}"),"00:00")),by="30 min",len=48)
  
}
PPC <- do.call(rbind,PPC_ls)
PPC_long <- tidyr::pivot_longer(PPC,matches("V"),values_to = "PPC")
PPC_agg <- PPC_long %>% group_by(date) %>% summarise(PPC_mean=mean(PPC))


date_seq <- seq.POSIXt(min(ds_sub2$date),max(ds_sub2$date),by="min")
ds_wide <- tidyr::pivot_wider(ds_sub2,matches("date|id|Versuch"),names_from=id,values_from="DSD0_roll",names_prefix = "DSD0_")
Versuch2 <- ds_wide$date[range(which(ds_wide$Versuch == 2))]
Versuch3 <- ds_wide$date[range(which(ds_wide$Versuch == 3))]

ds_approx <- data.frame(date=date_seq,
                        Versuch=NA,
                        DSD0_1=approx(x=ds_wide$date,y=ds_wide$DSD0_1,xout=date_seq)$y,
                        DSD0_2=approx(x=ds_wide$date,y=ds_wide$DSD0_2,xout=date_seq)$y,
                        DSD0_3=approx(x=ds_wide$date,y=ds_wide$DSD0_3,xout=date_seq)$y
                        )

ds_approx$Versuch[ds_approx$date >= Versuch2[1] & ds_approx$date <= Versuch2[2]] <- 2
ds_approx$Versuch[ds_approx$date >= Versuch3[1] & ds_approx$date <= Versuch3[2]] <- 3

ggplot()+
  geom_point(data=ds_wide,aes(date,DSD0_1))+
  geom_point(data=ds_approx,aes(date,DSD0_1))
colnames(ds_wide)

# ds_peak <- ds_sub2 %>%
#   filter(id==1) %>%
#   select(date,DSD0_roll,Wind,WindVel_30m_ms,id,range2,Versuch) %>%
#   group_by(Versuch) %>%
#   mutate(
#     slope = c(NA, diff(DSD0_roll)),
#     slope2 = c(NA, diff(slope)),
#     DSD0_min = RcppRoll::roll_minr(DSD0_roll,19,na.rm = T),
#     tal = ifelse(abs(slope) < 5e-3 & (slope2) > 0& !DSD0_roll > DSD0_min + 0.01, DSD0_roll, NA),
#     tal_approx = zoo::na.approx(tal,na.rm=F),
#     base = ifelse(tal_approx < DSD0_roll,tal_approx,DSD0_roll),
#     peak_min = DSD0_roll-DSD0_min,
#     peak = DSD0_roll - base
#     ) %>%
#   filter(!is.na(DSD0_min))


lim <- 1e-5
lim2 <- 0.01
ds_peak <- ds_approx %>%
  filter(!is.na(Versuch)) %>%
  group_by(Versuch) %>%
  select(date,DSD0_1,Versuch) %>%
  mutate(
    DSD0_roll = RcppRoll::roll_mean(DSD0_1,n=60,fill=NA),
    slope = c(NA, diff(DSD0_roll)),
    slope2 = RcppRoll::roll_mean(c(NA, diff(slope)),n=60,fill=NA),
    DSD0_min = RcppRoll::roll_minr(DSD0_1,19*60,na.rm = T),
    tal = ifelse(abs(slope) < lim & (slope2) > 0& !DSD0_1 > DSD0_min + lim2, DSD0_1, NA),
    tal_approx = zoo::na.approx(tal,na.rm=F),
    base = ifelse(tal_approx < DSD0_1,tal_approx,DSD0_1),
    #peak_min = DSD0_1-DSD0_min,
    peak = DSD0_1 - base
    ) %>%
  filter(!is.na(DSD0_min))

PPC_DS <- merge(ds_peak,PPC)
#PPC_DS_2 <- merge(ds_peak_2,PPC)
PPC_DS_long <- tidyr::pivot_longer(PPC_DS,matches("^V\\d"),values_to = "PPC")
range(PPC$date)
PPC_DS$date
ggplot()+
  #geom_line(data=PPC,aes(date,V33),col=3)+
  #geom_line(data=PPC_DS,aes(date,V19))+
  geom_line(data=subset(PPC_DS,date %in% round_date(date,"hours")),aes(date,V19))+

  geom_line(data=PPC_DS,aes(date,DSD0_roll),col=2)+
  geom_line(data=PPC_DS,aes(date,base),col=2)+
  geom_line(data=PPC_DS,aes(date,peak),col=2)

colnames(PPC_DS)
corcols <- grep("^V\\d|^peak$",colnames(PPC_DS))
cormat <- cor(PPC_DS[,corcols],use="complete")
best_cors <- names(which(sort(cormat[-1,1],decreasing=T)>0.8))
best_cor <- names(sort(cormat[-1,1],decreasing = T)[1:15])

sort(cormat[-1,1],decreasing = T)[1:15]
#aus Mail von Sven: 
#dein Versuchfeld lag in der nähe von Nummer 19 und 20.

PPC_DS$PPC <- PPC_DS[,"V19"]
PPC_DS <- PPC_DS[!grepl("^V\\d+$",colnames(PPC_DS))]
PPC$PPC <- PPC[,"V19"]
PPC <- PPC[!grepl("^V\\d+$",colnames(PPC))]

colnames(PPC)
colnames(PPC_DS)
####################
#save
save(PPC_DS,PPC,file=paste0(datapfad_harth,"PPC_DS.RData"))
############################

############################
#plots
###############################

ggplot(PPC)+
  geom_line(aes(date,V13,col="V13"))+
  geom_line(aes(date,V19,col="V19"))+
  geom_line(aes(date,V21,col="V21"))+
  geom_line(aes(date,V27,col="V27"))+
  geom_line(aes(date,V20,col="V20"))+
  ggsave(paste0(plotpfad_harth,"PPC_spatial_var.jpg"),width=7,height=5)

ggplot(PPC_long)+
  geom_line(aes(date,PPC,col=name))

  

ggplot(PPC)+
  geom_line(aes(date,PPC,col="V19"))+
  geom_line(aes(date,V20,col="V20"))+
  geom_line(aes(date,peak,col="DSD0_peak"))+
  xlim(range(ds_peak$date))

ggplot(PPC_long)+
  geom_line(aes(date,PPC,col=name))+
  geom_line(data = ds_sub,aes(date,DSD0_roll,col=range2))
ggplot(PPC_agg)+
  geom_line(aes(date,PPC_mean))+
  geom_line(data = ds_sub,aes(date,DSD0_roll,col=range2))

colnames(PPC_DS)
ggplot(subset(PPC_DS_long,name %in% best_cor))+
  geom_line(aes(date,PPC,col="PPC"))+
  geom_line(aes(date,peak,col="DSD0 peak"))+
  geom_line(aes(date,DSD0_roll,col="DSD0"))+
  ggsave(paste0(plotpfad_harth,"DS_PPC.jpg"),width=7,height=5)
ggplot(subset(PPC_DS_long,name %in% best_cors))+
  geom_smooth(aes(peak,PPC,col=name))+
  geom_point(aes(peak,PPC,col=name))+
  facet_wrap(~name)

lim <- 1e-5

ds_peak$tal2 <- ifelse(abs(ds_peak$slope) < lim & (ds_peak$slope2) > 0& !ds_peak$DSD0_1 > ds_peak$DSD0_min + lim2, 1, 0)
ggplot(ds_peak)+
  geom_line(aes(date,DSD0_1/1000,col="DSDD0"))+
  geom_line(aes(date,DSD0_min/1000,col="DSDD0_min"))+
  geom_point(data= subset(ds_peak,tal2==1),aes(date,DSD0_1/1000))+
  #geom_point(aes(date,tal,col="tal"))+
  geom_point(aes(date,slope,col="slope"))+
  geom_line(aes(date,slope2*50,col="slope2"))+
  geom_hline(yintercept = c(-lim,0,lim))+
  facet_wrap(~Versuch,scales="free")+
  ylim(c(-5e-4,5e-4))





# ds_peak$DSD0_base <- NA
# for(i in 2:3){
# ds_peak$DSD0_base[ds_peak$Versuch == i] <-  predict(glm(DSD0_min ~ I(poly(-date_int,2)),data=ds_peak[ds_peak$Versuch == i,]))
# }
unique(ds_peak$Versuch)
ggplot(ds_peak)+
  geom_line(aes(date,DSD0_1))+
  geom_line(aes(date,DSD0_min,col="base_min"))+
  geom_line(aes(date,base,col="base"))+
  geom_line(aes(date,peak,col="peak"))+
  facet_wrap(~Versuch,scales="free")
ggplot(ds_peak)+
  geom_line(aes(date,DSD0_roll))+
  geom_line(aes(date,DSD0_min,col="base",linetype="DSD0"))+
  geom_line(aes(date,base,col="base"))+
  geom_line(aes(date,base2,col="base2"))+
  geom_line(aes(date,peak,col="peak",linetype="DSD0"))+
  geom_line(aes(date,peak2,col="peak2"))+
  geom_line(aes(date,base4,col="base2"))+
  geom_line(aes(date,peak3,col="peak2"))+
  geom_line(aes(date,Wind/20,linetype="Wind"))+
  facet_grid(.~paste("Injektion",as.numeric(Versuch)-1),scales="free")+
  scale_x_datetime(date_label="%b %d")+
  ggsave(paste0(plotpfad_harth,"DS_peaks.jpg"),width=7,height=5)
  

ggplot(subset(ds_sub2))+
  geom_line(aes(date,DSD0_roll,col=id))+
  geom_line(aes(date,Wind/10))

ggplot(subset(ds_peak,id==1))+geom_point(aes(DSD0_roll,Wind))
ggplot(subset(ds_peak,id==1))+geom_point(aes(peak,Wind))
ggplot(subset(ds_peak,id==1))+geom_point(aes(peak3,Wind))

summary(lm(peak ~ Wind,data=ds_peak))
summary(lm(peak3 ~ Wind,data=ds_peak))

fm <- glm(peak ~ Wind,data=ds_peak)
R2 <- 1- fm$deviance / fm$null.deviance
ggplot(subset(ds_peak,id==1))+
  geom_point(aes(peak,Wind))+
  geom_smooth(aes(peak,Wind),method="glm",se=F)+
  annotate("text",x=0,y=4.5,label=paste("R² =",round(R2,2)),hjust=0)+
  ggsave(paste0(plotpfad_harth,"peaks_Wind_scatter.jpg"),width=7,height=5)

summary(lm(peak3 ~ Wind,data=ds_peak))

ds_sub$DSD0_roll
