hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)
theme_set(theme_classic())
##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

i <- 4
p <- list()
#for(i in 4:12){
  datelim <- c(pp_chamber$Start[i]-3600*10,pp_chamber$Ende[i]+3600*10)
#  datelim <- c(ymd_h("2022-04-13 18"),now())

data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "wide")
data_long <- read_sampler("sampler1u2",datelim = datelim, format = "long")

p[[paste0("CO2_",i)]] <-ggplot(subset(data_long,tiefe > -12))+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  #geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe),linetype="probe 2"))+
  geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe)))+
  scale_fill_manual(values = "black")+
  coord_cartesian(xlim=datelim)+
  guides(fill=F)+
  labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe",title=pp_chamber$Bemerkung[i])

if(exists("flux")){
  rm(flux)
  rm(flux_data)
}
gga_data_T <- !is.na(pp_chamber$GGA_kammermessung[i])
flux_ls <- chamber_arduino(datelim=datelim,gga_data = gga_data_T,return_ls = T,t_init=2,plot="",t_offset = 60,t_min=4,gga=pp_chamber$GGA_kammermessung[i])
flux <- flux_ls[[1]]
flux_data <- flux_ls[[2]]

p[[paste0("flux_",i)]] <- ggplot(flux)+
  geom_point(aes(date,CO2_mumol_per_s_m2,col="Dynament"))+
  geom_line(aes(date,CO2_mumol_per_s_m2,col="Dynament"))+
  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
  scale_fill_grey()+
  guides(fill  = F)+
  coord_cartesian(xlim=datelim)

data_30min <- data_probe1u2 %>% 
  select(matches("date|CO2_tiefe\\d_smp2")) %>% 
  mutate(date = round_date(date,"30 mins")) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean,na.rm=T)) %>% 
  rename_with(.cols=matches("CO2_tiefe\\d_smp2"),~str_remove(.,"_smp2"))

  

data <- merge(data_30min,flux[,c("date","T_C","CO2_mumol_per_s_m2")],by = "date",all=T)
data$T_C <- imputeTS::na_interpolation(data$T_C)

data$CO2_mumol_per_s_m2 <- RcppRoll::roll_mean(data$CO2_mumol_per_s_m2,3,fill=NA,na.rm = T)

data <- data %>% 
  mutate(across(matches("CO2_tiefe\\d"),~ppm_to_mol(.,T_C = data$T_C),.names = "{.col}_mol"),
         across(matches("CO2_tiefe\\d") & where(~length(which(!is.na(.)))>1),imputeTS::na_interpolation,maxgap=5))

data$dC_0_10 <- rowMeans(cbind(data$CO2_tiefe0_mol - data$CO2_tiefe1_mol,data$CO2_tiefe1_mol - data$CO2_tiefe2_mol)/-0.035)#mol/m4
#Ficks Law
#Fz = DS * dC/dz -> DS = Fz / (dC/dz)
data$DS <- data$CO2_mumol_per_s_m2 * 10^-6 / data$dC_0_10 #mol/s/m2 / (mol/m4) = m2/s
data$D0_m2_s <- D0_T_p(data$T_C,unit = "m2/s")
data$DSD0 <- data$DS / data$D0_m2_s

p[[paste0("DSD0_",i)]] <- 
  ggplot(data)+
  geom_line(aes(date,DSD0))
#}

j <- 4
egg::ggarrange(plots=p[grep(paste0(j,"$"),names(p))],ncol=1)
  
#####################
#klima daten dazu

load(file = paste(datapfad_PP_Kammer,"swc_long.RData"))
swc_sub <- sub_daterange(swc_long,datelim)
swc_wide_sub <- sub_daterange(swc_wide,datelim)

load(file = paste(datapfad_PP_Kammer,"klima_DWD.RData"))
if(as.numeric(difftime(now(),max(klima$date),unit="hours")) > 24){
  source("./PP_kammer/klima_dwd.R")
}
klima_sub <- sub_daterange(klima,datelim)

data_merge <- merge(data,swc_wide_sub)
data_merge <- merge(data_merge,klima_sub)
names(data)

sec_ax_T <- 4
p[[paste0("ws",i)]] <- ggplot(data_merge)+
  geom_line(aes(date,wind,col="WS"))+
    geom_line(aes(date,T_C/sec_ax_T,col="T"))+
    scale_y_continuous(sec.axis = sec_axis(~.*sec_ax_T,name=expression(T["atm"]~"(°C)")))+
    coord_cartesian(xlim=datelim)+
  labs(x="",y="windspeed (m/s)")


sec_ax_fac <- 0.7
swc_min <- min(swc_sub$swc,na.rm = T)/1.1
p[[paste0("swc",i)]] <- ggplot(swc_sub)+
  geom_ribbon(data=klima_sub,aes(x=date,ymin=swc_min,ymax=P24tot/sec_ax_fac + swc_min),fill="blue",alpha=0.8)+
  geom_line(aes(date,swc,col=as.factor(tiefe)))+
  xlim(datelim)+
  scale_y_continuous(sec.axis = sec_axis(~(. - swc_min)*sec_ax_fac,name=expression(italic(P)["24h"]*" (mm)")))+
  theme(
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"),
    axis.text.x = element_blank()
  )+
  labs(x="",y="SWC (Vol. %)",col="tiefe (cm)")
ggplot(data_merge)+
  #geom_point(aes(wind,CO2_mumol_per_s_m2))
  geom_point(aes(swc_7,dC_0_10,col=wind))
  geom_point(aes(swc_21,DSD0,col=wind))
  geom_point(aes(wind,DSD0,col=swc_7))
  geom_point(aes(wind,DSD0))

  data_merge$DSD0_roll <- RcppRoll::roll_mean(data_merge$DSD0,20,fill=NA)
  data_select <- data_merge[,c("DSD0_roll","wind","P24tot","T_C","swc_7")]
  M <- cor(data_select)
  corrplot::corrplot.mixed(M)
  PerformanceAnalytics::chart.Correlation(data_select)
  
  