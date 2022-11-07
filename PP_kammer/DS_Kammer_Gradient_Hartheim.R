hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS","data.table")
check.packages(packages)

#############################
#flux

##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

injections <- read_ods(paste0(metapfad_PP,"injektionen_hartheim.ods"))
injections$Start <- dmy_hm(injections$Start)
injections$Ende <- dmy_hm(injections$Ende)

Versuch <- nrow(pp_chamber)
Versuch <- 9
#for(Versuch in 20:nrow(pp_chamber)){
#datelim <- c(pp_chamber$Start[Versuch]-3600*24*0.5,pp_chamber$Ende[Versuch]+3600*24*0.5)
plot <-  T
load <-  T
# if(is.na(datelim[2])){
#   datelim[2] <- now()
# }

#i <- 4
p <- list()
#for(i in 4:12){
now <- now()
tz(now) <- "UTC"
datelim <- c(ymd_h("22.09.27 10"),now)


class(flux_data)
if(load){
  load(paste0(datapfad_PP_Kammer,"flux_ls.RData"))
  datelim_old <- range(flux$date)
  if(datelim[2] > datelim_old[2]){
    flux_ls <- chamber_arduino(datelim=c(max(datelim_old),max(datelim)),
                               gga_data = T,
                               return_ls = T,
                               t_init=1,
                               t_min=2,
                               t_max=2,
                               gas = c("CO2_GGA","CH4"))
    flux_new <- flux_ls[[1]]
    flux_data_new <- flux_ls[[2]]
    # flux <- flux_ls[[1]]
    # flux_data <- flux_ls[[2]]
    
    flux <- rbind(flux,flux_new)
    flux_data <- rbind(flux_data,flux_data_new)
    
    save(flux,flux_data,file=paste0(datapfad_PP_Kammer,"flux_ls.RData"))
  }
}else{
  flux_ls <- chamber_arduino(datelim=datelim,
                             gga_data = T,
                             return_ls = T,
                             t_init=1,
                             t_min=2,
                             t_max=2,
                             gas = c("CO2_GGA","CH4"),
                             plot = "facets")
  flux <- flux_ls[[1]]
  flux_data <- flux_ls[[2]]
  
}
names(flux) <- str_remove(names(flux),"_GGA")
#rm(flux_data)
#datelim <- c(pp_chamber$Start[i]-3600*10,pp_chamber$Ende[i]+3600*10)
#datelim <- c(ymd_h("2022-04-13 18"),now())

data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "wide")
data_long <- read_sampler("sampler1u2",datelim = datelim, format = "long")
data_PPC <- read_PP(datelim = datelim,table.name = "PP_1min",format = "wide")


data_probe1u2$Versuch <- NA
data_long$Versuch <- NA

for(i in 1:nrow(pp_chamber)){
  id_wide <- which(daterange_id(data_probe1u2,c(pp_chamber$Start[i] - 3600 * 24,pp_chamber$Ende[i] + 3600 * 24)))
  id_long <- which(daterange_id(data_long,c(pp_chamber$Start[i] - 3600 * 24,pp_chamber$Ende[i] + 3600 * 24)))
  data_probe1u2$Versuch[id_wide] <- i
  data_long$Versuch[id_long] <- i
}

p[[paste0("CO2_")]] <-ggplot(subset(data_long,tiefe > -12))+
#  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  #geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  #geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe),linetype="probe 2"))+
  geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe)))+
  scale_fill_manual(values = "black")+
  coord_cartesian(xlim=datelim)+
  guides(fill=F)+
  labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe")


p[[paste0("flux_")]] <- ggplot(flux)+
  geom_point(aes(date,CO2_mumol_per_s_m2,col="Dynament"))+
  geom_line(aes(date,CO2_mumol_per_s_m2,col="Dynament"))+
 # geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  #geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
  scale_fill_grey()+
  guides(fill  = F)+
  coord_cartesian(xlim=datelim)

p[[paste0("PPC_")]] <- ggplot(data_PPC)+
  geom_line(aes(date, PPC_2, col="PPC_2"))+
  geom_line(aes(date, PPC_6, col="PPC_atm"))

data_30min <- data_probe1u2 %>% 
  select(matches("date|CO2_tiefe\\d_smp2|Versuch")) %>% 
  mutate(date = round_date(date,"30 mins")) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean,na.rm=T)) %>% 
  mutate(Versuch = round(Versuch)) %>% 
  rename_with(.cols=matches("CO2_tiefe\\d_smp2"),~str_remove(.,"_smp2"))


DT <- setDT(flux_data)
CO2_atm <- DT[chamber == 0,date := round_date(date, "30 mins")][,.(CO2_tiefe0 = mean(CO2_GGA)) ,by = date]
flux_data <- as.data.frame(flux_data)
CO2_atm <- as.data.frame(CO2_atm)


data <- merge(data_30min,flux[,c("date","T_C","CO2_mumol_per_s_m2")],by = "date",all=T)
data <- merge(data,CO2_atm)
data <- merge(data,data_PPC)
#swc
source("./PP_kammer/SWC_hartheim.R")

swc_sub <- sub_daterange(swc_wide,datelim)
data <- merge(data,swc_sub,all.x =T)

data$T_C <- imputeTS::na_interpolation(data$T_C)

data$CO2_flux <- RcppRoll::roll_mean(data$CO2_mumol_per_s_m2,10,fill=NA,na.rm = T)

data <- data %>% 
  mutate(across(matches("CO2_tiefe\\d"),~ppm_to_mol(.,T_C = data$T_C),.names = "{.col}_mol"),
         across(matches("CO2_tiefe\\d") & where(~length(which(!is.na(.)))>1),imputeTS::na_interpolation,maxgap=5))

data$modus <- factor(data$Versuch,levels = 1:nrow(pp_chamber),labels = pp_chamber$Modus)

data$dC_0_10 <- rowMeans(cbind(data$CO2_tiefe0_mol - data$CO2_tiefe1_mol,data$CO2_tiefe1_mol - data$CO2_tiefe2_mol,data$CO2_tiefe2_mol - data$CO2_tiefe3_mol)/-0.035)#mol/m4
#Ficks Law
#Fz = DS * dC/dz -> DS = Fz / (dC/dz)
data$DS <- data$CO2_flux * 10^-6 / data$dC_0_10 #mol/s/m2 / (mol/m4) = m2/s
data$D0_m2_s <- D0_T_p(data$T_C,unit = "m2/s")
data$DSD0 <- data$DS / data$D0_m2_s

p[[paste0("DSD0_")]] <- 
  ggplot(data)+
  geom_line(aes(date,DSD0))
#}


egg::ggarrange(plots=p,ncol=1)
names(data)
ggplot(data)+
  geom_point(aes(PPC_6,dC_0_10,col=swc_14))
ggplot(data)+
  geom_point(aes(PPC_2,dC_0_10,col=T_C))+
  facet_wrap(~cut(swc_14,3))
ggplot(data)+
  geom_point(aes(T_C,DSD0,col=swc_14))
ggplot(data)+
  geom_point(aes(swc_14,DSD0,col=T_C))
names(data)
ggplot(data)+
  geom_line(aes(date,dC_0_10))+
  geom_line(aes(date,PPC_2+0.5))+
  facet_wrap(~paste(Versuch,modus),scales = "free_x")
#####################
#klima daten dazu


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


