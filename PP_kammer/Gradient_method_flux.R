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
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

#############################
#flux

load(file=paste0(datapfad_PP_Kammer,"DSD0_comsol.RData"))
data_wide <- read_sampler(datelim = PPC_daterange,format = "wide")

flux <- chamber_arduino(datelim=PPC_daterange,gga_data = F,return_ls = F,t_init=2,plot="facets",t_offset = 60,t_min=4,gga=pp_chamber$GGA_kammermessung[i])

data_30min <- data_wide %>% 
  select(matches("date|CO2_tiefe\\d_smp2")) %>% 
  mutate(date = round_date(date,"30 mins")) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean,na.rm=T)) %>% 
  rename_with(.cols=matches("CO2_tiefe\\d_smp2"),~str_remove(.,"_smp2"))

data_30min <- merge(data_30min,flux[,c("date","T_C","CO2_mumol_per_s_m2")],by = "date",all=T)
data_30min$T_C <- imputeTS::na_interpolation(data_30min$T_C)

data_30min <- data_30min %>% 
  mutate(across(matches("CO2_tiefe\\d"),~ppm_to_mol(.,T_C = data_30min$T_C),.names = "{.col}_mol"),
         across(matches("CO2_tiefe\\d") & where(~length(which(!is.na(.)))>1),imputeTS::na_interpolation,maxgap=5)
  )

data_GM <- merge(data_30min,subset(comsol_old,tiefe==1)[!grepl("tiefe",names(comsol_old))])

data_GM$dC_0_10 <- rowMeans(cbind(data_GM$CO2_tiefe0_mol - data_GM$CO2_tiefe1_mol,data_GM$CO2_tiefe1_mol - data_GM$CO2_tiefe2_mol)/-0.035)#mol/m4
#Ficks Law
#Fz = DS * dC/dz -> DS = Fz / (dC/dz)
data_GM$flux_GM <- data_GM$DS * data_GM$dC_0_10 *10^6 #mumol/s/m2 = m2/s * (mol/m4) * 10^6

ggplot(data_GM)+
  geom_line(aes(date,flux_GM,col="GM"))+
  geom_line(aes(date,CO2_mumol_per_s_m2,col="chamber"))+
  labs(y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="method")+
  ggsave(paste0(plotpfad_PPchamber,"flux_GM_vs_Chamber.png"),width=6,height = 4)
##################
