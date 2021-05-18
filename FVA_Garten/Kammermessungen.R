#pfade definieren


detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<-paste0(hauptpfad,"Daten/Metadaten/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 


#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg","dplyr")
check.packages(packages)
flux <- list()
datelim <- ymd_h(c("2021.04.26 11","2021.04.26 12"))
datelim2 <- ymd_hm(c("2021.05.10 10:00","2021.05.10 10:50"))
data <- read_GGA(datelim=datelim)
#ggplot(data)+geom_line(aes(date,CO2))

flux_ls <- chamber_flux(mess_dir = "FVA_Garten",aggregate = F,closing_lim=20,t_min=1,messnr = 1:3,t_max=1.5,t_init=0.5,adj_openings=T,return_data = T)
ggplot(flux_ls[[2]])+geom_line(aes(zeit,CO2_tara,col=as.factor(messid)))
flux <- flux_ls[[1]]
  
flux$date <- round_date(flux$date,"mins")
probe3 <-  read_sampler("sampler3",datelim = range(datelim,datelim2), format = "wide")
probe3$date <- round_date(probe3$date,"mins")
flux <- merge(flux,probe3[,c("date","temp_tiefe1")],all.x=T)

flux$CO2_mol_per_min_m2 <- ppm_to_mol(flux$CO2_ml_per_min_m2)
flux$CO2_mumol_per_s_m2 <- change_unit(flux$CO2_mol_per_min_m2,"mol/min/m2","micromol/s/m2")
flux$CH4_mol_per_min_m2 <- ppm_to_mol(flux$CH4_ml_per_min_m2)
flux$CH4_mumol_per_s_m2 <- change_unit(flux$CH4_mol_per_min_m2,"mol/min/m2","micromol/s/m2")
flux_agg <- flux %>% mutate(day = format(date,"%y-%m-%d")) %>% 
    group_by(day,kammer) %>% 
    summarise_all(mean)

ggplot(flux)+
    geom_point(aes(date,CO2_ml_per_min,col=kammer))+
  geom_line(data=flux_agg,aes(date,CO2_ml_per_min,col=kammer))


save(flux,flux_agg,file=paste(datapfad_FVAgarten,"chamber_flux.RData"))




