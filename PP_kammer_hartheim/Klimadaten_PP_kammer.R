#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
klimapfad_CR1000<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/Hartheim CR1000/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","dplyr")
check.packages(packages)
dirs <- list.dirs(klimapfad_CR1000,recursive = F)
files <- list.files(klimapfad_CR1000,pattern = "202(3|2)-\\d+-\\d+.csv",full.names = T)


colnames_list <- lapply(files,read.csv,skip=1,nrow=1,header=F,stringsAsFactors=F)
CR1000 <- lapply(files,read.csv,skip=3,stringsAsFactors =F,na.strings = c("NA","NAN"))
klima_data_list <- mapply(function(x,y){ 
  colnames(x) <- y
  return(x)
},x=CR1000,y=colnames_list,SIMPLIFY =F)


klima_data <- do.call("rbind",klima_data_list)
klima_data$date <- ymd_hms(klima_data$TIMESTAMP)

#klima_data <- subset(klima_data,date > ymd_h("22.08.01 10"))
colnames(klima_data) <- str_remove_all(colnames(klima_data),"MET_|_CS\\d+|_degC|_Avg|MovingTot|_Young\\d+|_Percent|_PTB100A")

# ###########
# files.mat <- str_subset(files,"\\.mat$")
# T1_Sx <- lapply(files.mat,function(x) read.mat(x)[[1]])
# 
# names(T1_Sx) <- str_extract(files.mat,"(?<=/)T1_S\\d_\\d+_\\d+(?=\\.mat$)")
# T1_Si <- lapply(1:4,function(x){ 
#   y <- do.call("rbind",T1_Sx[grep(paste0("T1_S",x),names(T1_Sx))])
#   colnames(y) <- paste0(c("u","v","w","SonicTemperatur"),"_S",x)
#   return(y)
#   })
# rm(T1_Sx)
# T1 <- do.call("cbind",T1_Si)

klima <- klima_data[,grep("date|Precip|^Ta|WindVel|PressureActual|SWin_CNR1",colnames(klima_data))]

# ggplot(klima_data)+geom_line(aes(date,Precip_Intensity_mmhr))
# ggplot(klima_data)+geom_line(aes(date,Precip_Last1hr_mm))
# ggplot(klima_data)+geom_line(aes(date,Precip_Last1hr_mm))
# ggplot(klima_data)+geom_line(aes(date,WindVel_30m_ms))
# ggplot(klima_data)+geom_line(aes(date,WindDir_30m_degN_WVT))

soil_data <- klima_data[,grep("^Soil(VWC|T)|date",colnames(klima_data))]
names(soil_data)
soil_data <- soil_data %>% 
  mutate(across(matches("^Soil"),as.numeric)) %>% 
  as_tibble()
soil_long <- tidyr::pivot_longer(soil_data,
                                 cols=grep("^Soil",colnames(soil_data)),
                                 names_to = c("unit","plot","tiefe"),
                                 names_pattern = "Soil(VWC|T)_([ABC])_(\\d+)cm",
                                 values_to = "value")


soil_agg_long <- soil_long %>%
  group_by(date,tiefe,unit) %>%
  summarise(mean = mean(value,na.rm =T),min=min(value,na.rm =T),max=max(value,na.rm =T),median = median(value,na.rm =T))


soil_agg <- tidyr::pivot_wider(soil_agg_long, names_from = c(unit),values_from = c(mean,min,max,median)) %>% as.data.frame()
soil_agg$tiefe <- as.numeric(soil_agg$tiefe)

soil_wide <- tidyr::pivot_wider(soil_agg_long, names_from = c(unit,tiefe),values_from = c(mean,min,max))
colnames(soil_wide) <- str_replace_all(colnames(soil_wide),c("(min|max)_(VWC|T)"="\\2_\\1", "value_VWC" = "VWC", "value_T" = "T_soil"))
#soil_wide <- tidyr::pivot_wider(soil_agg, names_from = tiefe, values_from = c(T_C,VWC))

# ggplot(subset(soil_long,unit=="T"))+geom_line(aes(date,value,col=depth,linetype=plot))
# ggplot(subset(soil_long,unit=="VWC"))+geom_line(aes(date,value,col=depth,linetype=plot))

names(klima) <- c("P_hPa","Wind_ms","Wind_ms_max","Wind_dir","Ta_50m","Ta_A_2m","Ta_B_2m","Ta_2m","Precip_mm","Precip_1hr_mm","date")
klima$Precip_24Tot <- RcppRoll::roll_sumr(klima$Precip_mm,24*60,fill=NA)
# ggplot(klima)+
#   geom_line(aes(date,Precip_1hr_mm))+
#   geom_line(aes(date,Precip_24Tot,col="24Tot"))+
#   geom_line(aes(date,Precip_mm,col="2"))+
#   xlim(ymd_h("22.11.08 10","22.11.14 00"))
#   geom_line(aes(date,Ta_2m))
#   geom_line(aes(date,P_hPa))



save(klima,soil_long,soil_agg,soil_wide,file=paste0(klimapfad_CR1000,"klima_data_PP_kammer.RData"))
