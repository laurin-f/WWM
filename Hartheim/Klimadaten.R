#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","rmatio")
check.packages(packages)
dirs <- list.dirs(klimapfad,recursive = F)
files <- list.files(klimapfad,recursive = T,full.names = T)

colnames_list <- lapply(str_subset(files,"HARTHM_MET"),read.csv,skip=1,nrow=1,header=F,stringsAsFactors=F)
CR1000 <- lapply(str_subset(files,"HARTHM_MET"),read.csv,skip=3,stringsAsFactors =F)
klima_data_list <- mapply(function(x,y){ 
  colnames(x) <- y
  return(x)
},x=CR1000,y=colnames_list,SIMPLIFY =F)


klima_data <- do.call("rbind",klima_data_list)
klima_data$date <- ymd_hms(klima_data$TIMESTAMP)

colnames(klima_data) <- str_remove_all(colnames(klima_data),"MET_|_CS\\d+|_degC|_Avg|MovingTot|_Young\\d+|_Percent")


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
klima <- klima_data[,grep("date|Precip_Intensity|^Ta|WindVel",colnames(klima_data))]

# ggplot(klima_data)+geom_line(aes(date,Precip_Intensity_mmhr))
# ggplot(klima_data)+geom_line(aes(date,Precip_Last1hr_mm))
# ggplot(klima_data)+geom_line(aes(date,Precip_Last1hr_mm))
# ggplot(klima_data)+geom_line(aes(date,WindVel_30m_ms))
# ggplot(klima_data)+geom_line(aes(date,WindDir_30m_degN_WVT))

soil_data <- klima_data[,grep("^Soil(VWC|T)|date",colnames(klima_data))]

soil_long <- reshape2::melt(soil_data,id="date")
soil_long$tiefe <- as.numeric(str_extract(soil_long$variable,"\\d+(?=cm)"))
soil_long$unit <- str_extract(soil_long$variable,"(?<=Soil)[A-Z|a-z]+")
soil_long$plot <- str_extract(soil_long$variable,"(?<=_)[ABC]+(?=_)")

soil_agg_long <- aggregate(list(value=soil_long$value),by=list(date=soil_long$date,tiefe= -soil_long$tiefe,unit= soil_long$unit),mean)
soil_agg <- tidyr::pivot_wider(soil_agg_long, names_from = unit,values_from = value)
colnames(soil_agg) <- str_replace(colnames(soil_agg),"^T$","T_C")
soil_wide <- tidyr::pivot_wider(soil_agg, names_from = tiefe, values_from = c(T_C,VWC))

# ggplot(subset(soil_long,unit=="T"))+geom_line(aes(date,value,col=depth,linetype=plot))
# ggplot(subset(soil_long,unit=="VWC"))+geom_line(aes(date,value,col=depth,linetype=plot))

save(klima,soil_long,soil_agg,soil_wide,file=paste0(klimapfad,"klima_data.RData"))
