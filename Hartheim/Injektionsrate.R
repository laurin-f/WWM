#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad_tracer<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
metapfad_harth<- paste0(hauptpfad,"Daten/Metadaten/Hartheim/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

Pumpzeiten <- readxl::read_xlsx(paste0(metapfad_harth,"Pumpstufen_Zeiten.xlsx"))
flux_old <- read.csv(paste0(metapfad_tracer,"Pumpstufen_flux.txt"))
flux_old$date <- ymd_hms(flux_old$date)
new_meas <- NULL
count <- 1
for( i in 1:nrow(Pumpzeiten)){
  if(!any((flux_old$date < Pumpzeiten$beginn[i] |flux_old$date > Pumpzeiten$ende[i])==F,na.rm=T)){
    new_meas[count] <- i
    count <- count+1
  }
}
if(!is.null(new_meas)){

Pumpstufen <- str_split(Pumpzeiten$Pumpstufe,",")

split_list <- lapply(new_meas,function(x) injectionrate(datelim = c(Pumpzeiten$beginn[x],Pumpzeiten$ende[x]),Pumpstufen = Pumpstufen[[x]],group="Pumpstufe",t_init = 0.1,spikes_th=390,difftime_th = 10,all_spikes_NA = F))

split_flux_list <- lapply(split_list,function(x) x[[1]])
split_data_list <- lapply(split_list,function(x) x[[2]])

for (i in seq_along(split_flux_list)){
  split_flux_list[[i]]$ID <- i
}

data <- do.call(rbind,split_data_list)
flux_messid <- do.call(rbind,split_flux_list)


flux <- aggregate(flux_messid[!colnames(flux_messid) %in% c("Pumpstufe","day")],list(Pumpstufe = flux_messid$Pumpstufe, ID = flux_messid$ID),mean)
flux <- flux[!colnames(flux) %in% c("ID","messid")]
flux$date <- lubridate::with_tz(flux$date, "UTC")

flux_all <- rquery::natural_join(flux,flux_old,by="date",jointype="FULL")
}
plot <- T
if(plot == T){
  ggplot(subset(flux_all,date > min(Pumpzeiten$beginn)))+
    geom_line(aes(date,ml_per_min,col=as.factor(Pumpstufe)))+
    ggnewscale::new_scale_color()+
    geom_point(data=flux_messid,aes(date,ml_per_min,col=as.factor(messid)))
}

write.csv(flux_all,file = paste0(metapfad_tracer,"Pumpstufen_flux.txt"),row.names = F)

