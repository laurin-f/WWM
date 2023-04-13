hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad<- paste0(hauptpfad,"Daten/Urdaten/O2_grove/")


detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

files <- list.files(datapfad,"\\d{6}.TXT",full.names = T)

data_ls <- lapply(files,read.table,sep = ";",header = T)
data <- do.call(rbind,data_ls)
class(data$O2_perc)
data$date <- ymd_hms(data$date)
ggplot(data)+
#  geom_line(aes(date, O2_perc))+
  geom_line(aes(date, RcppRoll::roll_mean(O2_perc,3600,fill = NA)))
