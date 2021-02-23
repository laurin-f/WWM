
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr"))
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
arduinopfad <- paste0(hauptpfad,"Daten/Urdaten/Arduino/")
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"
plotpfad_prod <- paste0(hauptpfad,"Dokumentation/Berichte/plots/produktionseimer/")


datelim <- ymd_h(c("2021-02-22-11"))
data <- read_sampler(table.name = "sampler3","long",datelim=datelim)
#gga <- read_db(db.name="GGA.db",table.name = "micro",datelim=datelim)

# files.new <- list.files(arduinopfad,".TXT",full.names = T)
# dyn.list <- lapply(files.new,read.csv,sep=";",stringsAsFactors = F,na.strings = c("NA","ovf","0.00","-0.00","-250.00"))
# 
# dyn <- do.call(rbind,dyn.list)
# 
# CO2_cols <- grep("CO2",colnames(dyn))
# dyn[,CO2_cols][dyn[,CO2_cols] < 0] <- NA
# 
# 
# dyn[dyn < -20] <- NA
# dyn[dyn > 7000] <- NA
# dyn$date <- lubridate::ymd_hms(dyn$date)
# 
# ggplot()+
#       geom_line(data=dyn,aes(date,CO2_tiefe6,col="tiefe 6"))+
#       geom_line(data=dyn,aes(date,CO2_tiefe7,col="tiefe 7"))+xlim(ymd_h(c("2021.02.11 0","2021.02.15 15")))

ggplot()+
      geom_line(data=data,aes(date,CO2,col=as.factor(tiefe)))+
      geom_vline(xintercept = ymd_h("2021.02.17 09"))


range(data$CO2,na.rm = T)
#bei Einspeisung 3 waren es auch 3600 ppm
test <- read_sampler(datelim=ymd_h(c("2020.07.19_01","2020.07.21_01")))
range(test$CO2_smp1,na.rm = T)

ggplot(data)+
  geom_line(aes(date,temp,col=as.factor(tiefe)))
  
