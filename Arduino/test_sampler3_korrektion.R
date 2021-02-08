
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr"))
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"


datelim <- ymd_h(c("2021-02-08-09"))
data <- read_sampler(table.name = "sampler3","long",datelim=datelim)
#gga <- read_db(db.name="GGA.db",table.name = "micro",datelim=datelim)

test <- read.csv(paste0(datapfad,"210208 (2).TXT"),sep=";",stringsAsFactors = F,na.strings = c("ovf","NA"))

test_long <- tidyr::pivot_longer(test,contains("tiefe"),names_pattern = "(CO2|temp)_tiefe(\\d)",names_to = c(".value","tiefe"))
test_long$date <- ymd_hms(test_long$date)
test_long$tiefe <- as.numeric(test_long$tiefe)*-3.5
test_long[test_long < (-100)] <- NA

ggplot()+
      geom_line(data=data,aes(date,CO2,col=as.factor(tiefe),linetype="db"))+
      geom_line(data=test_long,aes(date,CO2,col=as.factor(tiefe),linetype="test"))
ggplot(data)+
  geom_line(aes(date,temp,col=as.factor(tiefe),linetype="db"))+
  geom_line(data=test_long,aes(date,temp,col=as.factor(tiefe),linetype="test"))
  
  
