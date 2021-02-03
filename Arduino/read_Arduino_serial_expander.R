
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr"))
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"


datelim <- ymd_h(c("2021-02-03-09","2021-02-03-16"))
data <- read_sampler(table.name = "sampler3","long",datelim=datelim)
gga <- read_db(db.name="GGA.db",table.name = "micro",datelim=datelim)



ggplot()+
      geom_line(data=data,aes(date,CO2,col=as.factor(tiefe)))+
      geom_line(data=gga,aes(date,CO2))
ggplot(data)+
  geom_line(aes(date,temp,col=as.factor(tiefe)))
  
