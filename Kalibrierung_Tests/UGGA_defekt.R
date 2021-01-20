library(pkg.WWM)
check.packages(c("lubridate","ggplot2"))
datelim <- c("2020-07-06 13:28:14 UTC", "2020-07-14 17:47:09 UTC")
gga <- read_db("GGA.db","gga",datelim = datelim)
micro <- read_db("GGA.db","micro",datelim = datelim)

range(gga$date)
ggplot()+
  geom_point(data=gga,aes(date,CO2,col="UGGA"))+
  geom_point(data=micro,aes(date,CO2,col="micro"))+
  
  #xlim(ymd_hm("2020.07.10 11:50","2020.07.10 1300"))
  xlim(ymd_hm("2020.07.14 16:45","2020.07.14 1747"))+
  ylim(c(350,500))
