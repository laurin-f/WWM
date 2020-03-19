#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

data_long<-read.Em50("long")
#data_long<-reshape2::melt(data_wide,id.vars="date")


vwc_plot<-ggplot(subset(data_long,unit=="vwc"))+geom_line(aes(date,value,col=tiefe,alpha=key),size=1)+scale_alpha_manual("",values=rep(1,12))+facet_wrap(~plot)+scale_color_viridis_c()
vwc_plot+guides(alpha=F)

temp_plot<-ggplot(subset(data_long,unit=="temp"))+geom_line(aes(date,value,col=tiefe,lty=key),size=1)+scale_linetype_manual("",values=rep(1,7))+facet_wrap(~plot)+ylim(c(-3,10))#+xlim(ymd_hms(c("2019-12-20 00:00:00","2019-12-23 00:00:00")))
temp_plot+guides(lty=F)

temp_plot<-ggplot(subset(data_long,unit=="temp"))+geom_line(aes(date,value,col=abs(hour(date)-12),lty=key),size=1)+scale_linetype_manual("",values=rep(1,7))+facet_wrap(~plot)+ylim(c(-3,10))+xlim(ymd_hms(c("2019-12-20 00:00:00","2019-12-30 00:00:00")))
temp_plot+guides(lty=F)

pf_plot<-ggplot(subset(data_long,unit=="pF"))+geom_line(aes(date,value,col=tiefe,lty=key),size=1)+scale_linetype_manual("",values=rep(1,7))+facet_wrap(~plot)+ylim(c(0,8))
pf_plot+guides(lty=F)

