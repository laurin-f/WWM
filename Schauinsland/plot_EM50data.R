
#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce")
check.packages(packages)

data_long<-read.Em50("long")
data_wide<-read.Em50("wide")
bspl <- ymd_h("2022.04.10 10","2022.05.10 10")
#data_long<-reshape2::melt(data_wide,id.vars="date")
vwc_plot<-ggplot(subset(data_long,!is.na(vwc) & key != "EM8"))+geom_line(aes(date,vwc,col=as.factor(tiefe),group=key),size=1)+facet_wrap(~plot)#+
  #ggforce::facet_zoom(xlim = bspl)#+scale_color_viridis_c()
vwc_plot
#+geom_vline(xintercept = ymd_h("2022-01-20 11"))+annotate("text",x=ymd_h("2022-01-20 11"),y=60,label="Schneeschmelze \nplot B",hjust=1)


data_long$temp[data_long$temp < -30] <- NA
temp_plot<-ggplot(data_long)+geom_line(aes(date,temp,col=as.factor(tiefe),group=key),size=1,alpha=0.6)+facet_wrap(~plot)#+ylim(c(-3,10))#+xlim(ymd_hms(c("2019-12-20 00:00:00","2019-12-23 00:00:00")))
temp_plot

# temp_plot<-ggplot(subset(data_long,unit=="temp"))+geom_line(aes(date,value,col=abs(hour(date)-12),lty=key),size=1)+scale_linetype_manual("",values=rep(1,7))+facet_wrap(~plot)+ylim(c(-3,10))+xlim(ymd_hms(c("2019-12-20 00:00:00","2019-12-30 00:00:00")))
# temp_plot+guides(lty=F)

pf_plot<-ggplot(data_long)+geom_line(aes(date,pF,col=tiefe,lty=key),size=1)+scale_linetype_manual("",values=rep(1,19))+facet_wrap(~plot)
pf_plot+guides(lty=F)


