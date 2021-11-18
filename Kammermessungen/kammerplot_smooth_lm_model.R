
#pfade definieren

rm(list=ls())
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<-paste0(hauptpfad,"Daten/Metadaten/")

#flux.kammer<-function(ort="Schauinsland",
#                      messnr){

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg")
check.packages(packages)



flux_schaui <- chamber_flux(1,mess_dir = "Schauinsland",aggregate = F,
                            closing_lim=0,
                            opening_lim=0,
                            t_init=1,
                            t_max=4,
                            t_min=2,adj_openings=T,return_data = T)

data <- flux_schaui$data

data$date_int <- as.numeric(data$date)


test_plot <- ggplot(subset(data,!is.na(messid)),aes(date_int-min(date_int),CO2,group=messid))+
  geom_point()+
  geom_smooth(method="lm")

breaks <- which(round_date(data$date,"10secs") == round_date(data$date,"10mins"))
breaks <- breaks[diff(breaks)>10]
test_plot+
  scale_x_continuous(breaks=(data$date_int-min(data$date_int))[breaks],
                     labels = format(round_date(data$date,"10secs"),"%H:%M")[breaks])

ggplot(subset(data,!is.na(messid)),aes(date,CO2,group=as.factor(messid)))+
  geom_point(data=data,col="grey")+
  geom_point()+
  geom_smooth(formula=y~poly(x,1),
              method="lm")+
  scale_x_datetime(date_breaks = "10 mins",date_labels = "%H:%M")
