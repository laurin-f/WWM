


#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

#Vol.xlsx<-readxl::read_xlsx(paste0(metapfad,"Diffusionskammer.xlsx"))
#Vol_ml<-Vol.xlsx$Volumen_effektiv_ml

datelim1 <- c("2020.03.11 09:00:00","2020.03.17 18:00:00")
  # datelim <- c("2020.03.11 08:00:00","2020.03.11 18:00:00")
  # datelim <- c("2020.03.12 08:00:00","2020.03.12 18:00:00")
# datelim <- c("2020.03.16 08:00:00","2020.03.16 18:00:00")
Pumpstufen1 <- c(1:5,5,rep(NA,4),1:4,1:5)
split_1 <- injectionrate(datelim = datelim1,Pumpstufen = Pumpstufen1,group="Pumpstufe",t_init = 1)
split <- split_1[[2]]
flux <- split_1[[1]]

datelim<- data.frame(start=NA,stop=NA)
datelim[1,] <- c("2020.04.14 09:00:00","2020.04.17 18:00:00")
datelim[2,] <- c("2020.04.18 08:00:00","2020.04.18 15:00:00")
datelim[3,] <- c("2020.04.21 08:00:00","2020.04.21 09:07:00")
datelim[4,] <- c("2020.04.23 08:00:00","2020.04.23 10:00:00")
Pumpstufen <- list()
Pumpstufen[[1]]<-c(3,3)
Pumpstufen[[2]]<-c(3,3)
Pumpstufen[[3]]<-c(3,3)
Pumpstufen[[4]]<-c(3,3)

split_list <- lapply(seq_along(Pumpstufen),function(x) injectionrate(datelim = datelim[x,],Pumpstufen = Pumpstufen[[x]],group="Pumpstufe"))
split_list_messid <- lapply(seq_along(Pumpstufen),function(x) injectionrate(datelim = datelim[x,],Pumpstufen = Pumpstufen[[x]],group="messid"))
split_flux_list <- lapply(split_list,function(x) x[[1]])
split_flux_list_messid <- lapply(split_list_messid,function(x) x[[1]])
split_data_list <- lapply(split_list,function(x) x[[2]])

#ggplot(split[[2]])+geom_point(aes(zeit,CO2_tara,col=as.factor(Pumpstufe)))

data_all <- do.call(rbind,split_data_list)
flux_all <- do.call(rbind,split_flux_list)
flux_all_messid <- do.call(rbind,split_flux_list_messid)
flux2<- rbind(flux,flux_all)

flux_all_messid$messid_day <- paste(lubridate::date(flux_all_messid$date),flux_all_messid$messid,sep="_")

########################
#plots
########################

ggplot(flux_all)+
  geom_line(aes(date,ml_per_min,col=as.factor(Pumpstufe)))+geom_point(data=flux_all_messid,aes(date,ml_per_min,col=messid_day))



label.group <- sort(unique(na.omit(Pumpstufen1)))
label.char <- paste("Pumpstufe",label.group,"=",round(flux$tracer_ml_per_min,2),"ml/min")
lbl <- as_labeller(setNames(label.char, label.group))

ggplot(split)+geom_point(aes(date,CO2,col=as.factor(messid)))+facet_wrap(~messid,scales = "free_x")

Pumpstufen_plot <- ggplot(subset(split,!is.na(Pumpstufe)))+
  geom_smooth(aes(zeit,CO2_tara,col=as.factor(messid)),method="glm",linetype=2,lwd=0.6)+
  geom_line(aes(zeit,CO2_tara,col=as.factor(messid)),lwd=1)+
  facet_wrap(~Pumpstufe,labeller = lbl)
Pumpstufen_plot+guides(col=F)+labs(linetype="sampler included")


ggplot(subset(split,!is.na(Pumpstufe)))+
  geom_point(aes(zeit,CO2_tara,col=as.factor(Pumpstufe)))#+
  #geom_smooth(aes(zeit,CO2_tara,col=as.factor(Pumpstufe),linetype=as.factor(sampler_included)),method="glm")

ggplot(flux)+geom_point(aes(Pumpstufe,tracer_ml_per_min))+geom_abline(intercept=0,slope=0.1)

flux2[nrow(flux2)+1,]<-NA
flux2[nrow(flux2),1:(ncol(flux2)-1)]<- 0
flux2
write.csv(flux2,file = paste0(metapfad,"Pumpstufen_flux.txt"),row.names = F)
#write.csv(flux,file = paste0(metapfad,"Pumpstufen_flux.txt"),row.names = F)

#########
#


