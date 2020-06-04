


#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad_tracer<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

#Vol.xlsx<-readxl::read_xlsx(paste0(metapfad_tracer,"Diffusionskammer.xlsx"))
#Vol_ml<-Vol.xlsx$Volumen_effektiv_ml

datelim1 <- c("2020.03.11 09:00:00","2020.03.17 18:00:00")
  # datelim <- c("2020.03.11 08:00:00","2020.03.11 18:00:00")
  # datelim <- c("2020.03.12 08:00:00","2020.03.12 18:00:00")
# datelim <- c("2020.03.16 08:00:00","2020.03.16 18:00:00")
# datelim <- c("2020.03.16 08:00:00","2020.03.16 18:00:00")
Pumpstufen1 <- c(1:5,5,rep(NA,4),1:4,1:5)
split_1 <- injectionrate(datelim = datelim1,Pumpstufen = Pumpstufen1,group="Pumpstufe",t_init = 1,aggregate = T)
split <- split_1[[2]]
flux <- split_1[[1]]

datelim<- data.frame(start=NA,stop=NA)
datelim[1,] <- c("2020.04.14 09:00:00","2020.04.17 18:00:00")
datelim[2,] <- c("2020.04.18 08:00:00","2020.04.18 15:00:00")
datelim[3,] <- c("2020.04.21 08:00:00","2020.04.21 09:07:00")
datelim[4,] <- c("2020.04.23 08:00:00","2020.04.23 10:00:00")
datelim[5,] <- c("2020.05.01 08:00:00","2020.05.01 18:00:00")
datelim[6,] <- c("2020.05.04 08:00:00","2020.05.04 09:45:00")
#schlauch seite gewechselt
datelim[7,] <- c("2020.05.04 09:46:00","2020.05.04 12:00:00")
datelim[8,] <- c("2020.05.07 09:00:00","2020.05.07 11:00:00")
datelim[9,] <- c("2020.05.07 11:00:00","2020.05.07 13:00:00")
datelim[10,] <- c("2020.05.11 08:00:00","2020.05.11 13:00:00")

Pumpstufen <- list()
Pumpstufen[[1]]<-c(3,3)
Pumpstufen[[2]]<-c(3,3)
Pumpstufen[[3]]<-c(3,3)
Pumpstufen[[4]]<-c(3,3)
Pumpstufen[[5]]<-c(3,3)
Pumpstufen[[6]]<-c(3,3)
Pumpstufen[[7]]<-c(3,3)
Pumpstufen[[8]]<-c(3,3)
Pumpstufen[[9]]<-c(1.5,1.5)
#die ersten zwei mit druckausgleich Spirale die zweiten beiden ohne
Pumpstufen[[10]]<-rep(1.5,4)


split_list <- lapply(seq_along(Pumpstufen),function(x) injectionrate(datelim = datelim[x,],Pumpstufen = Pumpstufen[[x]],group="Pumpstufe"))
#split_list_messid <- lapply(seq_along(Pumpstufen),function(x) injectionrate(datelim = datelim[x,],Pumpstufen = Pumpstufen[[x]],group="messid"))
split_flux_list <- lapply(split_list,function(x) x[[1]])
#split_flux_list_messid <- lapply(split_list_messid,function(x) x[[1]])
split_data_list <- lapply(split_list,function(x) x[[2]])

for (i in seq_along(split_flux_list)){
  split_flux_list[[i]]$ID <- i
}

data_all <- do.call(rbind,split_data_list)
flux_all_messid <- do.call(rbind,split_flux_list)


flux_all <- aggregate(flux_all_messid[!colnames(flux_all_messid) %in% c("Pumpstufe","day")],list(Pumpstufe = flux_all_messid$Pumpstufe, ID = flux_all_messid$ID),mean)
flux_all <- flux_all[!colnames(flux_all) %in% c("ID","messid")]
flux_all$date <- lubridate::with_tz(flux_all$date, "UTC")


########################
#plots
########################
plot <- F
if(plot == T){
ggplot(flux_all)+
  geom_line(aes(date,ml_per_min,col=as.factor(Pumpstufe)))+
  ggnewscale::new_scale_color()+
  geom_point(data=flux_all_messid,aes(date,ml_per_min,col=as.factor(ID)))



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

}

flux_all <- rbind(flux,flux_all)
flux_all[nrow(flux_all)+1,]<-NA
flux_all[nrow(flux_all),1:(ncol(flux_all)-1)]<- 0

write.csv(flux_all,file = paste0(metapfad_tracer,"Pumpstufen_flux.txt"),row.names = F)


#########
#


