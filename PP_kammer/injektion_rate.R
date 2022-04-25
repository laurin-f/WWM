hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(inj_pfad,pattern = "_inj",full.names = T)


data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F,fill=T)



data_ls <- lapply(data_ls,"[",1:2)
# ls_names <- lapply(data_ls,colnames)
# right_names <- sapply(ls_names,function(x) all(c("date","CO2_ppm","inj") %in% x))
# data_ls <- data_ls[right_names]
# data_ls <- lapply(data_ls,function(x) x[,c("date","CO2_ppm","inj")] )

data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2")
#colnames(data) <- c("date","CO2","inj")



data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 7500] <- NA
max(data$CO2,na.rm=T)

range(data$date,na.rm=T)
daterange <- vector("list")
#daterange[[1]] <- ymd_h(c("22/03/09 19", "22/03/11 09"))
daterange[[1]] <- ymd_h(c("22/03/15 14", "22/03/16 17"))
#daterange[[1]] <- ymd_h(c("22/03/ 19", "22/03/11 09"))
#daterange[[1]] <- ymd_hm("2022.03.23 06:00","2022.03.24 12:00")
daterange[[2]] <- ymd_hm("2022.04.12 09:00","2022.04.14 12:00")

#daterange[[1]] <- ymd_h(c("21/04/22 12", "21/04/23 00"))
# daterange[[2]] <- ymd_hm(c("21/05/04 00:00", "21/05/04 11:30"))
# daterange[[3]] <- ymd_h(c("21/05/04 13", "21/05/10 12"))
# daterange[[4]] <- ymd_hm(c("21/04/14 16:00", "21/04/16 18:00"))
# #daterange[[1]] <- c(ymd_h("21/04/22 12"),now())
daterange_ges <-range(do.call(c,daterange)) 
#
  range(data$date,na.rm=T)

  inj_ls <- vector("list",length(daterange))
  inj_data_ls <- vector("list",length(daterange))
i<-1  
for(i in seq_along(daterange)){
  #data_sub <- subset(data, date >= min(daterange[[i]]) & date <= max(daterange[[i]])) 
  data_sub <- sub_daterange(data,daterange[[i]]) 
  
  #p <- ggplot(data_sub)+geom_line(aes(date,CO2))
  #data_sub$test <- 1
  #leave_NAtime_plot(data=data_sub,group="CO2",col="test")
  inj_ls_i <- injectionrate(data=data_sub,closing_lim = c(150,500,500,500)[i],opening_lim = c(-100,-100,-100,-100)[i],t_min=1,t_init = 0,Pumpstufen = 1,return_data = T,t_max=2,adj_openings=T)
  inj_ls[[i]] <- inj_ls_i[[1]]
  inj_data_ls[[i]] <- inj_ls_i[[2]]
  inj_ls[[i]]$Versuch <- as.character(i)
  inj_data_ls[[i]]$Versuch <- as.character(i)
}

inj <- do.call(rbind,inj_ls)
  inj$date
inj_data <- do.call(rbind,inj_data_ls)


ggplot(inj_data)+
  geom_line(aes(zeit,CO2_tara,col=as.factor(messid),linetype=as.factor(messid)))+guides(linetype=F)+
  geom_smooth(aes(zeit,CO2_tara,col=as.factor(messid)),method="lm",se=F)+guides(linetype=F)


#inj_plot <- 
  ggplot(inj)+
  geom_line(aes(date,CO2_ml_per_min))
#T_plt <- ggplot()+
#  geom_line(data=smp1u2,aes(date,T_C))+
#  xlim(range(inj$date))
#egg::ggarrange(inj_plot,T_plt)
  
  
  
  test <- read.table(files[9],sep=";",header=T,stringsAsFactors = F,fill=T)
  test$date <- ymd_hms(test$date)
ggplot(test)+geom_line(aes(date,CO2_ppm))+geom_vline(xintercept = ymd_hm("2022.03.23 15:08"))
