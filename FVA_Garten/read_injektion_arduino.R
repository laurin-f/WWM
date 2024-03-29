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
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Injektionsrate_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(inj_pfad,full.names = T)

data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F)
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2")

data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 9000] <- NA


range(data$date,na.rm=T)
daterange <- vector("list")
daterange[[1]] <- ymd_h(c("21/04/22 12", "21/05/04 00"))
#daterange[[1]] <- ymd_h(c("21/04/22 12", "21/04/23 00"))
daterange[[2]] <- ymd_hm(c("21/05/04 00:00", "21/05/04 11:30"))
daterange[[3]] <- ymd_h(c("21/05/04 13", "21/05/10 12"))
daterange[[4]] <- ymd_hm(c("21/04/14 16:00", "21/04/16 18:00"))
#daterange[[1]] <- c(ymd_h("21/04/22 12"),now())
daterange_ges <-range(do.call(c,daterange)) 
#
smp1u2 <- read_sampler(datelim=daterange_ges,format="wide",cols="T_C") 

#data$min10 <- round_date(data$date,"10mins")
#T_C_df <- smp1u2 %>% 
  # mutate(min10 = round_date(date,"10mins")) %>% 
  # group_by(min10) %>% 
  # summarise(T_C = mean(T_C,na.rm=T))


ggplot(smp1u2)+geom_point(aes(date,T_C))+
  geom_line(data=smp1u2,aes(date,T_C))
inj_ls <- vector("list",length(daterange))
inj_data_ls <- vector("list",length(daterange))

for(i in seq_along(daterange)){
data_sub <- subset(data, date >= min(daterange[[i]]) & date <= max(daterange[[i]])) 



inj_ls_i <- injectionrate(data=data_sub,closing_lim = c(100,300,100,500)[i],opening_lim = c(-100,-200,-200,-500)[i],t_min=1,t_init = 2,Pumpstufen = 1,return_data = T,t_max=4,adj_openings=T)
inj_ls[[i]] <- inj_ls_i[[1]]
inj_data_ls[[i]] <- inj_ls_i[[2]]
inj_ls[[i]]$Versuch <- as.character(i)
inj_data_ls[[i]]$Versuch <- as.character(i)
}

inj <- do.call(rbind,inj_ls)
inj_data <- do.call(rbind,inj_data_ls)


ggplot(inj_data)+geom_line(aes(zeit,CO2_tara,col=Versuch,linetype=as.factor(messid)))+guides(linetype=F)


inj_plot <- 
  ggplot(inj)+
  geom_line(aes(date,CO2_ml_per_min))
T_plt <- ggplot()+
    geom_line(data=smp1u2,aes(date,T_C))+
  xlim(range(inj$date))
egg::ggarrange(inj_plot,T_plt)


save(inj,file = paste(datapfad_FVAgarten,"injectionrates.RData"))

timediff_inj <- which(difftime(inj$date[-1],inj$date[-nrow(inj)],units= "hours") > 1)

#################################
#
#
###################################
files <- list.files(inj_pfad,full.names = T)

data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F)
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2")

data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 9000] <- NA

daterange <- ymd_hm(c("21/05/17 16:00", "21/05/17 18:00"))
daterange <- ymd_hm(c("21/04/14 16:00", "21/04/16 18:00"))

data_sub <- subset(data, date >= min(daterange) & date <= max(daterange)) 

range(data_sub$date)
ggplot(data_sub)+geom_line(aes(date,CO2))

#ggplot(subset(data,date < ymd_hm(c("21/05/17 16:00")))+geom_line(aes(date,CO2))
