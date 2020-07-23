#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad_ds<-paste0(hauptpfad,"Daten/Urdaten/DS_Labor/")
metapfad_ds<-paste0(hauptpfad,"Daten/aufbereiteteDaten/DS_Labor/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units")
check.packages(packages)

meta <- readxl::read_xls(paste0(metapfad_ds,"SAS Protokoll_22_0720Auswertung.xls"),sheet=2)
files <- list.files(datapfad_ds,full.names = T)
dat <- lapply(files, read.csv,skip=5,stringsAsFactors=F,header=F)
colnames <- lapply(files, read.csv,skip=3,nrow=1,header=F,stringsAsFactors=F)

for(i in seq_along(dat)){
  colnames(dat[[i]]) <- c("monthday","year",colnames[[i]][-1])
  dat[[i]]$date <- mdy_hms(paste(dat[[i]]$monthday,dat[[i]]$year,dat[[i]]$Time))
  dat[[i]]$t_secs <- as.numeric(difftime(dat[[i]]$date,min(dat[[i]]$date),units = "secs"))
  dat[[i]]$ID <- i
}

fm <- lapply(dat,function(x) glm(N2~t_secs,data=x)$coefficients[2])
fm2 <- lapply(dat,function(x) glm(N2~t_secs,data=subset(x,t_secs <=300&t_secs>0))$coefficients[2])

for(i in seq_along(dat)){
dat[[i]]$N2_pro_sec <- fm[[i]]   
dat[[i]]$N2_pro_sec2 <- fm2[[i]]   
}
dat_long <- do.call(rbind,dat)
Materialnr <- tolower(str_extract(meta$Dateiname[-1],"(?<=JL220).(?=01)"))

material <- meta$Material[-1]
dat_long$method_ID <- str_extract(dat_long$`Method Name`,"(?<=Jl220).(?=01.met)")
dat_long$material <- as.character(factor(dat_long$method_ID,levels = Materialnr,labels=material))

ggplot(subset(dat_long,t_secs <=300))+ geom_line(aes(t_secs,N2,col=as.factor(material),linetype=as.factor(ID)))#+facet_wrap(~material,scales="free_x")

ggplot(dat_long)+
  geom_boxplot(aes(material,N2_pro_sec,fill="alle"),alpha=0.4)+
  geom_boxplot(aes(material,N2_pro_sec2,fill="> 0"),alpha=0.4)
