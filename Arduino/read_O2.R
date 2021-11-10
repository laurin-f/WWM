hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
O2_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/O2test"
plotpfad_test <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Kalibrierung_tests/")

library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

all_files <- list.files(O2_pfad,full.names = T)
#files <- all_files[9]
#files <- all_files[grep("211020",all_files)]
files <- all_files[grep("211104",all_files)]
#files <- all_files[grep("211028.TXT",all_files)]

data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F,na.strings=c("NA","","ovf"))
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2","O2","temp")
#data$O2[data$O2 > 30 | data$O2 < 15] <- NA
data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 5000] <- NA

range(data$date,na.rm=T)

#daterange <- ymd_h(c("21/04/22 11", "21/04/22 16"))
#colnames(data)
#data <- subset(data, date >= min(daterange) & date <= max(daterange))
data$temp <- as.numeric(data$temp)
data$temp[data$temp<1 | data$temp > 50] <- NA
#colnames(data)
ggplot(data)+
  geom_line(aes(date,temp))
data$CO2
data$O2_raw <- data$O2
data$O2 <- RcppRoll::roll_mean(data$O2_raw,120,fill=NA)
range(data$date)

############
#28.10
# data_sub <- subset(data,date > ymd_hm("2021-10-28 14:30") & date < ymd_hm("2021-10-28 15:54"))
# O2 <- ggplot(data_sub)+
#   #geom_point(aes(date,O2,col="O2"))+
#   geom_line(aes(date,O2_raw),alpha=0.2)+
#   geom_line(aes(date,O2))+
#   labs(x="",y="O2 [V]")
# 
# CO2 <- ggplot(data_sub)+
#   geom_line(aes(date,CO2))+
#   labs(x="",y="CO2 [ppm]")
# ggpubr::ggarrange(CO2,O2,ncol=1)+ggsave(paste0(plotpfad_test,"O2_CO2_211028.png"),width=7,height=6)


###########################
#04.11

labor_ref <- all_files[grep("Laurin zum Vergleich",all_files)]
data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F)
data_ref <- readxl::read_xlsx(labor_ref,skip = 1)
colnames(data_ref) <- c("ymd","HMS","cycle",paste0("O2_mio",1:5))
data_ref[paste0(paste0("O2_mio",1:5))] <- sapply(data_ref[paste0(paste0("O2_mio",1:5))],as.numeric)

data_ref[paste0(paste0("O2_",1:5))] <- data_ref[paste0(paste0("O2_mio",1:5))] *10^-6

a <- 1:40
ggplot()+
  geom_point(data=data_ref[a,],aes(seq_along(O2_1)*30,O2_1+0.62,col="conventional Logger"))+
  geom_line(data=data_ref[a,],aes(seq_along(O2_1)*30,O2_1+0.62))+
  
  geom_line(data=data[1:(max(a)*30),],aes(seq_along(O2),O2_raw),alpha=0.2)+
  geom_point(data=data[a*30,],aes(seq_along(O2)*30,O2,col="Arduino"))+
  geom_line(data=data[1:(max(a)*30),],aes(seq_along(O2),O2))+
  labs(x="time [s]",y="O2 [V]")+
  ggsave(paste0(plotpfad_test,"Logger_Vergleich.png"),width=7,height=5)

