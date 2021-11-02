O2_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/O2test"

library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

all_files <- list.files(O2_pfad,full.names = T)
files <- all_files[grep("211019",all_files)]
labor_ref <- all_files[grep("Laurin zum Vergleich",all_files)]
data_ls <- lapply(files,read.table,sep=";",header=T,stringsAsFactors = F)
data_ref <- readxl::read_xlsx(labor_ref,skip = 1)
colnames(data_ref) <- c("ymd","HMS","cycle",paste0("O2_mio",1:5))
data_ref[paste0(paste0("O2_mio",1:5))] <- sapply(data_ref[paste0(paste0("O2_mio",1:5))],as.numeric)

data_ref[paste0(paste0("O2_",1:5))] <- data_ref[paste0(paste0("O2_mio",1:5))] *10^-6

ggplot()+
  geom_point(data=data_ref[1:75,],aes(seq_along(O2_1),O2_1))
treats <- str_extract(files,"(?<=_)[A-Za-z1-9]+(?=.TXT$)")
for(i in seq_along(data_ls)){
  data_ls[[i]]$treat <- treats[i]
}
data <- do.call(rbind,data_ls)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2","O2","temp","treat")
#data$O2[data$O2 > 30 | data$O2 < 15] <- NA
data$CO2 <- as.numeric(data$CO2)
data$CO2[ data$CO2 < 300| data$CO2 > 5000] <- NA


data$temp <- as.numeric(data$temp)
data$temp[data$temp<1] <- NA
ggplot(subset(data,treat == "normal"))+
  geom_point(aes(date,O2,col=treat))+
  geom_line(aes(date,O2,col=treat))
ggplot(subset(data,treat %in% c("normal","oversampling")))+
  geom_point(aes(date,O2,col=treat))+
  geom_line(aes(date,O2,col=treat))+labs(y="O2 [V]")

sort(unique(abs(diff(subset(data,treat == "normal")$O2))))[2]

5/2^10#10 bit resolution normal
sort(unique(abs(diff(subset(data,treat == "oversampling")$O2))))[2]
5/2^13#13 bit resolution with oversampling

unique(data$treat)
ggplot(data)+
  geom_point(aes(date,temp,col=treat))

ggplot(subset(data,treat == "KammerGarten"))+
  geom_line(aes(date,as.numeric(CO2),col=treat))

