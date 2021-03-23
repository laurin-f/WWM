O2_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/O2test"

files <- list.files(O2_pfad,full.names = T)
data <- read.table(files,sep=";",header = T)
data$date <- ymd_hms(data$date)
colnames(data) <- c("date","CO2","O2","T")
data$O2[data$O2 > 22 | data$O2 < 15] <- NA
ggplot(data)+
  geom_line(aes(date,O2,col="O2"))+
  geom_line(aes(date,CO2/70,col="CO2"))+
  scale_y_continuous(sec.axis = sec_axis(trans=~.*70,name="CO2"))

