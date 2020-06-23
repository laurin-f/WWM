#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

library(pkg.WWM)
packages<-c("stringr","lubridate","ggplot2","RSQLite")
sapply(packages,require,character.only = T)
gdusbpfad<-paste0(hauptpfad,"Daten/Urdaten/Druckschwankung/")
all.dirs<-list.dirs(gdusbpfad)




dirs<-all.dirs[grep("(gga|micro)_dichtigkeit",all.dirs)]
files<-list.files(dirs,pattern = ".db3",full.names = T)
log_files<-list.files(dirs,pattern = "(l|L)og",full.names = T)
messwerte<-vector("list",length(files))
for(i in 1:length(files)){
  
  con<-dbConnect(RSQLite::SQLite(),files[i])
  messwerte[[i]]<-dbGetQuery(con,"SELECT * FROM messwerte")
  dbDisconnect(con)
  messwerte[[i]]$date<-ymd_hms(messwerte[[i]]$zeit)+2*3600
  messwerte[[i]]$messwert[messwerte[[i]]$messwert>9e+07]<-NA
}
logs <- lapply(log_files,read.csv)

test1<-messwerte[[1]]
test2<-messwerte[[2]]
test3<-messwerte[[3]]



micro1 <- read_db("GGA.db","micro",datelim = range(test1$date))
micro2 <- read_db("GGA.db","micro",datelim = range(test2$date))
micro3 <- read_db("GGA.db","micro",datelim = range(test3$date))
log3<-logs[[1]]
log3$date <- ymd_hm(paste("2020.06.19", log3$zeit))

micro1$p_mbar <- change_unit(micro1$GasP_torr,"torr","mbar")
micro2$p_mbar <- change_unit(micro2$GasP_torr,"torr","mbar")
micro3$p_mbar <- change_unit(micro3$GasP_torr,"torr","mbar")

plt_1_mbar <- ggplot(test1)+geom_line(aes(date,messwert))+labs(y="p [mbar]")
plt_1_GasT <- ggplot(micro1)+geom_line(aes(date,GasT_C))
plt_1_AmbT <- ggplot(micro1)+geom_line(aes(date,AmbT_C))
plt_1_p <- ggplot(micro1)+geom_line(aes(date,p_mbar))
egg::ggarrange(plt_1_mbar,plt_1_p,plt_1_GasT,ncol=1)

plt_3_mbar <- ggplot(test3)+geom_line(aes(date,messwert))+labs(y="p [mbar]")
plt_3_GasT <- ggplot(micro3)+geom_line(aes(date,GasT_C))
plt_3_AmbT <- ggplot(micro3)+geom_line(aes(date,AmbT_C))
plt_3_p <- ggplot(micro3)+geom_line(aes(date,p_mbar))
egg::ggarrange(plt_3_mbar,plt_3_p,plt_3_GasT,plt_3_AmbT,ncol=1)

ggplot(test2)+geom_line(aes(date,messwert))+labs(y="p [mbar]")
ggplot(test3)+geom_line(aes(date,messwert))+
  geom_text(data=log3,aes(date,2.5+(0.1*1:9),label=kommentar),hjust="left")+
  geom_vline(data=log3,aes(xintercept= date))+labs(y="p [mbar]")

