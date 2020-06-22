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
  messwerte[[i]]$date<-ymd_hms(messwerte[[i]]$zeit)
  messwerte[[i]]$messwert[messwerte[[i]]$messwert>9e+07]<-NA
}
logs <- lapply(log_files,read.csv)
test1<-messwerte[[1]]
test2<-messwerte[[2]]
test3<-messwerte[[3]]
log3<-logs[[1]]
log3$date <- ymd_hm(paste("2020.06.19", log3$zeit))


ggplot(test1)+geom_line(aes(date,messwert))+labs(y="p [mbar]")
ggplot(test2)+geom_line(aes(date,messwert))+labs(y="p [mbar]")
ggplot(test3)+geom_line(aes(date+2*3600,messwert))+
  geom_text(data=log3,aes(date,2.5+(0.1*1:9),label=kommentar),hjust="left")+
  geom_vline(data=log3,aes(xintercept= date))+labs(y="p [mbar]")

