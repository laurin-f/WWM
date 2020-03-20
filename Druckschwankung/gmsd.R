#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
plotpfad<-paste0(hauptpfad,"/Daten/Ergebnisse/Plots/")

gdusbpfad<-paste0(hauptpfad,"Daten/Urdaten/Druckschwankung/")



#Packages laden
packages<-c("stringr","lubridate","ggplot2","RSQLite","readxl","egg")
check.packages(packages)

all.dirs<-list.dirs(gdusbpfad)

dirs<-all.dirs[grep("(gga|micro)_puffer_test",all.dirs)]
files<-list.files(dirs,pattern = ".db3",full.names = T)


data_gmsd_raw <- lapply(files, function(x){
  con<-dbConnect(RSQLite::SQLite(),x)
  temp_data <- dbGetQuery(con,"SELECT * FROM messwerte")
  dbDisconnect(con)
  temp_data$date<-ymd_hms(temp_data$zeit) + 3600
  temp_data
})

colnames_gmsd <- c("date","messwert","mittelwert","minpeakwert","maxpeakwert")
data_gmsd_list <- lapply(data_gmsd_raw, function(x) x[colnames_gmsd])

data_gmsd <- do.call("rbind",data_gmsd_list)
data_gmsd[,-1] <- apply(data_gmsd[,-1],2,function(x) ifelse(x > 9e+07, NA, x))


datelim <- c("2020.03.02 00:00:00","2020.03.04 23:00:00")
data_gga <- read_db("GGA.db","micro",datelim = datelim)


date_round <- round_date(data_gmsd$date,unit = "second")


data_gmsd_agg <- aggregate(data_gmsd[-1], list(date = date_round),mean)
data_gmsd_agg$p_diff <- data_gmsd_agg$maxpeakwert - data_gmsd_agg$minpeakwert 

data <- merge(data_gga,data_gmsd_agg,all.x = T)
colnames(data) <- str_replace(colnames(data), "peakwert", "")

data.split <- split_chamber(data,
                      closing_before = 5,
                      closing_after = 10,
                      opening_before = -5,
                      opening_after = -15,
                      t_max=30,
                      t_init=0,
                      t_min=2,
                      adj_openings = T)

pufferversuche <- rep(c("Membran_und_Sack","Membran","Sack"),2)
data.split$puffer <- as.character(factor(data.split$messid,levels=1:6,labels = pufferversuche))

xlim_vec <- ymd_hms("2020.03.04 11:10:00","2020.03.04 12:00:00")

puffer_range <- lapply(unique(data.split$messid)[-1],function(x) range(data.split$date[which(data.split$messid == x)]))
puffer_max <- lubridate::as_datetime(sapply(puffer_range,max))
puffer_min <- lubridate::as_datetime(sapply(puffer_range,min))
col<-scales::hue_pal()(3)

rect.df <- data.frame(puffer_min=puffer_min,puffer_max=puffer_max,puffer=as.factor(pufferversuche))

rect_puffer <- geom_rect(data=rect.df,aes(xmin=puffer_min,xmax=puffer_max,ymin=-Inf,ymax=Inf, fill = puffer), alpha=0.3)

druck_plot <- ggplot()+
  rect_puffer+
  geom_ribbon(data=data.split, aes(date, ymin=min,ymax=max),alpha=0.3)+
  geom_line(data=data.split, aes(date, mittelwert))+
  #geom_line(data=subset(data.split,!is.na(puffer)), aes(date, mittelwert,col=puffer))+
  xlim(xlim_vec)
#druck_plot
co2_plot <- ggplot(data.split)+
  rect_puffer+
  geom_line(aes(date, CO2))+
  guides(fill=F)+
  #geom_line(data=subset(data.split,!is.na(puffer)),aes(date, CO2,col=puffer))+
  xlim(xlim_vec)
egg::ggarrange(druck_plot,co2_plot,ncol=1)

ggplot(subset(data.split, !is.na(puffer)))+
  geom_line(aes(zeit,CO2,col=as.factor(messid)))+
  facet_wrap(~puffer,scales = "free_x")

ggplot(subset(data.split, !is.na(puffer)))+
  geom_line(aes(zeit,mittelwert,col=as.factor(messid)))+
  facet_wrap(~puffer,scales = "free_x")

ggplot(subset(data.split, !is.na(puffer)))+
  geom_line(aes(zeit,p_diff,col=as.factor(messid)))+
  facet_wrap(~puffer,scales = "free_x")

ggplot(data)+geom_point(aes(date,CO2))


