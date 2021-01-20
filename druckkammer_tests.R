
#Volumen abschätzung
h <- 45 #cm
r <- 55/2 # cm
A <- pi*r^2

V_cm3 <- A*h #cm^3
V_l <- V/1000

#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
plotpfad<-paste0(hauptpfad,"/Daten/Ergebnisse/Plots/")

gdusbpfad<-"C:/Users/ThinkPad/Documents/GDUSB FastView/"



#Packages laden
packages<-c("stringr","lubridate","ggplot2","RSQLite","readxl","egg")
check.packages(packages)

all.dirs<-list.dirs(gdusbpfad)


files<-list.files(all.dirs,pattern = ".db3$",full.names = T)
files.se3<-list.files(all.dirs,pattern = ".se3$",full.names = T)


data_raw <- lapply(files, function(x){
  con<-dbConnect(RSQLite::SQLite(),x)
  temp_data <- dbGetQuery(con,"SELECT * FROM messwerte")
  dbDisconnect(con)
  temp_data$date<-ymd_hms(temp_data$zeit) + 3600
  temp_data
})

kommentar_list <- lapply(files.se3, function(x){
  con<-dbConnect(RSQLite::SQLite(),x)
  temp_data <- dbGetQuery(con,"SELECT * FROM einstellungen")
  dbDisconnect(con)
  temp_data$kommentar
})

colnames_gmsd <- c("date","messwert","mittelwert","minpeakwert","maxpeakwert")
data_list <- lapply(data_raw, function(x) x[colnames_gmsd])
for(i in seq_along(kommentar_list)){
  data_list[[i]]$kommentar <- kommentar_list[[i]]
}

data <- do.call("rbind",data_list)
data[,!grepl("date|kommentar",colnames(data))] <- sapply(data[,!grepl("date|kommentar",colnames(data))],function(x) ifelse(x > 9e+07, NA, x))

ggplot(data[grep("passata",data$kommentar),])+geom_line(aes(date,messwert))+facet_wrap(~kommentar,scales="free_x")

kommentare <- unique(data$kommentar)
test <- PPC_calc(data[grep(kommentare[15],data$kommentar),],roll_width = 30,plot=T,perc=0.2)

test <- data[grep(kommentare[16],data$kommentar),]

range(test$date)
##########################
#funktion
############################
PPC_calc <- function(data,datelim=NULL,roll_width=5,plot=T,perc=0.1){
  if(is.null(datelim)){
    datelim <- range(data$date)
  }
  sub <- subset(data,date > min(datelim) & date < max(datelim))
sub$p <- zoo::rollapply(sub$mittelwert,roll_width,mean,fill=NA)
sub$pdiff <- c(NA,diff(sub$p))
sub$pdiff2 <- c(diff(sub$p),NA)


top_ID <- which(sub$pdiff >= 0 & sub$pdiff2 <= 0 & sub$p > quantile(sub$p,1-perc,na.rm=T))
bottom_ID <- which(sub$pdiff <= 0 & sub$pdiff2 >= 0 & sub$p < quantile(sub$p,perc,na.rm=T))

sub$pos <- 0
sub$pos[top_ID] <- "top"
sub$pos[bottom_ID] <- "bottom"
sub$period <- 0
period_i <- 1
period_tick <- "0"
for(i in seq_along(sub$pos)){
  if(sub$pos[i] == period_tick){
    if(period_tick == "top"){
      period_i <- period_i +1
    } 
    period_tick <- ifelse(period_tick == "top","bottom","top")
  }
  sub$period[i] <- period_i-1
}

periods <- unique(sub$period)[-1]
A <- rep(NA,length(periods))
T_s <- A
i <- 1

for(i in periods){
  sub_i <- subset(sub,period == i)
  A[i] <- max(sub_i$p[sub_i$pos == "top"],na.rm=T) - min(sub_i$p[sub_i$pos == "bottom"],na.rm=T)
  T_s[i] <- abs(difftime(sub_i$date[sub_i$pos == "top"][1], sub_i$date[sub_i$pos == "bottom"][1],units="secs"))
}
if(plot == T){
plt <- ggplot(sub)+
  geom_line(aes(date,p,col=as.factor(period)),show.legend = F)+
  ggnewscale::new_scale_color()+
  geom_point(data=sub[top_ID,],aes(date,p,col="top"))+
  geom_point(data=sub[bottom_ID,],aes(date,p,col="bottom"))+
  labs(title=unique(sub$kommentar),subtitle=paste("A =",signif(median(A,na.rm = T),2),"Pa, T =",signif(median(T_s,na.rm = T),2),"s"))
print(plt)
}

return(list(A=A,T_s=T_s))
}
