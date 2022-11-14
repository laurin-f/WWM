detach("package:pkg.WWM", unload = TRUE)
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg")
check.packages(packages)

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
sqlpfad<-paste0(hauptpfad,"Daten/aufbereiteteDaten/SQLite/")
unzipped_path <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/GGA/unzipped/"
db_log_path <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/GGA/db_log/"
files <- list.files(unzipped_path,pattern = "gga_2022-11-\\d[5-90]_f000\\d.txt",full.names = T)
files_all <- list.files(unzipped_path,pattern = "gga_.+txt",full.names = T)

data_list <- lapply(files,read.csv,skip=1)

lines_list <- lapply(files,readLines)

####################################
#Identify Time trouble
data <- do.call(rbind,data_list)
data$date <- parse_date_time(data$Time,"mdYHMS")

range(data$date)
files

t_diff <- which(diff_time(data$date)<0)
#t_diff <- which(diff_time(data$date)<1)
plot(diff_time(data$date))
data$date[t_diff+c(-1:1)]

sub <- data[t_diff+c(-1:1),]

change_time <- ymd_hm("22.11.06 02:00","22.11.10 09:42")
date_sub <- ymd_h("22.11.06 00","22.11.06 02")
range(which(daterange_id(data,date_sub)))
ggplot(data)+
  geom_path(aes(date,X.CO2._ppm))+
  xlim(ymd_h("22.11.06 00","22.11.06 02"))

################################
#change Time in File where time drops
i <- 1

files[i]  
data_i <- data_list[[i]]
date_raw <- parse_date_time(data_i$Time,"mdYHMS")
change <- which(diff_time(date_raw)<0)
date <- date_raw
date[change:length(date)] <- date_raw[change:length(date)] + 3600
plot(date_raw)
lines(date)

Time <- format(date,"%m/%d/%Y %H:%M:%S")
lines_i <- lines_list[[i]]

lines_i[-c(1:2,length(lines_i))] <- str_replace(lines_i[-c(1:2,length(lines_i))],"\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}",Time)

#writeLines(lines_i,files[i])

########################
#change other files

for(j in 2:length(data_list)){
  print(j)
  data_j <- data_list[[j]]
  date_raw_j <- parse_date_time(data_j$Time,"mdYHMS")
  
  date_id <- date_raw_j >= change_time[1] & date_raw_j <= change_time[2]
  if(any(date_id)){
    date_j <- ifelse(date_id,date_raw_j + 3600,date_raw_j) %>% 
      as_datetime()
    Time_j <- format(date_j,"%m/%d/%Y %H:%M:%S")
    lines_j <- lines_list[[j]]
    lines_j[-c(1:2,length(lines_j))] <- str_replace(lines_j[-c(1:2,length(lines_j))],"\\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}",Time_j)
    #writeLines(lines_j,files[j])
  }
}

files_short <- list.files(unzipped_path,pattern = "gga_2022-11-\\d[5-90]_f000\\d.txt",full.names = F)

#rm_file_from_db(filename = files_short,table_name = "gga",db_name = "GGA.db",db_log_path = db_log_path,file_path = unzipped_path)


test <- read_GGA(datelim = change_time,table.name = "gga")



#################
#delete datelim from db
table_name <- "gga"
db_name <- "GGA.db"
query<-paste0("DELETE FROM ",table_name)
#query2 <- paste0("SELECT datetime(date_int,'unixepoch') AS date,",cols," FROM ",table_name)
#falls datelim angegeben wurde wird es hier als integer in die query aufgenommen

from <- as.numeric(max(data$date))

#falls from oder to nicht ins ymd_hms format passt wird gestoppt

#datelim in die SQL abfrage einbinden
query_from<-paste0(query," WHERE date_int >= ",from)#," AND date_int <= ",to)
# query2<-paste0(query2," WHERE date_int >= ",from," AND date_int <= ",to)

con<-DBI::dbConnect(RSQLite::SQLite(),paste0(sqlpfad,db_name))
#daten abrufen
rs <- DBI::dbSendQuery(con,query_from)
DBI::dbClearResult(rs)
DBI::dbDisconnect(con)
