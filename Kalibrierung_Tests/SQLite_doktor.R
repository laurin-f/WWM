library(RSQLite)
library(lubridate)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
sqlpath<-paste0(hauptpfad,"Daten/aufbereiteteDaten/SQLite/")
dynpfad<-paste0(hauptpfad,"Daten/Urdaten/Dynament/")
db_name <- "dynament.db"
table_name <- "sampler1"
table.name <- "sampler1"
con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,db_name))

#dbRemoveTable(con,"sampler1")
#test <- dbReadTable(con,table_name)
#dbSendQuery(con,paste0("ALTER TABLE ",table_name," ADD COLUMN T_C REAL"))


#dyn <- test[,!colnames(test) %in% "CO2_tiefe0"]

# createquery<-paste0("CREATE TABLE IF NOT EXISTS ",table_name," (date_int INTEGER PRIMARY KEY",
#                     paste(",",colnames(dyn)[-1],"REAL",collapse = ""),")")

#dbRemoveTable(con,table_name)
#dbExecute(con, createquery)
#dbWriteTable(con,table_name,dyn,append=T)

#dbGetQuery(con, "PRAGMA table_info(sampler1);")


rm_file_from_db <- function(filename,
                            table_name = "sampler1",
                            db_name = "dynament.db",
                            path = dynpfad){
  
  file <- read.csv(paste0(path,filename),stringsAsFactors = F)
  file <- file[-1,]
  date <- dmy_hms(paste(file[,1],file[,2]))
  datelim <- range(date,na.rm = T)
  
  query<-paste0("DELETE FROM ",table_name)
  #query2 <- paste0("SELECT datetime(date_int,'unixepoch') AS date,",cols," FROM ",table_name)
  #falls datelim angegeben wurde wird es hier als integer in die query aufgenommen

  from <- as.numeric(ymd_hms(datelim[1]))
  to <- as.numeric(ymd_hms(datelim[2]))
  #falls from oder to nicht ins ymd_hms format passt wird gestoppt

  #datelim in die SQL abfrage einbinden
  query<-paste0(query," WHERE date_int >= ",from," AND date_int <= ",to)
 # query2<-paste0(query2," WHERE date_int >= ",from," AND date_int <= ",to)

  con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,db_name))
  #daten abrufen
  rs <- dbSendQuery(con,query)
  dbClearResult(rs)
  dbDisconnect(con)
  
  #auch aus db_log entfernen
  db_log<-read.csv(paste0(path,"db_log_",table_name,".txt"),stringsAsFactors = F)
  db_log_new <- db_log[!db_log$x %in% filename,]
#files Datei speichern
write.csv(db_log_new,
          paste0(path,"db_log_",table_name,".txt"),row.names = F)
}
rm_file_from_db()

con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,db_name))
query2 <- paste0("SELECT datetime(date_int,'unixepoch') AS date, * FROM ",table_name)
data<-dbGetQuery(con,query2)

db_log <- data.frame(x=list.files(dynpfad,"*.csv$"))
