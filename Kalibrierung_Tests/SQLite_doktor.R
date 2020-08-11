library(pkg.WWM)
check.packages(c("RSQLite","lubridate"))
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
sqlpath<-paste0(hauptpfad,"Daten/aufbereiteteDaten/SQLite/")
dynpfad<-paste0(hauptpfad,"Daten/Urdaten/Dynament/")
db_name <- "dynament.db"
table_name <- "dynament_test"

con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,db_name))

#dbRemoveTable(con,"sampler1")
test <- dbReadTable(con,table_name)
#dbSendQuery(con,paste0("ALTER TABLE ",table_name," ADD COLUMN T_C REAL"))


#dyn <- test[,!colnames(test) %in% "CO2_tiefe0"]

# createquery<-paste0("CREATE TABLE IF NOT EXISTS ",table_name," (date_int INTEGER PRIMARY KEY",
#                     paste(",",colnames(dyn)[-1],"REAL",collapse = ""),")")

#dbRemoveTable(con,table_name)
#dbExecute(con, createquery)
#dbWriteTable(con,table_name,dyn,append=T)

#dbGetQuery(con, "PRAGMA table_info(sampler1);")

all_files <- list.files(dynpfad)
files <- str_subset(all_files,"20200811")

#rm_file_from_db(files[1] ,table_name = "sampler1u2")
#sapply(files,rm_file_from_db,table_name = "sampler1u2")
con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,db_name))
query2 <- paste0("SELECT datetime(date_int,'unixepoch') AS date, * FROM ",table_name)
data<-dbGetQuery(con,query2)

db_log <- data.frame(x=list.files(dynpfad,"*.csv$"))
