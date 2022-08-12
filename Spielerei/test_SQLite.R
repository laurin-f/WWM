library(RSQLite)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
sqlpath<-paste0(hauptpfad,"Daten/aufbereiteteDaten/SQLite/")
con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,"GGA.db"))
#con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,"dynament.db"))

#falls tabelle in db nicht vorhanden wird sie hier erstellt
createquery<-paste0("CREATE TABLE IF NOT EXISTS ",testtable," (date_int INTEGER PRIMARY KEY",
                    paste(",",colnames(dyn)[-1],"REAL",collapse = ""),")")
dbExecute(con, createquery)

print(paste("appending",length(dyn.list.sub),"files to Database"))

#Primary Key dopplungen checken
db_duplicates <- dbGetQuery(con,paste("SELECT * FROM",testtable,"WHERE date_int >=",min(as.numeric(dyn$date_int)),"AND date_int <=",max(as.numeric(dyn$date_int))))

#falls im zeitraum der neuen messungen bereits werte in der db sind
if(nrow(db_duplicates) > 0){
  print("primary duplicates")
  #die neuen werte aufteilen in den gedoppelten zeitraum und den der nicht in der db auftaucht
  dyn_duplicate <- dyn[dyn$date_int %in% db_duplicates$date_int,]
  dyn2 <- dyn[!dyn$date_int %in% db_duplicates$date_int,]

  #dopplungen in db mit neuen werten joinen und NAs überschreiben
  db_duplicates_join <- rquery::natural_join(db_duplicates,dyn_duplicate,by="date_int")

  #die werte in string für die Query schreiben
  values_NA <- paste(apply(db_duplicates_join,1,paste,collapse = ", "),collapse="), (")
  values_NULL <- str_replace_all(values_NA, c("NA"="NULL"))
  replace_query <- paste0("REPLACE INTO ",testtable," (",paste(colnames(dyn),collapse = ", "),") VALUES (",values_NULL,");")
  #cases in db ersetzten
  rs <- dbSendQuery(con,replace_query)
  #dbClearResult(rs)
}
rs
dbWriteTable(con,testtable,dyn2,append=T)
dbread <- dbReadTable(con,testtable)
dbread
