


#' @title Funktion um alle .zip dateien in einem Ordner incl. Unterordnern zu entpacken
#'
#' @description  Alle .zip dateien in einem Ordner incl. Unterordnern werden entpackt und im Unterordner unzipped gespeichert
#'
#' @param path the path that the zip files are in
#'
#' @return nothing returned
#'
#' @export
#'
#' @examples unzip.files("H:/FVA-Projekte/P01677_WindWaldMethan/Daten/Urdaten/GGA/")
unzip.files<-function(path=ggapfad){
  #zip files auflisten
  zips<-list.files(path,pattern=".zip$",
                   recursive = T,full.names = T)

  #Schleife um zu testen ob die Dateien bereits im Ordner unzipped sind
  #und wenn nicht zu enpacken
  for(i in seq_along(zips)){
    #dateien der i-ten zip auflisten
    zip.list<-unzip(zips[i],list=T)

    if(!dir.exists(paste0(path,"unzipped/"))){
      dir.create(paste0(path,"unzipped/"))
    }
    #wenn die Datei nicht im unzipped Ordner steht
    if(!file.exists(paste0(path,"unzipped/",zip.list$Name))){
      #unzippen
      cat(paste("unzipping file:",zip.list$Name,"\n"))
      unzip(zips[i],exdir = paste0(path,"unzipped"))
    }#ende if
  }#ende for
}#ende function



######################################
#read all gga and micro files
#' @title update the GGA database
#' @description all files that are not listed in the db_log folder are formated and imported into the GGA database
#' @param path the path that the unzipped and the db_log folder are in
#' @param sqlpath the path to the database
#'
#' @return nothing returned
#' @export
#' @import stringr
#' @import lubridate
#' @import RSQLite
#' @examples update_GGA.db()
update_GGA.db<-function(table.name=c("gga","micro"),path=ggapfad,sqlpath=sqlpfad){

  #unzip Funktion anwenden
  unzip.files()

  #Spaltennamen die uns interessieren
  cols<-c("X.CO2._ppm","X.CH4._ppm","X.H2O._ppm","X.CH4.d_ppm",
          "X.CO2.d_ppm","GasP_torr","GasT_C","AmbT_C")

  #SPaltennamen wie sie im Datensatz heißen sollen
  colnames<-str_replace_all(cols,c("(^X|\\.|_ppm$)"="","d"="dry"))

  #Query um tabelle in db zu erstellen
  createquery<-paste0("CREATE TABLE IF NOT EXISTS tablename (date_int INTEGER PRIMARY KEY",
                      paste(",",colnames,"REAL",collapse = ""),")")

  #sensornamen
  sensor_names<-table.name
  #schleife um db beider sensoren zu aktualisieren
  for(i in sensor_names){

    #namen der bereits in der db existierenden files laden
    if(file.exists(paste0(path,"db_log/",i,"_files.txt"))){
      files.old<-read.csv(paste0(path,"db_log/",i,"_files.txt"),stringsAsFactors = F)
    }else{
      files.old<-NULL
    }

    #alle Dateien des unzipped Ordners mit gga oder micro im Namen auflisten
    files<-list.files(paste0(path,"unzipped"),
                      pattern = i,full.names = F)
    #neue files auswählen
    files.new<-files[!files %in% files.old$x]

    #falls neue dateien dazugekommen sind
    if(length(files.new)>0){
      print(paste0("reading ",length(files.new), " new ",i," files"))
      #die aufgelisteten Dateien in eine Liste einlesen
      data.list<-lapply(paste0(path,"unzipped/",files.new), read.csv,skip=1)

      #Listen in einen Datensatz zusammenfügen
      data.new<-do.call("rbind",data.list)

      #listen löschen
      rm(data.list)

      #datum formatieren
      data.new$date<-parse_date_time(data.new$Time,"mdYHMS")

      #datum NAs entfernen
      data.new<-data.new[!is.na(data.new$date),]

      #gewünschte spalten auswählen
      data<-data.new[c("date",cols)]

      #date in date_int umbenennen
      colnames(data)<-c("date_int",colnames)
      #data_int als integer
      data$date_int<-as.integer(data$date_int)

      #duplikate entfernen
      data<-data[!duplicated(data$date_int),]

      #mit db verbinden
      con<-odbc::dbConnect(RSQLite::SQLite(),paste0(sqlpath,"GGA.db"))
      #falls tabelle in db nicht vorhanden wird sie hier erstellt
      dbExecute(con, str_replace(createquery,"tablename",i))

      print("appending to Database")
      #tabelle in db aktualisieren
      dbWriteTable(con,name=i,value=data,append=T)

      #disconnect Database
      dbDisconnect(con)

      if(!dir.exists(paste0(path,"db_log/"))){
        dir.create(paste0(path,"db_log/"))
      }

      #files Datei speichern
      write.csv(c(files),
                paste0(path,"db_log/",i,"_files.txt"),row.names = F)
    }#ende if
  }#ende for
}#ende function

#####################################################
#update dynament.db funktion
#' @title update the dynament database
#' @description all files that are not listed in the db_log file are formated and imported into the dynament database
#' @param table.name name of the table in the databese that shall be updated
#' @param path the path that the db_log file and the data are in
#' @param sqlpath the path to the database
#' @param metapath the path to the metadata for correcting the sensor output of a specific sensor id
#' @return nothing returned
#' @export
#' @import stringr
#' @import lubridate
#' @import RSQLite
#' @importFrom rquery natural_join
#' @examples update_dynament.db("dynament_test")
update_dynament.db<-function(table.name="dynament_test",
                             path=dynpfad,
                             sqlpath=sqlpfad,
                             metapath=metapfad_dyn){

  #namen der bereits in der db existierenden files laden
  if(file.exists(paste0(path,"db_log_",table.name,".txt"))){
    files.old<-read.csv(paste0(path,"db_log_",table.name,".txt"),stringsAsFactors = F)
  }else{
    files.old<-NULL
  }

  #alle csv-Dateien aus dem dynament Ordner auflisten
  dyn.files<-list.files(path,".csv",full.names = F)

  #neue files auswählen
  files.new<-dyn.files[!dyn.files %in% files.old$x]

  ###################################################################
  #wenn neue files vorhanden sind...
  if(length(files.new)>0){
    print(paste("loading",length(files.new),"files"))
    #neue files laden
    dyn.list<-lapply(paste0(path,files.new), read.csv,skip=1,stringsAsFactors=F)

    #spaltennamen der neuen files
    if(table.name=="dynament_test"){
      db.colnames<-c("date_int",paste0("CO2_Dyn_",sprintf("%02d",0:21)),"T_C")
    }else if(table.name == "sampler1"){
     db.colnames <- c("date_int",paste0("CO2_tiefe",0:7),"T_C")
    }else if(table.name == "sampler1u2"){
     db.colnames <- c("date_int",paste0("CO2_tiefe",rep(0:7,2),"_smp",rep(1:2,each=8)),"T_C")
    }else if(!is.na(str_extract(table.name,"sampler"))){
     db.colnames <- c("date_int",paste0("CO2_tiefe",0:7))
    }else{
      stop('table.name has to be either \n "dynament_test" \nor \n "samplerX"')
    }



    dyn.colnames<-lapply(paste0(path,files.new), read.csv,nrows=1,header=F,stringsAsFactors=F)

    #die ersten beidne Spalten sind daymonthyear und HourMinSec
    for(i in seq_along(dyn.colnames)){
      dyn.colnames[[i]][1:2]<-c("dmy","HMS")
    }

    ##########################################################
    if(table.name=="dynament_test"){
    #ids der Datumsspalten und aller CO2_Dyn Spalten
      dyn.colnames <- lapply(dyn.colnames, function(x) as.data.frame(t(str_replace(x,"Hygro_S3_Temperatur", "T_C")),stringsAsFactors = F))
      col.ids<-lapply(dyn.colnames,function(x)
        x %in% c("dmy","HMS",db.colnames))
    }

    #########################################################
    #wenn die Tabelle samplerx gewählt wurde
    if(table.name=="sampler1u2"){

      col.ids<-lapply(dyn.colnames,function(x) {
        #die Spaltennamen aus der .csv werden so formatiert das sie der Namen in der .db enstprechen
        #beim sampler werden dabei keine Dyn Nummern abgelegt
        colnames_x_formatiert <- str_replace_all(x,c("_Dyn\\d+"="","dpth"="tiefe","Hygro_S3_Temperatur" = "T_C"))
        #die spaltennamen der db mit dem tablename zusammengefügt entsprechen nun den Spaltennamen der csv
        ids.temp <- colnames_x_formatiert %in% paste(db.colnames,sep="_")
        ids.temp[1:2]<-T
        ids.temp
        })
    }else if(!is.na(str_extract(table.name,"sampler"))){

      col.ids<-lapply(dyn.colnames,function(x) {
        #die Spaltennamen aus der .csv werden so formatiert das sie der Namen in der .db enstprechen
        #beim sampler werden dabei keine Dyn Nummern abgelegt
        colnames_x_formatiert <- str_replace_all(x,c("_Dyn\\d+"="","dpth"="tiefe","smp"="sampler","Hygro_S3_Temperatur" = "T_C_sampler1"))
        #die spaltennamen der db mit dem tablename zusammengefügt entsprechen nun den Spaltennamen der csv
        ids.temp <- colnames_x_formatiert %in% paste(db.colnames,table.name,sep="_")
        ids.temp[1:2]<-T
        ids.temp
        })
    }


    #nur die relevanten Spalten aller tabellen der liste auswählen
    dyn.list.kurz<-dyn.list
    for(i in seq_along(dyn.list)){
        dyn.list.kurz[[i]] <- dyn.list[[i]][,col.ids[[i]]]
        #spaltennamen übernehmen
        colnames(dyn.list.kurz[[i]])<-dyn.colnames[[i]][,col.ids[[i]]]
    }

    #anzahl spalten aller listenelemente
    col.nr<-sapply(dyn.list.kurz,ncol)
    #nur listenelemente mit mehr als 2 spalten übernehmen also mindestens eine CO2_Dyn spalte
    dyn.list.sub<-dyn.list.kurz[which(col.nr > ifelse(table.name=="dynament_test",2,8))]

    if(!is.na(str_extract(table.name,"sampler"))){

      load(paste0(metapath,"korrektur_fm.RData"))#,envir = .GlobalEnv)
      for(i in seq_along(dyn.list.sub)){
      data<-dyn.list.sub[[i]]

      #CO2 von mV in ppm umrechnen
      CO2_cols <- grep("CO2", colnames(data))
      data[, CO2_cols][which(data[,CO2_cols] > 2.7,arr.ind = T)]<-NA
      data[, CO2_cols][which(data[,CO2_cols] < 0.25,arr.ind = T)]<-NA
      data[,CO2_cols] <- (data[,CO2_cols]-0.4)/1.6*5000
      colnames.data.old <- colnames(data)
      colnames(data)<-str_replace(colnames(data), "_dpth\\d_smp\\d$", "")
      ################hiervor muss noch korrigiert werden
      names(fm) <- str_replace(names(fm),"Dyn_","Dyn")
      same.names <- names(fm)[names(fm) %in% colnames(data)]

      #Korrekturfaktoren anwenden
      data[same.names] <-
        sapply(same.names,function(x) predict(fm[[x]],newdata=data.frame(CO2_Dyn=data[[x]])))

      if(table.name == "sampler1u2"){
        colnames(data)<-str_replace_all(colnames.data.old,c("(_Dyn_?\\d{2})"="","dpth"="tiefe", "Hygro_S3_Temperatur" = "T_C"))
      }else{
      #Spaltennamen anpassen
        colnames(data)<-str_replace_all(colnames.data.old,c("(_Dyn_?\\d{2})|(_smp\\d$)"="","dpth"="tiefe", "Hygro_S3_Temperatur" = "T_C"))
      }
      dyn.list.sub[[i]] <- data
      }
      #rm(fm)
    }

    #bei jedem listenelement die nicht vorkommenden spalten als NAs hinzufügen
    for(i in seq_along(dyn.list.sub)){
      add<-db.colnames[!db.colnames %in% colnames(dyn.list.sub[[i]])]
      dyn.list.sub[[i]][add]<-NA
    }

    #gekürtzte liste in eine Tabelle
    dyn<-do.call("rbind",dyn.list.sub)

    if(!is.null(dyn)){
    #datum formatieren
    dyn$date_int<-as.numeric(dmy_hms(paste(dyn$dmy,dyn$HMS)))
    #spalten dmy und HMS weglassen
    dyn<-dyn[db.colnames]
    }
    if(table.name=="dynament_test"){
    #CO2 von mV in ppm umrechnen
    CO2_cols <- grep("CO2", colnames(dyn))
    #9999 werte zu NA
    dyn[, CO2_cols][which(dyn[,CO2_cols] > 10,arr.ind = T)]<-NA
    #0.2 Werte zu NA
    dyn[, CO2_cols][which(dyn[,CO2_cols] < 0.25,arr.ind = T)]<-NA
    #CO2 von mV in ppm umrechnen
    dyn[,CO2_cols] <- (dyn[,CO2_cols]-0.4)/1.6*5000
    }

    #are there date duplicates
    date_duplicate <- duplicated(dyn$date_int)

    #if yes then the cases with duplicates are joined
    #replacing NA values of the duplicated rows with the values in the corresponding cases
    if(any(date_duplicate)){
      dyn <- rquery::natural_join(dyn[date_duplicate,],dyn[!date_duplicate,],by="date_int", jointype = "FULL")
    }

    if(is.null(dyn)){
      print(paste("no new files for",table.name))
    }else{
    #db verbinden
    con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,"dynament.db"))
    #falls tabelle in db nicht vorhanden wird sie hier erstellt
    createquery<-paste0("CREATE TABLE IF NOT EXISTS ",table.name," (date_int INTEGER PRIMARY KEY",
                        paste(",",colnames(dyn)[-1],"REAL",collapse = ""),")")
    dbExecute(con, createquery)

    print(paste("appending",length(dyn.list.sub),"files to Database"))

    #Primary Key dopplungen checken
    db_duplicates <- dbGetQuery(con,paste("SELECT * FROM",table.name,"WHERE date_int >=",min(as.numeric(dyn$date_int)),"AND date_int <=",max(as.numeric(dyn$date_int))))

    #falls im zeitraum der neuen messungen bereits werte in der db sind
    if(nrow(db_duplicates) > 0){
      print("date duplicates")
      #die neuen werte aufteilen in den gedoppelten zeitraum und den der nicht in der db auftaucht
      dyn_duplicate <- dyn[dyn$date_int %in% db_duplicates$date_int,]
      dyn <- dyn[!dyn$date_int %in% db_duplicates$date_int,]

      #dopplungen in db mit neuen werten joinen und NAs überschreiben
      db_duplicates_join <- rquery::natural_join(db_duplicates,dyn_duplicate,by="date_int", jointype = "FULL")

      #die werte in string für die Query schreiben
      values_NA <- paste(apply(db_duplicates_join,1,paste,collapse = ", "),collapse="), (")
      values_NULL <- str_replace_all(values_NA, c("NA"="NULL"))
      replace_query <- paste0("REPLACE INTO ",table.name," (",paste(colnames(dyn),collapse = ", "),") VALUES (",values_NULL,");")
      #cases in db ersetzten
      rs <- dbSendQuery(con,replace_query)
      dbClearResult(rs)
    }


    #Database aktualisieren
    dbWriteTable(con,name=table.name,value=dyn,append=T)

    #disconnect Database
    dbDisconnect(con)
    }
    #files Datei speichern
    write.csv(c(dyn.files),
              paste0(path,"db_log_",table.name,".txt"),row.names = F)
  }else{#ende if files.new
    print(paste(table.name,"is up to date"))
  }
}#ende function


#funktionen anwenden
# update_GGA.db()
# update_dynament.db()
# update_dynament.db(table.name = "sampler1")

#test<-dbGetQuery(con,"SELECT * FROM dynament_test")

#dbRemoveTable(con,"dynament_test")
#################################################
#Funktion um daten aus db abzurufen
#' @title Function to get data from a sql database
#' @description get data from a sql database. A specific time period can be selected
#' @param db.name name of the database
#' for "dynament.db" either \code{"dynament_test"} or \code{"samplerx"}
#' for "GGA.db" either \code{"gga"} or \code{"micro"}
#' @param table.name name of the table in the database
#' @param datelim  limits of the time period that is loaded as character or POSIXct if all Data should be loaded \code{datelim = NULL}
#' @param cols names of the colums that will be loaded. default is all colums: \code{cols = "*"}
#' @param sqlpath path to the database
#' @param korrektur_dyn logical specifies whether dynament values are corrected for the specific sansor id or not if db.name is "dynament.db"
#' @param metapath path to the "korrektur_fm.RData" file for correction factors
#'
#' @return data.frame
#' @export
#' @import lubridate
#' @import RSQLite
#' @examples
#' #Zeitrahmen festlegen
#' datelim<-c("2020-01-15 10:00:00","2020-01-21 10:00:00")
#'
#' #read_db anwenden
#' dynament_raw<-read_db("dynament.db","dynament_test",datelim,korrektur_dyn=F)
#' GGA<-read_db("GGA.db","micro",datelim,"CO2,CO2dry")
read_db <- function(db.name="dynament.db", #name der db
                    table.name="dynament_test", #name der tabelle
                    datelim=NULL, #Zeitrahmen der geladen werden soll
                    cols="*", #Spalten die geladen werden sollen
                    sqlpath=sqlpfad,
                    korrektur_dyn=T,#sollen die Dynament werden korrigiert werden
                    metapath=metapfad_dyn){
  #dbs updaten
  update_fun<-get(paste0("update_",db.name))
  update_fun(table.name)

  #db verbinden
  con<-odbc::dbConnect(RSQLite::SQLite(),paste0(sqlpath,db.name))

  #date_int wird als datum formatiert
  query<-paste0("SELECT datetime(date_int,'unixepoch') AS date,",cols," FROM ",table.name)

  #falls datelim angegeben wurde wird es hier als integer in die query aufgenommen
  if(!is.null(datelim)){
    from_to <- as.numeric(ymd_hms(datelim))
    #falls from oder to nicht ins ymd_hms format passt wird gestoppt
    if(any(is.na(from_to))){
      stop("datelim has to be in the ymd_hms format")
    }
    #datelim in die SQL abfrage einbinden
    query<-paste0(query," WHERE date_int >= ",from_to[1])
    if(length(from_to)==2){
    query<-paste0(query," AND date_int <= ",from_to[2])
    }
  }

  #daten abrufen
  data<-dbGetQuery(con,query)
  #von db trennen
  odbc::dbDisconnect(con)
  #datum formatieren
  data$date<-ymd_hms(data$date)
  #date_int weglassen
  data<-data[,colnames(data)!="date_int"]

  #Spalten die nur aus NAs bestehen weglassen
  na.cols <- apply(data,2,function(x) any(is.na(x)==F))
  data<-data[,na.cols]

  #wenn bei dynament.db die tabelle dynament_test ausgewählt ist können die korrektur_faktoren angewandt werden
  if(db.name=="dynament.db" & table.name == "dynament_test" & korrektur_dyn==T){
    #RData mit Korrekturfaktoren laden
    load(paste0(metapath,"korrektur_fm.RData"))
    #Vektor mit namen die sowohl bei den Korekturfaktoren als auch im Data.frame vorkommen
    same.names <- names(fm)[names(fm) %in% colnames(data)]
    #Sensor nummern für die noch kein Korrekturfaktor vorliegt
    missing.names <- colnames(data)[!colnames(data) %in% c("date",names(fm),"T_C")]
    if(length(missing.names)>0){
      warning(paste("no correction factor for",missing.names,collapse =" "))
    }

    #Kalibrierfunktion anwenden
    data[same.names] <-
      sapply(same.names,function(x) predict(fm[[x]],newdata=data.frame(CO2_Dyn=data[[x]])))
  }#ende if korrektur
  return(data)
}#ende function
