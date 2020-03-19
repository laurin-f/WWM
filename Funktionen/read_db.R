


#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
ggapfad<-paste0(hauptpfad,"Daten/Urdaten/GGA/")
sqlpfad<-paste0(hauptpfad,"Daten/aufbereiteteDaten/SQLite/")
dynpfad<-paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad<-paste0(hauptpfad,"/Daten/Ergebnisse/Plots/")
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Dynament/")
codepfad<-paste0(hauptpfad,"Programme/Eigenentwicklung/Funktionen/")

source(paste0(codepfad,"check_packages.R"))

#Packages laden
packages<-c("stringr","lubridate","ggplot2","RSQLite","readxl","odbc","reshape2","ggnewscale","xlsx","zoo")
check.packages(packages)


#Funktion um Alle .zip dateien in einem Ordner incl. Unterordnern zu entpacken
unzip.files<-function(path=ggapfad){
  #zip files auflisten
  zips<-list.files(path,pattern=".zip$",
                   recursive = T,full.names = T)
  
  #Schleife um zu testen ob die Dateien bereits im Ordner unzipped sind 
  #und wenn nicht zu enpacken
  for(i in seq_along(zips)){
    #dateien der i-ten zip auflisten
    zip.list<-unzip(zips[i],list=T)
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
update_GGA.db<-function(path=ggapfad,sqlpath=sqlpfad){
  
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
  sensor_names<-c("gga","micro")
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
      con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,"GGA.db"))
      #falls tabelle in db nicht vorhanden wird sie hier erstellt
      dbExecute(con, str_replace(createquery,"tablename",i))
      
      print("appending to Database")
      #tabelle in db aktualisieren
      dbWriteTable(con,name=i,value=data,append=T) 
      
      #disconnect Database 
      dbDisconnect(con)
      
      #files Datei speichern
      write.csv(c(files),
                paste0(path,"db_log/",i,"_files.txt"),row.names = F)
    }#ende if
  }#ende for
}#ende function

#####################################################
#update dynament.db funktion
update_dynament.db<-function(table.name="dynament_test",path=dynpfad,sqlpath=sqlpfad,metapath=metapfad){
  
  #namen der bereits in der db existierenden files laden
  if(file.exists(paste0(path,"db_log_",table.name,".txt"))){
    files.old<-read.csv(paste0(path,"db_log_",table.name,".txt"),stringsAsFactors = F)
  }else{
    files.old<-NULL
  }
  
  #alle csv-Dateien aus dem dynament Ordner auflisten
  dyn.files<-list.files(path,".csv",full.names = T)
  
  #neue files auswählen
  files.new<-dyn.files[!dyn.files %in% files.old$x]
  
  #wenn neue files vorhanden sind...
  if(length(files.new)>0){
    print(paste("loading",length(files.new),"files"))
    #neue files laden
    dyn.list<-lapply(files.new, read.csv,skip=1,stringsAsFactors=F)
    
    #spaltennamen der neuen files
    if(table.name=="dynament_test"){
      db.colnames<-c("date_int",paste0("CO2_Dyn_",sprintf("%02d",0:21)))
    }
    if(!is.na(str_extract(table.name,"sampler"))){
     db.colnames <- c("date_int",paste0("CO2_tiefe",1:7)) 
    }

    
    
    dyn.colnames<-lapply(files.new, read.csv,nrows=1,header=F,stringsAsFactors=F)
    
    #die ersten beidne Spalten sind daymonthyear und HourMinSec
    for(i in seq_along(dyn.colnames)){
      dyn.colnames[[i]][1:2]<-c("dmy","HMS")
    }
    
    if(table.name=="dynament_test"){
    #ids der Datumsspalten und aller CO2_Dyn Spalten 
      col.ids<-lapply(dyn.colnames,function(x) x %in% c("dmy","HMS",db.colnames))
    }
    #wenn die Tabelle samplerx gewählt wurde
    if(!is.na(str_extract(table.name,"sampler"))){
      col.ids<-lapply(dyn.colnames,function(x) {
        ids.temp <- paste0("CO2",str_extract(x,"_tiefe\\d_sampler\\d$")) %in% paste(db.colnames,table.name,sep="_")
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
      
      load(paste0(metapath,"korrektur_fm.RData"),envir = .GlobalEnv)
      for(i in seq_along(dyn.list.sub)){
      data<-dyn.list.sub[[i]]
      
      #CO2 von mV in ppm umrechnen
      data[,-(1:2)][which(data[,-(1:2)] > 10,arr.ind = T)]<-NA
      data[,-(1:2)] <- (data[,-(1:2)]-0.4)/1.6*5000
      colnames.data.old <- colnames(data)
      colnames(data)<-str_replace(colnames(data),"_tiefe\\d_sampler\\d$","")
        ################hiervor muss noch korrigiert werden
       same.names <- names(fm_kal_5000)[names(fm_kal_5000) %in% colnames(data)]
       
       data[same.names] <- 
         sapply(same.names,function(x) predict(fm_kal_5000[[x]],newdata=data.frame(CO2_Dyn=data[[x]])))
      colnames(data)<-str_replace_all(colnames.data.old,c("(_Dyn_\\d{2})|(_sampler\\d$)"=""))
     dyn.list.sub[[i]] <- data
      }
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
    dyn$date_int<-dmy_hms(paste(dyn$dmy,dyn$HMS))
    #spalten dmy und HMS weglassen
    dyn<-dyn[db.colnames]
    }
    if(table.name=="dynament_test"){
    #9999 werte zu NA 
    dyn[,-1][which(dyn[,-1]>10,arr.ind = T)]<-NA
    #CO2 von mV in ppm umrechnen
    dyn[db.colnames[-1]] <- (dyn[db.colnames[-1]]-0.4)/1.6*5000
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
update_GGA.db()
update_dynament.db()
update_dynament.db(table.name = "sampler1")

#test<-dbGetQuery(con,"SELECT * FROM dynament_test")

#dbRemoveTable(con,"dynament_test")
#################################################
#Funktion um daten aus db abzurufen
read_db <- function(db.name="dynament.db", #name der db
                    table.name="dynament_test", #name der tabelle
                    datelim=NULL, #Zeitrahmen der geladen werden soll
                    cols="*", #Spalten die geladen werden sollen
                    sqlpath=sqlpfad,
                    korrektur_dyn=T,#sollen die Dynament werden korrigiert werden
                    metapath=metapfad){
  #dbs updaten
  update_dynament.db()
  update_GGA.db(table.name)
  
  #db verbinden
  con<-dbConnect(RSQLite::SQLite(),paste0(sqlpath,db.name))
  
  #date_int wird als datum formatiert
  query<-paste0("SELECT datetime(date_int,'unixepoch') AS date,",cols," FROM ",table.name)
  
  #falls datelim angegeben wurde wird es hier als integer in die query aufgenommen
  if(!is.null(datelim)){
    from <- as.numeric(ymd_hms(datelim[1]))
    to <- as.numeric(ymd_hms(datelim[2]))
    query<-paste0(query," WHERE date_int >= ",from," AND date_int <= ",to)
  }
  
  #daten abrufen
  data<-dbGetQuery(con,query)
  #datum formatieren
  data$date<-ymd_hms(data$date)
  #date_int weglassen
  data<-data[,colnames(data)!="date_int"]
  
  #Spalten die nur aus NAs bestehen weglassen
  na.cols <- apply(data,2,function(x) any(is.na(x)==F))
  data<-data[,na.cols]
  
  if(db.name=="dynament.db" & table.name=="dynament_test" & korrektur_dyn==T){
    load(paste0(metapath,"korrektur_fm.RData"),envir = .GlobalEnv)
    same.names <- names(fm_kal_5000)[names(fm_kal_5000) %in% colnames(data)]
    data[same.names] <- 
      sapply(same.names,function(x) predict(fm_kal_5000[[x]],newdata=data.frame(CO2_Dyn=data[[x]])))
  }
  return(data)
}#ende function

