#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad<-paste0(hauptpfad,"Daten/Urdaten/SChauinsland/")
plotpfad<-paste0(hauptpfad,"/Daten/Ergebnisse/Schauinsland/")
sqlpfad<-paste0(hauptpfad,"Daten/aufbereiteteDaten/SQLite/")
metapfad<-paste0(hauptpfad,"Daten/Metadaten/Schauinsland/")
codepfad<-paste0(hauptpfad,"Programme/Eigenentwicklung/Funktionen/")

source(paste0(codepfad,"check_packages.R"))

#Packages laden
packages<-c("stringr","lubridate","ggplot2","RSQLite","readxl","dplyr")
check.packages(packages)

Em50.update.db<-function(){

  if(file.exists(paste0(datapfad,"db_log.txt"))){
    files.old<-read.csv(paste0(datapfad,"db_log.txt"),stringsAsFactors = F)
  }else{
    files.old<-NULL
  }
  
  logger_colnames<-(read_xlsx(paste0(metapfad,"Gassammler_liste.xlsx"),sheet=3))
  tiefen_liste<-read_xlsx(paste0(metapfad,"Gassammler_liste.xlsx"))
  
  files<-list.files(paste0(datapfad,"Em50Data"),".*",full.names = T)
  files.new<-files[!files %in% files.old$x]
  x<-files[7]
  if(length(files.new)>0){
    
    print(paste("reading",length(files.new),"new files"))
    data<-lapply(files.new,function(x){
      
      logger_id<-str_extract(x,"(?<=/)(A|B)-(1|2)(?=\\s\\d)")
      colnames<- logger_colnames %>% pull(logger_id) %>% na.omit()
      
      sensor_id<-str_extract(colnames,"[A-Z]*\\d+$")
      
      tiefen<-sapply(sensor_id,function(x){
        tiefen_liste$`Tiefe [cm]`[tiefen_liste$id==x]})
      plotAB<-str_extract(logger_id,"(A|B)")
      colnames_tiefe<-paste(colnames,tiefen,plotAB,sep="_")
      
      dateityp<-str_extract(x,"\\..*")
      if(dateityp==".xls"){
        
        
        line1<-read_xls(x,n_max=1)
        n_cols<-ncol(line1)
        data.x<-read_xls(x,skip=2,col_types = c("date", rep("numeric",n_cols-1)))
      }else if(dateityp==".txt"){
      
        data.x<-read.csv(x,sep="\t",dec=",",stringsAsFactors = F,na.strings = "* * * ")
        
        data.x$Measurement.Time <- mdy_hm(data.x$Measurement.Time)
        
        data.x[,grep("vwc",colnames)+1]<-data.x[,grep("vwc",colnames)+1]*100
      }
        
        
        if(ncol(data.x) > (length(colnames)+1)){
          data.x<-data.x[,1:(length(colnames)+1)]
        }else if(ncol(data.x) < (length(colnames)+1)){
          data.x[tail(colnames_tiefe,1)]<-NA
        }
        
        colnames(data.x)<-c("date",colnames_tiefe)
        
        
        
          
        n_NA<-apply(data.x, 1, function(x){length(which(is.na(x)))})
        data.x<-data.x[n_NA<5,]
        return(data.x)}
      )
    
    logger_ids<-str_extract(files.new,"(?<=/)(A|B)-(1|2)(?=\\s\\d+[A-z]+\\d+)")
    
    
    data_rbind<-lapply(unique(logger_ids),function(x){
      element<-which(logger_ids==x)
      if(length(element)>1){
        logger.x <- data[element]
        data.logger.x<-do.call("rbind",logger.x)
        data.logger.x[order(data.logger.x$date),]
      }else{
        data[element]
      }
    })
    
    data_wide<-Reduce("full_join",data_rbind)
    data_wide<-data_wide[order(data_wide$date),]
    
    con<-dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"Em50_data.db"))
    print("appending to Database")
    dbWriteTable(con,"Em50",data_wide,append=T)
    dbDisconnect(con)
    
    write.csv(files,
              paste0(datapfad,"db_log.txt"),row.names = F)
  }
}


read.Em50<-function(format="long",query="SELECT datetime(date,'unixepoch') AS datetime,* FROM Em50"){
  Em50.update.db()
  con<-dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"Em50_data.db"))
  data_wide<-dbGetQuery(con,query)
  data_wide$date<-ymd_hms(data_wide$datetime)
  data_wide<-data_wide[,-1]
  dbDisconnect(con)
  if(format=="long"){
    data_long<-tidyr::gather(data_wide,"key","value",-date)
    
    data_long$tiefe<-as.numeric(str_extract(data_long$key,"-?\\d+(?=_[A|B]$)"))
    data_long$unit<-str_extract(data_long$key,"^[A-Z|a-z]+")
    data_long$plot<-str_extract(data_long$key,"[A|B]$")
    
    # key.split<-str_split(data_long$key,"_",simplify = T)
    # sensor_id <- key.split[,2]
    # replikat.1 <-paste0("EM",seq(1,11,2))
    # replikat.2 <-paste0("EM",seq(2,12,2))
    # data_long$unit[data_long$unit=="vwc"&sensor_id%in% replikat.1]<-"vwc1"
    # data_long$unit[data_long$unit=="vwc"&sensor_id%in% replikat.2]<-"vwc2"
    # 
    # 
    # data_long <- data_long[,-2]
    # data_long<-tidyr::spread(data_long,unit,value)
    return(data_long)
  }else{
    return(data_wide)
  }
}

