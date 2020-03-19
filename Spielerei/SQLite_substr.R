library(pkg.WWM)
check.packages(c("RSQLite"))
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
sqlpfad<-paste0(hauptpfad,"Daten/aufbereiteteDaten/SQLite/")


con<-RSQLite::dbConnect(RSQLite::SQLite(),paste0(sqlpfad,"test.db"))
r<-1000:5000
l<-5000:9000
length(l)
id<-paste0(r,l)
var<-"hi"
df<-data.frame(id=id,var=var)
dbWriteTable(con,"test",df,overwrite=T)

ids <- data.frame(r=1000:1010,l=5000:5010)
#substr(character, from, length) 
#CAST(x, AS type)
substr_r <-  "CAST(substr(id,1,4) AS integer)"
substr_l <-  "CAST(substr(id,5,4) AS integer)"

dbGetQuery(con,paste("SELECT substr(id,1,4), substr(id,5,4) FROM test WHERE ",substr_r,">", min(ids$r),"AND",substr_r,"<",max(ids$r),"AND",substr_l,">",min(ids$l),"AND",substr_l,"<",max(ids$l)))
