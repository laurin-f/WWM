
csv <- read.csv(paste0(datapfad,"^b0_work_20200504_103238_stufe_3u3_schlauchwechsl_3u3.csv"),skip=1)
csv$date <- dmy_hms(paste(csv[,1],csv[,2]))
db <- read_db(datelim=range(csv$date))
plot(csv$date,(csv[,11]-0.4)/1.6*5000)
lines(db$date,db$CO2_Dyn_08)

test <- as.numeric(csv$date)
range(lubridate::as_datetime(test))
range(csv$date)
