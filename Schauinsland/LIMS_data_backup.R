
library(pkg.WWM)

packages<-c("stringr","lubridate","ggplot2","RSQLite","RODBC")
check.packages(packages)

channel <- odbcConnect("WINLIMS8")

data<-sqlQuery(channel, "SELECT mst.PROJEKT, mst.Probenart, mst.VAR_ID AS plotID, mst.PROBENBEZ, mst.MST_ID, s.REMARKS, s.SAMPLE_NO, s.PN_DATUM, sp.NRESULT AS value, sp.PA_NAME AS variable, sp.PARAM_UNITS AS unit
                            FROM ((SAMPLE AS s
               INNER JOIN MSTVERWEIS AS mst ON s.MST_ID = mst.MST_ID)
               INNER JOIN SAMPLEPARAM AS sp ON s.SAMPLE_ID = sp.SAMPLE_ID)
               WHERE mst.PROJEKT = '1677DFG_Ma' AND sp.NRESULT IS NOT NULL")
odbcCloseAll()

data$date <- ymd(data$PN_DATUM)
data$tiefe<-(substr(data$PROBENBEZ,5,7))
data$tiefe[data$tiefe == "-WL"]<-80
data$tiefe<-as.numeric(data$tiefe)
data$unit <- str_replace(data$unit,"%","prozent")
data$value[data$variable == "CO2"] <- data$value[data$variable == "CO2"]*10^4
data$unit[data$variable == "CO2"] <-"ppm"
data$variable_unit <- paste(data$variable,data$unit,sep="_")

data_wide <- tidyr::spread(data[c("date","PROBENBEZ","tiefe","plotID","value","variable_unit")],variable_unit,value)

data_agg <- aggregate(data_wide[, !colnames(data_wide) %in% c("PROBENBEZ","date","tiefe","plotID")],as.list(data_wide[,c("date","tiefe","plotID")]), mean)

sort(unique(data$PROBENBEZ))
#zeitreihe
CO2_time_plot <- ggplot(data_agg)+geom_path(aes(date,CO2_ppm,col=as.factor(tiefe),linetype=plotID))
CH4_time_plot <- ggplot(data_agg)+geom_path(aes(date,CH4_ppm,col=as.factor(tiefe),linetype=plotID),show.legend = F)
egg::ggarrange(CO2_time_plot,CH4_time_plot,ncol=1)

#tiefenprofil
ggplot(subset(data,variable%in%c("CH4","CO2")))+geom_point(aes(value,tiefe,col=plotID))+facet_wrap(~variable,scales = "free_x")+geom_hline(yintercept = 0)+ylim(c(-22,22))
ggplot(subset(data,variable=="CO2"))+geom_point(aes(value,tiefe,col=variable))+facet_wrap(~plotID)



unique(data$date)
colnames(Data)
odbcGetInfo(channel)




