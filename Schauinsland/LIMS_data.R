
library(pkg.WWM)

packages<-c("stringr","lubridate","ggplot2","RSQLite","RODBC","egg","odbc","readxl","tidyr","dplyr")
check.packages(packages)

# channel <- odbcConnect("WINLIMS8")
# 
# data<-sqlQuery(channel, "SELECT mst.PROJEKT, mst.Probenart, mst.VAR_ID AS plot, mst.PROBENBEZ, mst.MST_ID, s.REMARKS, s.SAMPLE_NO, s.PN_DATUM, sp.NRESULT AS value, sp.PA_NAME AS variable, sp.PARAM_UNITS AS unit
#                             FROM ((SAMPLE AS s
#                INNER JOIN MSTVERWEIS AS mst ON s.MST_ID = mst.MST_ID)
#                INNER JOIN SAMPLEPARAM AS sp ON s.SAMPLE_ID = sp.SAMPLE_ID)
#                WHERE mst.PROJEKT = '1677DFG_Ma' AND sp.NRESULT IS NOT NULL")
# odbcCloseAll()
# 
# data$date <- ymd(data$PN_DATUM)
# data$tiefe<-(substr(data$PROBENBEZ,5,7))
# data$tiefe[data$tiefe == "-WL"]<-80
# data$tiefe<-as.numeric(data$tiefe)
# data$unit <- str_replace(data$unit,"%","prozent")
# data$value[data$variable == "CO2"] <- data$value[data$variable == "CO2"]*10^4
# data$unit[data$variable == "CO2"] <-"ppm"
# 
data <- read_LIMS()
data$variable_unit <- paste(data$variable,data$unit,sep="_")

data_wide_raw <- tidyr::spread(data[c("date","PROBENBEZ","tiefe","plot","value","variable_unit","REMARKS")],variable_unit,value)

data_agg <- aggregate(data_wide_raw[, !colnames(data_wide_raw) %in% c("PROBENBEZ","date","tiefe","plot","REMARKS")],as.list(data_wide_raw[,c("date","tiefe","plot")]), mean)


data_wide <- tidyr::pivot_wider(data_agg,c("date","plot"),names_from = tiefe,values_from = c(CO2_ppm,CH4_ppm))

data_wide$CO2_ppm_80[data_wide$plot == "A"] <- data_wide$CO2_ppm_80[data_wide$plot == "B"]

snowdates <- data_wide$date[data_wide$CO2_ppm_4 - data_wide$CO2_ppm_80 > 100 | data_wide$CO2_ppm_13 - data_wide$CO2_ppm_80 > 100|data_wide$CO2_ppm_24 - data_wide$CO2_ppm_80 > 100]
data_wide$snow <- ifelse(data_wide$CO2_ppm_13 - data_wide$CO2_ppm_80 > 100|data_wide$CO2_ppm_24 - data_wide$CO2_ppm_80 > 100,1,0)

snow_df <- subset(data_wide, snow==1)


range(data_agg$date)
lims <- ymd(c("2021-10-01",NA))
#zeitreihe
CO2_time_plot <- ggplot(data_agg)+
  geom_point(aes(date,CO2_ppm,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_ppm,col=as.factor(tiefe)))+
  xlim(lims)+
  
  facet_wrap(~plot)
CO2_time_plot+geom_vline(data=snow_df,aes(xintercept=date))
CH4_time_plot <- ggplot(data_agg)+
  geom_point(aes(date,CH4_ppm,col=as.factor(tiefe)),show.legend = F)+
  geom_line(aes(date,CH4_ppm,col=as.factor(tiefe)),show.legend = F)+
  facet_wrap(~plot)

ggpubr::ggarrange(CO2_time_plot,CH4_time_plot,ncol=1,common.legend = T,legend = "right")
ggplot(subset(data_agg,tiefe > 0))+geom_point(aes(date,tiefe,col=as.factor(tiefe)))+facet_wrap(~plot)

data_agg %>% group_by(tiefe,plot)%>% summarise(n=length(tiefe))
test <- ymd_h(c("2021-10-01-10","2021-12-01-10"))
test <- c(ymd_h("2021-10-01-10"),max(data_agg$date))
class(test)
#tiefenprofil

ggplot(subset(data_agg,date %in% snow_df$date))+
  geom_line(aes(CO2_ppm,tiefe,col=as.factor(date)),orientation = "y")+
  facet_wrap(~plot)
ggplot(subset(data,variable%in%c("CH4","CO2")))+geom_point(aes(value,tiefe,col=plot))+facet_wrap(~variable,scales = "free_x")+geom_hline(yintercept = 0)+ylim(c(-22,22))
ggplot(subset(data,variable=="CO2"))+geom_point(aes(value,tiefe,col=variable))+facet_wrap(~plot)

ggplot(subset(data_agg,!date %in% snowdates))+
  geom_line(aes(CO2_ppm,tiefe,col=as.factor(date)),orientation = "y")+
  facet_wrap(~plot)
ggplot(subset(data_agg,date %in% snowdates))+
  geom_line(aes(CO2_ppm,tiefe,col=as.factor(date)),orientation = "y")+
  facet_wrap(~plot)





