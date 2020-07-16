datelim <- ymd_hm("2020.06.09 11:00","2020.07.12 16:00")

datelim1 <- dmy_hm("09.06.2020	11:20",	"09.06.2020	12:16")
datelim2 <- dmy_hm("18.06.2020	12:50",	"18.06.2020	14:00")
datelim3 <- dmy_hm("10.07.2020	10:00",	"10.07.2020	12:00")


micro <- read_db("GGA.db","micro",datelim)
micro$date <- round_date(micro$date,"10 secs")
micro2 <- merge(klima,micro,all.x=T)
T_plot <- ggplot(micro2)+
  geom_line(aes(date,AmbT_C,col="micro"))+
  geom_line(aes(date,Ta_2m,col="Turm2"))+
  geom_line(aes(date,Ta_18m,col="Turm18"))+
  geom_line(aes(date,Ta_27m,col="Turm27"))
T_plot+xlim(datelim)
T_plot+xlim(datelim1)
T_plot+xlim(datelim2)
T_plot+xlim(datelim3)
ggplot(micro2)+geom_point(aes(Ta_2m,AmbT_C))

ggplot(micro2)+
  geom_point(aes(date,AmbT_C,col="micro"))+
  geom_point(aes(date,Ta_2m,col="Turm2"))+
  geom_line(aes(date,Ta_18m,col="Turm18"))+
  geom_line(aes(date,Ta_27m,col="Turm27"))

