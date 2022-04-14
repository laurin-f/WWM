intervall_s = 1
intervall_min = 0
relais_h = 3 #Pause zwischen inj messungen in stunden
ventil_mins = 6#6 #Zeitraum in dem das ventil offen ist und die inj Kammer misst
pumpe_mins = 1 #how many minutes does the pump pump
kammer_intervall = 30#10 #min
kammer_closing = 5 #min
dyn_on = 1 # is dynament sensor turned on or not
counter_01 = 1 # hilfsvariable um counter zu setzen
counter = 0


hour <- rep(0:1,each=60)
minute <- rep(0:59,2)
date <- ymd_hm(paste("2022.04.14",hour,minute))
inj_T <- (hour %% relais_h == 0 & minute <= (ventil_mins + pumpe_mins + 1 + kammer_closing+3) & minute >= kammer_closing +3)
inj_dyn <- minute <= (ventil_mins + 1 + kammer_closing +3)
inj_ventil <- (minute >= (1 + kammer_closing +3) & minute < (ventil_mins + 1 + kammer_closing +3))
inj_pumpe <- minute >= (ventil_mins + 1 + kammer_closing +3) & minute < (ventil_mins + pumpe_mins + 1 + kammer_closing+3)

dyn_off <- (minute - kammer_closing - 2)  %% kammer_intervall == 0
dyn_on <- (minute + 2)  %% kammer_intervall == 0
chamber_close <- minute %% kammer_intervall == 0
chamber_open <- (minute - kammer_closing) %% kammer_intervall == 0
plot(date,inj_T,type="l")
points(date,inj_dyn & inj_T,col=2)
points(date,inj_ventil-0.05,col=3)
points(date,inj_pumpe-0.1,col=4)
points(date,dyn_on-0.15,col=5)
points(date,dyn_off-0.15,col=6)
points(date,chamber_open-0.15,col=7)
points(date,chamber_close-0.15,col=8)
ggplot()+
  geom_point(data=inj_test,aes(date,CO2_ppm,col="inj"))+
  geom_point(data=chamber_test,aes(date,CO2_ppm,col="chamber"))+
  xlim(c(ymd_h("2022.04.14 00","2022.04.14 04")))
