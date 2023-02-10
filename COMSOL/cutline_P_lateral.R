hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
comsolpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/") 


detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)
theme_set(theme_bw())

data <- read.table(paste0(comsolpfad,"cutline_P_lateral.txt"),skip = 8)

colnames(data) <- c("cl","x","y","u","v","p")
data$x
data$probe <- paste("profile",ifelse(data$x > 2.5,1,2))
data$tiefe <- (data$y - 1) *100
ggplot(data)+
  geom_col(aes(v,tiefe,fill="vertical"),orientation = "y",width=1)+
  geom_col(aes(u,tiefe,fill="lateral"),orientation = "y",width=1)+
  geom_vline(xintercept = 0,linetype=2,col="grey")+
  scale_fill_brewer(type="qual")+
  labs(x = "flow (m/s)",y = "depth (cm)",fill="")+
  facet_wrap(~probe,ncol=2)+
  ggsave(paste0(hauptpfad,"Dokumentation/Praesentationen/Hottest_results_23_01/cutline_P_lateral.png"),width = 5,height = 2)
