hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad<- paste0(hauptpfad,"Daten/Urdaten/O2_grove/")
datapfad_Pico<- paste0(hauptpfad,"Daten/Urdaten/Picolog/")
datapfad_testo<- paste0(hauptpfad,"Daten/Urdaten/Testo_174H/")
plotpfad_O2_test <- paste0(hauptpfad,"Dokumentation/Berichte/plots/O2_test/")


detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

#files <- list.files(datapfad,"\\d{6}.*.TXT",full.names = T)

data_Pico <- read_Pico(c("230705_P_test.csv","230705_T.csv"),long = T,smooth = F)
  
names(data_Pico)
data_P <- read_PP(range(data_Pico$date))
data_P1 <- subset(data_P, id == 1) 

data_ges <- merge(data_P1,data_Pico)

range(data_Pico$date)
#Rauschplot <- 
  ggplot(sub_daterange(data_Pico,ymd_hm("23.07.05 16:55","23.07.05 17:00")))+
  geom_line(aes(date, O2, col = sensor))+
  geom_line(aes(date, O2, col = sensor))+
    labs(y = expression(O[2]~"(%)"),x = "")+
  #facet_wrap(~sensor,ncol=1,scales = "free_y")+
    ggsave(paste0(plotpfad_O2_test,"O2_Grundrauschen.png"),width = 7,height = 6)

data <- subset(data_ges,file == 1)

P_plot <- ggplot(data)+
  geom_line(aes(date,P))+
  theme(axis.title.x = element_blank())+
  labs(y="P (Pa)")

T_plot <- ggplot(data)+
  geom_line(aes(date,T_C))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y="T (°C)")
rh_plot <- ggplot(data)+
  geom_line(aes(date,rh))+
  theme(axis.title.x = element_blank())+
  labs(y="RH (%)")

O2_plot <- ggplot(data)+
  geom_line(aes(date,O2,col=sensor))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y=expression(O[2]~"(%)"))
O2_plot_facets <- ggplot(data)+
  geom_line(aes(date,O2,col=sensor))+
  facet_wrap(~sensor,ncol=1,scales = "free_y")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y=expression(O[2]~"(%)"))

ggpubr::ggarrange(O2_plot,
                  P_plot + theme(axis.text.x = element_blank()),
                  T_plot,rh_plot,ncol = 1,align = "v",common.legend = T,legend = "right",heights = c(3,2,1.2,1.5))+
  ggsave(paste0(plotpfad_O2_test,"Versuch6_P.png"),width = 7,height = 6)

ggpubr::ggarrange(O2_plot_facets,P_plot,ncol = 1,align = "v",common.legend = T,legend = "right",heights = c(3,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch6_P_detail.png"),width = 7,height = 6)

##################
#T-Versuch P änderung

data2 <- subset(data_ges,file == 2)
data2 <- data2 %>% 
  group_by(sensor) %>% 
  mutate(O2 = RcppRoll::roll_mean(O2,30,fill = NA),
         P =  RcppRoll::roll_mean(P,5,fill = NA))

P_plot2 <- ggplot(data2)+
  geom_line(aes(date,P))+
  theme(axis.title.x = element_blank())+
  labs(y="P (Pa)")

T_plot2 <- ggplot(data2)+
  geom_line(aes(date,T_C))+
  theme(axis.title.x = element_blank())+
  labs(y="T (°C)")
rh_plot2 <- ggplot(data2)+
  geom_line(aes(date,rh))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y="RH (%)")

O2_plot2 <- ggplot(data2)+
  geom_line(aes(date,O2,col=sensor))+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y=expression(O[2]~"(%)"))
O2_plot2_facets <- ggplot(data2)+
  geom_line(aes(date,O2,col=sensor))+
  facet_wrap(~sensor,ncol=1,scales = "free_y")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y=expression(O[2]~"(%)"))

ggpubr::ggarrange(O2_plot2,
                  P_plot2 + theme(axis.text.x = element_blank()),
                  T_plot2,ncol = 1,align = "v",common.legend = T,legend = "right",heights = c(3,2,1.5))#+
  #ggsave(paste0(plotpfad_O2_test,"Versuch5_T_P.png"),width = 7,height = 6)

ggplot(data2)+
  geom_point(aes(P,O2,col=T_C))+
  facet_wrap(~sensor)
ggplot(data2)+
  geom_point(aes(T_C,O2,col=P))+
  facet_wrap(~sensor)

ggpubr::ggarrange(O2_plot2_facets,P_plot2,ncol = 1,align = "v",common.legend = T,legend = "right",heights = c(3,1))#+
  #ggsave(paste0(plotpfad_O2_test,"Versuch6_P_detail.png"),width = 7,height = 6)



data_agg <- data %>% 
  mutate(date = round_date(date,"mins")) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean))


ggplot(data_agg)+
  geom_line(aes(date,P))
