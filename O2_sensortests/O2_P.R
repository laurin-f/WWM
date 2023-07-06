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

data_Pico <- read_Pico("230705_P_test.csv",long = T,smooth = F)
  
data_P <- read_PP(range(data_Pico$date))
data_P1 <- subset(data_P, id == 1) 

data <- merge(data_P1,data_Pico)

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

data_agg <- data %>% 
  mutate(date = round_date(date,"mins")) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean))


ggplot(data_agg)+
  geom_line(aes(date,P))
