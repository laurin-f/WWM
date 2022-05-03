#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","gganimate")
check.packages(packages)

pp_bemerkungen <-  readODS::read_ods(paste0(metapfad_PP,"PP_Kammer_Bemerkungen.ods"))
pp_bemerkungen$Start <- dmy_hm(pp_bemerkungen$Start)
pp_bemerkungen$Ende <- dmy_hm(pp_bemerkungen$Ende)

pp_chamber <- readODS::read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)
i<-11
#i<-nrow(pp_chamber)
#datelim <- t(pp_chamber[i,c("Start","Ende")]+(3600*2*c(-1,1)))
datelim <- c("2022-04-27 10:55:00","2022-04-27 15:00:00")
pp_bemerkungen$Start <- pp_bemerkungen$Start - 3600
pp_bemerkungen <- sub_daterange(pp_bemerkungen,datelim,"Start")

data <- read_PP(datelim = datelim,format="long")
data <- subset(data, id %in% 1:4)

dt <- round(median(diff_time(data$date[data$id == 1]),na.rm=T),2)
data <- data %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,5/dt,fill=NA))

#messung i=9 am Anfang alle P Sonden zusammen


ggplot(data)+geom_line(aes(date, PPC, col=id))
data_sub <- sub_daterange(data,ymd_hms("2022-04-12 14:00:00","2022-04-12 14:02:00"))
data_sub_2d <- sub_daterange(data,ymd_hms("2022-04-27 13:00:20","2022-04-27 13:02:20"))
data_sub_1d <- sub_daterange(data,ymd_hms("2022-04-27 13:09:30","2022-04-27 13:11:30"))
ggplot(subset(data_sub_2d,id %in% 1:4 ))+
  geom_line(aes(date,P_filter,col=id))
  date_start <- min(data_sub$date)
t_diff <- 1
datelim <- c(date_start,date_start + 60)



############
#gganimate

library(gganimate)
data_sub$date_int <- as.numeric(data_sub$date)
animation_2d <- data_sub_2d %>%
  ggplot(aes(date,P_filter,col=id)) +
  geom_line() +
  #scale_y_continuous(limits = c(0, most_sold), breaks = seq(0,1e5, by = 5000)) +
  #theme_bw() +
  ## gganimate functionality starts here
  labs(x = "", y = "Pressure (Pa)", col = "") +
  transition_reveal(date) +
  ease_aes("linear")

animate(animation_2d, height = 2, width =6,unit="in",res=150,duration=12)
anim_save(paste0(plotpfad_PPchamber,"2D_PP_reveal.gif"))

animation_1d <- data_sub_1d %>%
  ggplot(aes(date,P_filter,col=id)) +
  geom_line() +
  #scale_y_continuous(limits = c(0, most_sold), breaks = seq(0,1e5, by = 5000)) +
  #theme_bw() +
  ## gganimate functionality starts here
  labs(x = "", y = "Pressure (Pa)", col = "") +
  transition_reveal(date) +
  ease_aes("linear")

animate(animation_1d, height = 2, width =6,unit="in",res=150,duration=12)
anim_save(paste0(plotpfad_PPchamber,"1D_PP_reveal.gif"))
#########################
#Magick
## create a directory to which the images will be written
dir_out <- file.path(tempdir(), "pp_gif")
dir.create(dir_out, recursive = TRUE)
for(i in 1:60){
datelim <- datelim +t_diff
data_sub_i <- sub_daterange(data_sub,datelim)
g <- ggplot(subset(data_sub_i,id %in% 1:4 ))+
  geom_line(aes(date,P_filter,col=id))+
  ylim(range(data_sub$P_filter))+
  labs(x="",y="Pressure (Pa)",col="subchamber")+ggsave(file.path(dir_out, paste0(i, ".png")),width = 5,height=3,dpi=200)

}

imgs <- list.files(dir_out, full.names = TRUE)
imgs <- list.files(dir_out, full.names = F)
number <- str_remove(imgs,".png") %>% as.numeric()
imgs <- imgs[order(number)]
img_list <- lapply(paste0(dir_out,"/",imgs), magick::image_read)

## join the images together
img_joined <- magick::image_join(img_list)

## animate at 2 frames per second
img_animated <- magick::image_animate(img_joined, fps = 10)

## view animated image
#img_animated

## save to disk
magick::image_write(image = img_animated,
            path = paste0(plotpfad_PPchamber,"2D_PP.gif"))
