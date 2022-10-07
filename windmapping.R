#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
datapfad_WS<- paste0(hauptpfad,"Daten/Urdaten/windspeed_testo/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)


#################################
#Metadata
kammer_x <- 330
subchamber_x <- 82.5
kammer_y <- 192

Messpunkte <- data.frame(name = paste0("P",1:6),
                         x = c(rep(c(300,275),each=3)),
                         y = rep(c(20,96,172),2))

#################
#load WS testo files
files <- list.files(datapfad_WS,full.names = T)

data_ls <- lapply(files,read.csv2,
                  skip=1,
                  stringsAsFactors = F,
                  col.names = c("Datum_chr","T_C","WS",""),
                  colClasses = c("character","numeric","numeric","NULL"),
                  na.strings = c("testo405i","---"))

data_ws_all <- do.call(rbind,data_ls)
datelim <- ymd_hm("22.10.07 10:00","22.10.07 13:00")

data_ws_all$date <- dmy_hms(data_ws_all$Datum_chr)
data_ws_all <- data_ws_all[!is.na(data_ws_all$date),]
data_ws_all <- data_ws_all[!duplicated(data_ws_all$date),]

data_ws <- sub_daterange(data_ws_all,datelim)

###########################################
#read PP data
data_PPC <- read_PP(datelim)
data_PPC <- data_PPC %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         P_diff = abs(c(NA,diff(P_filter))),
         PPC5 = RcppRoll::roll_mean(P_diff,10*60,fill=NA),
         PPC1 = RcppRoll::roll_mean(P_diff,30,fill=NA),
         P_roll = RcppRoll::roll_mean(P,3*60,fill=NA))

###########################
#plot
names(data_ws)
PPC_plot <- ggplot(data_PPC)+
  geom_line(aes(date,PPC1,col=factor(id)))+
  xlim(range(data_ws$date))
ws_plot <- ggplot(data_ws)+
  geom_line(aes(date,WS))

egg::ggarrange(PPC_plot,ws_plot)


Messpunkte$ws <- runif(6)

formula <- as.formula(paste(cols, collapse = "~"))
value <- "CO2_mol_per_m3"
mat <- reshape2::acast(Messpunkte, x ~ y, value.var = "ws")


xyz <- lapply(dimnames(mat), as.numeric)
names(xyz) <- c("x","y")

xyz_seq <-
  (lapply(xyz, function(x)
    seq(min(x), max(x), by = 1)))

xyz_out <- expand.grid(xyz_seq)



approx_vec <- e1071::interpolate(xyz_out, mat)

approx_df <- cbind(xyz_out,ws = approx_vec)

ggplot(approx_df,aes(x,y,z=ws))+
  geom_contour_filled()
ggplot(Messpunkte,aes(x,y,z=ws))+
  geom_contour_filled()
