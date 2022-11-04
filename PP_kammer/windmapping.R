#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
datapfad_ws<- paste0(hauptpfad,"Daten/Urdaten/windspeed_testo/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

#######################
#time offset PP WS
PP_time <- ymd_hms("2022-10-20 9:46:36")
ws_time <- ymd_hms("2022-10-20 9:50:00")
t_lag <- as.numeric(difftime(ws_time, PP_time,unit="secs"))

#################################
#Metadata
kammer_x <- 330
subchamber_x <- 82.5
kammer_y <- 192
kammer_x - subchamber_x
Messpunkte <- data.frame(name = c(rep(paste0("P",1:6),2),paste0("P",7:12)),
                         file = 14:31,
                         x = c(rep(c(rep(c(300,275),each=3)),2),290,290,205,124,42,42),
                         y = c(rep(c(20,96,172,172,96,20),2),50,150,150,50,150,150),
                         z =c(rep(c(20,3),each=6),rep(3,5),50))
Messpunkte_ws <- data.frame(name = c("probe1","probe2","probe2","probe3"),
                         file = 32:35)
kammer_y-60
kammer_x - subchamber_x/2 -subchamber_x*3

Messpunkte$subchamber <- ceiling((Messpunkte$x / subchamber_x))
#################
#load ws testo files
files <- list.files(datapfad_ws,full.names = T)

data_ls <- lapply(files,read.csv2,
                  skip=1,
                  stringsAsFactors = F,
                  col.names = c("Datum_chr","T_C","ws",""),
                  colClasses = c("character","numeric","numeric","NULL"),
                  na.strings = c("testo405i","---"))
for(i in seq_along(data_ls)){
  data_ls[[i]]$file <- i
}

data_ws_all <- do.call(rbind,data_ls)

datelim1 <- ymd_hm("22.10.18 10:00","22.10.18 13:00")
datelim2 <- ymd_hm("22.10.20 09:00","22.10.20 13:00")
datelim <- ymd_hm("22.10.18 10:00","22.10.20 13:00")

datelim_ws <- ymd_hm("22.10.28 09:00","22.10.28 13:00")

data_ws_all$date <- dmy_hms(data_ws_all$Datum_chr)
data_ws_all <- data_ws_all[!is.na(data_ws_all$date),]
data_ws_all <- data_ws_all[!duplicated(data_ws_all$date),]

data_ws <- sub_daterange(data_ws_all,datelim)
data_ws2 <- sub_daterange(data_ws_all,datelim_ws)

data_ws2 <- data_ws2 %>% 
  group_by(file) %>% 
  mutate(ws_roll = RcppRoll::roll_mean(ws,5,fill=NA),
         ws_diff = abs(c(NA,diff(ws_roll))))
data_ws2$probe <- factor(data_ws2$file,levels = (Messpunkte_ws$file),labels = (Messpunkte_ws$name))
###########################################
#read PP data
data_PPC1 <- read_PP(datelim1)
data_PPC2 <- read_PP(datelim2)
data_PPC_ws <- read_PP(datelim_ws,format = "long")
data_PPC_ws$date <- data_PPC_ws$date +t_lag
data_PPC_ws <- sub_daterange(data_PPC_ws,range(data_ws2$date)+ 60*30 *c(-1,1))
data_PPC_ws <- data_PPC_ws %>%
  filter(id %in% 1:5) %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,3*60,fill=NA))
data_ws2_merge <- merge(data_ws2,data_PPC_ws)

data_PPC1$period <- 1
data_PPC2$period <- 2
data_PPC <- rbind(data_PPC1,data_PPC2)
data_PPC <- data_PPC %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         P_diff = abs(c(NA,diff(P_filter))),
         PPC5 = RcppRoll::roll_mean(P_diff,10*60,fill=NA),
         PPC1 = RcppRoll::roll_mean(P_diff,30,fill=NA),
         P_roll = RcppRoll::roll_mean(P,3*60,fill=NA))

data_PPC2 <- data_PPC
data_PPC2$date <- data_PPC$date + t_lag

data_merge <- merge(data_ws,subset(data_PPC2,id==4),all.x = T)
data_merge$PPC_diff <- c(NA,diff(data_merge$PPC1))
PPC_dates_raw1 <- data_merge$date[which(data_merge$PPC_diff >0.055 & data_merge$period == 1)]
PPC_dates_raw2 <- data_merge$date[which(data_merge$PPC_diff >0.045 & data_merge$period == 2)]
PPC_dates_raw <- c(PPC_dates_raw1,PPC_dates_raw2)
PPC_dates_start <- PPC_dates_raw[c(as.numeric(diff(PPC_dates_raw)),100) > 60]
PPC_dates <- sort(c(PPC_dates_start,PPC_dates_start + rep(c(2,4,6),each=length(PPC_dates_start))*60))
data_merge <- data_merge %>% 
  group_by(file) %>% 
  mutate(step = cumsum(ifelse(date %in% PPC_dates,1,0)),
         step = ifelse(step == 4, 0, step),
         ifelse(step ==0 ,0, abs(step - 4)))

ggplot(data_merge)+
  geom_point(aes(date,PPC_diff))+
  geom_hline(yintercept = 0.045)+
  facet_wrap(~period,scales="free_x")
#aggregate data
data_agg <- data_merge %>% 
  group_by(file,step,period) %>% 
  summarise(PPC = mean(PPC1),
            ws = mean(ws),
            ws_sd = sd(ws))

##ws_steps
ws_dates_raw <- data_ws2[which(data_ws2$ws_diff > 0.02),c("date","file")]
ws_dates_start <- ws_dates_raw$date[!duplicated(ws_dates_raw$file)]
ws_dates_start <- ws_dates_raw[c(as.numeric(diff(ws_dates_raw)),100) > 60]
ws_dates <- sort(c(ws_dates_start,ws_dates_start + c(3,3,2,2,6,6,4,4,9,9,6,6)*60))
ws_dates_df <- data.frame(start = c(min(data_ws2_merge$date),ws_dates),
                          stop = c(ws_dates,max(data_ws2_merge$date)),
                          PWM = c(0,rep(c(100,66,32,0),4)),
                          file= c(32,rep(32:35,each=4)))

data_ws2_merge <- data_ws2_merge %>% 
  group_by(file,id) %>% 
  mutate(step = cumsum(ifelse(date %in% ws_dates,1,0)),
         step = ifelse(step == 4, 0, step))

data_ws2_agg <- data_ws2_merge %>% 
  group_by(file,step,id) %>% 
  summarise(ws = mean(ws),
            P_roll = mean(P_roll))
###########################
#plot

PPC_plot <- 
  ggplot(subset(data_merge))+
  geom_line(aes(date,PPC1,col=factor(id)))+
  facet_wrap(~period,scales="free_x")
  #xlim(range(data_ws$date))
ws_plot <- 
  ggplot(data_merge)+
  facet_wrap(~period,scales="free_x")+
  geom_line(aes(date,ws,col=factor(file)))

 PPC_plot+
   geom_vline(xintercept = PPC_dates_start)+
   geom_vline(xintercept = PPC_dates,alpha=0.2)

 #############

egg::ggarrange(PPC_plot,ws_plot)


 ggplot(subset(data_PPC_ws,id %in% 1:5))+
   geom_line(aes(date,P_roll,col=as.factor(id)))+
   geom_line(data = data_ws2,aes(date,ws*10))
 
 
 
 ggplot(subset(data_ws2_merge,id %in% 1:5 & file %in% c(32,33)))+
   geom_vline(xintercept = ws_dates,alpha=0.3,linetype=2)+
   geom_rect(data = ws_dates_df[1:8,],aes(xmin = start,xmax=stop,ymin = -Inf, ymax = Inf,alpha = factor(PWM)))+
   geom_line(aes(date,P_roll/10,col=as.factor(id)))+
   geom_line(aes(date,ws_roll))+
   labs(col="P")+
   scale_alpha_discrete("pwm",range=c(0,0.4))+
   scale_y_continuous(name="windspeed (m/s)",sec.axis = sec_axis(~(.)*10,name="P (Pa)"))+
   theme_classic()+
   ggsave(paste0(plotpfad_PPchamber,"WS_Versuch_P_roll.png"),width = 7,height = 6)
 
 

 ggplot(data_ws2_merge)+
   #geom_vline(xintercept = ws_dates,alpha=0.3,linetype=2)+
   geom_rect(data = subset(ws_dates_df,PWM != 0),aes(xmin = start,xmax=stop,ymin = -Inf, ymax = Inf,alpha = factor(PWM)))+
   geom_line(aes(date,ws,col=factor(probe)))+
   facet_wrap(~factor(file,labels = 1:4),scales = "free_x")+
   scale_alpha_discrete("pwm",range=c(0.1,0.4))+
   theme_classic()+
   ggsave(paste0(plotpfad_PPchamber,"WS_Versuch_Testo.png"),width = 7,height = 6)

ggplot(data_ws2_agg)+
  geom_point(aes(step,P_roll,col=factor(id)))
ggplot(subset(data_ws2_agg,file == 32))+
  geom_point(aes(P_roll,ws,col=factor(id)))+
  facet_wrap(~file)

ggplot(subset(data_merge))+
  geom_line(aes(date,ws,col="ws"))+
  geom_line(aes(date,P_filter/30))+
  facet_wrap(~file,scales="free_x")

ggplot(subset(data_merge,file%in%c(20:25)))+
  geom_vline(xintercept = PPC_dates,alpha=0.3,linetype=2)+
  geom_line(aes(date,ws,col="ws"))+
  geom_line(aes(date,P/50,col="P"))+
  scale_y_continuous(name="windspeed (m/s)",sec.axis = sec_axis(~(.)*50,name="P (Pa)"))+
  facet_wrap(~factor(file,levels = Messpunkte$file,labels=Messpunkte$name),scales="free_x")+
  ggsave(paste0(plotpfad_PPchamber,"windmapping_raw.png"),width=8,height = 7)


#############################
#

Messpunkte_merge <- merge(Messpunkte,data_agg)

ggplot(Messpunkte_merge,aes(PPC,ws,col=factor(z)))+
  geom_point()+
  geom_line()+
  facet_wrap(~name)+
  labs(col="z (cm)",y= "windspeed (m/s)",x="PPC (Pa/s)")+
  ggsave(paste0(plotpfad_PPchamber,"windmapping.png"),width=6,height = 5)



#formula <- as.formula(paste(cols, collapse = "~"))
#value <- "CO2_mol_per_m3"
approx_df <- NULL
for(i in c(20,3)){
  for(j in unique(Messpunkte_merge$step)){
mat <- reshape2::acast(subset(Messpunkte_merge,z==i & step==j & subchamber == 4), x ~ y, value.var = "ws")
dimnames <- dimnames(mat)
xyz <- lapply(dimnames(mat), as.numeric)
names(xyz) <- c("x","y")

if(any(is.na(mat))){
  mat <- zoo::na.approx(mat)
  mat <- t(zoo::na.approx(t(mat)))
  dimnames(mat) <- dimnames
}

xyz_seq <-
  (lapply(xyz, function(x)
    seq(min(x), max(x), by = 1)))

xyz_out <- expand.grid(xyz_seq)



approx_vec <- e1071::interpolate(xyz_out, mat)

approx_ij <- cbind(xyz_out,ws = approx_vec,z = i,step=j)
approx_df <- rbind(approx_df,approx_ij)
  }
}

ggplot(subset(approx_df,z == 3),aes(x,y,fill=ws))+
  #geom_contour_filled()+
  geom_raster()+
  geom_point(data=subset(Messpunkte_merge,subchamber == 4),aes(x,y))+
  geom_text(data=subset(Messpunkte_merge,subchamber == 4),aes(x+2,y+5,label = name))+
  facet_wrap(~paste0("PPC step ",step))+
  scale_fill_viridis_c()+
  labs(title = "z = 3 cm")+
  ggsave(paste0(plotpfad_PPchamber,"windmapping_z3.png"),width=6,height = 6)
ggplot(subset(approx_df,z == 20),aes(x,y))+
  geom_raster(aes(fill=ws))+
  #geom_contour_filled()+
  geom_point(data=subset(Messpunkte_merge,subchamber == 4),aes(x,y))+
  geom_text(data=subset(Messpunkte_merge,subchamber == 4),aes(x+2,y+5,label = name))+
  facet_wrap(~paste0("PPC step ",step))+
  scale_fill_viridis_c()+
  labs(title = "z = 20 cm")+
  ggsave(paste0(plotpfad_PPchamber,"windmapping_z20.png"),width=6,height = 6)
  
