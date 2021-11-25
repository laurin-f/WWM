#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_tracer<- paste0(metapfad,"Tracereinspeisung/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg","dplyr","oce","akima")

check.packages(packages)


######################################
#data_agg
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))

#######################
#einheiten anpassen für COMSOl
#Tracersignal in COMSOL Einheit umrechnen
offset_method <- "drift"
######################

data$CO2_mol_per_m3 <- ppm_to_mol(data[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$T_soil)
data$CO2_mol_per_m3[data$tiefe == 0]<- 0
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0


data$date_hour <- round_date(data$date,"hours")
mod_dates <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 & data$date %in% data$date_hour]))


z_soil_cm <- 150

#############
#data auf stunden aggregieren
data_mod_range <- subset(data,date > min(mod_dates) & date < max(mod_dates))
data_agg_mod <- data_mod_range %>% group_by(date_hour,tiefe) %>% summarise_all(mean)

#####################################
#Datei mit Parameter sweep
#anzahl DS werte
n_DS <- 3

CO2_sweep_list <- vector("list",2)
#################################################
#Datei einlesen
CO2_sweep_list[[1]] <- readLines(paste0(comsolpfad,"sweep_Hartheim_",n_DS,"DS_mod_datespos8.txt"))
#################################################
#Datei einlesen
CO2_sweep_list[[2]] <- readLines(paste0(comsolpfad,"sweep_Hartheim_",n_DS,"DS_mod_dates3.txt"))
CO2_df_list <- vector("list",2)
##################################################

############
#Formatieren

#Parameter die in der Datei gesweept wurden
pars <- c("injection_rate",paste0("DS_",1:n_DS))
#Regular Expression für die unterschiedlichen Werte die die Parameter annehmen
value_regexp <- "\\d+(\\.\\d+)?(E-\\d)?"

for(i in seq_along(CO2_sweep_list)){
#Spaltennahmen der sweep datei ausschneiden
colnames_sweep <- str_extract_all(CO2_sweep_list[[i]][9],paste0("(?<=% )r|z|",paste0(pars,"=",value_regexp,collapse=", ")),simplify = T)
#ab Spalte 10 stehen die Werte in der Datei diese werden bei leerzeichen getrennt 
CO2_sweep_mat <- str_split(CO2_sweep_list[[i]][10:length(CO2_sweep_list[[i]])],"\\s+",simplify = T)
#die matrix als data.frame mit numerischen werden 
CO2_sweep_i <- as.data.frame(apply(CO2_sweep_mat,2,as.numeric))
#Spaltennamen
colnames(CO2_sweep_i) <- colnames_sweep
CO2_df_list[[i]] <- CO2_sweep_i
}

CO2_sweep <- do.call(merge,CO2_df_list)

rm(list=c("CO2_df_list","CO2_sweep_mat","CO2_sweep_i","colnames_sweep","CO2_sweep_list"))
#################
#ins long format
sweep_long <- tidyr::pivot_longer(CO2_sweep,cols=-(1:2),names_patter= paste0(paste(pars,collapse = "=(.*), "),"=(.*)"),values_to="CO2_mol_per_m3",names_to=pars) %>% 
  sapply(.,as.numeric) %>% as.data.frame()

#inj_rates_2 <- unique(sweep_long2$injection_rate) %>% as.numeric() %>% round(3)
#CO2_sweep <- CO2_sweep[,grep(paste(inj_rates_2,collapse = "|"),colnames(CO2_sweep))]
#tiefe umrechnen
#sweep_long$tiefe <- set_units(sweep_long$z - z_soil_cm,cm)

inj_meas <- unique(data$inj_mol_m2_s[data$date %in% mod_dates])

# i <- 129
# j <- unique(sweep_long$DS_3)[1]
# k <- unique(sweep_long$injection_rate)[1]
for(i in unique(sweep_long$z)){
for(j in unique(sweep_long$DS_3)){
for(k in unique(sweep_long$injection_rate)){
  
sweep_i <- subset(sweep_long, z==i & DS_3==j&injection_rate == k)

######
#2d
sweep_mat <- reshape2::acast(sweep_i,DS_1~DS_2)
approx_out <- akima::interp(sweep_i$DS_1,sweep_i$DS_2,sweep_i$CO2_mol_per_m3,nx=nout,ny=nout)
approx_mat <- approx_out$z
approx_df <- pivot_longer

colnames(approx_mat) <- approx_out$x
rownames(approx_mat) <- approx_out$y
test <- reshape2::melt(approx_mat)
test <- data.frame(c(approx_mat$z),approx_mat$x,rep(approx_mat$y))

}
}
  }
# 
# xy <- lapply(dimnames(sweep_mat),as.numeric)
# names(xy) <- c("DS_1","DS_2")
# nout <- 40
# 
# xy_seq <- as.data.frame(sapply(xy,function(x) seq(min(x),max(x),len=nout)))
# 
# xy_out <- tidyr::expand(xy_seq,DS_1,DS_2) %>% as.data.frame()


#approx_vec <- pracma::interp2(xy[[1]],xy[[2]],sweep_mat,xy_out$DS_1,xy_out$DS_2)
#approx_mat2 <- matrix(approx_vec,nout,nout,byrow=F)


par(mfrow=c(3,1))
image(sweep_mat)
image(approx_mat)
image(approx_mat2)

############
#3d
sweep_mat <- reshape2::acast(sweep_i,DS_1~DS_2~injection_rate)

xyz <- lapply(dimnames(sweep_mat),as.numeric)
names(xyz) <- c("DS_1","DS_2","injection_rate")
nout <- 40

xyz_seq <- as.data.frame(sapply(xyz,function(x) seq(min(x),max(x),len=nout)[-c(1,nout)]))
xyz_seq$DS_1
xyz_out <- tidyr::expand(xyz_seq,DS_1,DS_2,injection_rate) %>% as.data.frame()

xyz[[3]][1] <- 0.049
approx_vec <- oce::approx3d(xyz[[1]],xyz[[2]],xyz[[3]],sweep_mat,xyz_out$DS_1,xyz_out$DS_2,xyz_out$injection_rate)




###################
#matrix vergrößern
###############
extend_sweep <- F
if(extend_sweep==T){
  df <- as.data.frame(sapply(sweep_long,as.numeric))
  df_wide <- df
  for(i in (unique(df$DS_1))){
    for(j in (unique(df$DS_2))){
      for(k in (unique(df$DS_3))){
      for(l in (unique(df$z))){
        for(m in (unique(df$injection_rate))){
          dfi <- subset(df, DS_2 == j & DS_3 == k & z == l & injection_rate == m)
          dfj <- subset(df, DS_1 == i & DS_3 == k & z == l & injection_rate == m)
          #dfk <- subset(df, DS_1 == i & DS_2 == j & z == l & injection_rate == m)
          
          approxi <- approx(dfi$DS_1,dfi$CO2_mol_per_m3,seq(min(dfi$DS_1),max(dfi$DS_1),len=40))
          approxj <- approx(dfj$DS_2,dfj$CO2_mol_per_m3,seq(min(dfj$DS_2),max(dfj$DS_2),len=40))
          #approxk <- approx(dfk$DS_3,dfk$CO2_mol_per_m3,seq(min(dfk$DS_3),max(dfk$DS_3),len=40))
          
          #df_wide[df$DS_1 == i & df$DS_2 == j & df$DS_3 == k & df_wide$injection_rate==m & df_wide$z == l ,paste0("DS_1=",approxi$x)] <- approxi$y
          #df_wide[df$DS_1 == i & df$DS_2 == j & df$DS_3 == k & df_wide$injection_rate==m & df_wide$z == l ,paste0("DS_2=",approxj$x)] <- approxj$y
          #df_wide[df_wide$DS_1 == i & df_wide$DS_2 == j & df_wide$DS_3 == df_wide$DS_3[1] & df_wide$injection_rate==m & df_wide$z == l ,paste0("DS_3=",approxk$x)] <- approxk$y
        }
      }
      #print(paste0("k=",k))
      }
      #print(paste0("j=",j))
    }
    print(paste0("i=",i))
  }
  
  
  save(df_wide,file=paste0(comsolpfad,"df_wide_pos8.RData"))
}
load(file=paste0(comsolpfad,"df_wide_pos8.RData"))

df_wide <- df_wide[!is.na(df_wide$`DS_3=1.00769230769231e-06`),!grepl("DS_3$|CO2_mol_per_m3",colnames(df_wide))]


df_long <- tidyr::pivot_longer(df_wide, matches("DS_\\d=.*"),names_prefix = "DS_3=",names_to = "DS_3",values_to = "CO2_mol_per_m3")

df_long$DS_3 <- signif(as.numeric(df_long$DS_3),4)
df_long <- as.data.frame(df_long)

for(i in c("injection_rate",paste0("DS_",1:3))){
  df_long[,i] <- paste0(i,"=",df_long[,i])
}
CO2_sweep <- tidyr::pivot_wider(df_long,names_from = matches("injection|DS"),names_sep=", ",values_from=CO2_mol_per_m3) %>% as.data.frame()



#########################
#test
#########################
test3d <- data.frame(DS1=rep((1:4)*0.2,3*3),DS2=rep(1:3,each=4*3),DS3=rep(rep(1:3,3),each=4),CO2=runif(12*3))
mat3d <- reshape2::acast(test3d,DS1 ~DS2 ~DS3) 
names(mat3d)

xyz <- lapply(dimnames(mat3d),as.numeric)
xp <- seq(0.21,0.79,len=9)
yp <- seq(1.1,2.9,len=9)
zp <- seq(1.1,2.9,len=9)
xp2 <- rep(xp,length(zp)*length(yp))
yp2 <- rep(rep(yp,length(zp)),each=length(xp))
zp2 <- rep(zp,each=length(xp)*length(yp))
approxoce <- oce::approx3d(xyz[[1]],xyz[[2]],xyz[[3]],mat3d,xp2,yp2,zp2)

aprox3d <- array(approxoce,dim=rep(9,3)) 
long3d <- data.frame(CO2=approxoce,DS1=xp2,DS2=yp2,DS3=zp2)


par(mfrow=c(3,4))
for(i in 1:9){
image(aprox3d[,,i])  
  title(unique(zp2)[i])
}

for(i in 1:3){
image(mat3d[,,i])
  title(i)
}



test <- data.frame(DS1=rep(1:4,3),DS2=rep(1:3,each=4),CO2=rnorm(12))

wide <- tidyr::pivot_wider(test,DS1,names_from = DS2,values_from = CO2)
mat <- as.matrix(wide[,-1])
rownames(mat) <- wide$DS1
xp <- seq(1,3,len=10)
zp <- seq(1,4,len=10)
xp2 <- rep(xp,length(zp))
zp2 <- rep(zp,each=length(xp))
interp2d <- pracma::interp2(as.numeric(colnames(mat)),wide$DS1,mat,xp2,zp2)
interakima <- akima::interp(test$DS1,test$DS2,test$CO2,ny=10,nx=10,linear = T)$z
mat_interp <- matrix(interp2d,length(xp),length(zp),byrow=T)

oce::approx3d()
par(mfrow=c(3,1))
image(mat)
image(interakima)
image(mat_interp)
