
#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
getwd()
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
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg","dplyr","e1071")

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
data <- subset(data, Position %in% 7:8)
data$CO2_mol_per_m3 <- ppm_to_mol(data[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$T_soil)
data$CO2_mol_per_m3[data$tiefe == 0]<- 0
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0


data$date_hour <- round_date(data$date,"hours")
mod_dates <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 & data$date %in% data$date_hour]))


z_soil_cm <- 150

#####################################
#Datei mit Parameter sweep
#anzahl DS werte
n_DS <- 3

sweep_list <- vector("list",2)
#################################################
#Datei einlesen
sweep_list[[1]] <- readLines(paste0(comsolpfad,"sweep_Hartheim_",n_DS,"DS_mod_datespos8.txt"))
#################################################
#Datei einlesen
sweep_list[[2]] <- readLines(paste0(comsolpfad,"sweep_Hartheim_",n_DS,"DS_mod_dates3.txt"))

sweep_long_list <- vector("list",2)
sweep_wide_list <- vector("list",2)
##################################################

############
#Formatieren

#Parameter die in der Datei gesweept wurden
pars <- c("injection_rate",paste0("DS_",1:n_DS))
#Regular Expression für die unterschiedlichen Werte die die Parameter annehmen
value_regexp <- "\\d+(\\.\\d+)?(E-\\d)?"

for(i in seq_along(sweep_list)){
  #Spaltennahmen der sweep datei ausschneiden
  colnames_sweep <- str_extract_all(sweep_list[[i]][9],paste0("(?<=% )r|z|",paste0(pars,"=",value_regexp,collapse=", ")),simplify = T)
  #ab Spalte 10 stehen die Werte in der Datei diese werden bei leerzeichen getrennt 
  CO2_sweep_mat <- str_split(sweep_list[[i]][10:length(sweep_list[[i]])],"\\s+",simplify = T)
  #die matrix als data.frame mit numerischen werden 
  CO2_sweep_i <- as.data.frame(apply(CO2_sweep_mat,2,as.numeric))
  #Spaltennamen
  colnames(CO2_sweep_i) <- colnames_sweep
  
  sweep_long_i <- tidyr::pivot_longer(CO2_sweep_i,cols=-(1:2),names_patter= paste0(paste(pars,collapse = "=(.*), "),"=(.*)"),values_to="CO2_mol_per_m3",names_to=pars) %>% 
    sapply(.,as.numeric) %>% as.data.frame()
  sweep_long_i$file <- i
  sweep_long_list[[i]] <- sweep_long_i
  sweep_wide_list[[i]] <- CO2_sweep_i
}

sweep_long <- do.call(rbind,sweep_long_list)

pars1u2 <- lapply(sweep_long[-7],unique)
pars1 <- lapply(subset(sweep_long,file == 1)[-7],unique)
pars2 <- lapply(subset(sweep_long,file == 2)[-7],unique)
pars1$DS_3
pars2$DS_3
pars2$injection_rate

rm(list=c("CO2_df_list","CO2_sweep_mat","CO2_sweep_i","colnames_sweep","sweep_list"))
#################
#ins long format
#sweep_long <- tidyr::pivot_longer(CO2_sweep,cols=-(1:2),names_patter= paste0(paste(pars,collapse = "=(.*), "),"=(.*)"),values_to="CO2_mol_per_m3",names_to=pars) %>% 
#  sapply(.,as.numeric) %>% as.data.frame()


inj_meas <- unique(signif(data$inj_mol_m2_s[data$date %in% mod_dates],2))

inj_meas %in% pars$injection_rate
sort(inj_meas)
sort(pars$injection_rate)
data$inj_mol_m2_s
ggplot(data)+
  geom_line(aes(date,inj_mol_m2_s))
############
#n-D
byout <- 2e-7
# x <- pars$DS_1
# seq(min(x), max(x), by = byout)




sweep_extend <- data.frame()

for (i in unique(sweep_long$z)) {
  print(paste("z =", i))
  for (j in inj_meas) {
    print(paste("injection_rate =", j))
    
    sweep_i <- subset(sweep_long, z == i & injection_rate == j)
    cols <-  c("DS_1", "DS_2", "DS_3")
    formula <- as.formula(paste(cols, collapse = "~"))
    value <- "CO2_mol_per_m3"
    sweep_mat <- reshape2::acast(sweep_i, formula, value.var = value)
    
    
    xyz <- lapply(dimnames(sweep_mat), as.numeric)
    names(xyz) <- cols
    
    xyz_seq <-
      (lapply(xyz, function(x)
        seq(min(x), max(x), by = byout)))
    # xyz_seq <-
    #   as.data.frame(sapply(xyz, function(x)
    #     seq(min(x), max(x), by = byout)))
    
    xyz_out <- tidyr::crossing(xyz_seq$DS_1,
                               xyz_seq$DS_2,
                               xyz_seq$DS_3) %>% 
      setNames(.,cols) %>% 
      as.data.frame()
    #xyz_out_tidy <- tidyr::expand(xyz_seq,DS_1,DS_2,DS_3,injection_rate) %>% as.data.frame()
    
    #xyz_out <- expand.grid(xyz_seq)
    
    
    approx_vec <- e1071::interpolate(xyz_out, sweep_mat)
    
    
    approx_list <-
      cbind(
        z = i,
        injection_rate = j,
        xyz_out,
        CO2_mol_per_m3 = approx_vec
      )
    sweep_extend <- rbind(sweep_extend, approx_list)
    
  }
}


extend_long <- sweep_extend

for(i in c("injection_rate",paste0("DS_",1:3))){
  sweep_extend[,i] <- paste0(i,"=",sweep_extend[,i])
}
extend_wide <- tidyr::pivot_wider(sweep_extend,names_from = matches("injection|DS"),names_sep=", ",values_from=CO2_mol_per_m3) %>% as.data.frame()
rm(sweep_extend)

###############
#save
##############
save(extend_long,file=paste0(comsolpfad,"extend_long.RData"))
save(extend_wide,file=paste0(comsolpfad,"extend_wide.RData"))
save(sweep_wide_list,file=paste0(comsolpfad,"sweep_wide_list.RData"))


# sweep_inj_ext <- sweep_extend %>% 
#   group_by(across(c("z",paste0("DS_",1:3)))) %>% 
#   summarise(
#     CO2_mol_per_m3_2 = approx(injection_rate,CO2_mol_per_m3,inj_meas)$y,
#     injection_rate_2 = approx(injection_rate,CO2_mol_per_m3,inj_meas)$x
#     ) %>% 
#   rename(CO2_mol_per_m3 = CO2_mol_per_m3_2,
#          injection_rate = injection_rate_2)
colnames(sweep_long)

formula2 <- as.formula(paste(colnames(sweep_extend)[-6],collapse = "~"))
approx_mat <- reshape2::acast(sweep_extend,formula2,value.var = value)
dimnames(sweep_mat)
dimnames(approx_mat)
dim(approx_mat)
image(sweep_mat[,,1])
image(approx_mat[1,1,,,])

