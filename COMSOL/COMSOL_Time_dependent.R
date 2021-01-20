###################################
#dateien für COMSOL exportieren####

#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <-
  "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <-
  paste0(hauptpfad, "Daten/aufbereiteteDaten/sampler_data/")
comsolpfad <- paste0(hauptpfad, "Daten/aufbereiteteDaten/COMSOL/")
metapfad <- paste0(hauptpfad, "Daten/Metadaten/")
metapfad_harth <- paste0(metapfad, "Hartheim/")
metapfad_comsol <- paste0(metapfad, "COMSOL/")
soilpfad <- paste0(hauptpfad, "Daten/Urdaten/Boden_Hartheim/")
klimapfad <- paste0(hauptpfad, "Daten/Urdaten/Klimadaten_Hartheim/")
kammer_datapfad <-
  paste0(hauptpfad, "Daten/aufbereiteteDaten/Kammermessungen/")
plotpfad <- paste0(hauptpfad, "Dokumentation/Berichte/plots/COMSOL/")
COMSOL_exepath <-
  "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
COMSOL_progammpath <-
  "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
#Packages laden
library(pkg.WWM)
packages <-
  c("lubridate", "stringr", "ggplot2", "units", "ggforce", "dplyr")

check.packages(packages)

load(paste0(samplerpfad, "Hartheim_CO2.RData"))

                       tiefe)))

#td_long_1 <- td_long
sub <- subset(data, Position == 8 & Pumpstufe != 0)

ggplot(sub) + geom_line(aes(date, CO2_tracer_gam, col = as.factor(tiefe)))

mod_dates = unique(round_date(sub$date, "hours"))[2:4]
timeperiod_s <-
  difftime(mod_dates[-1], mod_dates[-length(mod_dates)], units = "secs") %>% as.numeric() %>% unique


timeperiod_s = 3600#s
overwrite = T
read_all = F
n_DS = 3

offset_method = "gam"
#which optimization method should be used nelder or snopt

modelname = "time_dependent_freeSoil_anisotropy_optim_3DS"


n_DS_ch <- paste0(n_DS, "DS")

#CO2 in tracer in mol pro m3
if (!any(grepl("PressureActual_hPa", names(data)))) {
  print("no Pressure Data available using 101.3 kPa as default")
  data$PressureActual_hPa <- 1013
}
if (!any(grepl("T_soil", names(data)))) {
  print("no T_soil Data available using 20 °C as default")
  data$T_soil <- 20
}
data$D0 <-
  D0_T_p(data$T_soil, p_kPa = data$PressureActual_hPa / 10, "m^2/s")
data$CO2_mol_per_m3 <-
  ppm_to_mol(data[, paste0("CO2_tracer_", offset_method)],
             "ppm",
             p_kPa = data$PressureActual_hPa / 10,
             T_C = data$T_soil)
#negative werte auf null setzen
data$CO2_mol_per_m3[data$tiefe == 0] <- 0
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0] <- 0

#model coordinates
z_soil_cm <-  150
data$z <- z_soil_cm + data$tiefe
data$r <- 0

data_sub <-
  lapply(mod_dates, function(x)
    data[data$date >= x &
           data$date < x + timeperiod_s, c(
             "tiefe",
             "date",
             "CO2_mol_per_m3",
             "inj_mol_m2_s",
             "T_soil",
             "PressureActual_hPa",
             "CO2_ref"
           )])
names(data_sub) <- mod_dates


##############################################
#Comsol ausführen
##############################################
#
for (j in seq_along(data_sub)) {
  sub_j <- data_sub[[j]]
  sub_j$t_secs <-
    as.numeric(difftime(sub_j$date, min(sub_j$date), units = "secs"))
  injection_rate <- unique(na.omit(sub_j$inj_mol_m2_s))
  
  #schreibe messungen in files die in COMSOL als Objective verwendet werden
  sub_j <- subset(sub_j,t_secs%%600==0)
  for (i in 1:7) {
    write.table(
      sub_j[sub_j$tiefe == (1:7 * -3.5)[i], c("t_secs", "CO2_mol_per_m3")],
      paste0(metapfad_comsol, "td_dom", i, ".csv"),
      col.names = F,
      row.names = F,
      sep = ","
    )
  }
  
  #command der an commandline gesendet wird um comsolbatch.exe zu starten
  
  outfile <- "CO2_optim"
  #snopt gibt nicht automatisch die DS werte aus deshalb wird ein batch in der GUI angelegt in dem der Output exportiert wird
  cmd <-
    paste0(
      "cd ",
      COMSOL_exepath,
      "&& comsolbatch.exe -inputfile ",
      COMSOL_progammpath,
      modelname,
      ".mph -job b1 -pname injection_rate -plist ",
      mean(injection_rate)
    )
  
  date_chr <- format(mod_dates[j], "%m_%d_%H_%M")
  outfile_name <-
    paste0(modelname, "_", offset_method, "_", date_chr, ".txt")
  outfile_full_name <- paste0(comsolpfad, outfile_name)
  if (file.exists(outfile_full_name) & overwrite == F) {
    print(paste("file", outfile_name, "exists set overwrite = T to replace it"))
  } else{
    print(paste("starting with", mod_dates[j]))
    tictoc::tic()
    #console_out <-
    #commandline befehl ausführen
    shell(cmd, translate = T, intern = F)
    #print(paste(mod_dates[j],"calculated in:"))
    tictoc::toc()
    #comsoloutfiles_raw <- paste0(comsolpfad,c("Objective_table.txt","Probe_table.txt"))
    #outputdateien von COMSOL umbenennen damit sie beim nächsten run nicht überschrieben werden
    
    comsoloutfiles_raw <-
      list.files(comsolpfad, "CO2_optim_td.txt", full.names = T)
    
    #im Dateiname steht jetzt die methode und das datum
    
    if (length(comsoloutfiles_raw) == 1) {
      file.rename(comsoloutfiles_raw, outfile_full_name)
    }
  }
}


##########################################
#read Comsol Output
###########################################
read_all = F
if (read_all == T) {
  date_pattern <- "\\d{2}(_\\d{2}){2,3}"
  mod_files <-
    list.files(comsolpfad,
               pattern = paste(modelname, offset_method, date_pattern, sep = "_"))
  
  mod_date_all_chr <-
    sort(unique(str_extract(mod_files, date_pattern)))
  
  mod_dates_all <- ymd_hm(paste("2020", mod_date_all_chr))
  mod_dates_all <- mod_dates_all[mod_dates_all %in% data$date]
} else{
  mod_dates_all <- mod_dates
}

data_list <- lapply(mod_dates_all, function(x)
  data[data$date == x,])
names(data_list) <- as.character(mod_dates_all)

F_Comsol <- data.frame(date = mod_dates_all, Fz = NA)

####################################
#read loop
#######################################

CO2_optim_list <- lapply(mod_dates_all, function (x) {
  date_chr <- format(x, "%m_%d_%H_%M")
  file_x <-
    paste0(comsolpfad,
           modelname,
           "_",
           offset_method,
           "_",
           date_chr,
           ".txt")
  td <- read.csv(file_x,
                 skip = 9,
                 sep = "",
                 header = F)
  colnames_td <- readLines(file_x, n = 9)
  colnames(td) <-
    str_extract_all(colnames_td[9],
                    "r|z|(c|DS_\\d)( .mol/m.3.)? @ t=\\d+",
                    simplify = T) %>% str_replace_all(c("( .mol/m.3.)? @" = "", "c" = "CO2_mod"))
  td_long <-
    tidyr::pivot_longer(
      td,
      cols = matches("t=\\d+"),
      names_to = c(".value", "t"),
      names_pattern = "(CO2_mod|DS_\\d) t=(\\d+)"
    )
  td_long$date <- as.numeric(td_long$t) + x
  return(td_long)
})

CO2_optim <- do.call(rbind, CO2_optim_list)
unique(CO2_optim$t)
CO2_optim$tiefe <- CO2_optim$z -150
ggplot(CO2_optim) +
  geom_line(aes(date, CO2_mod, col = as.factor(tiefe))) +
  geom_line(data = subset(data, date < max(mod_dates_all) + 3600 &
                            date > min(mod_dates_all)),
            aes(date, CO2_mol_per_m3, col = as.factor(tiefe)))#+
  facet_wrap(~tiefe,scales="free_y")
ggplot(CO2_optim) +
  geom_line(aes(date, DS_1, col = "DS_1")) +
  geom_line(aes(date, DS_2, col = "DS_2")) +
  geom_line(aes(date, DS_3, col = "DS_3"))

unique(CO2_optim$DS_3)
slope_0_7cm <-
  glm(CO2_ref ~ tiefe, data = subset(obs_j, tiefe >= -7))#ppm/cm
#plot(obs_j$tiefe,obs_j$CO2_ref)
#abline(slope_0_7cm)
dC_dz <- -slope_0_7cm$coefficients[2]

dC_dz#ppm/cm
#DS = -FZ * dz / dC

dC_dz_mol <-
  ppm_to_mol(
    dC_dz,
    "ppm",
    out_class = "units",
    p_kPa = unique(obs_j$PressureActual_hPa) / 10,
    T_C = obs_j$T_soil[obs_j$tiefe == -3.5]
  )#mol/m^3/cm

Fz_mumol_per_s_m2 <-
  best_DS$DS_1  * dC_dz_mol * 100 * 10 ^ 6#m2/s * mol*10^6/m3/cm*100 = mumol/s/m2

F_Comsol$Fz[F_Comsol$date == mod_dates_all[[j]]] <-
  Fz_mumol_per_s_m2
for (k in 1:n_DS) {
  F_Comsol[F_Comsol$date == mod_dates_all[[j]], paste0("DSD0", k)] <-
    DS_profil$DS[k] / DS_profil$D0[k]
  F_Comsol[F_Comsol$date == mod_dates_all[[j]], paste0("DS", k)] <-
    DS_profil$DS[k]
}

return(F_Comsol)



