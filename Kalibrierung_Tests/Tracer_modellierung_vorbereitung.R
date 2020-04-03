hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")

CO2_ppm <- 407.4
p_kPa <- 101.3
T_deg <- 20
p_Pa <- p_kPa*1000#PA = N/m2 = kg/(m s2)
#Temperatur
T_K <- T_deg+273.15 #K
#allgemeine Gaskonstante
R <- 8.314 #kg m2/(s2 mol K)
#Molare Masse CO2 und CH4
M<-data.frame(CO2 = 44.01, CH4 = 16.04)#g/mol

mol_per_m3 <- p_Pa/(R*T_K) #kg/(m s2) / kg m2 * (s2 mol K)/ K = mol/m3
CO2_mol_per_m3 <- mol_per_m3 * CO2_ppm *10^-6
CO2_mol_per_m3

flux <- read.csv(paste0(metapfad,"Pumpstufen_flux.txt"))
ml_per_min_PSt5 <-flux$tracer_ml_per_min[flux$Pumpstufe == 5]
A_inj <- pi * 1^2  / 10^6 #m^2
ml_per_min_PSt5 / 10^6 / 60 * mol_per_m3 / A_inj#m3/s *mol/m3 /m2 = #mol/(s m2)


