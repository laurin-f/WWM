library(pkg.WWM)
A <- 2*0.85
V <- A*0.8 #m^3
F_Fan <- 125 #m3/h
#F_CO2_mol <- 3 #mumol/m2/s
F_CO2_mol <- 3*A/V #mumol/m^2/s *m^2  = mumol/s


F_CO2 <- ppm_to_mol(F_CO2_mol/10^6*60,"mol/m^3")#mol/m3/min -> ppm/min
F_CO2#ppm/min
