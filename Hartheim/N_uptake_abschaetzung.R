CO2_nitrif <- 1000 *0.23 # kg/C/ha/a
CO2_nitrif/(3600*24*365)/(10^4)*1000
CO2_g_m2_s <- change_unit(CO2_nitrif,unit_in="kg/hectare/yr",unit_out = "g/m^2/s")
c_gmol <- 12#g/mol
change_unit(1,"m","microm")
C_mumol_m2_s <- CO2_g_m2_s / c_gmol * 10^6
C_mumol_m2_s/2*100#% vom Resirationflux

