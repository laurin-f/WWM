#Flowrate
eps<-0.3
r<-0.05#m
S<-4*pi*r^2#m2
D0_He<-6.5*10^-5#m2/s
D0_CO2 <- 0.159 #cm2/s #quelle Hydrus
Flow_He <- 2*S*eps^3*D0_He
#V/D0 < 2 m^-1
Flow_CO2 <- 2*S*eps^3*D0_CO2#m-1*m2*m2/s = m3/s
Flow_He*60*10^6#ml/min
Flow_CO2*60*10^6#ml/min
# 
# #quelle Hydrus
# D0_CO2 <- 0.159 #cm2/s
# D0_CO2 <- 0.14 #cm2/s MASS DIFFUSIVITY DATA Isodoro Martinez
# 
# 
# D0_CO2 <- 0.138#cm2/s quelle: R. C. Roberts Molecular diffusion of  Gases
# #temperatur und Druckkorrektur nach R.C. Roberts
# T0 <- 273 #°K
# p0 <- 101.3 #kPa
# p0 <- 760 #kPa
# alpha_CO2 <- 2
# T_C <-c(17.6,15.2,15.7)+273
# p <- c(751,364,309)#*0.133
# D0_Tp <- D0_CO2 *(T_C/T0)^alpha_CO2*(p0/p)
# D0_Tp



#TRANSPORT AND RATE PROCESSES
#Yaşar Demirel Air-Carbon dioxide	273 K	0.136 cm2/s
#CO2 	https://www.engineeringtoolbox.com/air-diffusion-coefficient-gas-mixture-temperature-d_2010.html
#20°C->	0.16 
#100°C ->	0.252 
#200°C ->	0.39 
#300°C ->	0.549
#400°C ->0.728 
