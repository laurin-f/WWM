#Flowrate
eps<-0.3
r<-0.05#m
S<-4*pi*r^2#m2
D0_He<-6.5*10^-5#m2/s
D0_CO2 <- 1.38*10^-5#m2/s
Flow_He <- 2*S*eps^3*D0_He
#V/D0 < 2 m^-1
Flow_CO2 <- 2*S*eps^3*D0_CO2#m-1*m2*m2/s = m3/s
Flow_He*60*10^6#ml/min
Flow_CO2*60*10^6#ml/min
