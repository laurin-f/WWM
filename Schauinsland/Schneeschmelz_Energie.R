s <- 335 #kJ/kg
#1Ws = 1 J
ro <- 0.1 * 10^6 / 10^3 #kg/m3
V <- 2*2*0.5 #m3
m <- V * ro #kg
E <- m*s #kJ
P <- 2 #kW
t <- E / P#kJ / kW = kWs / kW = s
t/3600#h
