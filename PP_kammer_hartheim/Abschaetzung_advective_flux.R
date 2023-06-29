
# 
# Ludftdruckänderung:
#   
#   Bei einem Tiefdruck gebiet das in 5h von 1000hPa auf 950hPa geht, haben wir 50hPa in 5 h =10hPa/ h. Der Grundwasserspiegel liegt dort bei 7 m , AFPS ist 30%, Luftleitfähigkeit nicht limitierend = 3 m Bodenluftsäule; Luftdruckänderung 50hPa in 5h sind = 3m *50hPa/1000hPa in 5h = 15cm in 5 Stunden= 3 cm Luft in 1h......
#   
#   
#   
#   Grundwasserspiegeländerung bekannt in Hartheim:
#     
#     60cm in 40h aus Maier et al 2010, ich glaub es gab auch noch mehr.....

# Und wieviel Advektion ergbit nun dein Versuch mit +/-3Pa überdruck??? Aus den Gasprofilen hätte ich gesagt das ist eine ähnliche Grössenordnung wie beim Grundwasserspiegel, also nicht Faktor 10.100 verschieden, vielleicht Faktor 0.5-2…..
# 
# Wenn man eine Luftleitfähigkeit von 200µm² annimmt und die Kammerform in Comsol steckt [oder einfacher: Boden  mit Luftleitfähigkeit von 200µm²  und 5cm Mächtigkeit als Min Wert und 10cm Mächtigkeit als Maxwert, 1D Schätzen , wegen der Rasenkante tiefe]sollte man einen Wert bekommen…..( der wahrscheinlich 1000% Unsicherheit hat….) Aber vielleicht kann man das irgendwie intelliegent einfangen 

# a <- 1.225
# units(a) <- "kg/m^3"
# units::set_units(a,"g/cm^3")
d = 1.225 *1000 / 10^6#Dichte der Luft (gcm-3) 1.225 kg/m³ bei 15°C und Meerespiegelniveau

g = 981#Gravitation (981 cms-2)
#1cm = 10000mum
#18.2 #muPa*s = mukg/m/s²*s = mg/m/s
nl = 18.2 /1000 / 100#Viskosität der Luft (gcm-1s-1)
koo = 200 * 10^-8#Luftleitfähigkeitskoeffizient (10-8 cm2) = mum2
kl = koo * d* g/nl#Luftleitfähigkeitskoeffizient (cms-1)
#kl = 2 # cm/s
dh = 8*3#pneumatische Druckdifferenz (cm Luftsäule)
l = 5#Fließlänge (cm)
# dh/l = pneumatischer Druckgradient (-)

#8 #m/hPa
#8*100/100 #cm/Pa
Vl = kl * dh/l #
#Vl #Volumenstrom Luft durch Bodenquerschnitt und Zeit (cm3cm-2s-1)
Vl * 3600  #cm/h

#oder

Vl = koo x d x g/ηl x dh/l (3)
Vl = wie in (2)
koo = Luftleitfähigkeitskoeffizient (10-8 cm2)
dh/l = wie in (2)