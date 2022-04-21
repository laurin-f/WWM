offset <- 0
amp <- 255
x <- 1:3600



pwm <- function(amp,
                offset,
                period){#
y <- amp*sin((x-offset)/period*2*pi)
y0 <- amp*sin(x/3600*2*pi)
  
xp <- (x/(period/2))-floor(x/(period/2))
y <- ifelse(xp <= 0.5,amp,y)
y <- ifelse(y < 0,-y,y)


plot(x,y0,type="l",lty=2)
abline(h=0)
abline(v=0:6*600,lty=3)
lines(x,y,col=2)
#lines(rep((0:6*600),each=3),rep(c(NA,-amp,amp),7))
}
pwm(255,0,60*10)
x <- 1:3600
p <- 600
xp <- (x/p)-floor(x/p)
xp <- ifelse(xp <= 0.25,0,xp)
plot(xp)
lines(x/p)


#################################
#
min_sec <- 0:(59*3)
offset <- 0
offset2 <- 10
offset3 <- 20
offset4 <- 30
period <- 60
Amp <- 100
amp_offset <- -0.8
speed1 <- Amp * sin((min_sec-offset)/period*2*pi)
speed2 <- Amp * sin((min_sec-offset2)/period*2*pi)
speed3 <- Amp * sin((min_sec-offset3)/period*2*pi)
speed4 <- Amp * sin((min_sec-offset4)/period*2*pi)

rel_time <- min_sec/period - floor(min_sec/period)
rel_time_2 <- min_sec/period*2 - floor(min_sec/period*2)

speed1_adj <- ifelse(rel_time_2 <= 0.5,ifelse(rel_time < 0.5,Amp,-Amp),(speed1))
speed1_adj2 <- ifelse(speed1_adj <= 0, 
                      ifelse(rep(amp_offset,length(speed1)) < 0,speed1_adj*abs(amp_offset),speed1_adj),
                      ifelse(rep(amp_offset,length(speed1)) > 0,speed1_adj*abs(amp_offset),speed1_adj))

#speed1 <- abs(speed1)
plot(min_sec,speed1)
lines(min_sec,speed1_adj)
lines(min_sec,speed1_adj2,col=2)
lines(min_sec,rel_time*100)
lines(min_sec,rel_time_2*100,col=2)
lines(c(period,period),c(-Amp,Amp))
lines(min_sec,speed2,col=2)
lines(min_sec,speed3,col=3)
lines(min_sec,speed4,col=4)