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
