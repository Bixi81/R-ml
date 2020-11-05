# Simulate a normal distribution with mean=100, sd=5
sn = data.frame(x=rnorm(100000, mean=100, sd=5))
sn$z = scale(sn$x)
plot(density(sn$x), main="N~(1,5)")
# Sclae (aka get z-scores)
plot(density(sn$z), main="z-score N~(1,5) (aka: sclaed)")

# Look at z-scores at diferent loci
# Z-score x0.25
sn$z[which.min(abs(sn$x + quantile(sn$x,0.25))),]
# Z-score x0.75
sn$z[which.min(abs(sn$x - quantile(sn$x,0.75))),]
# Z-score 0.999
sn$z[which.min(abs(sn$x + quantile(sn$x,0.999))),]
# Z-score 0.001
sn$z[which.min(abs(sn$x - quantile(sn$x,0.001))),]
# Z-score xmin
sn$z[which.min(abs(sn$x + quantile(sn$x,0.0001))),]
# Z-score xmax
sn$z[which.min(abs(sn$x - quantile(sn$x,0.9999))),]

# Simulate 300 (standard) normal distributions (or whatever you like) and draw z-scores
x25=list()
x75=list()
x975=list()
x025=list()
xmin=list()
xmax=list()
n=1
for (x in seq(1,300)){
  sn = data.frame(x=rnorm(100000, mean=0, sd=1))
  sn$z = scale(sn$x)
  x25[[n]]<-sn$z[which.min(abs(sn$x + quantile(sn$x,0.25))),]
  x75[[n]]<-sn$z[which.min(abs(sn$x - quantile(sn$x,0.75))),]
  x975[[n]]<-sn$z[which.min(abs(sn$x + quantile(sn$x,0.975))),]
  x025[[n]]<-sn$z[which.min(abs(sn$x - quantile(sn$x,0.025))),]
  xmin[[n]]<-sn$z[which.min(abs(sn$x + quantile(sn$x,0.999))),]
  xmax[[n]]<-sn$z[which.min(abs(sn$x - quantile(sn$x,0.001))),]
  n=n+1
}

res = do.call(rbind, Map(data.frame, x25=x25, x75=x75, x025=x025, x975=x975, xmin=xmin, xmax=xmax))
mean(res$x25)
mean(res$x75)
mean(res$x025)
mean(res$x975)
mean(res$xmin)
mean(res$xmax)
