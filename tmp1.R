rural = STFDF(stations, dates, data.frame(PM10 = as.vector(air)))
rr = rural[,"2005::2010"]
unsel = which(apply(as(rr, "xts"), 2, function(x) all(is.na(x))))
r5to10 = rr[-unsel,]
summary(r5to10)
rn = row.names(r5to10@sp)[4:7]

par(mfrow=c(2,2))
# select 4, 5, 6, 7
for(i in rn) acf(na.omit(r5to10[i,]), main = i)
par(mfrow=c(1,1))
acf(na.omit(as(r5to10[rn,], "xts")))


rs = sample(dim(r5to10)[2], 100)

lst = lapply(rs, function(i) { x = r5to10[,i]; x$ti = i; rownames(x@coords) = NULL; x} )
pts = do.call(rbind, lst)
v = variogram(PM10~ti, pts[!is.na(pts$PM10),], dX=0)
vmod = fit.variogram(v, vgm(100, "Exp", 200))
plot(v, vmod)

vv = variogram(PM10~1, r5to10, width=20, tlags=0:5)
plot(vv)
plot(vv, map = FALSE)

metricVgm <- vgmST("metric",
                   joint=vgm(50,"Exp",100,0),
                   stAni=50)
metricVgm <- fit.StVariogram(vv, metricVgm)
plot(vv, metricVgm)

sepVgm <- vgmST("separable",
                space=vgm(0.9,"Exp", 123, 0.1),
                time =vgm(0.9,"Exp", 2.9, 0.1),
                sill=100)
sepVgm <- fit.StVariogram(vv, sepVgm, method = "L-BFGS-B",
                          lower = c(10,0,0.01,0,1),
                          upper = c(500,1,20,1,200))
plot(vv, list(sepVgm, metricVgm))

library(lattice)
plot(vv, list(sepVgm, metricVgm), all=T, wireframe=T, zlim=c(0,120),
     zlab=NULL,
     xlab=list("distance (km)", rot=30),
     ylab=list("time lag (days)", rot=-35),
     scales=list(arrows=F, z = list(distance = 5)))

# simple demo of 3D interpolation of 50 points with random normal values,
# randomly located in the unit cube
library(sp)

library(gstat)

n <- 50

data3D <- data.frame(x = runif(n), y = runif(n), z = runif(n), v = rnorm(n))

coordinates(data3D) = ~x+y+z

range1D <- seq(from = 0, to = 1, length = 20)

grid3D <- expand.grid(x = range1D, y = range1D, z = seq(from = 0, to = 1, length = 50))

gridded(grid3D) = ~x+y+z

res3D <- krige(formula = v ~ 1, data3D, grid3D, model = vgm(1, "Exp", .2))

#[using ordinary kriging]

data2d<-data.frame(x = runif(n), y = runif(n))
coordinates(data2d) <- ~x+y
data2dt <- STFDF(data2d, dates[1:50], data.frame(v = as.vector(as.matrix(runif(50*50) ,nrow = 50,ncol = 50))))
vv = variogram(v~1, data2dt, width=.1, tlags=0:50)
plot(vv)
grid2d <- expand.grid(x = seq(from = 0, to = 1, length = 20), y = seq(from = 0, to = 1, length = 20))
coordinates(grid2d) = ~x+y
grid2dt <- STF(grid2d, dates[1:50])
res <- krigeST(formula = v ~ 1, data = data2dt, newdata = grid2dt, sumMetricVgm)

v = variogram(v~1, data3D, dX=0)
vmod = fit.variogram(v, vgm(1, "Exp", .2))
plot(v, vmod)

library(lattice)

levelplot(var1.pred ~ x + y | z, as.data.frame(res3D))
