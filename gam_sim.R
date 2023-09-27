# gam simulation test

# generate independent variables
# basic
sim_sst   <-arima.sim(model= list(order = c(1, 1, 0),ar=0.9), n=1000)
sim_fmort <-arima.sim(model= list(order = c(1, 1, 0),ar=0.9), n=1000)
  
sim_growth<-1.5*sim_sst + 2*sim_sst^2 + 3*sim_fmort
obs_growth<-sim_growth*rnorm(length(sim_growth),1,0.5)

par(mfrow=c(3,1))
plot(obs_growth~sim_sst)
plot(obs_growth~sim_fmort)
plot(sim_growth)
points(obs_growth)
# make 

library(mgcv)

mod<-gam(obs_growth~s(sim_sst,sim_fmort))
summary(mod)
par(mfrow=c(1,1))
plot(mod)
mod

plot(obs_growth)
lines(mod$fitted.values,lty=2,col=2,lwd=2)

#==simulate with autocorrelation in growth (and not just sst and fmort)

