#Michael Wells 11/18/2022
rm(list = ls())

#Poisson process simulation

#Homogeneous Poisson process

#intensity functions
lam.1 = function(x)
{
  exp(x)
}
lam.2 = function(x)
{
  cos(x)^2
}
lam.3 = function(x)
{
  x^2
}
lam.4 = function(x)
{
  1/(x+1)
}
lam.5 = function(x)
{
  1/(x^2+1)
}
#endpoints of interval
a = 0
b = 10
#intensity of process.  Choose between the different lam functions
lambda = lam.2
#get Big lambda (expected number of points on interval)
Lambda = integrate(lambda,lower=a,upper=b)$value
#step-size.  Decrease this for better accuracy
h = 0.001
#x-axis
xstar = seq(from = a,to=b,by=h)
#accept-reject ratio for points in xstar
ratio = lambda(xstar)
#random uniforms scaled so number of points is approximately Lambda
unif = runif(length(xstar),min=0,max=length(xstar)/(b-a))
idx = unif < ratio
#keep only accepted points
pts = xstar[idx]

#generate height of jump process at points in x
#input:
#x - vector of points to evaluate at
#Output:
#num.jumps - vector of number of jumps at points in x
f = function(x)
{
  mat = outer(pts,x,function(s,t){s > t})
  num.jumps = apply(mat,2,function(v){length(v[which(v==F)])})
  return(num.jumps)
}

plot(xstar,f(xstar),type='l',xlab='',ylab='',main = "Inhomogeneous Poisson point process")

#generate samples of Poisson point process and plot mean, standard error
#and Int_0^x lambda(t) dt which is the theoretical expected value of the process at time x.
num.samples = 3

samps = matrix(0,nrow=num.samples,ncol=length(xstar))
for(i in 1:num.samples)
{
  unif = runif(length(xstar),min=0,max=length(xstar)/(b-a)) #max for the uniform sample mysteriously determined!
  ratio = lambda(xstar)
  idx = unif < ratio
  pts = xstar[idx]
  samps[i,] = f(xstar)
}

mean.samps = apply(samps,2,mean)
sd.samps = apply(samps,2,sd)/sqrt(num.samples)
upper.limit = max(max(mean.samps+sd.samps),Lambda)
#upper.limit = max(mean.samps + sd.samps)
plot(xstar,mean.samps+sd.samps,type='l',xlab='',ylab='',main=paste0('Mean of ', num.samples,' inhomogeneous samples'),ylim=c(min(mean.samps-sd.samps),upper.limit))
if(identical(lambda,lam.1))
  lines(xstar,exp(xstar)-1,col='blue',lwd=2)
if(identical(lambda,lam.3))
  lines(xstar,xstar^3/3,col='blue',lwd=2)
if(identical(lambda ,lam.2))
  lines(xstar,xstar/2+sin(2*xstar)/4,col='blue')
if(identical(lambda,lam.4))
  lines(xstar,log(xstar+1),col='blue',lwd=2)
if(identical(lambda,lam.5))
  lines(xstar,atan(xstar),col='blue',lwd=2)
lines(xstar,mean.samps-sd.samps)
lines(xstar, mean.samps, lwd=2)
print(max(sd.samps))