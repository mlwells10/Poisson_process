#Michael Wells 11/18/2022
rm(list = ls())

#Poisson process simulation

#Homogeneous Poisson process


#endpoints of interval
a = 0
b = 10
#intensity of process
lambda = 5.2/(b-a)
#number of points on [a,b]
N = rpois(1,lambda*(b-a))
#generate jump points
pts = sort(runif(N,min=a,max=b))

#set step-size
h =0.01
#x interval
xstar = seq(from=a,to=b,by=h)
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

plot(xstar,f(xstar),type='l',xlab='',ylab='',main = paste0('Poisson jump process with intensity ',lambda,' over inerval [',a,',',b,']'))

#sample multiple Poisson processes
num.samples = 3
samps = matrix(0,nrow=num.samples,ncol = length(xstar))
N = rpois(num.samples,lambda*(b-a))
for(i in 1:num.samples)
{
  pts = runif(N[i],min=a,max=b)
  samps[i,] = f(xstar)
  
}

mean.samp = apply(samps,2,mean)
sd.samp = apply(samps,2,sd)/sqrt(num.samples)
upper.limit = max(max(mean.samp+sd.samp),lambda*(b-a))
lower.limit = min(mean.samp - sd.samp)
plot(xstar,mean.samp+sd.samp, type='l',xlab='',ylab='',main = paste0("Mean of ",num.samples," samples and standard error"),ylim=c(lower.limit,upper.limit))
lines(xstar,lambda*xstar,col='blue',lwd=2)
lines(xstar,mean.samp - sd.samp)
lines(xstar,mean.samp, lwd=2)
print(max(sd.samp))