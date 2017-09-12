library(lattice)

# Example: Sampling distribution of the variance


install.packages("lattice")
library(lattice)

n<-5
vec.var<-c(rep(0,500))
for(i in 1:500)
{x<-rnorm(n,mean=5,sd=2)
vec.var[i]<-var(x)*(n-1)/(2^2)}
qqmath(vec.var,distribution=function(p)qchisq(p,n-1),main=paste('Q-QPlot 
Chi-Square,n=',n)
,xlab="Theoretical quantiles",ylab="Empirical quantiles")

#############################################

n<-15
vec.var<-c(rep(0,500))
for(i in 1:500)
{x<-rnorm(n,mean=5,sd=2)
vec.var[i]<-var(x)*(n-1)/(2^2)}
qqmath(vec.var,distribution=function(p)qchisq(p,n-1),main=paste('Q-QPlot 
Chi-Square,n=',n)
,xlab="Theoretical quantiles",ylab="Empirical quantiles")


#############################################

n<-30
vec.var<-c(rep(0,500))
for(i in 1:500)
{x<-rnorm(n,mean=5,sd=2)
vec.var[i]<-var(x)*(n-1)/(2^2)}
qqmath(vec.var,distribution=function(p)qchisq(p,n-1),main=paste('Q-QPlot 
Chi-Square,n=',n)
,xlab="Theoretical quantiles",ylab="Empirical quantiles")


#############################################


n<-60
vec.var<-c(rep(0,500))
for(i in 1:500)
{x<-rnorm(n,mean=5,sd=2)
vec.var[i]<-var(x)*(n-1)/(2^2)}
qqmath(vec.var,distribution=function(p)qchisq(p,n-1),main=paste('Q-QPlot 
Chi-Square,n=',n)
,xlab="Theoretical quantiles",ylab="Empirical quantiles")











