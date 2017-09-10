

#################   Exercise iris 1.   ############################


class(iris)  
head(iris) # returns the first part of the iris data frame 
str(iris) # compactly display the internal structure of an R object
names(iris)

data<-iris  #rename the data frame
data<-edit(data)  #rename the variables using editor 


names(data)<-c("SL","SW","PL","PW","S")
names(data)


# Ex. iris 1. a)

apply(data[,1:4],2,mean) # mean of the 4 variables
# the 2 stands for columns, if we put 1, than the mean of the row would be calculated

var.SL<-var(data[,1]) #variance of SL
var.SL
apply(data[,1:4],2,var) #variance of the 4 variables
apply(data[,1:4],2,summary)  
apply(data[,1:4],2,sd)
?fivenum
apply(data[,1:4],2,fivenum) #min,lower-hinge,median,upper-hinge,max
table(data[,5]) #uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels


# Ex. iris 1. b)

install.packages("moments")
library(moments)
skewness(data[,2]) # skewness of the SW
skewness(data[,1:4]) # skewness of the 4 variables
skewness(data[data$S=='setosa',2]) # skewness of SW (setosa)
skewness(data[data$S=='versicolor',2]) # skewness of SW (versicolor)
skewness(data[data$S=='virginica',2]) # skewness of SW (virginica)
by(data$SW,data$S,skewness) # function by is an object-oriented wrapper for "tapply" applied to data frames. 



# Ex. iris 1. c)

pdf("bx.pdf") 
boxplot(data$SW,data$SL,data$PW,data$PL)
bx<-boxplot(data$SW,data$SL,data$PW,data$PL)
bx
dev.off() # returns the number and name of the new active device

#stats	
#a matrix, each column contains the extreme of the lower whisker, 
#the lower hinge, the median, the upper hinge and the extreme of 
#the upper whisker for one group/plot. If all the inputs have the
# same class attribute, so will this component.

#n	
#a vector with the number of observations in each group.

#conf	
#a matrix where each column contains the lower and upper extremes of the notch.

#out	
#the values of any data points which lie ?boxplot the extremes of the whiskers.

#group	
#a vector of the same length as out whose elements indicate
# to which group the outlier belongs.

#names	
#a vector of names for the groups.


# Ex. iris 1. d)

par(mfrow=c(2,4))
hist(data$SW,main="SL",col="red",prob=T,xlab="")
hist(data$SL,main="SW",col="blue",xlab="")
hist(data$PW,main="PL",col="green1",xlab="")
hist(data$PL,main="PW",col="pink",xlab="")
boxplot(data$SL,col="red",horizontal=T)
boxplot(data$SW,col="blue",horizontal=T)
boxplot(data$PL,col="green1",horizontal=T)
boxplot(data$PW,col="pink",horizontal=T)


# Ex. iris 1. e)

pie(table(data$S),labels=c("setosa","versicolor","virginica"), main="species",)
?pie

#adding percentages
lbls<-c("setosa","versicolor","virginica")
pct <- round(pct<-table(data$S)/sum(table(data$S))*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(table(data$S),labels=lbls,main="species")



# Ex. iris 1. f)

#r
install.packages("psych")
library(psych)
par(mfrow=c(1,2))
error.bars(data[,1:4],alpha=0.05,bars=TRUE,sd=TRUE)  # error bar with sd
error.bars(data[,1:4],alpha=0.05,bars=TRUE)  #error bar with CI



# Ex. iris 1. g)

plot(density(data$SL),main="kernel densities",xlim=c(min(data$SL,data$SW,data$PL,data$PW),max(data$SL,data$SW,data$PL,data$PW)),ylim=c(0,1))
lines(density(data$SW),col="red")
lines(density(data$PL),col="blue")
lines(density(data$PW),col="green")
legend(4.5,0.8,legend=c("SL","SW","PL","PW"),lwd=1,col=c("black","red","blue","green"),cex=0.7)



#################   Exercise 2.   ############################

# Ex. 2. a)

# Variable quantitative, ratio (continuos data)


# Ex. 2. b)

bw<-c(3.3,3.5,3.6,3.6,3.7,3.8,3.8,3.8,3.9,3.9,3.9,4.0,4.0,4.0,4.0,4.1,4.1,4.1,4.2,4.2,4.3,4.3,4.4,4.5)
length(bw)
summary(bw)
tfre<-table(bw) #table of frequencies
tfre
cbind(Freq=tfre,Cumul=cumsum(tfre),relative=prop.table(tfre)) #Combine R objects by rows or columns
skewness(bw) # skewness of the bw
kurtosis(bw) # kurtosis of the bw


# Ex. 2. c)

boxplot(bw,main='Butterfly wing lengths',col='gold')
boxplot(bw,main='Butterfly wing lengths',col='gold',horizontal=T)
hist(bw,main='Butterfly wing lengths',breaks="Sturges", col="pink",xlab="Wing Lengths")



#################   Exercise 3.   ############################

dados<-read.table("Tumor.txt",header=T)
dados
summary(dados)
var(dados)
tfre1<-table(dados) #table of frequencies
cbind(Freq=tfre1,Cumul=cumsum(tfre1),relative=prop.table(tfre1))
boxplot(dados,main='Weigths of tumors',col='darkgray')
boxplot(dados,main='Weigths of tumors',col='gold',horizontal=T)

hist(dados$Tumor,main='Weigths of tumors',col="blue",xlab="Weigths")



#################   Exercise 4.   ############################

# Ex. 4. a)

wl<-c(10.4,10.8,11.1,10.2,10.3,10.2,10.7,10.5,10.8,11.2,10.6,11.4)
tl<-c(7.4,7.6,7.9,7.2,7.4,7.1,7.4,7.2,7.8,7.7,7.8,8.3)
summary(wl)
summary(tl)
dwl<-edit(wl)
dwl
par(mfrow=c(2,2))
hist(wl,main='Wing length',col='red')
hist(tl,main='Tail length',col='blue')
boxplot(wl,main='Wing length',col='red',horizontal=T)
boxplot(tl,main='Tail length',col='blue',horizontal=T)
dev.off() 

plot(wl,tl,main='Scatterplot',xlab='Wing length',ylab='Tail length',pch=19)
cbind(wl, tl)
xx<-lm(tl ~ wl) # lm is used to fit linear models
xx
abline(xx, col="red")


# Ex. 4. b)

?cor
cor(wl,tl,method='pearson')
cor(wl,tl)
cor(wl,tl,method='spearman')
cor(wl,tl,method='kendal')



#################   Exercise 5.   ############################

as1<-c(10,3,8,9,8,9,8,8,8,8,7,8,6,8,9,9,9,9,8,9,3,9,7,10)
as5<-c(10,6,9,10,9,10,9,9,9,9,9,9,9,10,10,10,10,9,10,9,3,9,10,10)

par(mfrow=c(2,2))
hist(as1,main='Apgar score at 1 minute',col='red')
hist(as5,main='Apgar score at 5 minutes',col='blue')
boxplot(as1, main= 'Apgar score at 1 minute' ,col='red',horizontal=T)
boxplot(as5, main='Apgar score at 5 minutes',col='blue',horizontal=T)

tfre2<-table(as5) #table of frequencies
cbind(Freq=tfre2,Cumul=cumsum(tfre2),relative=prop.table(tfre2))
summary(as5)

par(mfrow=c(1,1))
plot(as1,as5,main='Scatterplot',xlab='Apgar Score 1min',ylab='Apgar Score 5min',pch=24)
abline(lm(as5~as1), col="red") # regression line (y~x) 
lines(lowess(as1,as5), col="blue") # lowess line (x,y)

y<-lm(as5~as1) # linear model
coef(y) # coefficients of the model 

#abline - this function adds one or more straight lines through the current plot
#lines - add connected line segments to a plot



cor(as1,as5,method='spearman')
cor(as1,as5)






