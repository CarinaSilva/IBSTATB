
#################   Exercise 1.   ############################

# Ex. 1. a)
a<-251/502
a

# Ex. 1. b)
b<-(103+25)/251
b

#################   Exercise 2.   ############################

# Ex. 2. a)
sens<-19/213
sens

# Ex. 2. b)
spec<-417/432
spec

# Ex. 2. c)
ppv<-(19/213*0.02)/(19/213*0.02+(1-417/432)*0.98)
ppv

# Ex. 2. d)
npv<-(417/432*0.98)/(417/432*0.98+(1-19/213)*0.02)
npv


#################   Exercise 3.   ############################

# Ex. 3. a)
#  X r.v. that represents the number of stormy days in a winter
#  X  ~  Bi(90, 1/3)


# Ex. 3. b)
mean<-90*(1/3) #E(X)
mean
var<-mean*(1-(1/3)) #var(X)
var

1-pbinom(30,90,1/3) # P(X>30)=1-P(X<=30)
# dbinom(0,90,1/3)

pbinom(30,90,1/3, lower.tail = FALSE) #P(X>30)

# Ex. 3. c)

# set.seed(0.898)
bino_storm<-rbinom(100,90,1/3)
bino_storm
hist(bino_storm,main="Distribution of the number of stormy days in a season",col="cyan",prob=T)
hist(bino_storm,main="Distribution of the number of stormy days in a season",col="purple", border = "pink",freq=T)
mbino_storm<-mean(bino_storm)
mbino_storm


bino_storm<-rbinom(10000,90,1/3) # Generation of 10000Binomial variables
bino_storm
hist(bino_storm,main="Distribution of the number of stormy days in a season",col="cyan",prob=T)
hist(bino_storm,main="Distribution of the number of stormy days in a season",col="purple", border = "pink",freq=T)
mbino_storm<-mean(bino_storm)
mbino_storm

#################   Exercise 4.   ############################

# Ex. 4. a)

#  X r.v. that represents the number of purines in a microRNA of size 20
#  X  ~  Bi(20, 0.7)

a<-dbinom(14,20,0.7)
a


# Ex. 4. b)
b<-pbinom(14,20,0.7)
b

# Ex. 4. c)
c<-pbinom(15,20,0.7)-pbinom(10,20,0.7)-dbinom(15,20,0.7)
c
c1<-pbinom(14,20,0.7)-pbinom(10,20,0.7) # X is a r.v. discrete
c1


# Ex. 4. d)
d<-mean_10<-20*0.7
d
sd<-sqrt(20*0.7*0.3)
sd


#################   Exercise 5.   ############################

# Ex. 5. a)

a.5<-pnorm(1.2,1.6,0.42)
a.5

# Ex. 5. b)
b.5<-pnorm(2,1.6,0.42)-pnorm(1.2,1.6,0.42)
b.5

# Ex. 5. c)
c.5<-qnorm(0.15,1.6,0.42)
c.5

# Ex. 5. d)
d.5<-rnorm(1000,1.6,0.42)
sample_1000<-mean(d.5)
sample_1000
sd_sample_1000<-sd(d.5)
sd_sample_1000



#################   Exercise 6.   ############################

# Ex. 6. a)

set<-c(0.12,0.24,0.01,0.06,0.18,0.55,0.89,1.00,1.45,2.5)

plot(density(set))
plot(density(set),col="darkblue")
qqnorm(set)
qqline(set,col="red")


# Ex. 6. b)

new_set<-log2(set)

plot(density(new_set),col="darkblue")
qqnorm(new_set)
qqline(new_set,col="turquoise3")


# Ex. 6. c)

par(mfrow=c(1,2))
qqnorm(set)
qqline(set,col="magenta3")
qqnorm(new_set)
qqline(new_set,col="royalblue1")


