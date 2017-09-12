###################### ###########################

# This function computes the estimate of skewness of the distribution
# of x, following Box 6.2 in Sokal and Rohlf

skew <- function(x) {
  m <- mean(x)
  s <- sd(x)
  y <- x-m
  sumy3 <- sum(y^3)
  n <- length(x)
  ( n/((n-1)*(n-2)) ) * sumy3/s^3
}


# This function computes the estimate of the standard error of the
# skewness of the distribution of x, following Box 7.1 in Sokal and
# Rohlf

skew.se <- function(x) {
  n <- length(x)
  sqrt( 6*n*(n-1) / ((n-2)*(n+1)*(n+3)) )
}


# This function computes the estimate of kurtosis of the distribution
# of x, following Box 6.2 in Sokal and Rohlf

kurtosis <- function(x) {
  m <- mean(x)
  s <- sd(x)
  y <- x-m
  sumy4 <- sum(y^4)
  n <- length(x)
  ( (n+1)*n/((n-1)*(n-2)*(n-3)) ) * sumy4/s^4 - 3*(n-1)^2/((n-2)*(n-3))
}


# This function computes the estimate of the standard error of the
# kurtosis of the distribution of x, following Box 7.1 in Sokal and
# Rohlf

kurtosis.se <- function(x) {
  n <- length(x)
  sqrt( 24*n*(n-1)^2 / ((n-3)*(n-2)*(n+3)*(n+5)) )
}

###################### ###########################

# packages needed to perform SIGN.test

install.packages("BSDA")
install.packages("lattice")
install.packages("e1071")
install.packages("class")

library(BSDA)
library(lattice)
library(e1071)
library(class)

#####################






# Confidence interval 

# dataset: Tumor

dados<-read.table("Tumor.txt",header=T)
dados
# View(dados)
summary(dados)
tfre1<-table(dados) #table of frequencies
cbind(Freq=tfre1,Cumul=cumsum(tfre1),relative=prop.table(tfre1))
boxplot(dados,main='Weigths of tumors',col='gold',horizontal=T)

int_conf<- t.test(dados,alternative="two.sided", conf.level=0.95)
names(int_conf)
int_conf$conf.int
int_conf$p.value
# int_conf<- t.test(dados,alternative="two.sided", conf.level=0.99)

?t.test

t.test(dados,alternative="two.sided", conf.level=0.95)$conf.int #$conf.int

# t.test(dados,alternative="two.sided", conf.level=0.95)$p.value


m<-mean(dados[,1])
m
sdados<-sd(dados[,1])
sdados
n<-length(dados[,1])
n

m-qt(0.975,n-1,lower.tail=T)*sdados/sqrt(n)  # lower bound
m+qt(0.975,n-1,lower.tail=T)*sdados/sqrt(n)  # upper bound




#################   Exercise 1.   ############################


m1<-c(84.63, 84.38, 84.08, 84.41, 83.82, 83.55, 83.92, 83.69, 84.06, 84.03)
m2<-c(83.15, 83.72, 83.84, 84.20, 83.92, 84.16, 84.02, 83.60, 84.13, 84.24)


dif<-m1-m2
dif

summary(dif)
hist(dif,main='Concentration of paracetamol',col='red')
boxplot(dif,main='Concentration of paracetamol',col='gold')
qqnorm(dif)
qqline(dif)

shapiro.test(dif)
install.packages("moments")
library(moments)


skew(dif)
skew.se(dif)
ss_dif<-skew(dif)/skew.se(dif)
ss_dif
pvalue_dif<-1-pnorm(ss_dif, 0, 1)
pvalue_dif


t.test(dif,mu=0)

wilcox.test(dif, alternative ="two.sided",exact=F,correct=F) 
wilcox.test(m1,m2,alternative="two.sided",exact=T,correct=F,paired=T)
# SIGN.test(m1,m2,alternative = "two.sided", conf.level = 0.95) # if it has not symmetry


#################   Exercise 2.   ############################


x<-30
n<-260
p.hat<-x/n
p.hat


# Ex. 2. b)


prop.test(x,n,alternative="two.sided",conf.level=0.95)  # or prop.test(x,n)
prop.test(x,n)

# Ex. 2. c)


prop.test(x,n,p=0.1,alternative="greater")



?prop.test(x,n)
prop.test(x,n,alternative="two.sided",conf.level=0.95)
prop.test(x,n,alternative="two.sided",conf.level=0.90)


#################   Exercise 3.   ############################


a<-read.table("Data_P4.txt",header=T)
length(a[,1])  
View(a)


#  Ex. 3. a)

summary(a[,1])
var(a[,1])
cv<-var(a[,1])/mean(a[,1])*100
cv

hist(a[,1],main='Body temperature',col='pink')
boxplot(a[,1],main='Body temperature',col='gold')
t.test(a[,1],alternative="two.sided", conf.level=0.95)

qqnorm(a[,1])
qqline(a[,1])

shapiro.test(a[,1])
bartlett.test(list(w.women,w.men))
t.test(a[,1],mu=98.6,alternative="two.sided")


#  Ex. 3. b)

w.women<-a[a[,2]=="female",1]
w.women
w.men<-a[a[,2]=="male",1]
w.men
summary(w.women)
summary(w.men)

par(mfrow=c(1,2))
hist(w.women,main='women',col='pink')
hist(w.men,main='men',col='blue')

par(mfrow=c(1,2))
boxplot(w.women,main='women',col='pink')
boxplot(w.men,main='men',col='blue')

boxplot(w.women,w.men)

qqnorm(w.women)
qqline(w.women)
qqnorm(w.men)
qqline(w.men)

shapiro.test(w.women)
shapiro.test(w.men)

t.test(w.women,w.men,alternative="two.sided")


# Ex. 3. c)


t.test(w.women,w.men,alternative="two.sided",confidence=0.9)$conf.int

# If "1" belongs to the CI 90%, do not reject H0.



#################   Exercise 4.   ############################

d<-c(91.9,97.8,111.4,122.3,105.4,95.0,103.8,99.6,96.6,119.3,104.8,101.7)

summary(d)
hist(d,col='red')
boxplot(d,col='gold')
qqnorm(d)
qqline(d)

shapiro.test(d)

wilcox.test(d,mu=105,alternative="two.sided")
t.test(d,mu=105,alternative="two.sided")


######################################

tt<-t.test(d,mu=105,alternative="two.sided")
names(tt)
tt$method
tt$statistic
tt$null.value
tt$p.value


####################################


#################   Exercise 5.   ############################

premier<-c(24.5, 23.4, 25.3, 23.4, 22.1)
super<-c(26.4, 27.0, 25.2, 25.8, 27.1)

summary(premier)
summary(super)

par(mfrow=c(1,2))
hist(premier,main='premier',col='pink')
hist(super,main='super',col='blue')

par(mfrow=c(1,2))
boxplot(premier,main='premier',col='pink')
boxplot(super,main='super',col='blue')

boxplot(premier,super)

qqnorm(premier)
qqline(premier)
qqnorm(super)
qqline(super)

shapiro.test(premier)
shapiro.test(super)


skew(premier)
skew.se(premier)
ss<-skew(premier)/skew.se(premier)
ss
pvalue_ss<-2*pnorm(ss, 0, 1)
pvalue_ss
skew(super)
skew.se(super)
sss<-skew(super)/skew.se(super)
sss
pvalue_sss<-2*pnorm(sss, 0, 1)
pvalue_sss

wilcox.test(premier,super,exact=F)


################################################
install.packages("BSDA")
install.packages("lattice")
install.packages("e1071")
install.packages("class")

library(BSDA)
library(lattice)
library(e1071)
library(class)

################################################


SIGN.test(premier,super,alternative = "two.sided", conf.level = 0.95)

SIGN.test(x, y = NULL, md = 0, alternative = "two.sided", conf.level = 0.95)




#################   Exercise 6.   ############################


a<-matrix(c(0.857,0.5,0,1,0.5,0.833,0,0.667,1,1,0.167,1,0,1,0,0.75,0,1,0,1,0,1,0,1,1,1,1,1),14,2,byrow=TRUE)
View(a)

diff<-c(a[,2]-a[,1])  #After-Before
diff

summary(diff)
hist(diff,col='red')
boxplot(diff,col='gold')
qqnorm(diff)
qqline(diff)

shapiro.test(diff) 


# Ex. 6. b)


wilcox.test(diff, alternative =  "greater")  
    
       
# Ex. 6. c)

# Visualize the nonparametric confidence interval

wilcox.test(diff, conf.level = 0.90)$conf.int

# It is not possible to compute the confidence interval!



#################   Exercise 7.   ############################

sedif<-(16.4-20.1)/(-1.779)
sedif
criticalv<-qt(0.05,18)
criticalv
# see exercice 4



#################   Exercise 8.   ############################

#See Ex. 9


#################   Exercise 9.   ############################

smale<-c(220.1,218.6,229.6,228.8,222.0,224.1,226.5)
sfemale<-c(223.4,221.5,230.2,224.3,223.8,230.8)
summary(smale)
summary(sfemale)

par(mfrow=c(1,2))
hist(smale,main='smale',col='blue')
hist(sfemale,main='sfemale',col='pink')

par(mfrow=c(1,2))
boxplot(smale,main='smale',col='blue')
boxplot(sfemale,main='sfemale',col='pink')

boxplot(smale,sfemale)

qqnorm(smale)
qqline(smale)
qqnorm(sfemale)
qqline(sfemale)



wilcox.test(smale,sfemale,exact=F)
?wilcox.test



#################   Exercise 10.   ############################

# Ex. 10. a)

bb<-c(36.3,36.0,40.8,44.9,32.8,28.8,38.4,31.1,29.8,30.2)
aa<-c(35.3,38.3,37.9,37.6,28.8,27.1,42.6,34.7,30.6,36.8)

dif<-bb-aa
dif

summary(dif)
par(mfrow=c(1,2))
hist(dif,main='Enterocyte heigth',col='red')
boxplot(dif,main='Enterocyte heigth',col='gold')

qqnorm(dif)
qqline(dif)

shapiro.test(dif)

t.test(dif,mu=0)


# Ex. 10. b)

t.test(dif,alternative="two.sided", conf.level=0.90)


# compute skewness error

skew(data$SL)
skew.se(data$SL)




#################   Exercise 11.   ############################

colors=rbind(c(32,43,16,9),c(55,65,64,16))
colors
colors1<-chisq.test(colors)
colors1
names(colors1)
colors1$expected



#################   Exercise 12.   ############################

bird=rbind(c(163,135,71,43),c(86,77,40,38))
bird
bird1<-chisq.test(bird)
bird1
names(bird1)
bird1$expected



################## Chi-square test (examples)

#  Chi-square test of independence



library (MASS)  # load the MASS package 
tbl=table(survey$Smoke, survey$Exerc)
tbl    # the contingency table 
chisq.test(tbl)
fisher.test(tbl)

