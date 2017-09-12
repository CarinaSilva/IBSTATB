



# Example in R: CI for the expected value
# ----------------------------------------

mean<-60.57
s<-1.9
n<-20


mean-qt(0.995,19,lower.tail=T)*s/sqrt(n)  # lower bound
mean+qt(0.995,19,lower.tail=T)*s/sqrt(n)  # upper bound


# mean-qt(0.005,19,lower.tail=T)*s/sqrt(n)  # lower bound
# mean+qt(0.995,19,lower.tail=T)*s/sqrt(n)  # upper bound


# Example - Vitamin loss in a food product
# ----------------------------------------

x<-c(45, 32, 47, 40, 38, 41, 37, 52, 37)
y<-c(38, 40, 35, 38, 34, 35, 38, 38, 40)
wilcox.test(x, y, alternative = "two.sided", paired=TRUE)

dif<-x-y
hist(dif, col="red")
boxplot(dif, col="blue")
qqnorm(dif)
qqline(dif)
shapiro.test(dif)
t.test(dif, mu=0)
t.test(x,y,paired=T)

# Example - Genetic inheritance
# -----------------------------

x<-c(8.50, 9.48, 8.65, 8.16, 8.83, 7.76, 8.63)
y<-c(8.27, 8.20, 8.25, 8.14, 9.00, 8.10, 7.20, 8.32, 7.70)
wilcox.test(x, y, alternative = "two.sided", paired = FALSE)
t.test(x,y,paired=F)


# Example - Cows' weights
# -----------------------

a<-c(30.28, 27.5, 27.9, 29.33)
b<-c(34.26, 32.55, 21.78, 25.59, 35.08, 26.86)
c<-c(39.47, 30.15, 33.40, 27.38, 30.39, 25.85, 29.11, 26.22)
d<-c(33.54, 30.40, 29.60, 28.82, 30.70, 30.83, 33.84)
x<-list(a,b,c,d)
x
kruskal.test(x)

