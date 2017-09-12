
# chi-square test 

# Ex. 1


wt<-c(3,12)
f31<-c(9,6)
data.frame(wt,f31)

wtp=3/15; wtp
f31p=9/15; f31p

tt<-chisq.test(data.frame(wt,f31))
names(tt)
tt$expected
# tt$statistic
# tt$method
tt

xxx<-chisq.test(data.frame(wt,f31),correct = F)
xxx
xxx$expected


# Ex. 2


wt1<-c(3,12)
f311<-c(9,6)
f175<-c(0,15)
dd<-data.frame(wt1,f311,f175);dd

wt1p=3/15; wt1p
f311p=9/15; f311p
f175p=0/15; f175p

xx2<-chisq.test(dd)
xx2
names(xx2)
xx2$expected


fisher.test(dd)




