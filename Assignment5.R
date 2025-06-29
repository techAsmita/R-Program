#Assignment 5
#some continuous distribution.
#Q1
total_min<- 60
1-punif(45,0,total_min)
punif(30,0,60)-punif(20,0,60)

#Q2
k=1/2
dexp(3,k)
a<- c(0:5)
plot(dexp(a,k)) #pdf
pexp(3,k)
plot(pexp(a,k)) #cdf
g<- rexp(1000,k)
hist(g)
hist(rexp(1000,k))

#Q3
1-pgamma(1,2,1/3)
qgamma(0.70,2,1/3,lower.tail = F)
