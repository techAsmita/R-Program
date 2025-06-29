#Assignment3
#Q1
n=12
prob=1/6
pbinom(9, size = 12, prob = 1/6) - pbinom(6, size = 12, prob = 1/6)
prob2<- sum(dbinom(7:9,12,1/6))
print(prob2)

#Q2
mean_x=72
std_x=15.2
X=84
prob=1-pnorm(X,mean_x,std_x)
print(prob*100)

#Q3
#no cars arrive at this time 
dpois(0,5)
ppois(50,50)-ppois(47,50)

#Q4
p=17/250
dbinom(3,5,p)

#Q5
n=31
prob=0.447
d=dbinom(0:31,31,0.447)
p=pbinom(0:31,31,0.447)
var_x=n*p*(1-p)
print(var_x)
std_x=sqrt(var_x)
print(std_x)
mean(n*p)
plot(d)
plot(p)
barplot(d)
barplot(p)
dbinom(0:31,31,0.447)
