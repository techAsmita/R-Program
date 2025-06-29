#Assingment 8
#sampling distribution
# 1)
set.seed(123)
num_sample<- 1000
n<- 10
p<- 0.4
x<- 30
sample2<- replicate(num_sample, mean(rbinom(x,n,p)))
print(sample2)
mean(sample2)
var(sample2)
skewness(sample2)
hist(sample2)

# 2)
install.packages('moments')
library(moments)
set.seed(123)
num_sample<- 1000
n<- 10
p<- 0.8
x<- 30
sample3<- replicate(num_sample, mean(rbinom(x,n,p)))
print(sample3)
mean(sample3)
var(sample3)
skewness(sample3)
hist(sample3)

#poisson distribution
set.seed(123)
num_sample<- 1000
n<- 50
lam2<- 4
sample4<- replicate(num_sample, mean(rpois(n,lam2)))
print(sample4)
mean(sample4)
var(sample4)
skewness(sample4)
hist(sample4)

#theoritical
mu=4
var=4

#calculate sample distribution for different lambda=1,4,10
fun2<- function(lambda)
{
  replicate(num_sample, mean(rpois(n,lambda)))
}
s11<- fun2(1)
print(s11)
s12<- fun2(4)
print(s12)
s13<- fun2(10)
print(s13)

hist(s11)
hist(s12)
hist(s13)
skewness(s11)
skewness(s12)
skewness(s13)

#exponential distribution
set.seed(123)
num_sample<- 1000
n<- 10
sample4<- replicate(num_sample, mean(rexp(n,1.5)))
print(sample4)
mean(sample4)
var(sample4)
skewness(sample4)
hist(sample4)
curve(dnorm(x,mean(sample4), sd(sample4)), col='skyblue',add=TRUE)

set.seed(123)
num_sample<- 1000
n<- 50
sample7<- replicate(num_sample, mean(rexp(n,1.5)))
print(sample7)
mean(sample7)
var(sample7)
skewness(sample7)
hist(sample7)
curve(dnorm(x,mean(sample7), sd(sample7)), col='blue',add=TRUE)


#normal distribution
samp3<- 5000
fun4<- function(n)
{
  replicate(samp3, mean(rnorm(n,mean=70,sd=10)))
}

n1<- fun4(50)
n2<- fun4(100)
hist(n1)
hist(n2)
curve(dnorm(x,mean(n1),sd(n1)),col='black',add=T)
curve(dnorm(x,mean(n2),sd(n2)),col='orange',add=T)

#gamma distribution
samp3<- 5000
fun4<- function(n)
{
  replicate(samp3, mean(rgamma(n,2,1)))
}
gamma1<- fun4(10)
gamma2<- fun4(100)
hist(gamma1)
hist(gamma2)
curve(dnorm(x,mean(gamma1),sd(gamma1)),col='black',add=T)
curve(dnorm(x,mean(gamma2),sd(gamma2)),col='pink',add=T)
