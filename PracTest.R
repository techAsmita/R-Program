install.packages('pracma')
library(pracma)
#sample paper
#Q1
x<- 0:3
y<- 1:2
func1<- outer(x,y,function(x,y) (x+y))
s<- sum(func1)
print(s)
c=1/s
print(c)

#Q2
alpha=3
beta=1/5
#a)
1-pgamma(2,alpha,beta)
#b)
qgamma(0.90,alpha,beta)

#Q3
data(mtcars)
print(data)
print(mtcars)
head(mtcars,5)
#draw sample size
n<-10
num_samples<- 1000
vec1<- numeric(num_samples)
for (i in 1:num_samples)
{
  samp<- sample(mtcars$drat,n,replace=T)
  vec1[i]=mean(samp)
}
mean(vec1)
sd(vec1)
hist(vec1,prob=T)
curve(dnorm(x,mean(vec1),sd(vec1)),col='yellow',add=T)

n<-30
num_samples<- 1000
vec1<- numeric(num_samples)
for (i in 1:num_samples)
{
  samp<- sample(mtcars$disp,n,replace=T)
  vec1[i]=mean(samp)
}
mean(vec1)
sd(vec1)
hist(vec1,prob=T)
curve(dnorm(x,mean(vec1),sd(vec1)),col='red',add=T)

n<-50
num_samples<- 1000
vec1<- numeric(num_samples)
for (i in 1:num_samples)
{
  samp<- sample(mtcars$disp,n,replace=T)
  vec1[i]=mean(samp)
}
mean(vec1)
sd(vec1)
hist(vec1,prob=T)
curve(dnorm(x,mean(vec1),sd(vec1)),col='pink',add=T)

#Q5
qf(0.95,5,10)
pf(1.5,5,10)
1-pf(1.5,5,10)
qf(c(0.25,0.5,0.75,0.9),5,10)
y<- rf(1000,5,10)
hist(y)

#Q7
Years<-c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7)
Salary<- c(39343, 46205, 37731, 43525, 39891, 56642, 60150, 54445, 64445, 57189)
plot(Years,Salary)
model1<- lm(Salary~Years)
abline(model1,col='purple')
predict(model1,data.frame(Years=2.5))

#Q9
print(CO2)
data("CO2") #load dataset data('name of dataset')
print(data)
co2_values <- as.numeric(co2)
mean(co2_values)
var(co2_values)

num_samples<- 100
n<-10
vec2<- numeric(num_samples)
for( i in 1:num_samples)
{
  sample2<-sample(CO2$conc,n,replace=T)
  vec2[i]<- sample2
}
sd(vec2)
hist(vec2,xlab = expression((n-1)*S^2/sigma^2))
curve(dchisq(x,n-1)* length(vec2) * diff(hist(vec2, plot=FALSE)$breaks[1:2]),col='blue',add=T)
#chi squared distribution n-1*s^2/sigma^2.

#Q10
mg1 <- c(284, 279, 289, 292, 287, 295, 285, 279, 306, 298)
mg2 <- c(298, 307, 297, 297, 291, 335, 299, 300, 306, 291)

# Paired t-test, alternative hypothesis: mg1 < mg2
t1 <- t.test(mg1, mg2, alternative = 'less', paired = TRUE)
print(t1)

# Extract p-value
p3 <- t1$p.value
print(p3)
# Decision based on significance level alpha = 0.05
if (p3 < 0.05) {
  print('Reject null hypothesis.')
} else {
  print('Failed to reject null hypothesis.')
}

#Q8
x<- 0:3
y<- 0:2
fun2<- outer(x,y, function(x,y) (4*x+7*y)/15)
sum(fun2)
dimnames(fun2)=list(x=x,y=y)
print(fun2)
g_x<- sum(fun2['3',])
print(g_x)


#Q6
x1 <- 1:3
y1 <- 1:3

# f(x,y) and g(x,y) using outer
f_xy <- outer(x1, y1, function(x, y) (2 * x) / y)
g_xy <- outer(x1, y1, function(x, y) (x * y) / 2)

# Print f and g matrices
print(f_xy)
print(g_xy)

# Initialize h(x,y)
h_xy <- matrix(0, nrow = 3, ncol = 3)

# Calculate h(x,y) = f(x,y) * g(x,y)
for (i in 1:3) {
  for (j in 1:3) {
    h_xy[i, j] <- f_xy[i, j] * g_xy[i, j]
  }
}

# Print h(x,y)
print(h_xy)

# Check if h is a joint PDF
h1 <- sum(h_xy)
print(h1)

#read.csv(file.choose())

#Q4
func2 <- function(x, y) {
  (x + y)
}

I <- integral2(func2, 0, 1, 0, 1)
print(I$Q)

func3 <- function(x, y) {
  (x / (y^3) + (x^2) * y) * (x + y)
}

int2 <- integral2(func3, 0, 1, 1e-6, 1)  # Use 0 as the lower limit for y here.
print(int2$Q)
val2 <- int2$value
first_m <- val2

func5 <- function(x, y) {
  (x / (y^3) + (x^2) * y)^2 * (x + y)
}

second_m <- integral2(func5, 0, 1, 1e-6, 1)  # Use 0 as the lower limit for y here.
print(second_m$Q)
val3 <- second_m$value

variance_xy <- val3 - (first_m)^2
print(variance_xy)


