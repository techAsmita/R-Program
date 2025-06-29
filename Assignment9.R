#Assignment 9
#Q1
data<- read.csv(file.choose())
print(data)
#to count the no. of rows
nrow(data)
ncol(data)
head(data,10)
tail(data,10)
#population mean
mu<- mean(data$Wall.Thickness)
print(mu)
hist(data$Wall.Thickness)
abline(v=mu,col='orange')

#sample size 10.
#set base sample size
n_sample<-1000
s<-10
vec_10<-numeric(n_sample)
for (i in 1:n_sample)
{
  sample<- sample(data$Wall.Thickness,s,replace=T)
  vec_10[i]<- mean(sample)
}
mu<- mean(vec_10)
std<- sd(vec_10)
hist(vec_10,prob=T)
curve(dnorm(x,mean=mu,sd=std), col='purple', add=T)
#for small n no bell shaped curve.
#for 50,500,9000
n<-50
vec_50<- numeric(n_sample)
for (i in 1:n_sample)
{
  sample<- sample(data$Wall.Thickness,n,replace=T)
  vec_50[i]<- mean(sample)
}
mu<- mean(vec_50)
std<- sd(vec_50)
hist(vec_50,prob=T)
curve(dnorm(x,mean=mu,sd=std), col='brown', add=T)

n<-500
vec_500<- numeric(n_sample)
for (i in 1:n_sample)
{
  sample<- sample(data$Wall.Thickness,n,replace=T)
  vec_500[i]<- mean(sample)
}
mu<- mean(vec_500)
std<- sd(vec_500)
hist(vec_500,prob=T)
curve(dnorm(x,mean=mu,sd=std), col='purple', add=T)

n<-9000
vec_9000<- numeric(n_sample)
for (i in 1:n_sample)
{
  sample<- sample(data$Wall.Thickness,n,replace=T)
  vec_9000[i]<- mean(sample)
}
mu<- mean(vec_9000)
std<- sd(vec_9000)
hist(vec_9000,prob=T)
curve(dnorm(x,mean=mu,sd=std), col='purple', add=T)

#Q2
#regression line
Age<- c(58, 69, 43, 39, 63, 52, 47, 31, 74, 36)
Cholesterol<- c(189, 235, 193, 177, 154, 191, 213, 165, 198, 181)
plot(Age,Cholesterol)
model2<- lm(Cholesterol~Age)
summary(model2)
abline(model2,col='violet')
predict(model2, data.frame(Age=60))

#Q3
help(t.test)
Before_test<- c(145, 173, 158, 141, 167, 159, 154, 167, 145, 153)
After_test<-c(155, 167, 156, 149, 168, 162, 158, 169, 157, 161)
test_sample<- t.test(Before_test,After_test,alternative='less',paired=T)
print(test_sample)
p=test_sample$p.value #probability value.
if(p<0.05)
{
  print('Reject null hypothesis')
  
}else if(p>=0.05)
{
  print('Failed to reject null hypothesis')
}else
{ NULL}
