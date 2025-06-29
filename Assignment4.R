#Assignment 4
#Q1
#Discrete r.v
x<- c(0,1,2,3,4)
p_x<- c(0.41,0.37,0.16,0.05,0.01)
avg_mean=sum(x*p_x) #expectation value
print(avg_mean)
weighted.mean(x,p_x) #expectation value
e_x<- c(x%*%p_x)
print(e_x) #expectation value
#Three ways we can find out expected mean.

#Q2
#Continuous r.v
f=function(t)
{
  t*0.1*exp(-0.1*t)
}
expectation_val=integrate(f,0,Inf)
print(expectation_val)

#Q3
x<- c(0, 1, 2, 3)
p_x<- c(0.1, 0.2, 0.2,0.5)
e_x=sum(x*p_x)
e_x
weighted.mean(x,p_x)
e_y=12*e_x+2*(3-e_x)-18
print(e_y)

#Q4
mean_x=function(x)
{
  x*0.5*exp(-(abs(x)))
}
first_moment=integrate(mean_x,1,10)
print(first_moment$value)
var_x=function(x)
{
  (x^2)*0.5*exp(-(abs(x)))
}
second_moment=integrate(var_x,1,10)
y=second_moment$value-first_moment$value*first_moment$value
print(y)
#Q5
z<- c(1,2,3,4,5)
f=function(x)
{
  (0.75)*(0.25^(sqrt(x)-1))
}
g=function(x)
{
  x^2
}
y=sapply(z,g)
y
py=sapply(y,f)
py
m=sum(y*py)
m
e=sum(y*y*py)
e
var=e-m^2
var