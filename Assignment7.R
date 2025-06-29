#Assignment 7
#Q1
#t distribution
t_values<- rt(100,99)
print(t_values)
hist(t_values)
curve(dt(x,99),col='purple',add=T,lwd=2)

#Q2
#chi-square distribution
chi_values<- function(df)
{
  rchisq(100,df)
}
chi2<- chi_values(2)
hist(chi2)
curve(dchisq(x,2),col='pink',add=T,lwd=2)
chi3<- chi_values(10)
hist(chi3)
curve(dchisq(x,10),col='red',add=T,lwd=2)
chi4<- chi_values(25)
hist(chi4)
curve(dchisq(x,25),col='green',add=T,lwd=2)

#Q3
v<- seq(-6,6,length=100)
print(v)
t_values<- function(df1)
{
  dt(v,df1)
}
t_df1 <- t_values(1)
t_df4 <- t_values(4)
t_df10 <- t_values(10)
t_df30 <- t_values(30)
plot(v,t_df30,type='l',col="blue")
lines(v, t_df1, col = "red", )
lines(v, t_df4, col = "green")
lines(v, t_df10, col = "purple")
#seq helps to generate vector random values between two points.

#Q4
df1<- 10
df2<- 20
f_value<- qf(0.95,df1,df2)
print(f_value)
#for area [0,1.5]
pf(1.5,10,20)
#for area[1.5, inf] area=probility.
1-pf(1.5,10,20)

quan<- function(q)
{
  qf(q,10,20)
}
q1<- quan(0.25)
q2<- quan(0.5)
q3<- quan(0.75)
q4<- quan(0.999)
print(q1)
print(q2)
print(q3)
print(q4)

r_value<-rf(1000,10,20)
hist(r_value)
hist(rf(1000,10,20))
curve(df(x,10,20),col='skyblue',add=T,lwd=2)
