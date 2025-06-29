#Assignment 6
#Q1
install.packages('pracma')
library(pracma)
joint_pdf<- function(x,y)
{
  (2/5)*(2*x+3*y)
}
i<- integral2(joint_pdf,0,1,0,1)
print(i$Q)

g<- function(y)joint_pdf(1,y)
g_x<- integral(g,0,1)
print(g_x)
h<-function(x)joint_pdf(x,0)
h_y<- integral(h,0,1)
print(h_y)

joint_pdf2<- function(x,y) x*y*joint_pdf(x,y)
e_xy<- integral2(joint_pdf2,0,1,0,1)
e_xy$Q

#Q2
x<- 0:3
y<- 0:2
fun<- outer(x,y, function(x,y) (x+y)/30)
dimnames(fun)<- list(x=x,y=y)
print(fun)

sum(fun)
g_x1<- rowSums(fun)
g_x1
colSums(fun)

p_x1<- fun['0','1']
p_x<- sum(fun[,'1'])
cond_prob1<- p_x1/p_x
cond_prob1

e_x<- sum(x*g_x)
e_x
e_hy<- sum(y*h_y)
e_hy
#variance var(x)
e_x<- sum(x^2 * g_x)
print(e_x)
e_y<- sum(y^2 * h_y)
print(e_y)
var_x<- e_x-e_gx^2
var_x
var_y<- e_y-e_hy^2
var_y
joint_pmf2<- outer(x,y, function(x,y) x*y*(x+y)/30)
dimnames(joint_pmf2)<- list(x=x,y=y)
joint_pmf2
e_xy=sum(joint_pmf2)
e_xy
#covariance
cov<-e_xy-e_gx*e_hy
cov
#correlation coefficient
corr_coeff<- cov/sqrt(var_x*var_y)
corr_coeff