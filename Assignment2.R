#Assignment 2

#Q1 a.
chest<- c(rep("GOLD",20),rep("SILVER",30),rep("BRONZE",50))
print(chest)
sample(chest,10,replace=F) #without replacement.

#sample command doesn't give all sample spaces it randomly picks one.
#Q1 b.
surgical_proc<- c("success","failure")
sample(surgical_proc,10,replace=TRUE,prob<- c(0.9,0.1))
#as the percentage was higher so we did with replacement

#Q2
#problem of birthday paradox.
pbirthday(n=15,classes=365,coincident=2) #probablity that two people have same
#birthday in the room of n people let n=15 in this case.
qbirthday(classes=365,prob<- c(0.5), coincident=2)

#Q3
#Conditional probabality
P_rc=function(P_cloud,P_rain,P_cr)
{
  return (P_cr*P_rain/P_cloud)
}
P_cloud=0.4
P_rain=0.2
P_cr=0.85
Output=P_rc(P_cloud,P_rain,P_cr)
print(Output)

#Q4
iris #dataset for flowers.
head(iris,5) #prints first few rows for iris dataset
#by default if we not write the values, it prints first six rows.
str(iris)
#Sepal.Length
sepal.length<- iris[,"Sepal.Length"]
range(iris$Sepal.Length)
range(sepal.length)
mean(sepal.length)
median(sepal.length)
quantile(iris$Sepal.Length,c(0.25,0.75))
quantile(iris$Sepal.Length,c(0.50))
sd(iris$Sepal.Length)
var(iris$Sepal.Length)

#Sepal.Width
sepal.width=iris[,"Sepal.Width"]
range(sepal.width)
mean(sepal.width)
median(iris$Sepal.Width)
quantile(iris$Sepal.Width,c(0.25,0.75))
quantile(iris$Sepal.Width,c(0.50))
sd(iris$Sepal.Width)
var(iris$Sepal.Width)

#Petal.Length
petal.length=iris[,"Petal.Length"]
range(petal.length) #or
range(iris$Petal.Length)
mean(petal.length)
median(petal.length)
quantile(iris$Petal.Length,c(0.25,0.75))
quantile(iris$Petal.Length,c(0.50))
sd(petal.length)
var(petal.length)

#Petal.Width
petal.width=iris[,"Petal.Width"]
range(iris$Petal.Width)
mean(petal.width)
median(petal.width)
quantile(iris$Petal.Width,c(0.25,0.75))
quantile(iris$Petal.Width,c(0.50))
sd(petal.width)
var(petal.width)

summary(iris)

#Q5
#finding mode
v1<- c(10,10,9,9,8,7,3,4,5,5,5,4,5,5,6,10)
sort(v1)
table(v1)
sort(table(v1))
sort(table(v1),decreasing=T)[1] #returns value at first index.
#Hence, the max occuring number is 5.