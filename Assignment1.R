#Question 1
C<- c(5, 10, 15, 20, 25, 30)
max(C)
min(C)

#or

max_val=C[1]
min_val=C[1]
for(i in C)
{
  if(max_val<i)
  {
    max_val=i
  }
  if(min_val>i)
  {
    min_val=i
  }
}
print(max_val)
print(min_val)

#Question 2
a<- as.integer(readline(prompt="Enter the number for which you want to find factorial: "))
fact=1
if(a<0)
{
  print("Try again")
}else
{
  for(i in 1:a)
  {
    fact=fact*i
  }
}
fact
#or for direct factorial
print(paste("Fact is: ", factorial(6)))

#Question 3
fibonacci=function(n)
{
  a<- 0
  b<- 1
  for(i in 1:n)
  {
    cat(a," ") #single line answer
    temp<- b
    b<- a+b
    a<- temp
  }
}
fibonacci(5)


#Question 4
a<- as.integer(readline(prompt="Enter first number:"))
b<- as.integer(readline(prompt="Enter second number:"))
c<- as.character(readline(prompt="Enter the operation you want to perform:"))

res<- switch(c,
  "+"=cat("Addition is:",a+b,"\n"),
  "-"=cat("Subtraction is:",a-b,"\n"),
  "*"=cat("Multiply is:",a*b,"\n"),
  "/"=if(b!=0)
    {
      cat("Division is:",a/b,"\n")
    }else
    {
      print("Division by zero not possible")
    },
    cat("The calculator is defective.")
  
)

#Question 5
x=1:10
y=x^2
plot(x,y,main="BASIC PLOT",type="l",xlab="X-AXIS",ylab="Y-AXIS")
d<- c(10,20,30,40,50)
barplot(d,main="BAR PLOT",xlab="X-AXIS",ylab="Y-AXIS")
values <- c(30, 20, 50)
labels <- c("Category 1", "Category 2", "Category 3")
pie(values, labels = labels, main = "Pie Chart", col = c("red", "yellow", "blue"))
