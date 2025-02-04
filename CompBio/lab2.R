boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Length, ylab="Sepal Length", xlab=" ", main="Iris(genus)", col = "blue")

boxplot(iris$Petal.Width~iris$Species, ylab="Petal Width", xlab="Iris species ", main="Iris:Petal Width by Species",
        col = c("red", "blue", "yellow"))
?lm()
mod2<- lm(iris$Petal.Width~iris$Petal.Length)
plot(iris$Petal.Length, iris$Petal.Width) 
abline(mod2, col = "grey", lty = 3, lwd = 5)

mod3<- lm(iris$Sepal.Width~iris$Sepal.Length)
plot(iris$Sepal.Length, iris$Sepal.Width, main = "Sepal Width by Length in Iris", col=as.numeric(as.factor(iris$Species)), pch=as.numeric(as.factor(iris$Species)))
legend(x = "topright", legend = c("Setosa", "Versicolor", "Virginica"),pch = 1:3)
abline(mod3)
summary(mod3)

sum<- function(x,y)
{
  res<- x+y
  return(res)
  
}

sum(6, 9)



mylist<- list(values = 1:3, ids = c("A", "B", "C"))

mylist$ids
mylist$values
mylist[1]
mylist[[1]]
mylist[2]



for(i in seq(20, 30, by = 2)) {
  print(i)
}


##2,3,5,7,11,13,17,19,23,29

primeNumbers<- c(2,3,5,7,11,13,17,19,23,29)

sample(primeNumbers)


norm1<- rnorm(100)
norm2<- runif(100)
norm3<- rchisq(100, 2)
norm4<- rbinom(100, 2, 0.5)

hist(norm1)
hist(norm2)
hist(norm3)
hist(norm4)

plot(density(norm1), col = "blue", ylim = c(0,1.5), xlim = c(-5, 18))
lines(density(norm2), col = "red")
lines(density(norm3), col = "black")
lines(density(norm4), col = "green")


use_git_config(user.name = "Tanvijain29", user.email = "tanvi.29701@gmail.com")
edit_git_config()
use_git()

options("install.lock"=FALSE)
