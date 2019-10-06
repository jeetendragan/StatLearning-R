library(MASS)
library(ISLR)

names(Boston)
plot(medv~lstat, Boston)
fit = lm(medv ~ lstat, data = Boston)
fit
summary(fit)
abline(fit, col ="red")
names(fit)
confint(fit)
predict(fit, data.frame(lstat= c(5, 10, 15)), interval="confidence")

plot(medv~age, data = Boston);
fit2 = lm(medv~ lstat+age, data = Boston)
fit2
summary(fit2)

fit3 = lm(medv~., data = Boston)
summary(fit3)
par(mfrow = c(2, 2))
plot(fit3)

fit4 = update(fit3, ~.-age-indus);
summary(fit4)

par(mar=c(1,1,1,1))
plot(lstat~age, data=Boston)
fit5 = lm(medv~lstat*age, Boston)
summary(fit5)
par(mfrow = c(2, 2))
plot(fit5)

fit6 = lm(medv~lstat + I(lstat^2), Boston)
summary(fit6);
par(mfrow= c(1,1))
plot(medv~lstat, data=Boston)
attach(Boston)
points(Boston$lstat, fitted(fit6), col="red", pch=20)

#fit7 = lm(medv~poly(lstat, 4))
#points(Boston$lstat, fitted(fit7), col="blue", pch=20)

fix(Carseats)

regplot = function(x, y, ...){
  fit = lm(y~x);
  plot(x, y, ...);
  abline(fit, col="red");
}
attach(Carseats)
regplot(Carseats$Price, Carseats$Sales, xlab="Price", ylab="Sales", col="blue", pch=20)

