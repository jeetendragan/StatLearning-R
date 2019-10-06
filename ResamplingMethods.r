require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower, data = Auto)

## LOOCV
glm.fit = glm(mpg~horsepower, data = Auto)
result = cv.glm(Auto, glm.fit)
loocv = function(fit){
  h = lm.influence(fit)$h
  mean( (residuals(fit)/(1-h))^2 )
}

loocv(glm.fit)

cv.error =rep(0, 5)
degree = 1:5
for(d in degree){
  glm.fit = glm(mpg~poly(horsepower, d), data = Auto)
  cv.error[d] = loocv(glm.fit)
}
plot(degree, cv.error, type="b")

# 10 fold CV
cv.error10 = rep(0, 5)
for(d in degree){
  glm.fit = glm(mpg ~ poly(horsepower, d), data = Auto)
  cvResult = cv.glm(Auto, glm.fit, K = 10)
  cvResult
  cv.error10[d] = cvResult$delta[1]
}

lines(degree, cv.error10, type="b", col="red")

fit = glm(mpg ~ poly(horsepower, d), data = Auto)
result = cv.glm(Auto, fit, K = 10)
result

