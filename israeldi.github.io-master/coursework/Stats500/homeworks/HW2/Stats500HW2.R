library(faraway)
data(uswages)

uswages = uswages[uswages$exper >= 0,]

## PART A
# Problem 1
educExpWages = lm(formula = wage ~ educ + exper,  data = uswages)
hist(educExpWages$residuals)

which.max(educExpWages$residuals)
summary(educExpWages)

# Problem 2
# The adj R^2 = .1343

# Problem 3
#n = dim(uswages)[1]
#p = 2
# Calculating Regression educ and exper on wage
#X = cbind(1, as.matrix(uswages[, 2:3]))
#XtX = t(X) %*% X
#XtXi = solve(XtX)
#beta = XtXi %*% t(X) %*% uswages[,1]
# Residual
#resid = uswages[,1]-X %*% beta
# sort the resid vector, the case number is 15387 and the max is 7249.1740

# Problem 4
#residMean = mean(resid)
#residMedian = median(resid)
residMean = mean(educExpWages$residuals)
residMedian = median(educExpWages$residuals)

# The mean of the residual is near zero, but the Median is a negative number

# Problem 5
# The difference in weekly wages is the coefficent of exper which is 9.7748

# Problem 6
#fitted = X %*% beta
#cor(resid,fitted)
cor(educExpWages$fitted.values,educExpWages$residuals)
plot(educExpWages$fitted.values, educExpWages$residuals, pch = 16, cex = 1, cex.main = .9, cex.lab = .9, col = "blue", 
     main = "RESIDUALS PLOTTED AGAINST FITTED VALUES", xlab = "fitted", 
     ylab = "residual")


## PART B
# Problem 1
X = matrix(c(1,1,1,1,1,1,1,1,1,1,2,-1,3,3,2,1,0,0,-1,0,-2,-2,-2,3,3,3,0,0,0,1),nrow = 10,ncol = 3)
e = rnorm(10,0,1)
beta = matrix(c(1,-1,2),nrow=3,ncol=1)
Y = X %*% beta + e
XtXi = solve(t(X)%*%X)
betahat = XtXi %*% t(X) %*% Y

# Problem 2
# We know from 1) that the variance of e is 1, so 
varBetaHat = XtXi

# Problem 3
n = 10
p = 2
rss = sum((Y - X %*% betahat)^2)
sigma2 = rss / (n - (p+1))

# Problem 4
# empty space for beta estimates
uProb4 = matrix(c(0,0,0),3,1000) #beta1,beta2,beta3
varBetaHat1000 = matrix(c(0,0,0),3,1000) #varBetaHat1,varBetaHat2,varBetaHat3

# Iterate 1000 times
for(i in 1:1000)
{
  e = rnorm(10,0,1)
  Y = X%*%beta + e
  XtXi = solve(t(X)%*%X)
  betahat = XtXi %*% t(X) %*% Y
  rss = sum((Y - X %*% betahat)^2)
  sigma2 = rss / (n - (p+1))
  varBetaHat = sigma2 * (XtXi)
  coeffs = diag(varBetaHat)
  uProb4[,i] = betahat
  varBetaHat1000[,i] = coeffs
}
# Making Histograms of Betas
hist(uProb4[1,], cex.main = .9, cex.lab = .9, main = "Histogram of betaHat1", xlab = "Beta1 Values")
hist(uProb4[2,], cex.main = .9, cex.lab = .9, main = "Histogram of betaHat2", xlab = "Beta2 Values")
hist(uProb4[3,], cex.main = .9, cex.lab = .9, main = "Histogram of betaHat3", xlab = "Beta3 Values")
# The variances of betaHats are exactly the same as those in problem 2

# Problem 5
uProb5 = matrix(c(0,0,0),3,1000) #beta1,beta2,beta3
sigma2Prob5 = matrix(c(0),1000,1)

# Iterate 1000 times
for(i in 1:1000)
{
  e = rnorm(10,0,1)
  Y = X%*%beta + e
  XtXi = solve(t(X)%*%X)
  betahat = XtXi %*% t(X) %*% Y
  uProb5[,i] = betahat
  n = 10
  p = 2
  rss = sum((Y - X %*% betahat)^2)
  sigma2 = rss / (n - (p+1))
  sigma2Prob5[i,] = sigma2
}
# Making Histogram of sigma2
hist(sigma2Prob5, cex.main = .9, cex.lab = .9, main = "Histogram of sigmaHat2", xlab = "sigmaHat2 Values")

# Problem 6
# Demeaned Poisson with Variance 1
eps = rpois(10,1) - 1
mean(eps)
var(eps)

# Problem 4 Repeat with demeaned Poisson distribution
uProb4Repeat = matrix(c(0,0,0),3,1000) #beta1,beta2,beta3
varBetaHat1000Repeat = matrix(c(0,0,0),3,1000) #varBetaHat1,varBetaHat2,varBetaHat3

# Iterate 1000 times
for(i in 1:1000)
{
  eps = rpois(10,1) - 1
  Y = X%*%beta + eps
  XtXi = solve(t(X)%*%X)
  betahat = XtXi %*% t(X) %*% Y
  varBetaHat = XtXi
  coeffs = diag(varBetaHat)
  uProb4Repeat[,i] = betahat
  varBetaHat1000Repeat[,i] = coeffs
}
# Making Histograms of Betas with Poisson distributed errors
hist(uProb4Repeat[1,], cex.main = .9, cex.lab = .9, main = "Histogram of betaHat1 (Problem 4 repeat)", xlab = "Beta1 Values")
hist(uProb4Repeat[2,], cex.main = .9, cex.lab = .9, main = "Histogram of betaHat2 (Problem 4 repeat)", xlab = "Beta2 Values")
hist(uProb4Repeat[3,], cex.main = .9, cex.lab = .9, main = "Histogram of betaHat3 (Problem 4 repeat)", xlab = "Beta3 Values")
# The variances of betaHats are exactly the same as those in problem 2

# Problem 5 Repeat with demeaned Poisson distribution
uProb5Repeat = matrix(c(0,0,0),3,1000) #beta1,beta2,beta3
sigma2Prob5Repeat = matrix(c(0),1000,1)

# Iterate 1000 times
for(i in 1:1000)
{
  eps = rpois(10,1) - 1
  Y = X%*%beta + eps
  XtXi = solve(t(X)%*%X)
  betahat = XtXi %*% t(X) %*% Y
  uProb5Repeat[,i] = betahat
  n = 10
  p = 2
  rss = sum((Y - X %*% betahat)^2)
  sigma2 = rss / (n - (p+1))
  sigma2Prob5Repeat[i,] = sigma2
}
# Making Histogram of sigma2
hist(sigma2Prob5Repeat, cex.main = .9, cex.lab = .9, main = "Histogram of sigmaHat2 (Problem 5 repeat)", xlab = "sigmaHat2 Values")

