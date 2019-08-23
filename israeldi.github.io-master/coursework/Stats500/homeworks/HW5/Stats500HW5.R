rm(list = ls())
library(faraway)
data(sat)

# Problem 1
# a) OLS
regLS = lm(total ~ takers + ratio + salary + expend, data = sat)
summary(regLS)

# b) LAD
library(quantreg)
regLad = rq(total ~ takers + ratio + salary + expend, data = sat)
summary(regLad)

# c) Huber's
library(MASS)
regHuber = rlm(total ~ takers + ratio + salary + expend, data = sat)
regHuberTvals = matrix(c(21.2533,13.6470,1.6894,0.9293,0.3935),nrow = 5,ncol = 1)
regHuberpValues = 2 * (1- pt(regHuberTvals, df=45))
summary(regHuber)


# Problem 2
rm(list = ls())
library(faraway)
data(prostate)

# a) Backward elimination
#1)
regModel = lm(lpsa ~ ., data = prostate)
summary(regModel)
#2)
regModel = update(regModel, . ~ . - gleason)
summary(regModel)
#3)
regModel = update(regModel, . ~ . - age)
summary(regModel)
#4)
regModel = update(regModel, . ~ . - lcp)
summary(regModel)
#5)
regModel = update(regModel, . ~ . - pgg45)
summary(regModel)
#6)
regModel = update(regModel, . ~ . - lbph)
summary(regModel)


# b) Adjusted R^2
library(leaps)
b = regsubsets(lpsa ~ ., data = prostate)
summary(b)
# plot adjusted R2 against p+1
rs = summary(b)
plot(1:8, rs$adjr2, cex.main = .9, cex.lab = .9, main = "Adj R^2 Plot for prostate data",
     xlab="No. of Parameters", ylab="Adjusted Rsq",ylim = c(.5,.65))
# select model with largest adjusted R2
which.max(rs$adjr2)
# predictors: lcavol, lweight, age, lbph, svi, lcp, and pgg45

# c)  Mallows cp
library(leaps)
b = regsubsets(lpsa ~ ., data = prostate)
rs = summary(b)
which.min(rs$cp)

plot(1:8, rs$cp, cex.main = .9, cex.lab = .9, main = "Cp Plot for prostate data",
     xlab="No. Parameters", ylab="Cp", ylim = c(0,27))
abline(0, 1)
# predictors: lcavol, lweight, lbph, svi


regFull = lm(lpsa ~ ., data = prostate)
summary(regFull)

regBack = lm(lpsa ~ lcavol + lweight + svi, data = prostate)
summary(regBack)

regR2 = lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + pgg45, data = prostate)
summary(regR2)

regCp = lm(lpsa ~ lcavol + lweight + lbph + svi, data = prostate)
summary(regCp)








