rm(list = ls())
library(faraway)
data(longley)

# Problem 1
## Condition number
regModel = lm(Employed ~ ., data = longley)
summary(regModel)
X = model.matrix(regModel)[, -1]
e = eigen(t(X) %*% X)
e$val
round(sqrt(max(e$val)/e$val), 3)

# Problem 2
## Correlation matrix
CorrMatrix = round(cor(longley)[1:7, 1:7], digits = 2)
write.csv(CorrMatrix, paste0("~/Documents/Senior Year/Winter 2016/Stats 500/homeworks/HW4/CorrMatrix.csv"))#,row.names=FALSE)

# Problem 3
## Variance inflation factor
require(faraway)
round(vif(X), 3)

# Problem 4
regModelNew = lm(Employed ~ GNP.deflator + Unemployed + Armed.Forces, data = longley)
summary(regModelNew)
summary(regModel)










