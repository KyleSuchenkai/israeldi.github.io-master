## Problem Set 4 Question 2b
##
## In this question we continue Question 2 from Problem Set 3 in order to 
## practice practice parallel, asynchronous, and batch computing.
##
## This file is part of a set of files consisting of:
## ps4.pdf , ps4.Rmd , ps4_q1.R , ps4_q2_funcs.R , ps4_q2a.R , ps4_q2b.R , 
## ps4_q2c.R, run_ps4_q2b.pbs , run_ps4_q2c.pbs, ps4_q2b.Rout , 
## ps4_q2c-X.Rout (X = 1, 2, 4), ps4_q3.sas , ps4_q3c.csv , ps4_q3d.csv
##
## Author: Israel Diego (israeldi@umich.edu)
## Due: December 10, 2018

rm(list = ls())
# Packages
library(data.table)
library(dplyr)
library(parallel)
library(doParallel)
source('ps4_q2_funcs.R')

# QUESTION 2B -----------------------------------------------------------------
set.seed(42)

# FUNCTION - runSimulation
# 
# Simulate Y from Y|X ~ N(XB, SIGMA) where SIGMA_ij = rho * beta_i * beta_j 
# and compute p-values corresponding to
# Wald tests for B != 0. Repeat mc_rep times.
#
# We call sim_beta() function stored in ps4_q2_funcs.R to run this procedure,
# then the p-values are passed to evaluate() function.
#
# Arguments:
#   rho: correlation
#   sigma: standard deviation to be passed into sim_beta
#
# Output: table containing estimates for FWER, FDR, Sensitivity, and Specificity
runSimulation = function(rho, sigma)
{
  n = 1e3; p = 1e2; r = .1
  beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
  dim(beta) = c(p, 1)
  
  # Initialize Sigma
  Sigma = matrix(0, nrow = p, ncol = p)
  # X ~ N(0, Sigma):
  for(i in 1:p)
  {
    for(j in 1:p)
    {
      Sigma[i,j] = rho * beta[i] * beta[j]
    }
  }
  diag(Sigma) = 1
  
  # Ensure it is symmetric, then rescale to give variances all equal to one
  R = chol(Sigma)
  
  # Here is an X for testing:
  X = matrix( rnorm(n*p), n, p) %*%  R
  
  P = sim_beta(X, beta, sigma)
  
  evaluate(P, 1:10)
  all0 =
    lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x){
      evaluate( apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
    })
  all = data.table::rbindlist(all0)
  all[ , method := c('Holm', 'Bonferroni', 'BH', 'BY') ]
  
  return(all)
}

# Values of rho to be iterated where rho = (-0.75, -0.5, -0.25,...,0.75)
rhoVec = (1/4) * c(-3:3)
# Values of sigma to be iterated where sigma = (0.25, 0.5, 1)
sigmaVec = c(1/4, 1/2, 1)

# How many cores to use in the cluster? #
ncores = 4 

# set up a cluster called 'cl'
cl = makeCluster(ncores)

# register the cluster
registerDoParallel(cl)

## Do parallel computations with nested foreach on rho and sigma
resultsB = foreach(rho = rhoVec) %:%
  foreach(sigma = sigmaVec) %dopar% {
    runSimulation(rho, sigma)
  }

## Always shut the cluster down when done
stopCluster(cl)

# Initialize our results_q4b dataframe ---------------------------------------
# Specify dataframe column names and names of metrics
resultsDFColNames = c("rho", "sigma", "metric", "method", "est", "se")
metricList = c("fwer", "fdr", "sens", "spec")

# Initialize empty results_q4b dataframe
results_q4b = data.frame(matrix(0, nrow = 0, ncol = length(resultsDFColNames)))
colnames(results_q4b) = resultsDFColNames

# Loop through our mclapply results in order to organize data and add it to 
# results_q4b data frame

# Loop through each of the different values of rho
for(i in 1:length(rhoVec))
{
  # Loop through each of the diffferent values of sigma
  for(j in 1:length(sigmaVec))
  {
    # Loop through each metric
    for(metric in metricList)
    {
      # Initialize a temporary dataframe that has the same names as results_q4b 
      # data frame
      tempDF = data.frame(matrix(0, nrow = 4, ncol = length(resultsDFColNames)))
      colnames(tempDF) = resultsDFColNames
      
      # Extract the necessary info from our parallelism results
      tempDF$rho = rhoVec[i]
      tempDF$sigma = sigmaVec[j]
      tempDF$metric = metric
      tempDF[,c("method", "est", "se")] = 
        as.data.frame(resultsB[[i]][j])[,c("method", metric, 
                                        paste0(metric, "_se"))]
      
      # Append tempDf to results_q4b
      results_q4b = rbind(results_q4b, tempDF)
    }
  }
}

# Save Dataframe
save(results_q4b, file = "results_q4b.RData")

# Report results to stdout
stdout = capture.output(results_q4b)
stdout

# PLOT RESULTS ----------------------------------------------------------------
# Plots in RMARKDOWN
