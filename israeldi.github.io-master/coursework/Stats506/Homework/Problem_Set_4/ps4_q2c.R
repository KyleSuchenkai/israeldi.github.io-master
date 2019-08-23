## Problem Set 4 Question 2c
##
## Designed to be called from the command line with command line args:
##   sigma (standard deviation), mcrep (Monte Carlo runs), 
##   n_cores (number of cores)
##
## To run with default parameters: Rscript ps4_q2c.R 
##
## To pass parameters from the command line, use --args:
##  
##    Rscript ps4_q2c.R -- sigma args n_cores=4 mcrep=1e4
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
# install.packages('future')
library(future)
source('ps4_q2_funcs.R')

# QUESTION 2C -----------------------------------------------------------------

# Default Arguments
args_list = list(
  sigma = 1,
  mc_rep = 1e4,
  n_cores = 4
)

## get parameters from command line
args = commandArgs(trailingOnly = TRUE)
print(args)

# functions for finding named arguments
args_to_list = function(args){
  ind = grep('=', args)  
  args_list = strsplit(args[ind], '=')
  names(args_list) = sapply(args_list, function(x) x[1])
  
  args_list = lapply(args_list, function(x) as.numeric(x[2]))
  args_list
}

# get named arguments
args_list_in = args_to_list(args)

# update non default arguments
ignored = c()
for (arg in names(args_list_in) ) {
  # Check for unknown argument
  if ( is.null(args_list[[arg]]) ) {
    ignored = c(ignored, arg)
  } else{
    # update if known
    args_list[[arg]] = args_list_in[[arg]]
  }
}

# Print warning message about unknown arguments
if ( length(ignored) > 0 ) {
  cat('Ignoring unkown arguments:', paste(ignored,collapse = ', '), '\n')
}
set.seed(42)
# QUESTION 2B -----------------------------------------------------------------
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
runSimulation = function(rho, sigma, mc_rep)
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
  
  P = sim_beta(X, beta, sigma = sigma, mc_rep = mc_rep)
  
  evaluate(P, 1:10)
  all0 =
    lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x){
      evaluate( apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
    })
  all = data.table::rbindlist(all0)
  all[ , method := c('Holm', 'Bonferroni', 'BH', 'BY') ]
}

# Values of rho
rhoVec = (1/4) * c(-3:3)
# Parallel Computing w/ future ------------------------------------------------
# Parallel computations using `plan(multicore)`
args_list = lapply(args_list, function(x) as.numeric(x))

plan(multicore, workers = as.numeric(args_list["n_cores"]))

resultsC = list()
for(i in 1:length(rhoVec)){
  resultsC[[i]] = with(args_list, 
                       future({runSimulation(rhoVec[i], sigma, mc_rep)}))
}

# Initialize our results_q4c dataframe ---------------------------------------
# Specify dataframe column names and names of metrics
resultsDFColNames = c("rho", "sigma", "metric", "method", "est", "se")
metricList = c("fwer", "fdr", "sens", "spec")

# Initialize empty results_q4c dataframe
results_q4c = data.frame(matrix(0, nrow = 0, ncol = length(resultsDFColNames)))
colnames(results_q4c) = resultsDFColNames

# Loop through our mclapply results in order to organize data and add it to 
# results_q4c data frame

# Loop through each of the different values of rho
for(i in 1:length(rhoVec))
{
  # Loop through each metric
  for(metric in metricList)
  {
    # Initialize a temporary dataframe that has the same names as results_q4c 
    # data frame
    tempDF = data.frame(matrix(0, nrow = 4, ncol = length(resultsDFColNames)))
    colnames(tempDF) = resultsDFColNames
    
    # Extract the necessary info from our parallelism results
    tempDF$rho = rhoVec[i]
    tempDF$sigma = as.numeric(args_list["sigma"])
    tempDF$metric = metric
    tempDF[,c("method", "est", "se")] = 
      as.data.frame(value(resultsC[[i]]))[,c("method", metric, 
                                         paste0(metric, "_se"))]
    
    # Append tempDf to results_q4c
    results_q4c = rbind(results_q4c, tempDF)
  }
}


# Report results to stdout
stdout = capture.output(results_q4c)
stdout