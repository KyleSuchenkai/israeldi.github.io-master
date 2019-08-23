## Problem Set 3 Question 3
##
##
## In this question we work with the 'mtcars' dataset and calculate univariate
## regressions written in data.table and dplyr. We also write functions that 
## compute univariate regressions based on arbitrary independent, dependent, 
## and grouping variables.
##
## This file is part of a set of files consisting of:
## ps3_q1.R, ps3_q2.R, ps3_q3.R, ps3.Rmd, and ps3.pdf
##
## Author: Israel Diego (israeldi@umich.edu)
## Updated: November 19, 2018

rm(list = ls())
library(data.table)
library(dplyr)

# QUESTION 3A -----------------------------------------------------------------
# Part A in literal translation
mtcarsDT = data.table(mtcars)
mtcarsDT = mtcarsDT[order(cyl), .(mpg, cyl, disp, hp, wt)]

beta_cylDT = mtcarsDT[, .(mpg,
                          disp_gc = disp - mean(disp),
                          hp_gc = hp - mean(hp),
                          wt_gc = wt - mean(wt)), 
                      by = .(cyl)]

Xpmg = beta_cylDT[, .(dispXmpg = sum(mpg * disp_gc),
                      vdisp = var(disp_gc),
                      hpXmpg = sum(mpg*hp_gc), 
                      vhp = var(hp_gc),
                      wtXmpg = sum(mpg*wt_gc), 
                      vwt = var(wt_gc)),
                  by = .(cyl)]
betas = Xpmg[, .(beta_cyl_disp = dispXmpg / (vdisp),
                       beta_cyl_hp = hpXmpg / (vhp),
                       beta_cyl_wt = wtXmpg / (vwt)), by = .(cyl)]

# QUESTION 3B -----------------------------------------------------------------

universalRegByGroup = function(DT, indepVarsList, dependVar, groupVar){
  
  numRows = dim(unique(DT[, .(get(groupVar))]))[1]
  numCols = length(indepVarsList)
  betas = data.frame(matrix(0, nrow = numRows, ncol = numCols + 1))
  names(betas)[1] = groupVar
  betas[,groupVar] = unique(DT[, .(get(groupVar))])
  
  for (i in 1:length(indepVarsList) )
  {
    indepVar = indepVarsList[i]
    beta_groupDT = DT[, .(dependVar = get(dependVar), 
                          varCenter = get(indepVar) - mean( get(indepVar))
    ),
    by = c(groupVar)]
    
    beta_groupDT = beta_groupDT[,.(
      beta = sum(dependVar * varCenter) / var(varCenter)),
      by = c(groupVar)]
    
    names(betas)[i + 1] = paste0('beta_', indepVar)
    betas[,(i + 1)] = beta_groupDT$beta
  }
  
  betas
}

indepVarsList = c("disp", "hp", "wt")
dependVar = "mpg"
groupVar = "cyl"

# Test Function

betasTest = universalRegByGroup(mtcarsDT, indepVarsList, dependVar, groupVar)


# QUESTION 3C -----------------------------------------------------------------
betas = mtcars %>%
  group_by(cyl) %>%
  summarise_at(vars(disp, hp, wt), 
               funs(beta_cylDT = sum(mpg * (. - mean(.)) ) / var(. - mean(.)) )
               )

# QUESTION 3D -----------------------------------------------------------------
getRegressCoeffDplyr = function(df, group_var, args, dependVar)
{
  group_var = enquo(group_var)
  dependVar = enquo(dependVar)
    
  df %>%
    group_by(!!group_var) %>%
    summarise_at(
      vars(!!!args), 
      funs(beta = 
             sum(!!dependVar * (. - mean(.)) ) / var(. - mean(.)) )
    )
}

# Testing our function
args = list(disp = quo(disp), hp = quo(hp), wt = quo(wt))
getRegressCoeffDplyr(mtcars, cyl, args, mpg)

