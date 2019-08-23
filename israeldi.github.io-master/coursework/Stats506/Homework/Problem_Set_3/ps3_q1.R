## Problem Set 3 Question 1
##
##
## The RECS 2015 Data used in this script can be found at the link below: 
## https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
## In particular, see the csv data file, the code book, and the note on 
## computing standard errors using replicate weights.
##
## In this problem we analyze some properties of each division, such as walltype
## usage, average total electricity usage, and internet usage. We also stratify 
## some results by Urban and Rural Status for each division. In part D) we 
## also compute percent of families with income greater than 80K per division.
##
## This file is part of a set of files consisting of:
## ps3_q1.R, ps3_q2.R, ps3_q3.R, ps3.Rmd, and ps3.pdf
##
## Author: Israel Diego (israeldi@umich.edu)
## Updated: November 19, 2018

# QUESTION 1A -----------------------------------------------------------------
rm(list = ls())
# libraries
library(tidyverse)
library(data.table)

# Multiplier for confidence level: --------------------------------------------
m = qnorm(.975)

## data.table version
recs = fread('https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv')

# Replicate weights: ----------------------------------------------------------
## data.table version
weights = recs[, c('DOEID', paste0('BRRWT', 1:96))]

# -----------------------------------------------------------------------------

weights_long = melt(weights, id.vars = c("DOEID"),
                      measure.vars = c(paste0('BRRWT', 1:96)),
                      variable.name = 'repl', value.name = 'w')


# Division map: ---------------------------------------------------------------
divisions = c(
  'New England',
  'Middle Atlantic',
  'East North Central',
  'West North Central',
  'South Atlantic',
  'East South Central',
  'West South Central',
  'Mountain North',
  'Mountain South',
  'Pacific'
)

############
## Part a ##
############
# Notes: Stucco construction as major outside wall material by Division.
## Stucco: WALLTYPE == 4

# Point estimate: ------------------------------------------------------------
p_stucco = 
  recs[,.(DOEID, 
            division = factor(DIVISION, 1:10, divisions),
            NWEIGHT,
            WALLTYPE)
         ][, p_stucco := (sum(NWEIGHT*{WALLTYPE == 4} ) / sum(NWEIGHT)), 
           by = .(division)
           ][, head(.SD, 1), by = .(division),
             .SDcols = c("p_stucco")]

# Estimates from replicate weights: -------------------------------------------
p_stucco_r =
  merge(weights_long, 
        recs[,.(DOEID,
                  DIVISION,
                  division = factor(DIVISION, 1:10, divisions),
                  NWEIGHT,
                  WALLTYPE)]
        , by = 'DOEID', all = TRUE)
p_stucco_r = 
  p_stucco_r[, r_stucco := (sum( w*{WALLTYPE == 4} ) / sum(w)), 
               by = .(division, repl)] 
p_stucco_r = p_stucco_r[order(repl),
                            c("division", "repl", "r_stucco")]

# Compute standard errors: ----------------------------------------------------
p_stucco_r = 
  merge(p_stucco_r,
        p_stucco, by = 'division', all = TRUE)

# -----------------------------------------------------------------------------
p_stucco = 
  p_stucco_r[,se_stucco := 2 * sqrt( mean( {r_stucco - p_stucco}^2) ),
               by = 'division'
               ][, head(.SD, 1), by = .(division),
                 .SDcols = c("p_stucco", "se_stucco")]
# -----------------------------------------------------------------------------
p_stucco = 
  p_stucco[, .(division,
                 p_stucco,
                 se_stucco,
    lwr = pmax(p_stucco - m*se_stucco, 0),
                 upr = p_stucco + m*se_stucco)]

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Repeat using a function: ---------------------------------------------------
est_recs = function(DT, weights, fay = .5, m = qnorm(.975), 
                    ci_format = '%4.1f (%4.1f, %4.1f)', groupVarsList){
  
  # Point estimate
  pe = DT[, .(est = sum(w * x) / sum(w)), by = c(groupVarsList)]
  
  # Replicate estimates
  pe_r = DT[, -c("w")]
  pe_r = merge(pe_r, weights, by = "id", all = TRUE)
  pe_r = 
    pe_r[,r := sum(w * x ) / sum(w), by = c(groupVarsList,"repl")
         ][, head(.SD, 1), by = c(groupVarsList,"repl")]
  
  # Std error and confidence interval
  pe_r = 
    merge(pe_r, pe, by = c(groupVarsList), all = TRUE)
  pe_r = pe_r[, se := 1/fay * sqrt( mean( {r - est}^2 ) ), 
              by = c(groupVarsList)
              ][, head(.SD, 1), by = c(groupVarsList)]
  
  pe_r = 
    pe_r[, lwr := pmax(est - m*se, 0)
         ][, upr := est + m*se,
           ][, ci := sprintf(ci_format, 
                            est, est - m*se, est + m*se)
             ][, !(c("repl","id", "x", "w" , "r"))]
}

# Test the function above gives the previous results: -------------------------
weights_long = weights_long[,.(id = DOEID, repl, w)]

testDT = recs[, .(id = DOEID, w = NWEIGHT, x = 100*{WALLTYPE == 4},
                  division = factor(DIVISION, 1:10, divisions))]

groupVars = c("division")
testDT = est_recs(testDT, weights_long, groupVarsList = groupVars,
                  ci_format = '%4.1f%% (%4.1f, %4.1f)')

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


# QUESTION 1B -----------------------------------------------------------------
# Notes: What is the average total kwh of electricity usage by division?
# By each division Urban and rural subgroups?

# Point estimate: ------------------------------------------------------------
kwh = 
  recs[, .(id = DOEID, w = NWEIGHT, x = KWH,
             division = factor(DIVISION, 1:10, divisions))]
groupVars = c("division")
kwh = est_recs(kwh, weights_long, groupVarsList = groupVars)


# -----------------------------------------------------------------------------

kwh_div_urban = 
  recs[, .(id = DOEID, w = NWEIGHT, x = KWH,
             division = factor(DIVISION, 1:10, divisions),
             urban = UATYP10 %in% c('U', 'C'))]

groupVars = c("division", "urban")
kwh_div_urban = 
  est_recs( kwh_div_urban, weights_long, groupVarsList = groupVars )


# QUESTION 1C -----------------------------------------------------------------
# Internet access data: -------------------------------------------------------
internet =  recs[ ,.(id = DOEID, w = NWEIGHT, x = 100*INTERNET,
                        division = factor(DIVISION, 1:10, divisions),
                        urban = UATYP10 %in% c('U', 'C'))]

groupVars = c("division", "urban")

# Urban/rural estimates for each division: -----------------------------------
internet_ru = est_recs(internet, 
                           weights_long, 
                           groupVarsList = groupVars,
                           ci_format = '%4.1f%% (%4.1f, %4.1f)')

# Point estimate for difference: ---------------------------------------------
pe = 
  internet[, est := sum(w*x) / sum(w), by = groupVars
             ][, head(.SD, 1), by = groupVars]
pe = dcast(pe, division ~ urban, value.var = "est")
pe = pe[, est := `TRUE` - `FALSE`]

# Replicate estimates for difference: ----------------------------------------
pe_r = internet[,!("w")]
pe_r = merge(pe_r, weights_long, by = 'id', all = TRUE)
pe_r = pe_r[, r := sum(w*x) / sum(w), by = c(groupVars, "repl")
                ][, head(.SD, 1), by = c(groupVars, "repl")
                  ][order(division)]
pe_r = dcast(pe_r, division + repl ~ urban, value.var = "r")
pe_r = pe_r[, r := `TRUE` - `FALSE`]


# Std error and confidence interval for differnce: ---------------------------
internet_disp = merge(pe_r, pe, by = 'division', all = TRUE)
internet_disp = 
  internet_disp[ , .(est = est[1], 
                       se = 2 * sqrt( mean( {r - est}^2 ))
                       ),
                   by = 'division'
                   ][, ci := sprintf('%4.1f%% (%4.1f, %4.1f)', 
                                    est, est - m*se, est + m*se)]

# Join urban & rural estimates to differences: -------------------------------
internet_temp =
  dcast(internet_ru[, .(division, urban, ci)], 
        division ~ urban, value.var = "ci")
internet_disp = merge(internet_disp, internet_temp, 
                       by = 'division')

setnames(internet_disp, c("FALSE", "TRUE", "ci"), 
         c("Rural", "Urban", "Diff"))

# QUESTION 1D -----------------------------------------------------------------
# Percent of Families with income greater than 80K by Division

# Set up our variables
p_Income80K = recs[, .(id = DOEID, w = NWEIGHT, x = 100*{MONEYPY %in% c(5:8)},
                  division = factor(DIVISION, 1:10, divisions))]

groupVars = c("division")
# Call our Estimate function
p_Income80K = est_recs(p_Income80K, weights_long, groupVarsList = groupVars,
                  ci_format = '%4.1f%% (%4.1f, %4.1f)')

