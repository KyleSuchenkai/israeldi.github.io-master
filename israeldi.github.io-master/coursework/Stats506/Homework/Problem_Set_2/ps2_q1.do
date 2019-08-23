* ---------------------------------------------------------------------------- *
* Statistics 506, Fall 2018	  	          
* Solution to question 1, problem set 2.  
* 
* Files: recs2015_public.dta 
*  	 imported from the address below:
* 
* https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
*     	 
* This file is part of a set of files consisting of:
* ps2.pdf
* ps2.Rmd
* ps2_q1.do
* recs2015_usage.csv
* ps2_q2.do
* ps2_q2.log
* ps2_q3.R
*
* Author: Israel Diego (israeldi@umich.edu)
* Date: October 17, 2018
* ---------------------------------------------------------------------------- *

*------------------ *
* Set up workspace *
*------------------ *
clear

*cd "/Users/israeldiego/Google Drive/Documents Google Drive/Fall 2018/Stats 506/Homework/Problem Set 2/"
set more off

* Install outreg2 command for the first time, for exporting tables to files
* ssc install outreg2

* Load Data
import delimited recs2015_public_v3.csv

// Keep only needed variables
*list brrwt1-brrwt96
local weights brrwt1-brrwt96
keep doeid kwh cufeetng gallonlp gallonfo nweight `weights'

svyset [pweight = nweight], brrweight(`weights') vce(brr) fay(.5)

svy: total kwh cufeetng gallonlp gallonfo

matrix data = r(table)'
putexcel set recs2015_usage.csv, replace 
putexcel A1 = ("Variables") A2 = ("KWH") /*
	  */ A3 = ("Natural Gas") A4 = ("Propane")  A5 = ("Fuel/Kerosene") /*
	  */ B1 = ("Total") C1 = ("BRR Std. Err.") /*
	  */ F1 = ("95% Lower") G1 = ("95% Upper") /*
	  */ B2 = matrix(data)
	  
putexcel set ps2_q2_Stata.xlsx, sheet(q1) replace 
putexcel A1 = ("Variables") A2 = ("KWH") /*
	  */ A3 = ("Natural Gas") A4 = ("Propane")  A5 = ("Fuel/Kerosene") /*
	  */ B1 = ("Total") C1 = ("BRR Std. Err.") /*
	  */ F1 = ("95% Lower") G1 = ("95% Upper") /*
	  */ B2 = matrix(data)























