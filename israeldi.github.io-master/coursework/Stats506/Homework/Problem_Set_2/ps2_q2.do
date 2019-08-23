* ---------------------------------------------------------------------------- *
* Statistics 506, Fall 2018	  	          
* Solution to question 2, problem set 2.  
* 
* Files: demographics_weights2005.dta
*		 oral_health2005.dta
*  	 imported from the address below:
* 
* https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OHX_D.XPT
* https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT     	 
*
* Author: Israel Diego (israeldi@umich.edu)
* Date: October 17, 2018
* ---------------------------------------------------------------------------- *

*------------------ *
* Set up workspace  *
*------------------ *
clear
* cd "/Users/israeldiego/Google Drive/Documents Google Drive/Fall 2018/Stats 506/Homework/Problem Set 2/"
log using ps2_q2.log, text replace
set more off

* Load Data
fdause DEMO_D.XPT
save demographics_weights2005.dta, replace
fdause OHX_D.XPT
save oral_health2005.dta, replace

* Part a) ---------------------------------------------------------------------
// Merge data
use oral_health2005.dta, clear
merge 1:1 seqn using demographics_weights2005.dta, keep(match)

* Save Data if we want to use it later
* save nhanes2005.dta, replace

* Part b) ---------------------------------------------------------------------
keep seqn ohx04htc ridagemn riagendr ridreth1 indfmpir sdmvpsu wtmec2yr sdmvstra
rename (ohx04htc riagendr ridagemn ridreth1 indfmpir) /*
*/(bicuspid isMale ageMonths ethnicity incomePovertyRatio)

* Dropping any missing values from our data set in order to do our logistic 
* regression. 
drop if missing(ageMonths)
drop if missing(bicuspid)
drop if (bicuspid == 9)

* Create indicator variable which is 1 if primary tooth is missing or 
* permanent tooth is present
generate notPrimary = 1 * (bicuspid ~= 1)

* Running our logit regression
logit notPrimary ageMonths, nolog
matrix data = r(table)'
putexcel set ps2_q2_Stata.xlsx, sheet(partb) modify
putexcel A1 = ("notPrimary") A2 = ("ageMonths") /*
	  */ A3 = ("constant") B1 = ("Coef.") /*
	  */ C1 = ("Std. Err.") D1 = ("z") /*
	  */ E1 = ("P>|z|") F1 = ("95% Lower") /*
	  */ G1 = ("95% Upper") B2 = matrix(data)

* Manually computing ages (months) at 25, 50, and 75% 
matrix coeff = e(b)

scalar p = 0.25
generate age25 = round( (-log(1/p - 1) - coeff[1,2]) / coeff[1,1] )
scalar p = 0.50
generate age50 = round( (-log(1/p - 1) - coeff[1,2]) / coeff[1,1] )
scalar p = 0.75
generate age75 = round( (-log(1/p - 1) - coeff[1,2]) / coeff[1,1] )

generate age25_year = floor(age25 / 12)
generate age75_year = ceil(age75 / 12)

putexcel set ps2_q2_Stata.xlsx, sheet(Ages) modify 
putexcel A1 = ("25th Percentile") B1 = ("50th Percentile") /*
	  */ C1 = ("75th Percentile") D1 = ("Age 25th percentile (years)") /*
	  */ E1 = ("Age 75th percentile (years)") A2 = (age25) /*
	  */ B2 = (age50) C2 = (age75) /*
	  */ D2 = (age25_year) E2 = (age75_year)

* Part c) ---------------------------------------------------------------------
drop if missing(isMale)
drop if missing(ethnicity)
replace isMale = (isMale == 1)

* Create ethnicity groups
generate byte mexican  = 0
replace mexican = 1 if ethnicity == 1

generate byte black = 0
replace black = 1 if ethnicity == 4

generate byte other  = 0
replace other = 1 if ethnicity == 2 | ethnicity == 5

*------------------------------------- *
* Running Regressions Checking for BIC *
*------------------------------------- *

* Running our logit regression only with Age
logit notPrimary ageMonths, nolog

* Checking BIC of our model
estat ic, n()
matrix BIC = r(S)

putexcel set ps2_q2_Stata.xlsx, sheet(partc) modify 
putexcel A2 = ("BIC") B1 = ("age only") /*
	  */ C1 = ("age + gender") D1 = ("age + mexican") /*
	  */ E1 = ("age + black") F1 = ("age + black + other") /*
	  */ G1 = ("age + black + inPovRatio") /*
	  */ B2 = matrix(BIC[1,6])
	  
* -----------------------------------------------------------------------------
* Running our logit regression with Gender
logit notPrimary ageMonths isMale, nolog

* Checking BIC of our model and see if BIC improves
estat ic, n() // Gender does not improve BIC
matrix BIC = r(S)
putexcel C2 = matrix(BIC[1,6])

* -----------------------------------------------------------------------------
* Running our logit regression with Mexican ethnicity
logit notPrimary ageMonths mexican, nolog

* Checking BIC of our model and see if BIC improves
estat ic, n() // Mexican does not improve BIC
matrix BIC = r(S)
putexcel D2 = matrix(BIC[1,6])

* -----------------------------------------------------------------------------
* Running our logit regression with Black ethnicity
logit notPrimary ageMonths black, nolog

* Checking BIC of our model and see if BIC improves
estat ic, n() // Black improves BIC
matrix BIC = r(S)
putexcel E2 = matrix(BIC[1,6])

* -----------------------------------------------------------------------------
* Running our logit regression with Other ethnicity
logit notPrimary ageMonths black other, nolog

* Checking BIC of our model and see if BIC improves
estat ic, n() // Other does not improve BIC
matrix BIC = r(S)
putexcel F2 = matrix(BIC[1,6])

* -----------------------------------------------------------------------------
* Running our logit regression with Income Poverty Ratio
logit notPrimary ageMonths black incomePovertyRatio, nolog

* Checking BIC of our model and see if BIC improves
estat ic, n() // Income Poverty Ratio improves BIC
matrix BIC = r(S)
putexcel G2 = matrix(BIC[1,6])

* RESULT 
* We added Black ethnicity and Income Poverty Ratio to the regression which 
* improved BIC

logit notPrimary ageMonths black incomePovertyRatio, nolog

matrix data = r(table)'
putexcel set ps2_q2_Stata.xlsx, sheet(partc2) modify 
putexcel A1 = ("notPrimary") A2 = ("ageMonths") /*
	  */ A3 = ("black") A4 = ("inPovRatio")  A5 = ("constant") /*
	  */ B1 = ("Coef.") C1 = ("Std. Err.") D1 = ("t") /*
	  */ E1 = ("P>|t|") F1 = ("95% Lower") /*
	  */ G1 = ("95% Upper") B2 = matrix(data)

* Part d) ---------------------------------------------------------------------

* 1) Computing Adjusted Predictions at the mean for representative ages

* Running our logit regression only with Age
logit notPrimary ageMonths, nolog

* Postestimation adjusted predictions at the mean for representative ages
margins, at(ageMonths = (96 108 120 132 144)) atmeans vsquish
matrix data = r(table)'
putexcel set ps2_q2_Stata.xlsx, sheet(partd1) modify 
putexcel A1 = ("Rep. Ages") A2 = ("age1") /*
	  */ A3 = ("age2") A4 = ("age3")  A5 = ("age4") A6 = ("age5") /*
	  */ B1 = ("Margin") C1 = ("Std. Err.") D1 = ("z") /*
	  */ E1 = ("P>|z|") F1 = ("95% Lower") /*
	  */ G1 = ("95% Upper") B2 = matrix(data)

* 2) Marginal Effects at the mean using Black categorical variable at 
* representative ages

* Running our logit regression only with Age
logit notPrimary ageMonths black incomePovertyRatio, nolog

* Postestimation adjusted predictions at the mean for representative ages
margins, dydx(black) at(ageMonths = (96 108 120 132 144)) atmeans
matrix data = r(table)'
putexcel set ps2_q2_Stata.xlsx, sheet(partd2) modify 
putexcel A1 = ("Rep. Ages") A2 = ("age1") /*
	  */ A3 = ("age2") A4 = ("age3")  A5 = ("age4") A6 = ("age5") /*
	  */ B1 = ("dy/dx") C1 = ("Std. Err.") D1 = ("z") /*
	  */ E1 = ("P>|z|") F1 = ("95% Lower") /*
	  */ G1 = ("95% Upper") B2 = matrix(data)

* 3) Computing Average Marginal Effects using Black categorical variable at 
* representative ages
margins, dydx(black) at(ageMonths = (96 108 120 132 144))
matrix data = r(table)'
putexcel set ps2_q2_Stata.xlsx, sheet(partd3) modify 
putexcel A1 = ("Rep. Ages") A2 = ("age1") /*
	  */ A3 = ("age2") A4 = ("age3")  A5 = ("age4") A6 = ("age5") /*
	  */ B1 = ("dy/dx") C1 = ("Std. Err.") D1 = ("z") /*
	  */ E1 = ("P>|z|") F1 = ("95% Lower") /*
	  */ G1 = ("95% Upper") B2 = matrix(data)


* Part e) ---------------------------------------------------------------------

svyset sdmvpsu [pweight = wtmec2yr], strata(sdmvstra) vce(linearized)

svy: logit notPrimary ageMonths black incomePovertyRatio

matrix data = r(table)'
putexcel set ps2_q2_Stata.xlsx, sheet(parte) modify 
putexcel A1 = ("notPrimary") A2 = ("ageMonths") /*
	  */ A3 = ("black") A4 = ("inPovRatio")  A5 = ("constant") /*
	  */ B1 = ("Coef.") C1 = ("Std. Err.") D1 = ("t") /*
	  */ E1 = ("P>|t|") F1 = ("95% Lower") /*
	  */ G1 = ("95% Upper") B2 = matrix(data)

log close
























