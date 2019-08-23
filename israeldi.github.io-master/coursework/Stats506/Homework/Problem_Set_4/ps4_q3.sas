/* Problem Set 4 Question 3

In this question we use the 2016 Medicare Provider Utilization and Payment data 
and determine the MRI procedures with the highest volume, highest total payment, 
and highest average payment. We also do this task with PROQ SQL and show that 
results are the same 

This file is part of a set of files consisting of: ps4.html, ps4.Rmd, ps4_q1.R,
ps4_q2_funcs.R, ps4_q2a.R, ps4_q2b.R, ps4_q2c.R, run_ps4_q2b.pbs, 
run_ps4_q2c.pbs, ps4_q2b.Rout, ps4_q2c-X.Rout (X = 1, 2, 4),
ps4_q3.sas, ps4_q3c.csv, ps4_q3d.csv

Author: Israel Diego
Due: December 10, 2018 */

/* QUESTION 1A --------------------------------------------------------------*/
DATA Medicare_PS_PUF;
	LENGTH
		npi              					$ 10
		nppes_provider_last_org_name 		$ 70
		nppes_provider_first_name 			$ 20
		nppes_provider_mi					$ 1
		nppes_credentials 					$ 20
		nppes_provider_gender				$ 1
		nppes_entity_code 					$ 1
		nppes_provider_street1 				$ 55
		nppes_provider_street2				$ 55
		nppes_provider_city 				$ 40
		nppes_provider_zip 					$ 20
		nppes_provider_state				$ 2
		nppes_provider_country				$ 2
		provider_type 						$ 55
		medicare_participation_indicator 	$ 1
		place_of_service					$ 1
		hcpcs_code       					$ 5
		hcpcs_description 					$ 256
		hcpcs_drug_indicator				$ 1
		line_srvc_cnt      					8
		bene_unique_cnt    					8
		bene_day_srvc_cnt   				8
		average_Medicare_allowed_amt   		8
		average_submitted_chrg_amt  		8
		average_Medicare_payment_amt   		8
		average_Medicare_standard_amt		8;
	INFILE './data/Medicare_Provider_Util_Payment_PUF_CY2016.txt'

		lrecl=32767
		dlm='09'x
		pad missover
		firstobs = 3
		dsd;

	INPUT
		npi             
		nppes_provider_last_org_name 
		nppes_provider_first_name 
		nppes_provider_mi 
		nppes_credentials 
		nppes_provider_gender 
		nppes_entity_code 
		nppes_provider_street1 
		nppes_provider_street2 
		nppes_provider_city 
		nppes_provider_zip 
		nppes_provider_state 
		nppes_provider_country 
		provider_type 
		medicare_participation_indicator 
		place_of_service 
		hcpcs_code       
		hcpcs_description 
		hcpcs_drug_indicator
		line_srvc_cnt    
		bene_unique_cnt  
		bene_day_srvc_cnt 
		average_Medicare_allowed_amt 
		average_submitted_chrg_amt 
		average_Medicare_payment_amt
		average_Medicare_standard_amt;

	LABEL
		npi     							= "National Provider Identifier"       
		nppes_provider_last_org_name 		= "Last Name/Organization Name of the Provider"
		nppes_provider_first_name 			= "First Name of the Provider"
		nppes_provider_mi					= "Middle Initial of the Provider"
		nppes_credentials 					= "Credentials of the Provider"
		nppes_provider_gender 				= "Gender of the Provider"
		nppes_entity_code 					= "Entity Type of the Provider"
		nppes_provider_street1 				= "Street Address 1 of the Provider"
		nppes_provider_street2 				= "Street Address 2 of the Provider"
		nppes_provider_city 				= "City of the Provider"
		nppes_provider_zip 					= "Zip Code of the Provider"
		nppes_provider_state 				= "State Code of the Provider"
		nppes_provider_country 				= "Country Code of the Provider"
		provider_type	 					= "Provider Type of the Provider"
		medicare_participation_indicator 	= "Medicare Participation Indicator"
		place_of_service 					= "Place of Service"
		hcpcs_code       					= "HCPCS Code"
		hcpcs_description 					= "HCPCS Description"
		hcpcs_drug_indicator				= "Identifies HCPCS As Drug Included in the ASP Drug List"
		line_srvc_cnt    					= "Number of Services"
		bene_unique_cnt  					= "Number of Medicare Beneficiaries"
		bene_day_srvc_cnt 					= "Number of Distinct Medicare Beneficiary/Per Day Services"
		average_Medicare_allowed_amt 		= "Average Medicare Allowed Amount"
		average_submitted_chrg_amt 			= "Average Submitted Charge Amount"
		average_Medicare_payment_amt 		= "Average Medicare Payment Amount"
		average_Medicare_standard_amt		= "Average Medicare Standardized Payment Amount";
RUN;

/* QUESTION 1B --------------------------------------------------------------*/
data medicare;
	set Medicare_PS_PUF;
	where prxmatch("/MRI/", hcpcs_description) AND prxmatch("/^7/", hcpcs_code);
run;

/* QUESTION 1C --------------------------------------------------------------*/
/* Calculate Volume */
proc summary data = medicare;
	class hcpcs_description;
	output out = volumeTable
		sum(line_srvc_cnt) = volume;
		label volume = "Volume";
	proc print data = volumeTable;
run;

/* Calculate Total Payments and Average Payments */
proc summary data = medicare;
	class hcpcs_description;
	var average_Medicare_payment_amt;
	weight line_srvc_cnt;
	output out = paymentsTable
		sum(average_Medicare_payment_amt) = total_payments;
		mean(average_Medicare_payment_amt) = average_payments;
		label total_payments = "total_payments"
			average_payments = "average payments";
	proc print data = paymentsTable;
run;

/* Merge the two tables above */
data highestStatistics;
	merge volumeTable paymentsTable;
		by = hcpcs_description;
		DROP _TYPE_;
		DROP _FREQ_;
	output out = highestStatistics;
	proc print data = highestStatistics;
run;

proc sort data = highestStatistics;
	by descending volume total_payments average_payments;
RUN;

/* QUESTION 1D --------------------------------------------------------------*/

/* Part B in SQL */ 
proc sql;
create table sqlTable as
	select hcpcs_description, line_srvc_cnt, average_Medicare_payment_amt, hcpcs_code
		from Medicare_PS_PUF
		where hcpcs_description like "%MRI%" and hcpcs_code like "7%";
quit;
run;

/* Part C in SQL */ 
proc sql;
create table sql_Results as
	select hcpcs_description, sum(line_srvc_cnt) as volume, sum(line_srvc_cnt*average_Medicare_Payment_amt) as total_payment,
		sum(line_srvc_cnt*average_Medicare_payment_amt)/sum(line_srvc_cnt) as average_payment
		from sql_Table
		group by hcpcs_description
		order by -average_payment;
quit;
run;

/* QUESTION 1E --------------------------------------------------------------*/
proc export data = highestStatistics
	outfile='./ps4_q3c.csv'
	dbms=csv
	replace;
RUN;

proc export data = sql_Results
	outfile = './ps4_q3d.csv'
	dbms = csv
	replace;
run;
