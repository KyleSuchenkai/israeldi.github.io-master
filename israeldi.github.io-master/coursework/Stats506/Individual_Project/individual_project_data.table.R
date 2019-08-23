## Individual Project ---------------------------------------------------------
##
##
## In this project we would like to compute the average cost over time 
## (2012-2016) for the most common heart procedure in the US and analyze the 
## trend in average cost by state.
##
## This file is part of a set of files consisting of:
## optional_project.R, optional_project.Rmd, and optional_project.html
##
## Author: Israel Diego (israeldi@umich.edu)
## Due: December 16, 2018



# With data.table -------------------------------------------------------------
heartProcedures = medicare[grep("heart", HCPCS_DESCRIPTION, ignore.case = TRUE), 
                           .(.SD, n=.N), 
                           by = .(HCPCS_DESCRIPTION)
                           ][order(-n), .(SD)]