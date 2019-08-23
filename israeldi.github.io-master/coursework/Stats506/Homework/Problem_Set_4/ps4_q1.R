## Problem Set 4 Question 1
##
##
## In this question we work with the 'mtcars' dataset and calculate univariate
## regressions written in data.table and dplyr. We also write functions that 
## compute univariate regressions based on arbitrary independent, dependent, 
## and grouping variables.
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
install.packages('Lahman')
install.packages('RSQLite')
install.packages('dbplyr')
install.packages('tidyverse')
library(tidyverse)
library(dbplyr)
library(Lahman)

# QUESTION 1 -----------------------------------------------------------------

# Write an SQL query to construct a table showing the all-time leader in hits 
# (“H” from the “batting” table) for each birth country (“birthCountry” in the 
# “master” table). 
#
# An all-time leader is the player (“playerID”) with the most total hits across 
# all rows (e.g. seasons/stints). Limit your table to players/countries with at 
# least 200 hits and order the table by descending number of hits. 
# 
# Create a nicely formatted table with the following columns as your final 
# output: Player (nameFirst nameLast), Debut (debut), 
#         Country of Birth (birthCountry), Hits (H)

# Create a local SQLlite database of the Lahman data
lahman = lahman_sqlite()

batting = lahman %>% tbl("BATTING")


allTimeHits = lahman %>% 
  tbl(sql('
          SELECT firstName || " " || lastName as Name, 
          strftime("%m-%d-%Y", Debut) as Debut, birthCountry, max(Hits) as Hits
          FROM(
            SELECT bat.playerID, m.nameFirst firstName, m.nameLast lastName, 
            m.debut Debut, 
            m.birthCountry birthCountry, sum(H) as Hits
            FROM batting bat
            INNER JOIN master m ON bat.playerID = m.playerID
            GROUP BY bat.playerID
            HAVING Hits >= 200
          )
          GROUP BY birthCountry
          ORDER BY -Hits')) %>% collect()
