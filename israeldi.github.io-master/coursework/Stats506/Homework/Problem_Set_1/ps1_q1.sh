#!/bin/bash  

## Problem Set 1 Question 1
##
## In this file we practice subsetting columns of data from RECS2015 data set. 
## We count the total number of distinct values for reach Region and also 
## analyze Division and Region to create unique combinations of the two.
## 
## This file is part of a set of files consisting of:
## ps1_q1.sh, ps1_q2.R, ps1_q3.R, and ps1.Rmd
##
## Author: Israel Diego (israeldi@umich.edu)
## Updated: October 3, 2018 - Last modified date

# cd "/Users/israeldiego/Google Drive/Documents Google Drive/Fall 2018/Stats 506/Homework/Problem Set 1"

# Question 1 Part A -----------------------------------------------------------
# i) Counting rows for Region 3
cut -d ',' -f 2 recs2015_public_v3.csv | grep 3 | wc -l

# Part A
# ii)
# This command finds the column numbers where the column names are located in 
# the csv file
# cat recs2015_public_v3.csv | head -1 | grep -o '[^,]\+' | grep -n -o -e "DOEID\|NWEIGHT\|BRRWT"

# This command takes the column the variables: DOEID, NWEIGHT, and
# BRRWT1-BRRWT96. and saves the data into a compressed file named 'newData'
cut -d ',' -f 1,475-571 recs2015_public_v3.csv | gzip > newData.gz

# Question 1 Part B -----------------------------------------------------------
# i) Bash for loop that counts and prints the number of observations within each 
# region
for region in 1 2 3 4
do

  echo $region
  cut -d ',' -f 2 recs2015_public_v3.csv| grep $region | wc -l

done

# Part B
# ii) Produces a file region_division.txt providing a sorted list showing unique
# combinations of values from REGIONC and DIVISION.
cut -d ',' -f 2,3 recs2015_public_v3.csv| sort -n | uniq > region_division.txt









