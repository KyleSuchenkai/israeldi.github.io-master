## Problem Set 1 Question 2
##
## The nycflights2014 Data used in this script can be found at the link below: 
## https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv'
## The nycflights2013 Data used in this script can be found in the 
## nycflights2013 R package
##
## In this problem set we compare proportions of flights per airline leaving 
## three New York Airports in 2013 and 2014. 
##
## This file is part of a set of files consisting of:
## ps1_q1.sh, ps1_q2.R, ps1_q3.R, and ps1.Rmd
##
## Author: Israel Diego (israeldi@umich.edu)
## Updated: October 3, 2018 - Last modified date

## @knitr question2
# 80: -------------------------------------------------------------------------
# Libraries: ------------------------------------------------------------------
# install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
library(knitr)

# Functions: ------------------------------------------------------------------
decode_airline = function(x){
  # Decodes character codes for carrier variable to Airline labels
  #
  # Args: 
  #   x: a single character list for a carrier variable
  #
  # Returns: The Airline label
  str_replace_all(x, 
                  c('9E' = 'Endeavor Air Inc.',
                    'AA' = 'American Airlines Inc.',
                    'AS' = 'Alaska Airlines Inc.',
                    'B6' = 'JetBlue Airways',
                    'DL' = 'Delta Air Lines Inc.',
                    'EV' = 'ExpressJet Airlines Inc.',
                    'F9' = 'Frontier Airlines Inc.',
                    'FL' = 'AirTran Airways Corporation',
                    'HA' = 'Hawaiian Airlines Inc.',
                    'MQ' = 'Envoy Air',
                    'OO' = 'SkyWest Airlines Inc.',
                    'UA' = 'United Air Lines Inc.',
                    'US' = 'US Airways Inc.',
                    'VX' = 'Virgin America',
                    'WN' = 'Southwest Airlines Co.',
                    'YV' = 'Mesa Airlines Inc.'))
}
CI_Pop_Proportion = function(p, n){
  # Computes 95% Confidence Intervals for Population Proportion
  #
  # Args: 
  #   p: sample proportion
  #   n: total number of observations
  #
  # Returns: a vector of lower and upper 95% Confidence bounds
  se = sqrt((p * (1 - p)) / n)
  lower = p - qnorm(0.975, 0, 1, lower.tail = TRUE) * se
  upper = p + qnorm(0.975, 0, 1, lower.tail = TRUE) * se
  ci = c(lower, upper)
  
  return(ci)
}
CI_TwoPop_Proportion = function(p1, p2, n1, n2){
  # Computes 95% Confidence Intervals for Population Proportion
  #
  # Args: 
  #   p1: sample proportion of first population
  #   p2: sample proportion of second population
  #   n1: total number of observations in first population
  #   n2: total number of observations in second population
  #
  # Returns: a vector of lower and upper 95% Confidence bounds
  se = sqrt((p1 * (1 - p1)) / n1 + (p2 * (1 - p2)) / n2)
  lower = (p2 - p1) - qnorm(0.975, 0, 1, lower.tail = TRUE) * se
  upper = (p2 - p1) + qnorm(0.975, 0, 1, lower.tail = TRUE) * se
  ci = c(lower, upper)
  
  return(ci)
}
Create_Table_Per_Airport = function(airPortData, airport, n){
  # This is helper function for Part C of Question 2. It helps with creating
  # a table for each of the 3 airports and computes CIs.
  #
  # Args:
  #   airPortData: Data table from Question 2 Part C, containing the airlines
  #                from Part a and their proportion of flights for each airport
  #   airport: The name of the airport we want to create a table for
  #   n: The total number of flights in this airport
  #
  # Returns: a vector of lower and upper 95% Confidence bounds
  TOTAL_FLIGHTS_AIRPORT = n
  airPortData[, c('carrier', airport)] %>%
    transmute(carrier, 
              PercentFlights = eval((parse(text = airport)))) %>%
    rowwise() %>%
    mutate(LowerCI = CI_Pop_Proportion(PercentFlights, 
                                       TOTAL_FLIGHTS_AIRPORT)[1],
           UpperCI = CI_Pop_Proportion(PercentFlights, 
                                       TOTAL_FLIGHTS_AIRPORT)[2])
}

# Loading our New York Flights Data sets from 2013 and 2014 -------------------
nyc2013 = nycflights13::flights
nyc2014 = read.csv(paste0('https://raw.githubusercontent.com/wiki/',
'arunsrinivasan/flights/NYCflights14/flights14.csv'))


# Initializing Constants for total flights in the first 10 months -------------
# of 2013 and 2014
TOTAL_FLIGHTS_2013 = nyc2013 %>% filter(month != 11, month != 12) %>% nrow()
TOTAL_FLIGHTS_2014 = nyc2014 %>% nrow()


# QUESTION 2 PART A -----------------------------------------------------------
# Create a table using nyc flight Data from 2013 and we compute proportion of 
# total flights up to October 31st. At the end we filter out only those airlines
# consisting of more than 1% of total flights
Flights_2013 = nyc2013 %>% 
  filter(month != 11, month != 12) %>%
  transmute(carrier) %>%
  mutate(carrier = decode_airline(carrier)) %>%
  group_by(carrier) %>% 
  tally() %>% 
  arrange(carrier) %>% 
  rename(numFlights2013 = n) %>%
  rowwise() %>%
  summarise(carrier,
            numFlights2013,
            PercentFlights2013 = (numFlights2013 / TOTAL_FLIGHTS_2013)) %>%
  filter(PercentFlights2013 > .01)


# This is the list of carriers in PART A that make up more than 1% of 
# total flights
CARRIER_LIST_2013 = Flights_2013$carrier


# QUESTION 2 PART B -----------------------------------------------------------
# Create a table using nyc flight Data from 2014 and we compute proportion of 
# total flights up to October 31st. We only compute proportions for flights
# found in Part A
Flights_2014 = nyc2014 %>% 
  transmute(carrier) %>%
  mutate(carrier = decode_airline(carrier)) %>%
  filter(carrier %in% CARRIER_LIST_2013) %>%
  group_by(carrier) %>% 
  tally() %>%
  rename(numFlights2014 = n) %>%
  rowwise() %>%
  summarise(carrier,
            numFlights2014,
            PercentFlights2014 = (numFlights2014 / TOTAL_FLIGHTS_2014))

# Here we combine flight Proportions from 2013 and 2014
flightData = merge(Flights_2013, Flights_2014, by = "carrier")
rm(Flights_2014)

# We compute 95% CIs for 2013 and 2014 proportions per airline, and we compute
# CIs for the percent change in proportions per airline
flightData = flightData %>% 
  mutate(PercentChange = PercentFlights2014 - PercentFlights2013) %>%
  rowwise() %>%
  mutate(LowerCI_2013 = CI_Pop_Proportion(PercentFlights2013, 
                                          TOTAL_FLIGHTS_2013)[1],
         UpperCI_2013 = CI_Pop_Proportion(PercentFlights2013, 
                                          TOTAL_FLIGHTS_2013)[2],
         LowerCI_2014 = CI_Pop_Proportion(PercentFlights2014, 
                                          TOTAL_FLIGHTS_2014)[1],
         UpperCI_2014 = CI_Pop_Proportion(PercentFlights2014, 
                                          TOTAL_FLIGHTS_2014)[2],
         LowerChangeCI = CI_TwoPop_Proportion(PercentFlights2013,
                                              PercentFlights2014,
                                              TOTAL_FLIGHTS_2013,
                                              TOTAL_FLIGHTS_2014)[1],
         UpperChangeCI = CI_TwoPop_Proportion(PercentFlights2013,
                                              PercentFlights2014,
                                              TOTAL_FLIGHTS_2013,
                                              TOTAL_FLIGHTS_2014)[2]) %>%
  select(carrier,
         numFlights2013,
         PercentFlights2013,
         LowerCI_2013,
         UpperCI_2013,
         numFlights2014,
         PercentFlights2014,
         LowerCI_2014,
         UpperCI_2014,
         PercentChange,
         LowerChangeCI,
         UpperChangeCI)

# Fixing up our table with 2013 and 2014 data in order to display CIs in one
# column
Flights_2013[, 3] = signif(Flights_2013[, 3] * 100, 3)
flightData[, c(3:5, 7:12)] = signif(flightData[, c(3:5, 7:12)] * 100, 3)

flightData = flightData %>% mutate(CI_2013 = paste0('[', LowerCI_2013, ', ', 
                                                    UpperCI_2013, ']'),
                                   CI_2014 = paste0('[', LowerCI_2014, ', ', 
                                                    UpperCI_2014, ']'),
                                   CI_Change = paste0('[', LowerChangeCI, ', ', 
                                                      UpperChangeCI, ']')) %>%
  select(carrier,
         numFlights2013,
         PercentFlights2013,
         CI_2013,
         numFlights2014,
         PercentFlights2014,
         CI_2014,
         PercentChange,
         CI_Change)

# Flights with the Largest Percent Increase and Decrease
flight_Largest_IncDec = 
  flightData[which(flightData$PercentChange %in% 
                     c(min(flightData$PercentChange), 
                       max(flightData$PercentChange))),] %>%
  select(carrier, PercentChange)


# QUESTION 2 PART C -----------------------------------------------------------
airPortData = nyc2013 %>% 
  transmute(origin, carrier) %>%
  mutate(carrier = decode_airline(carrier)) %>%
  group_by(origin) %>%
  add_tally() %>%
  rename(numFlightsPerAirport = n) %>%
  {. ->> numFlightsPerAirport} %>%
  ungroup(origin) %>%
  filter(carrier %in% CARRIER_LIST_2013) %>%
  group_by(origin, carrier) %>%
  add_tally() %>%
  rename(numFlightsPerAirline = n) %>%
  distinct() %>%
  rowwise() %>%
  summarise(origin, 
            carrier,
            Proportion = numFlightsPerAirline / numFlightsPerAirport) %>%
  tidyr::spread(origin, Proportion)


numFlightsPerAirline = nyc2013 %>% 
  transmute(origin, carrier) %>%
  mutate(carrier = decode_airline(carrier)) %>%
  group_by(origin, carrier) %>%
  add_tally() %>%
  filter(carrier %in% CARRIER_LIST_2013) %>%
  rename(numFlights = n) %>%
  summarise(numFlightsPerAir = sum(numFlights)) %>% 
  tidyr::spread(origin, numFlightsPerAir)
  
  
# Creating a table with the total number of flights in each airport
numFlightsPerAirport = numFlightsPerAirport %>%
  group_by(origin) %>%
  tally() %>%
  rename(numFlightsPerAirport = n)

# Creating 3 tables corresponding to the 3 airports with the for loop
for (i in 1:nrow(numFlightsPerAirport)) {
  # Initializing airport name
  airport = numFlightsPerAirport$origin[i]
  
  # Extracting total number of flights for this airport
  TOTAL_FLIGHTS_AIRPORT = numFlightsPerAirport$numFlightsPerAirport[i]
  
  # Creating a table for this specific airport using the Create_Table function
  airportTable = Create_Table_Per_Airport(airPortData, 
                                          airport, 
                                          TOTAL_FLIGHTS_AIRPORT)
  
  # Set NA values = 0
  airportTable[is.na(airportTable)] = 0
  
  # Fixing up CI lower and upper columns so that CIs appear in only one column
  airportTable[, 2:4] = signif(airportTable[, 2:4] * 100, 3)
  airportTable = airportTable %>%
    mutate(CI = paste0('[',LowerCI,', ', UpperCI, ']')) %>%
    select(carrier,
           PercentFlights,
           CI)
  
  assign(paste0('Table', airport), airportTable)
  
  # Finding the Largest Carrier at each airport
  assign(paste0('max', airport), 
         airportTable[which(airportTable$PercentFlights ==
                              max(airportTable$PercentFlights)),])
  
  rm(airportTable)
}





  
































