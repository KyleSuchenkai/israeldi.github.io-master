## Problem Set 1 Question 3
##

## The RECS 2015 Data used in this script can be found at the link below: 
## https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
## In particular, see the csv data file, the code book, and the note on 
## computing standard errors using replicate weights.
##
## In this problem we analyze some properties of each division, such as walltype
## usage, average total electricity usage, and internet usage. We also stratify 
## some results by Urban and Rural Status for each division.
##
## This file is part of a set of files consisting of:
## ps1_q1.sh, ps1_q2.R, ps1_q3.R, and ps1.Rmd
##
## Author: Israel Diego (israeldi@umich.edu)
## Updated: October 3, 2018 - Last modified date

## @knitr question3
# 80: -------------------------------------------------------------------------

# Libraries: ------------------------------------------------------------------
library(readr)
library(tidyverse)

# Functions: ------------------------------------------------------------------
decode_division = function(x){
  # Decodes numeric codes for DIVISION to Census Division labels
  #
  # Args: 
  #   x: a single numeric code for a DIVISION
  #
  # Returns: The division label
  if (!is.numeric(x)) 
    stop('decode_division expects numeric input indexed from 1!')
  switch(x,
         'New England',
         'Middle Atlantic',
         'East North Central',
         'West North Central',
         'South Atlantic',
         'East South Central',
         'West South Central',
         'Mountain North',
         'Mountain South',
         'Pacific')
}
decode_all_divisions = function(x){
  # Vectorizes decode_division above
  #
  # Args: 
  #  x: a vector of integer-valued DIVISION
  #
  # Returns: A vector of Census Division labes or a "list" if some are unmatched.
  sapply(x, decode_division)
}
decode_walltype = function(x){
  # Decodes numeric codes for WALLTYPE to walltype labels
  #
  # Args: 
  #   x: a single numeric code for a WALLTYPE
  #
  # Returns: The walltype label
  if (!is.numeric(x)) 
    stop('decode_walltype expects numeric input indexed from 1!')
  switch(x,
         'Brick',
         'Wood',
         'Siding',
         'Stucco',
         'Shingle',
         'Stone',
         'Concrete',
         'Other')
}
decode_all_walltypes = function(x){
  # Vectorizes decode_walltype above
  #
  # Args: 
  #  x: a vector of integer-valued WALLTYPE
  #
  # Returns: A vector of walltype labes or a "list" if some are unmatched.
  sapply(x, decode_walltype)
}


# Loading Data Set ------------------------------------------------------------
recs_tib = read_csv("https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv")


# QUESTION 3 PART A -----------------------------------------------------------
# Set the value of Walltype, 9 to 8 in order for the decode_walltype function 
# to work properly
recs_tib[which(recs_tib$WALLTYPE == 9), "WALLTYPE"] = 8

# We create a table consisting of all proportions of wall users per division
wall_type_prop = recs_tib %>% 
  transmute(Division = DIVISION, 
            Wall = WALLTYPE, 
            Weight = NWEIGHT) %>%
  complete(Division, Wall) %>%
  replace_na( list(Weight = 0) ) %>%
  mutate(Division = decode_all_divisions(Division), 
         Wall = decode_all_walltypes(Wall)) %>%
  group_by(Division, Wall) %>%
  summarize(Homes = sum(Weight)) %>%
  group_by(Division) %>%
  mutate( pct = 100*Homes / sum(Homes) ) 


###########################################################
## Compute replicate weighted proportions using group_by ##
###########################################################

# Key values for each observation: --------------------------------------------
wall_type = recs_tib %>% 
  transmute(DOEID, 
            Division=DIVISION, 
            Wall = WALLTYPE, 
            Weight = NWEIGHT) %>%
  replace_na( list(Weight = 0) ) %>%
  group_by(Division, Wall)

# Convert weights to long: ----------------------------------------------------
weights_long = recs_tib[, c(1,475:571)] %>% 
  gather(key = 'repl', value = 'w', BRRWT1:BRRWT96 )

# Join wall type to weights: --------------------------------------------------
wall_type_rep = 
  weights_long %>% 
  left_join(wall_type %>% mutate( DOEID = as.integer(DOEID) ) , by = 'DOEID' )

# Check nothing is lost
if( nrow(weights_long) != nrow(wall_type_rep) ) {
  stop("DOEID mismatch!")
}

# Replicate weighted proportions: --------------------------------------------
wall_type_prop_repl = 
  wall_type_rep %>%
  group_by(Division, Wall, repl) %>%
  summarize(Homes_r = sum(w)) %>%
  group_by(Division, repl) %>%
  mutate( pct_r = 100*Homes_r / sum(Homes_r) ) 

# Add labels and join with point estimates: -----------------------------------
wall_type_prop_repl = 
  wall_type_prop_repl %>% 
  ungroup() %>%
  mutate(Division = decode_all_divisions(Division), 
         Wall = decode_all_walltypes(Wall)) %>%
  left_join(wall_type_prop, by = c('Division', 'Wall') )

# Compute standard errors: ----------------------------------------------------
wall_type_prop =
  wall_type_prop_repl %>%
  group_by(Division, Wall) %>%
  summarize( pct = pct[1],
             std_err = 2 * sqrt( mean( {pct_r - pct}^2 ) )
  ) %>%
  mutate( lwr = pct - qnorm(.975)*std_err,
          upr = pct + qnorm(.975)*std_err)

# Filter out Stucco Wall Users
wall_type_prop = wall_type_prop %>% filter(Wall == 'Stucco')

# Check which divisions have the highest and lowest proportions of Stucco Usage
wall_type_propMaxMin = 
  wall_type_prop[which(wall_type_prop$pct %in% c(min(wall_type_prop$pct),
                                                 max(wall_type_prop$pct))),]

# Adjusting CI column so the CI lwr and upr bounds appear under one column
wall_type_prop[, 3:6] = signif(wall_type_prop[, 3:6], 3)

wall_type_prop = wall_type_prop %>% 
  mutate(CI = paste0('[', lwr, ', ', upr, ']')) %>%
  select(Division,
         pct,
         std_err,
         CI)


# QUESTION 3 PART B -----------------------------------------------------------
# i. Analyze mean KWH only by Division
kwh_mean = recs_tib %>% 
  transmute(Division = DIVISION, 
            Kwh = KWH, 
            Weight = NWEIGHT) %>%
  replace_na( list(Weight = 0) ) %>%
  mutate(Division = decode_all_divisions(Division)) %>%
  group_by(Division) %>%
  mutate(KwhDiv = Kwh * Weight) %>%
  summarise(MeanKWH = sum(KwhDiv) / sum(Weight))

###########################################################
## Compute replicate weighted proportions using group_by ##
###########################################################

# Key values for each observation: --------------------------------------------
kwh = recs_tib %>% 
  transmute(DOEID, 
            Division=DIVISION, 
            Kwh = KWH, 
            Weight = NWEIGHT) %>%
  replace_na( list(Weight=0) ) %>%
  group_by(Division)


# Join kwh to weights: --------------------------------------------------------
kwh_rep = 
  weights_long %>% 
  left_join(kwh %>% mutate( DOEID = as.integer(DOEID) ) , by = 'DOEID' )

# Check nothing is lost
if( nrow(weights_long) != nrow(kwh_rep) ) {
  stop("DOEID mismatch!")
}

# Replicate weighted means: ---------------------------------------------------
kwh_mean_repl = 
  kwh_rep %>%
  group_by(Division, repl) %>%
  mutate(KwhDiv_r = Kwh * w) %>%
  summarise(MeanKWH_r = sum(KwhDiv_r) / sum(w))

# Add labels and join with point estimates: -----------------------------------
kwh_mean_repl = 
  kwh_mean_repl %>% 
  ungroup() %>%
  mutate(Division = decode_all_divisions(Division)) %>%
  left_join(kwh_mean, by = c('Division') )

# Comptue standard errors: ----------------------------------------------------
kwh_mean =
  kwh_mean_repl %>%
  group_by(Division) %>%
  summarize( MeanKWH = MeanKWH[1],
             std_err = 2 * sqrt( mean( {MeanKWH_r - MeanKWH}^2 ) )
  ) %>%
  mutate( lwr = MeanKWH - qnorm(.975)*std_err,
          upr = MeanKWH + qnorm(.975)*std_err )

kwh_mean[, 2:5] = signif(kwh_mean[, 2:5], 4)

# Adjusting CI column so the CI lwr and upr bounds appear under one column
kwh_mean = kwh_mean %>% 
  mutate(CI = paste0('[', lwr, ', ', upr, ']')) %>%
  select(Division,
         MeanKWH,
         std_err,
         CI)

# Question 3 Part B -----------------------------------------------------------
# ii. Analyze mean KWH only by Division and Urban/Rural
kwh_UrbRur_mean = recs_tib %>% 
  transmute(Division = DIVISION, 
            UrbRur = UATYP10, 
            Kwh = KWH, 
            Weight = NWEIGHT) %>%
  replace_na( list(Weight = 0) ) %>%
  mutate(Division = decode_all_divisions(Division)) %>%
  group_by(Division, UrbRur) %>%
  mutate(KwhDiv = Kwh * Weight) %>%
  summarise(MeanKWH = sum(KwhDiv) / sum(Weight))


###########################################################
## Compute replicate weighted proportions using group_by ##
###########################################################

# Key values for each observation: --------------------------------------------
kwh_urbrur = recs_tib %>% 
  transmute(DOEID, 
            Division=DIVISION, 
            UrbRur = UATYP10, 
            Kwh = KWH, 
            Weight = NWEIGHT) %>%
  replace_na( list(Weight=0) ) %>%
  group_by(Division, UrbRur)


# Join kwh_urbrur to weights: --------------------------------------------------
kwh_urbrur_rep = 
  weights_long %>% 
  left_join(kwh_urbrur %>% mutate( DOEID = as.integer(DOEID) ) , 
            by = 'DOEID' )

# Check nothing is lost
if( nrow(weights_long) != nrow(kwh_urbrur_rep) ) {
  stop("DOEID mismatch!")
}

# Replicate weighted means: ---------------------------------------------------
kwh_urbrur_repl = 
  kwh_urbrur_rep %>%
  group_by(Division, UrbRur, repl) %>%
  mutate(KwhDiv_r = Kwh * w) %>%
  summarise(MeanKWH_r = sum(KwhDiv_r) / sum(w))
  
# Add labels and join with point estimates: -----------------------------------
kwh_urbrur_repl = 
  kwh_urbrur_repl %>% 
  ungroup() %>%
  mutate(Division = decode_all_divisions(Division)) %>%
  left_join(kwh_UrbRur_mean, by = c('Division', 'UrbRur') )

# Comptue standard errors: ----------------------------------------------------
kwh_UrbRur_mean =
  kwh_urbrur_repl %>%
  group_by(Division, UrbRur) %>%
  summarize( MeanKWH = MeanKWH[1],
             std_err = 2 * sqrt( mean( {MeanKWH_r - MeanKWH}^2 ) )
  ) %>%
  mutate( lwr = MeanKWH - qnorm(.975)*std_err,
          upr = MeanKWH + qnorm(.975)*std_err
  ) %>%
  filter(UrbRur %in% c("U", "R"))

# Changing names of Urban Rural Status
kwh_UrbRur_mean = kwh_UrbRur_mean %>% 
  mutate(UrbRur = replace(UrbRur, UrbRur == 'R', 'Rural'),
         UrbRur = replace(UrbRur, UrbRur == 'U', 'Urban'))

kwh_UrbRur_mean[, 3:6] = signif(kwh_UrbRur_mean[, 3:6], 4)

# Fixing KWH Urban/Rural table to include CI
kwh_UrbRur_mean = kwh_UrbRur_mean %>% 
  mutate(CI = paste0('[', lwr, ', ', upr, ']')) %>%
  select(Division,
         UrbRur,
         MeanKWH,
         std_err,
         CI)


# QUESTION 3 PART C -----------------------------------------------------------
# Creating a table with proportions of internet and non internet users by
# Division and further grouping by Rural and Urban status
internet_prop = recs_tib %>% 
  transmute(Division = DIVISION, 
            UrbRur = UATYP10, 
            InternetUsage = INTERNET,
            Weight = NWEIGHT) %>%
  complete(Division) %>%
  replace_na( list(Weight = 0) ) %>%
  mutate(Division = decode_all_divisions(Division)) %>%
  group_by(Division, UrbRur, InternetUsage) %>%
  summarize(Homes = sum(Weight)) %>%
  group_by(Division, UrbRur) %>%
  mutate( pct = 100*Homes / sum(Homes) )

###########################################################
## Compute replicate weighted proportions using group_by ##
###########################################################

# Key values for each observation: --------------------------------------------
internetUsage = recs_tib %>% 
  transmute(DOEID,
            Division = DIVISION, 
            UrbRur = UATYP10, 
            InternetUsage = INTERNET,
            Weight = NWEIGHT) %>%
  replace_na( list(Weight = 0) ) %>%
  group_by(Division, UrbRur, InternetUsage)

# Convert weights to long: ----------------------------------------------------
weights_long = recs_tib[,c(1,475:571)] %>% 
  gather(key = 'repl', value = 'w', BRRWT1:BRRWT96 )

# Join internetUsage to weights: ----------------------------------------------
internet_rep = 
  weights_long %>% 
  left_join(internetUsage %>% mutate( DOEID=as.integer(DOEID) ) , by='DOEID' )

# Check nothing is lost
if( nrow(weights_long) != nrow(internet_rep) ) {
  stop("DOEID mismatch!")
}

# Replicate weighted proportions: --------------------------------------------
internet_prop_repl = 
  internet_rep %>%
  group_by(Division, UrbRur, InternetUsage, repl) %>%
  summarize(Homes_r = sum(w)) %>%
  group_by(Division, UrbRur, repl) %>%
  mutate( pct_r = 100*Homes_r / sum(Homes_r) ) 


# Add labels and join with point estimates: -----------------------------------
internet_prop_repl = 
  internet_prop_repl %>% 
  ungroup() %>%
  mutate(Division = decode_all_divisions(Division)) %>%
  left_join(internet_prop, by = c('Division', 'UrbRur', 'InternetUsage') )

# Comptue standard errors: ----------------------------------------------------
internet_prop =
  internet_prop_repl %>%
  group_by(Division, UrbRur, InternetUsage) %>%
  summarize( pct = pct[1],
             std_err = 2 * sqrt( mean( {pct_r - pct}^2 ) )
  ) %>%
  mutate( lwr = pct - qnorm(.975)*std_err,
          upr = pct + qnorm(.975)*std_err
  )


# We filter out non-Internet Users and filter out the Urban Cluster
internet_prop = internet_prop %>% 
  filter(InternetUsage == 1, 
         UrbRur %in% c('R','U'))


internet_prop[, 4:7] = signif(internet_prop[, 4:7], 3)
# Adjusting CI column so the CI lwr and upr bounds appear under one column
internet_prop = internet_prop %>% 
  mutate(CI = paste0('[', lwr, ', ', upr, ']')) %>%
  select(Division,
         UrbRur,
         pct,
         std_err,
         CI)
  

# Setting up to find the Max Disparity between rural and urban proportions
internet_prop_MaxDisparity = internet_prop %>% 
  select(Division, UrbRur, pct) %>%
  group_by(Division, UrbRur) %>%
  tidyr::spread(UrbRur, pct) %>%
  select(Division, 
         RuralInternetProp = R, 
         UrbanInternetProp = U) %>%
  mutate(absDifference = abs(RuralInternetProp - UrbanInternetProp))

# Changing names of Urban Rural status
internet_prop = internet_prop %>%
  ungroup() %>%
  mutate(UrbRur = replace(UrbRur, UrbRur == 'R', 'Rural'),
         UrbRur = replace(UrbRur, UrbRur == 'U', 'Urban'))

# Result for MaxDisparity between rural and urban
internet_prop_MaxDisparity =
  internet_prop_MaxDisparity[which(internet_prop_MaxDisparity$absDifference == 
        max(internet_prop_MaxDisparity$absDifference)),]


# CLEANUP ---------------------------------------------------------------------
rm(internet_prop_repl,internet_rep, internetUsage, 
   kwh, kwh_mean_repl, kwh_rep, kwh_urbrur_rep, kwh_urbrur_repl,
  kwh_urbrur, wall_type, wall_type_prop_repl, wall_type_rep)










