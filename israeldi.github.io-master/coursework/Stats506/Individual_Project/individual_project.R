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

rm(list = ls())
if(FALSE)
{
# Packages
library(data.table)
library(tidyr)
library(dplyr)

# Variables we need for our code
colsToExtract = c("HCPCS_CODE", 
                  "HCPCS_DESCRIPTION", 
                  "NPPES_PROVIDER_STATE",
                  "LINE_SRVC_CNT",
                  "AVERAGE_MEDICARE_PAYMENT_AMT");

# Get column names from data
colNames = fread('./data/Medicare_Provider_Util_Payment_PUF_CY2016.txt',
                 nrows = 1)

# Use Column Indices instead of names to extract data from each annual data set
colIndices = which(names(colNames) %in% colsToExtract)

# Figure out the most common heart procedure 
year = 2016
topProcCode = 
  # Read data from 2016
  fread(paste0('./data/Medicare_Provider_Util_Payment_PUF_CY', 
               year, '.txt'),
        select = colIndices) %>%
  # Throw away first row
  slice(2:n()) %>%
  # Rename Column variables
  transmute(state = .[[1]],
            proc_code = .[[2]],
            description = .[[3]],
            line_count = .[[4]],
            avg_payment = .[[5]]) %>%
  # Keep only the results with heart in the description
  slice(grep("heart", description, ignore.case = TRUE)) %>%
  {. ->> heartData} %>%
  # Find the most popular proc code
  group_by(proc_code) %>%
  summarise(count = sum(line_count)) %>%
  arrange(desc(count)) %>%
  slice(1:5)

# Extract the Descriptions given the HCPCS code
topDescriptionCol = matrix(0, nrow = 0, ncol = 1)
for(i in 1:5)
{
  index = grep(topProcCode[i,1], heartData$proc_code)[1]
  topDescriptionCol[i] = heartData[index, "description"]
}

# Add Description to our top heart procedures
topProcCode[,"description"] = topDescriptionCol

# Extract the average cost ---------------------------------------------------- 
# for the heart procedure found above, and group results by state
for(year in 2012:2016)
{
  # Initialize variable name to be assigned at the end of our loop
  varName = paste0("avg", year)
  
  # Assign dataframe name
  assign(paste0("medicare", year,"_Avg_byState"),
         fread(paste0('./data/Medicare_Provider_Util_Payment_PUF_CY', 
                      year, '.txt'),
               select = colIndices) %>%
           # Throw away first row
           slice(2:n()) %>%
           # Rename Column variables
           transmute(state = .[[1]],
                     proc_code = .[[2]],
                     description = .[[3]],
                     line_count = .[[4]],
                     avg_payment = .[[5]]) %>%
           # Keep only the most common heart procedure and group by state
           filter(proc_code == as.numeric(topProcCode[1,1])) %>%
           group_by(state) %>%
           # Calculate Average Cost
           summarise(avg = sum(line_count * avg_payment) / sum(line_count)) %>%
           transmute(state, !!varName := avg))
}

# Aggregate our state average payments for each year --------------------------

# Initialize aggregateStateAvgPayment table with averages from 2012
aggregateStateAvgPayment = medicare2012_Avg_byState

# Left Join each set averages to our Aggregate State Average Payment table
for(year in 2013:2016)
{
  aggregateStateAvgPayment = 
    aggregateStateAvgPayment %>%
    left_join(get(paste0("medicare", year, "_Avg_byState")), by = 'state')
}

# Keep the 5 states with the highest average cost -----------------------------
# over our 5-year period and also the 5 states with the lowest average cost
aggregateStateAvgPayment = 
  aggregateStateAvgPayment %>%
  rowwise() %>%
  # Compute the average over all years
  mutate(avgOverall = mean(c(avg2012, avg2013, avg2014, avg2015, avg2016))) %>%
  arrange(desc(avgOverall)) %>%
  # Keep top 5 and bottom 5
  slice(c(1:5, (n() - 4):n())) %>%
  # Save current table
  {. ->> stateRanks } %>%
  # Convert to long format for plotting
  gather(year,
         avgPayment, 
         avg2012:avg2016, factor_key = FALSE) %>%
  select(-avgOverall) %>%
  group_by(state)
  
# Remove "avg" from year variable name
aggregateStateAvgPayment$year = gsub("^.{3}","",aggregateStateAvgPayment$year)

# Plot in Rmarkdown -----------------------------------------------------------
}
# Save/Load Results for later use
#save.image("individualProj.Rdata")
load("individualProj.Rdata")
