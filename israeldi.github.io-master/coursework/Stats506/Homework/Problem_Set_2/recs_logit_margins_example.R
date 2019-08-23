# 80: --------------------------------------------------------------------------
# load packages: ---------------------------------------------------------------
library(tidyverse) 


# Obtain or restore data: -----------------------------------------------------
recs = readr::read_delim(
  "https://www.eia.gov/consumption/residential/data/2009/csv/recs2009_public.csv",
  delim = ',' )

# read or load data: -----------------------------------------------------------

weights = readr::read_delim( 
  'https://www.eia.gov/consumption/residential/data/2009/csv/recs2009_public_repweights.csv',
  delim = ',')

# tidy data: -------------------------------------------------------------------
# function to decode region
decode_region = function(rvec){
  sapply(rvec, function(r) switch(r, "NE", "MW", "S", "W"))
}

recs = recs %>% 
  mutate(es_fridge = ifelse(ESFRIG < 0, NA, ESFRIG),
         totsqft = TOTSQFT / 100,
         region = decode_region(REGIONC)
  ) %>% 
  filter(!is.na(es_fridge))


fit0 = glm(es_fridge ~ 0 + region + totsqft, data=recs,
           family=binomial(link='logit'))


#### At the mean
# Set up a new data frame for predictions: ------------------------------------
recs_regions_atmean = 
  tibble(region = unique(recs$region)) %>%
  mutate(totsqft = mean(recs$totsqft))

## Use predict to estimate probabilities for the new data
m0 = predict(fit0, recs_regions_atmean, type='response', se=TRUE)

## We can include these in the data frame and plot to compare
recs_regions_atmean %>%
  mutate(fit=m0$fit, se=m0$se.fit, lwr=fit - 2*se, upr=fit + 2*se) %>%
  mutate(region=factor(region,region[order(fit)])) %>% # order
  ggplot(aes(y=region, x=fit)) +
  geom_point() + 
  geom_errorbarh(aes(xmin=lwr, xmax=upr), height=.1) + 
  geom_vline( aes( xintercept = fit), lty = 'dashed', color='grey', alpha =.5 ) + 
  xlab('Predicted probability for an average-sized house') +
  ggtitle('Estimated usage of Energy Star compliant refrigerators by region.') +
  theme_bw() +
  xlim(c(0,1))

df_regions =
  recs %>% 
  group_by(region) %>%
  summarize(Count=n()) %>%
  spread(region,Count) %>%
  mutate(Total=MW+NE+S+W,
         MW=MW/Total, 
         NE=NE/Total,
         S =S/Total,
         W =W/Total
  ) %>%
  gather(region, prop, MW:W)

p_regions = df_regions$prop
names(p_regions) = df_regions$region
p_regions
