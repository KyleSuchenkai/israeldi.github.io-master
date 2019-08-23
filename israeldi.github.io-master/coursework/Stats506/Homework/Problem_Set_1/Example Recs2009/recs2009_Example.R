# Question 3

library(readr)
library(tidyverse)

decode_state = function(x){
  if(!is.numeric(x)) stop('decode_states expects numeric input indexed from 1!')
  switch(x,
         "CT, ME, NH, RI, VT", "MA", "NY", "NJ", "PA", "IL", "IN, OH", "MI", "WI",
         "IA, MN, ND, SD", "KS, NE", "MO", "VA", "DE, DC, MD, WV", "GA",
         "NC, SC" , "FL", "AL, KY, MS", "TN", "AR, LA, OK",
         "TX", "CO", "ID, MT, UT, WY", "AZ", "NV, NM",
         "CA", "AK, HI, OR, WA"
  )
}
decode_all_states = function(x){
  sapply(x, decode_state)
}
decode_house_type = function(x){
  if(!is.numeric(x)) stop('decode_house_type expects numeric input indexed from 1!')
  switch(x,
         'MobileHome',
         'SingleFamilyDetached',
         'SingleFamilyAttached',
         'ApartmentFew',
         'ApartmentMany'
  )
}
decode_all_house_types = function(x){
  sapply(x, decode_house_type)
  }



recs_tib = readr::read_delim(
  "https://www.eia.gov/consumption/residential/data/2009/csv/recs2009_public.csv",
  delim = ',' )
readr::write_delim(recs_tib, path = file, delim = ',')

recs_tib %>% 
  transmute(State=REPORTABLE_DOMAIN, Type=TYPEHUQ, Weight = NWEIGHT) %>%  
  mutate(State=decode_all_states(State), Type=decode_all_house_types(Type)) %>%
  group_by(State, Type) %>%
  summarize(Homes=sum(Weight)) %>%
  tidyr::spread(Type,Homes) #%>%
  mutate(
    Total = ApartmentFew + ApartmentMany + MobileHome + 
      SingleFamilyAttached + SingleFamilyDetached,
    ApartmentFew         = 100*ApartmentFew/Total,
    ApartmentMany        = 100*ApartmentMany/Total,
    MobileHome           = 100*MobileHome/Total,
    SingleFamilyAttached = 100*SingleFamilyAttached/Total,
    SingleFamilyDetached = 100*SingleFamilyDetached/Total
    )





