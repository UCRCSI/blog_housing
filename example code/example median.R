library(tidyverse)
library(srvyr)

dta <- read_rds("pums_final.rds") %>%
  mutate(statefip = as_factor(lbl_clean(statefip)))

income$ownershp
pums_weighted <- income %>%
  as_survey_design(ids = 1,weight = perwt)

pums_weighted %>%
  group_by(aapi_alone) %>%
  summarize(median_income = survey_median(income_2017,na.rm = T)) -> final_dta1

#example
weighted_df <- data %>% 
  as_survey_design(ids = 1, weight = yourweight)

weighted_df %>% 
  group_by(racial_group) %>% 
  summarize(medianHHincome = survey_median(income, na.rm = TRUE)) -> final_income_table


