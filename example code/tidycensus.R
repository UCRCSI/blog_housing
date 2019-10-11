install.packages("tidycensus")
#To get started working with tidycensus, users should load the package along with the tidyverse package, and set their Census API key. A key can be obtained from http://api.census.gov/data/key_signup.html

library(tidycensus)
library(tidyverse)


# normally you can do this ------------------------------------------------
#national level median family income
fincome_avg_us <- get_acs(table = "B19113", year = 2017, survey = "acs1", geography = "us")
fincome_blk_us <- get_acs(table = "B19113B", year = 2017, survey = "acs1", geography = "us")
fincome_aian_us <- get_acs(table = "B19113C", year = 2017, survey = "acs1", geography = "us")
fincome_asn_us <- get_acs(table = "B19113D", year = 2017, survey = "acs1", geography = "us")
fincome_nhpi_us <- get_acs(table = "B19113E", year = 2017, survey = "acs1", geography = "us")
fincome_wt_us <- get_acs(table = "B19113H", year = 2017, survey = "acs1", geography = "us")
fincome_hisp_us <- get_acs(table = "B19113I", year = 2017, survey = "acs1", geography = "us")
#DIVISION level median family income
fincome_avg_us <- get_acs(table = "B19113", year = 2017, survey = "acs1", geography = "division")
fincome_blk_us <- get_acs(table = "B19113B", year = 2017, survey = "acs1", geography = "division")
fincome_aian_us <- get_acs(table = "B19113C", year = 2017, survey = "acs1", geography = "division")
fincome_asn_us <- get_acs(table = "B19113D", year = 2017, survey = "acs1", geography = "division")
fincome_nhpi_us <- get_acs(table = "B19113E", year = 2017, survey = "acs1", geography = "division")
fincome_wt_us <- get_acs(table = "B19113H", year = 2017, survey = "acs1", geography = "division")
fincome_hisp_us <- get_acs(table = "B19113I", year = 2017, survey = "acs1", geography = "division")

# Or I will do this -------------------------------------------------------
family_income <- c("B19113_001", "B19113B_001", "B19113C_001", 
                   "B19113D_001", "B19113E_001", "B19113H_001", "B19113I_001")
race_recode <- function(dta){
  dta <- dta %>% 
    mutate(group = case_when(
      variable == "B19113_001" ~"US Average",
      variable == "B19113B_001" ~"Black or African American alone",
      variable == "B19113C_001" ~"AIAN alone",
      variable == "B19113D_001" ~"Asian alone",
      variable == "B19113E_001" ~"NHPI alone",
      variable == "B19113H_001" ~"White alone (Non-Hispanic)",
      variable == "B19113I_001" ~"Hispanic of any race"))
}
#pull raw data
national <- get_acs(variables = family_income, year = 2017, geography = "us", survey = "acs1")
divisions <- get_acs(variables = family_income, year = 2017, geography = "division", survey = "acs1")
#recode race
national <- race_recode(national) %>% 
  select(group, estimate)
divisions <- race_recode(divisions) %>% 
  select(NAME, group, estimate) %>% 
  rename(census_divs = NAME)
