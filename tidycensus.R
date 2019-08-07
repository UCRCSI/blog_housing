install.packages("tidycensus")
#To get started working with tidycensus, users should load the package along with the tidyverse package, and set their Census API key. A key can be obtained from http://api.census.gov/data/key_signup.html

library(tidycensus)
library(tidyverse)
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

