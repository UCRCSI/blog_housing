# Load relevant libraries
library(tidyverse)
library(survey)
library(srvyr)
library(forcats)
library(reshape2)
library(openxlsx)


# Loading and cleaning data -----------------------------------------------
# Load data
housing <- read.csv("ahs2017_flat_r.csv")

# Clean data
# relabeling HH race and ethnicity levels
levels(housing$HHRACE3) <- c("HH AIAN", "HH Asian", "HH Black", "HH NHPI", "HH White")
levels(housing$HHSPAN2) <- c("HH Hispanic or Latinx", "HH Not Hispanic or Latinx")
levels(housing$RACEETH) <- c("HH AIAN", "HH Asian", "HH Black", "HH NHPI", "HH White NH")

# relabeling column variables
levels(housing$TENURE)[2:3] <- c("Owner", "Renter")
levels(housing$HUDSUB)[1:3] <- c("Other renter", "Public housing", "Voucher recipient")
levels(housing$RENTCNTRL)[1:2] <- c("No rent control", "Rent control")
levels(housing$DBMISSRENT)[1:2] <- c("Not missed rent", "Missed rent")

# converting NAs to (Missing)
var.races <- colnames(housing)[grepl("^RACE(_|\\d+$)", names(housing))]
var.span <- colnames(housing)[grepl("^SPAN\\d+$", names(housing))]
var.flag <- colnames(housing)[grepl("FLAG$", names(housing))]
var.num <- colnames(housing)[grepl("^NUM", names(housing))]
var.rating <- colnames(housing)[grepl("^RATING", names(housing))]
var.pov <- colnames(housing)[grepl("+POV", names(housing))]

col_factor <- colnames(housing)[!(colnames(housing) %in%
                                    c(var.races, var.span, var.num, var.rating,
                                      var.flag, var.pov, "X.1", "X", "HHAGE", 
                                      "HINCP", "TOTHCAMT", "WEIGHT"))]

housing[,col_factor] <- lapply(housing[,col_factor], function(x) fct_explicit_na(x)) %>% as.data.frame

# Weight data
housing_weighted <- housing %>% as_survey_design(ids = 1, weight = WEIGHT)


# Sample-info sheets ------------------------------------------------------
################################
# Sheet 1: sample-info-race    #
################################

housing_weighted %>% filter(HOUSEHOLDRACE != "(Missing)") %>% 
  group_by(HOUSEHOLDRACE) %>% summarize(n = survey_total()) -> race_household
housing_weighted %>% filter(HHRACE3 != "(Missing)") %>% group_by(HHRACE3) %>% 
  summarize(n = survey_total()) -> race_HH

colnames(race_household)[1] <- colnames(race_HH)[1] <- "Household race"
tmp <- rbind(race_household, race_HH)

# write.csv(tmp, "sample-info-race.csv")
write.xlsx(tmp, "exploratory-analysis.xlsx", sheetNAME = "sample-info-race", append = TRUE)

################################
# Sheet 2: sample-info-span    #
################################

housing_weighted %>% filter(HOUSEHOLDSPAN != "(Missing)") %>% 
  group_by(HOUSEHOLDSPAN) %>% summarize(n = survey_total()) -> span_household
housing_weighted %>% filter(HHSPAN2 != "(Missing)") %>% 
  group_by(HHSPAN2) %>% summarize(n = survey_total()) -> span_HH

colnames(span_household)[1] <- colnames(span_HH)[1] <- "Household span"
tmp <- rbind(span_household, span_HH)

# write.csv(tmp, "sample-info-span.csv")
write.xlsx(tmp, "exploratory-analysis.xlsx", sheetNAME = "sample-info-span", append = TRUE)

##################################
# Sheet 3: sample-info-raceeth   #
##################################

housing_weighted %>% filter(HOUSEHOLDRACEETH != "(Missing)") %>% 
  group_by(HOUSEHOLDRACEETH) %>% summarize(n = survey_total()) -> raceeth_household
housing_weighted %>% filter(RACEETH != "(Missing)") %>% 
  group_by(RACEETH) %>% summarize(n = survey_total()) -> raceeth_HH

colnames(raceeth_household)[1] <- colnames(raceeth_HH)[1] <- "Household raceeth"
tmp <- rbind(raceeth_household, raceeth_HH)

write.csv(tmp, "sample-info-raceeth.csv")

# Analysis sheets ---------------------------------------------------------

########################
# Sheet 4: analysis    #
########################

# Columns:


## Racial proportions ------------------------------------------------------
house_race <- housing_weighted %>% filter(HOUSEHOLDRACE != "(Missing)") %>% 
  group_by(HOUSEHOLDRACE) %>% summarize(race_prop = survey_mean())
hh_race <- housing_weighted %>% filter(HHRACE3 != "(Missing)") %>% 
  group_by(HHRACE3) %>% summarize(race_prop = survey_mean())
colnames(house_race)[1] <- colnames(hh_race)[1] <- "Household race"
race_prop <- rbind(house_race, hh_race) # COL1

house_span <- housing_weighted %>% filter(HOUSEHOLDSPAN != "(Missing)") %>% 
  group_by(HOUSEHOLDSPAN) %>% summarize(span_prop = survey_mean())
hh_span <- housing_weighted %>% filter(HHSPAN2 != "(Missing)") %>% 
  group_by(HHSPAN2) %>% summarize(span_prop = survey_mean())
colnames(house_span)[1] <- colnames(hh_span)[1] <- "Household span"
span_prop <- rbind(house_span, hh_span) # COL1


## Geography (Census division) ---------------------------------------------
# by #1: household
tmp <- housing_weighted %>% 
  filter(HOUSEHOLDRACE != "(Missing)" & DIVISION != "(Missing)") %>% 
  group_by(HOUSEHOLDRACE, DIVISION) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDRACE ~ DIVISION, value.var = "prop")
tmp3 <- dcast(tmp, HOUSEHOLDRACE ~ DIVISION, value.var = "prop_se")
colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
division.byhouserace <- cbind(tmp2, tmp3[,c(2:10)])
# by #2: householder
tmp <- housing_weighted %>% 
  filter(HHRACE3 != "(Missing)" & DIVISION != "(Missing)") %>% 
  group_by(HHRACE3, DIVISION) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHRACE3 ~ DIVISION, value.var = "prop")
tmp3 <- dcast(tmp, HHRACE3 ~ DIVISION, value.var = "prop_se")
colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
division.byhhrace <- cbind(tmp2, tmp3[,c(2:10)])

colnames(division.byhouserace)[1] <- colnames(division.byhhrace)[1] <- "Household race"
division.byrace <- rbind(division.byhouserace, division.byhhrace) # COL 2

# by spaneth
## by #1: household
tmp <- housing_weighted %>% 
  filter(HOUSEHOLDSPAN != "(Missing)" & DIVISION != "(Missing)") %>% 
  group_by(HOUSEHOLDSPAN, DIVISION) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDSPAN ~ DIVISION, value.var = "prop")
tmp3 <- dcast(tmp, HOUSEHOLDSPAN ~ DIVISION, value.var = "prop_se")
colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
division.byhousespan <- cbind(tmp2, tmp3[,c(2:10)])
## by #2: householder
tmp <- housing_weighted %>% 
  filter(HHSPAN2 != "(Missing)" & DIVISION != "(Missing)") %>% 
  group_by(HHSPAN2, DIVISION) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHSPAN2 ~ DIVISION, value.var = "prop")
tmp3 <- dcast(tmp, HHSPAN2 ~ DIVISION, value.var = "prop_se")
colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
division.byhhspan <- cbind(tmp2, tmp3[,c(2:10)])

colnames(division.byhousespan)[1] <- colnames(division.byhhspan)[1] <- "Household race"
division.byspan <- rbind(division.byhousespan, division.byhhspan) # COL 2


## Median income -----------------------------------------------------------
# by #1: household
housing_weighted %>% 
  filter(HOUSEHOLDRACE != "(Missing)" & !(is.na(HINCP))) %>% 
  group_by(HOUSEHOLDRACE) %>%
  summarize(median_income = survey_median(HINCP)) -> inc.byhouserace 
housing_weighted %>% 
  filter(HOUSEHOLDSPAN != "(Missing)" & !(is.na(HINCP))) %>% 
  group_by(HOUSEHOLDSPAN) %>%
  summarize(median_income = survey_median(HINCP)) -> inc.byhousespan
# by #2: householder
housing_weighted %>% 
  filter(HHRACE3 != "(Missing)" & !(is.na(HINCP))) %>% 
  group_by(HHRACE3) %>%
  summarize(median_income = survey_median(HINCP)) -> inc.byhhrace
housing_weighted %>% 
  filter(HHSPAN2 != "(Missing)" & !(is.na(HINCP))) %>% 
  group_by(HHSPAN2) %>%
  summarize(median_income = survey_median(HINCP)) -> inc.byhhspan

colnames(inc.byhouserace)[1] <- colnames(inc.byhhrace)[1] <- "Household race"
inc.byrace <- rbind(inc.byhouserace, inc.byhhrace) # COL3
colnames(inc.byhousespan)[1] <- colnames(inc.byhhspan)[1] <- "Household span"
inc.byspan <- rbind(inc.byhousespan, inc.byhhspan) # COL3


## Income as % of poverty level --------------------------------------------
housing_weighted %>% 
  filter(HOUSEHOLDRACE != "(Missing)" & !(is.na(PERPOVLVL))) %>% 
  group_by(HOUSEHOLDRACE) %>%
  summarize(median_income = survey_median(PERPOVLVL)) -> pov.byhouserace
housing_weighted %>% 
  filter(HOUSEHOLDSPAN != "(Missing)" & !(is.na(PERPOVLVL))) %>% 
  group_by(HOUSEHOLDSPAN) %>%
  summarize(median_income = survey_median(PERPOVLVL)) -> pov.byhousespan
# by #2: householder
housing_weighted %>% 
  filter(HHRACE3 != "(Missing)" & !(is.na(PERPOVLVL))) %>% 
  group_by(HHRACE3) %>%
  summarize(median_income = survey_median(PERPOVLVL)) -> pov.byhhrace
housing_weighted %>% 
  filter(HHSPAN2 != "(Missing)" & !(is.na(PERPOVLVL))) %>% 
  group_by(HHSPAN2) %>%
  summarize(median_income = survey_median(PERPOVLVL)) -> pov.byhhspan
colnames(pov.byhouserace)[2:3] <- colnames(pov.byhousespan)[2:3] <- 
  colnames(pov.byhhrace)[2:3] <- colnames(pov.byhhspan)[2:3] <- c("pov_level", "pov_level_se")

colnames(pov.byhouserace)[1] <- colnames(pov.byhhrace)[1] <- "Household race"
pov.byrace <- rbind(pov.byhouserace, pov.byhhrace) # COL4

colnames(pov.byhousespan)[1] <- colnames(pov.byhhspan)[1] <- "Household span"
pov.byspan <- rbind(pov.byhousespan, pov.byhhspan) # COL4

## Tenure ------------------------------------------------------------------
# by #1: household
tmp <- housing_weighted %>% 
  filter(HOUSEHOLDRACE != "(Missing)" & TENURE != "(Missing)") %>% 
  group_by(HOUSEHOLDRACE, TENURE) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDRACE ~ TENURE, value.var = "prop")
# colnames(tmp2)[3:4] <- c("Owner", "Renter")
tmp3 <- dcast(tmp, HOUSEHOLDRACE ~ TENURE, value.var = "prop_se")
colnames(tmp3)[3:4] <- paste(colnames(tmp2)[3:4], "_se", sep = "")
tenure.byhouserace <- cbind(tmp2[c(1,3:4)], tmp3[c(3:4)])
# by #2: householder
tmp <- housing_weighted %>%
  filter(HHRACE3 != "(Missing)" & TENURE != "(Missing)") %>% 
  group_by(HHRACE3, TENURE) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHRACE3 ~ TENURE, value.var = "prop")
# colnames(tmp2)[3:4] <- c("Owner", "Renter")
tmp3 <- dcast(tmp, HHRACE3 ~ TENURE, value.var = "prop_se")
colnames(tmp3)[3:4] <- paste(colnames(tmp2)[3:4], "_se", sep = "")
tenure.byhhrace <- cbind(tmp2[c(1,3:4)], tmp3[c(3:4)])

colnames(tenure.byhouserace)[1] <- colnames(tenure.byhhrace)[1] <- "Household race"
tenure.byrace <- rbind(tenure.byhouserace, tenure.byhhrace) # COL5

# by span eth
## by #1: household
tmp <- housing_weighted %>% 
  filter(HOUSEHOLDSPAN != "(Missing)" & TENURE != "(Missing)") %>% 
  group_by(HOUSEHOLDSPAN, TENURE) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDSPAN ~ TENURE, value.var = "prop")
# colnames(tmp2)[3:4] <- c("Owner", "Renter")
tmp3 <- dcast(tmp, HOUSEHOLDSPAN ~ TENURE, value.var = "prop_se")
colnames(tmp3)[3:4] <- paste(colnames(tmp2)[3:4], "_se", sep = "")
tenure.byhousespan <- cbind(tmp2[c(1,3:4)], tmp3[c(3:4)])
## by #2: householder
tmp <- housing_weighted %>% 
  filter(HHSPAN2 != "(Missing)" & TENURE != "(Missing)") %>% 
  group_by(HHSPAN2, TENURE) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHSPAN2 ~ TENURE, value.var = "prop")
# colnames(tmp2)[3:4] <- c("Owner", "Renter")
tmp3 <- dcast(tmp, HHSPAN2 ~ TENURE, value.var = "prop_se")
colnames(tmp3)[3:4] <- paste(colnames(tmp2)[3:4], "_se", sep = "")
tenure.byhhspan <- cbind(tmp2[,c(1,3:4)], tmp3[,c(3:4)])

colnames(tenure.byhousespan)[1] <- colnames(tenure.byhhspan)[1] <- "Household span"
tenure.byspan <- rbind(tenure.byhousespan, tenure.byhhspan) # COL5

## Housing assistance ------------------------------------------------------

## HUDSUB
# by #1: household
tmp <- housing_weighted %>% 
  filter(HOUSEHOLDRACE != "(Missing)" & HUDSUB != "(Missing)") %>% 
  group_by(HOUSEHOLDRACE, HUDSUB) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDRACE ~ HUDSUB, value.var = "prop")
# colnames(tmp2)[2:4] <- c("Other renter", "Public housing", "Voucher recipient")
tmp3 <- dcast(tmp, HOUSEHOLDRACE ~ HUDSUB, value.var = "prop_se")
colnames(tmp3)[2:4] <- paste(colnames(tmp2)[2:4], "_se", sep = "")
hudsub.byhouserace <- cbind(tmp2[,c(1,2:4)], tmp3[,c(2:4)])
# by #2: householder
tmp <- housing_weighted %>% 
  filter(HHRACE3 != "(Missing)" & HUDSUB != "(Missing)") %>% 
  group_by(HHRACE3, HUDSUB) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHRACE3 ~ HUDSUB, value.var = "prop")
# colnames(tmp2)[2:4] <- c("Other renter", "Public housing", "Voucher recipient")
tmp3 <- dcast(tmp, HHRACE3 ~ HUDSUB, value.var = "prop_se")
colnames(tmp3)[2:4] <- paste(colnames(tmp2)[2:4], "_se", sep = "")
hudsub.byhhrace <- cbind(tmp2[,c(1,2:4)], tmp3[,c(2:4)])

colnames(hudsub.byhouserace)[1] <- colnames(hudsub.byhhrace)[1] <- "Household race"
hudsub.byrace <- rbind(hudsub.byhouserace, hudsub.byhhrace) # COL 6

# by span eth
## by #1: household
tmp <- housing_weighted %>% 
  filter(HOUSEHOLDSPAN != "(Missing)" & HUDSUB != "(Missing)") %>% 
  group_by(HOUSEHOLDSPAN, HUDSUB) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDSPAN ~ HUDSUB, value.var = "prop")
# colnames(tmp2)[2:4] <- c("Other renter", "Public housing", "Voucher recipient")
tmp3 <- dcast(tmp, HOUSEHOLDSPAN ~ HUDSUB, value.var = "prop_se")
colnames(tmp3)[2:4] <- paste(colnames(tmp2)[2:4], "_se", sep = "")
hudsub.byhousespan <- cbind(tmp2[,c(1,2:4)], tmp3[,c(2:4)])
# by #2: householder
tmp <- housing_weighted %>% 
  filter(HHSPAN2 != "(Missing)" & HUDSUB != "(Missing)") %>% 
  group_by(HHSPAN2, HUDSUB) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHSPAN2 ~ HUDSUB, value.var = "prop")
# colnames(tmp2)[2:4] <- c("Other renter", "Public housing", "Voucher recipient")
tmp3 <- dcast(tmp, HHSPAN2 ~ HUDSUB, value.var = "prop_se")
colnames(tmp3)[2:4] <- paste(colnames(tmp2)[2:4], "_se", sep = "")
hudsub.byhhspan <- cbind(tmp2[,c(1,2:4)], tmp3[,c(2:4)])

colnames(hudsub.byhousespan)[1] <- colnames(hudsub.byhhspan)[1] <- "Household span"
hudsub.byspan <- rbind(hudsub.byhousespan, hudsub.byhhspan) # COL 6

# RENTCNTRL
# by #1: household
tmp <- housing_weighted %>% 
  filter(HOUSEHOLDRACE != "(Missing)" & RENTCNTRL != "(Missing)") %>% 
  group_by(HOUSEHOLDRACE, RENTCNTRL) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDRACE ~ RENTCNTRL, value.var = "prop")
# colnames(tmp2)[2:3] <- c("No rent control", "Rent control")
tmp3 <- dcast(tmp, HOUSEHOLDRACE ~ RENTCNTRL, value.var = "prop_se")
colnames(tmp3)[2:3] <- paste(colnames(tmp2)[2:3], "_se", sep = "")
rentcntrl.byhouserace <- cbind(tmp2, tmp3[,c(2:3)])
# by #2: householder
tmp <- housing_weighted %>% 
  filter(HHRACE3 != "(Missing)" & RENTCNTRL != "(Missing)") %>% 
  group_by(HHRACE3, RENTCNTRL) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHRACE3 ~ RENTCNTRL, value.var = "prop")
# colnames(tmp2)[2:3] <- c("No rent control", "Rent control")
tmp3 <- dcast(tmp, HHRACE3 ~ RENTCNTRL, value.var = "prop_se")
colnames(tmp3)[2:3] <- paste(colnames(tmp2)[2:3], "_se", sep = "")
rentcntrl.byhhrace <- cbind(tmp2, tmp3[,c(2:3)])

colnames(rentcntrl.byhouserace)[1] <- colnames(rentcntrl.byhhrace)[1] <- "Household race"
rentcntrl.byrace <- rbind(rentcntrl.byhouserace, rentcntrl.byhhrace) # COL 7

# by span
## by #1: household
tmp <- housing_weighted %>% 
  filter(HOUSEHOLDSPAN != "(Missing)" & RENTCNTRL != "(Missing)") %>% 
  group_by(HOUSEHOLDSPAN, RENTCNTRL) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDSPAN ~ RENTCNTRL, value.var = "prop")
# colnames(tmp2)[2:3] <- c("No rent control", "Rent control")
tmp3 <- dcast(tmp, HOUSEHOLDSPAN ~ RENTCNTRL, value.var = "prop_se")
colnames(tmp3)[2:3] <- paste(colnames(tmp2)[2:3], "_se", sep = "")
rentcntrl.byhousespan <- cbind(tmp2, tmp3[,c(2:3)])
## by #2: householder
tmp <- housing_weighted %>% 
  filter(HHSPAN2 != "(Missing)" & RENTCNTRL != "(Missing)") %>% 
  group_by(HHSPAN2, RENTCNTRL) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHSPAN2 ~ RENTCNTRL, value.var = "prop")
# colnames(tmp2)[2:3] <- c("No rent control", "Rent control")
tmp3 <- dcast(tmp, HHSPAN2 ~ RENTCNTRL, value.var = "prop_se")
colnames(tmp3)[2:3] <- paste(colnames(tmp2)[2:3], "_se", sep = "")
rentcntrl.byhhspan <- cbind(tmp2, tmp3[,c(2:3)])

colnames(rentcntrl.byhousespan)[1] <- colnames(rentcntrl.byhhspan)[1] <- "Household span"
rentcntrl.byspan <- rbind(rentcntrl.byhousespan, rentcntrl.byhhspan) # COL 7

## Housing delinquency -----------------------------------------------------

## DBMISSRENT
# by #1: household
tmp <- housing_weighted %>%
  filter(HOUSEHOLDRACE != "(Missing)" & DBMISSRENT != "(Missing)") %>% 
  group_by(HOUSEHOLDRACE, DBMISSRENT) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDRACE ~ DBMISSRENT, value.var = "prop")
# colnames(tmp2)[2:3] <- c("Not missed rent", "Missed rent")
tmp3 <- dcast(tmp, HOUSEHOLDRACE ~ DBMISSRENT, value.var = "prop_se")
colnames(tmp3)[2:3] <- paste(colnames(tmp2)[2:3], "_se", sep = "")
missrent.byhouserace <- cbind(tmp2[,c(1,2:3)], tmp3[,c(2:3)])
# by #2: householder
tmp <- housing_weighted %>% 
  filter(HHRACE3 != "(Missing)" & DBMISSRENT != "(Missing)") %>% 
  group_by(HHRACE3, DBMISSRENT) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHRACE3 ~ DBMISSRENT, value.var = "prop")
# colnames(tmp2)[2:3] <- c("Not missed rent", "Missed rent")
tmp3 <- dcast(tmp, HHRACE3 ~ DBMISSRENT, value.var = "prop_se")
colnames(tmp3)[2:3] <- paste(colnames(tmp2)[2:3], "_se", sep = "")
missrent.byhhrace <- cbind(tmp2[,c(1,2:3)], tmp3[,c(2:3)])

colnames(missrent.byhouserace)[1] <- colnames(missrent.byhhrace)[1] <- "Household race"
missrent.byrace <- rbind(missrent.byhouserace, missrent.byhhrace) # COL 8

# by span
## by #1: household
tmp <- housing_weighted %>% 
  filter(HOUSEHOLDSPAN != "(Missing)" & DBMISSRENT != "(Missing)") %>% 
  group_by(HOUSEHOLDSPAN, DBMISSRENT) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HOUSEHOLDSPAN ~ DBMISSRENT, value.var = "prop")
# colnames(tmp2)[2:3] <- c("Not missed rent", "Missed rent")
tmp3 <- dcast(tmp, HOUSEHOLDSPAN ~ DBMISSRENT, value.var = "prop_se")
colnames(tmp3)[2:3] <- paste(colnames(tmp2)[2:3], "_se", sep = "")
missrent.byhousespan <- cbind(tmp2, tmp3[,c(2:3)])
## by #2: householder
tmp <- housing_weighted %>% 
  filter(HHSPAN2 != "(Missing)" & DBMISSRENT != "(Missing)") %>% 
  group_by(HHSPAN2, DBMISSRENT) %>% summarize(prop = survey_mean())
tmp2 <- dcast(tmp, HHSPAN2 ~ DBMISSRENT, value.var = "prop")
# colnames(tmp2)[2:3] <- c("Not missed rent", "Missed rent")
tmp3 <- dcast(tmp, HHSPAN2 ~ DBMISSRENT, value.var = "prop_se")
colnames(tmp3)[2:3] <- paste(colnames(tmp2)[2:3], "_se", sep = "")
missrent.byhhspan <- cbind(tmp2, tmp3[,c(2:3)])

colnames(missrent.byhousespan)[1] <- colnames(missrent.byhhspan)[1] <- "Household race"
missrent.byspan <- rbind(missrent.byhousespan, missrent.byhhspan) # COL 8

# Writing tables into Excel -----------------------------------------------

## by race
all_race_stats <- cbind(race_prop, inc.byrace[,-1], pov.byrace[,-1], tenure.byrace[,-1], 
                        hudsub.byrace[,-1], rentcntrl.byrace[,-1], missrent.byrace[,-1],
                        division.byrace[,-1])
write.csv(all_race_stats, "analysis-byrace.csv")

## by span eth
all_span_stats <- cbind(span_prop, inc.byspan[,-1], pov.byspan[,-1], tenure.byspan[,-1], 
                        hudsub.byspan[,-1], rentcntrl.byspan[,-1], missrent.byspan[,-1],
                        division.byspan[,-1])
write.csv(all_span_stats, "analysis-byspan.csv")