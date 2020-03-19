# Load relevant libraries
library(tidyverse)
library(forcats)
library(reshape2)
library(openxlsx)
library(rlang)
library(tidycensus)
library(tidyr)
library(BSDA)
library(dplyr)

library(survey)
library(srvyr)


# DATA --------------------------------------------------------------------


# Loading and cleaning data -----------------------------------------------

## set working directory
# setwd("~/blog_housing/analysis")

## Load data
housing <- read.csv("raw data/ahs2017_flat_r.csv")

## Clean data
# relabeling HH race and ethnicity levels
levels(housing$HHRACE3) <- c("HH AIAN", "HH Asian", "HH Black", "HH NHPI", "HH White")
levels(housing$HHSPAN2) <- c("HH Hispanic or Latinx", "HH Not Hispanic or Latinx")
levels(housing$RACEETH) <- c("HH AIAN", "HH Asian", "HH Black", "HH NHPI", "HH White NH")

# relabeling column variables
levels(housing$TENURE)[2:3] <- c("Owner", "Renter")
levels(housing$HUDSUB)[1:3] <- c("Other renter", "Public housing", "Housing voucher")
levels(housing$RENTCNTRL)[1:2] <- c("No rent control", "Rent control")
levels(housing$DBMISSRENT)[1:2] <- c("Not missed rent", "Missed rent")

# converting to characters
housing$HUDINCLIM_80_FLAG <- as.factor(as.character(housing$HUDINCLIM_80_FLAG))
housing$HUDINCLIM_50_FLAG <- as.factor(as.character(housing$HUDINCLIM_50_FLAG))
housing$HUDINCLIM_30_FLAG <- as.factor(as.character(housing$HUDINCLIM_30_FLAG))

## Creating combined tenure and federal subsidy variable

# FEDSUB4
## has 5 categories: Owner, Public housing, Housing voucher, Renter without assistance 
housing <- housing %>% 
  mutate(FEDSUB4 = 
           ifelse(TENURE == "Owner", "Owner",
                  ifelse((TENURE == "Occupied without payment of rent" |
                            TENURE == "Renter") & 
                           !(HUDSUB %in% c("Public housing", "Housing voucher")), 
                         "Renter without assistance", levels(HUDSUB)[HUDSUB])))

housing$FEDSUB4 <- as.factor(housing$FEDSUB4)
# levels(housing$FEDSUB4)
# summary(housing$FEDSUB4)

# Creating combined citizenship and education variables
## HHCITSHP3
housing <- housing %>%
  mutate(HHCITSHP3 = ifelse(HHCITSHP %in% c("Native, born in US", 
                                             "Native, born in Puerto Rico or US outlying area",
                                             "Native born abroad of US parent(s)"),
                            "US citizen, native", 
                            ifelse(HHCITSHP == "Foreign born, US citizen by naturalization", 
                                   "US citizen, naturalized", 
                                   ifelse(HHCITSHP == "Foreign born, not a US citizen", 
                                          "Not US citizen", NA))))
housing$HHCITSHP3 <- as.factor(housing$HHCITSHP3)

# HHGRAD4
# levels(housing$HHGRAD4)
housing$HHGRAD4 <- as.numeric(gsub("'", '', housing$HHGRAD))
housing <- housing %>% 
  mutate(HHGRAD4 = 
           ifelse(31 <= HHGRAD4 & HHGRAD4 <= 38, 
                  "1_< HS",
                  ifelse(39 <= HHGRAD4 & HHGRAD4 <= 40, 
                         "2_HS diploma or equivalent",
                         ifelse(41 <= HHGRAD4 & HHGRAD4 <= 43, 
                                "3_Associate degree or vocational",
                                ifelse(44 <= HHGRAD4 & HHGRAD4 <= 47, 
                                       "4_Bachelor's degree or higher", NA)))))
housing$HHGRAD4 <- as.factor(housing$HHGRAD4)

# HHCITSHP2
housing <- housing %>% 
  mutate(HHCITSHP2 = ifelse(HHCITSHP3 %in% c("US citizen, native", 
                                             "US citizen, naturalized"),
                            "US citizen", 
                            ifelse(HHCITSHP3 == "Not US citizen", 
                                   "Not US citizen", NA)))
housing$HHCITSHP2 <- as.factor(housing$HHCITSHP2)


# HOUSEHOLDCITSHP (PERSON CITSHP)
var.citshp <- colnames(housing)[grepl("^CITSHP\\d+$", names(housing))]

# At least one person in the household is a citizen (1=yes, 0=no)
housing$HOUSEHOLDCITSHP_I <- 
  ifelse(apply(housing[var.citshp], 1, 
        function(x) any(x %in% c("'1'", "'2'", "'3'", "'4'"))), 1, 0)

housing <- housing %>% 
  mutate(HOUSEHOLDCITSHP = 
           ifelse(HHCITSHP2 == "US citizen", "Householder is citizen",
                  ifelse(HHCITSHP2 == "Not US citizen" & HOUSEHOLDCITSHP_I == 1,
                         "At least 1 citizen, but not householder",
                         "No citizens in household")))
housing$HOUSEHOLDCITSHP <- as.factor(housing$HOUSEHOLDCITSHP)

# All persons in the household are citizens (1=yes, 0=no)
housing$HOUSEHOLDCITSHP2_I <- 
  ifelse(apply(housing[var.citshp], 1, 
               function(x) all(!(x == "'5'"))), 1, 0)

# All, at least one (but not all), none
housing <- housing %>% 
  mutate(HOUSEHOLDCITSHP2 = 
           ifelse(HOUSEHOLDCITSHP2_I == 1, "All members are citizens",
                  ifelse(HOUSEHOLDCITSHP2_I == 0 & HOUSEHOLDCITSHP_I == 1,
                         "At least 1 citizen (but not all members)",
                         "No citizens")))
housing$HOUSEHOLDCITSHP2 <- as.factor(housing$HOUSEHOLDCITSHP2)

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
                                      "HINCP", "FINCP", "TOTHCAMT", "WEIGHT",
                                      var.citshp, "HOUSEHOLDCITSHP_I",
                                      "HOUSEHOLDCITSHP2_I"))]

housing[,col_factor] <- lapply(housing[,col_factor], 
                               function(x) fct_explicit_na(x)) %>% as.data.frame



# Weighting data ----------------------------------------------------------
housing_weighted <- housing %>% as_survey_design(ids = 1, weight = WEIGHT)



# Creating family income limits ----------------------------

## Defining HUD (ACS) Median Family Income
# acs.mfi <- 68000
# acs.mfi.80 <- 0.80 * acs.mfi
# acs.mfi.50 <- 0.50 * acs.mfi
# acs.mfi.30 <- 0.30 * acs.mfi
# 
# housing <- housing %>% mutate(ACSMFI_80_FLAG = as.factor(ifelse(FINCP < acs.mfi.80, 1, 0)),
#                               ACSMFI_50_FLAG = as.factor(ifelse(FINCP < acs.mfi.50, 1, 0)),
#                               ACSMFI_30_FLAG = as.factor(ifelse(FINCP < acs.mfi.30, 1, 0)))

# ## Defining ACS Regional Median Family Incomes
# sample-info
# finc.byrace <- apply_by_defs_two(housing_weighted, "twoway_median",
#                                  race.label, race.defs, "FINCP")
# finc.byspan <- apply_by_defs_two(housing_weighted, "twoway_median",
#                                  span.label, span.defs, "FINCP")

# reading in ACS median family income tables by geography
census_api_key("11654cc6c6ee4ff4791e54461c9b48da31c5ff68", install = TRUE, overwrite = TRUE)
fincome_avg_division <- get_acs(table = "B19113", year = 2017, survey = "acs1",
                                cache_table = TRUE,
                                geography = "division")

# WHEN THE CENSUS API ISN'T WORKING ON YOUR WIFI
# fincome_avg_division <- data.frame("NAME" = c("New England Division", "Middle Atlantic Division",
#                                               "East North Central Division", "West North Central Division",
#                                               "South Atlantic Division", "East South Central Division",
#                                               "West South Central Division", "Mountain Division",
#                                               "Pacific Division"), 
#                                    "estimate" = c(91189, 81703, 72215, 75430, 70430, 
#                                                   61255, 66541, 72080, 81910))
fincome_avg_division$NAME <- as.character(fincome_avg_division$NAME)
fincome_avg_division <- 
  fincome_avg_division %>% 
  select(-c(GEOID, variable, moe)) %>% 
  mutate(DIVISION = substr(fincome_avg_division$NAME, 1,
                           nchar(fincome_avg_division$NAME)-9),
         inclim_30 = 0.30 * estimate,
         inclim_50 = 0.50 * estimate,
         inclim_80 = 0.80 * estimate)
# merging housing data with ACS median family income data
housing <- merge(housing, fincome_avg_division[,c("DIVISION", "inclim_80", 
                                                  "inclim_50", "inclim_30")],
                 by = "DIVISION", all.x = TRUE)
housing <- housing %>% mutate(ACSREG_80_FLAG = as.factor(ifelse(FINCP < inclim_80, 1, 0)),
                              ACSREG_50_FLAG = as.factor(ifelse(FINCP < inclim_50, 1, 0)),
                              ACSREG_30_FLAG = as.factor(ifelse(FINCP < inclim_30, 1, 0)))
# 
# # comparing HUD federal level income limits with regional income limits (ACS data)
# ## most lenient standard
summary(housing$ACSMFI_80_FLAG)
summary(housing$ACSREG_80_FLAG)
# 
# ## strictest standard
summary(housing$ACSMFI_30_FLAG)
summary(housing$ACSREG_30_FLAG)


# Recleaning and redefining data with new variables ----------------------------
var.flag <- colnames(housing)[grepl("FLAG$", names(housing))]

col_factor <- colnames(housing)[!(colnames(housing) %in% 
                                    c(var.races, var.span, var.num, var.rating,
                                      var.flag, var.pov, "X.1", "X", "HHAGE", 
                                      "HINCP", "FINCP", "TOTHCAMT", "WEIGHT",
                                      var.citshp, "HOUSEHOLDCITSHP_I",
                                      "HOUSEHOLDCITSHP2_I", "estimate", 
                                      "inclim_80", "inclim_50", "inclim_30"))]

housing[,col_factor] <- lapply(housing[,col_factor], 
                               function(x) fct_explicit_na(x)) %>% as.data.frame

# if error, check which columsn not factor
## sapply(housing[,col_factor], class)

# Weighting data 
housing_weighted <- housing %>% as_survey_design(ids = 1, weight = WEIGHT)


# Setup and definitions ---------------------------------------------------

## Setting up Excel workbooks
excelfile_graph1 <- createWorkbook()
excelfile_graph2 <- createWorkbook()
# excelfile_graph2b <- createWorkbook()
excelfile_graph3 <- createWorkbook()
excelfile_graph4 <- createWorkbook()
excelfile_graph5 <- createWorkbook()


## Race definitions and labels
race.defs <- c("HOUSEHOLDRACE", "HHRACE3")
race.label <- "Household race"

span.defs <- c("HOUSEHOLDSPAN", "HHSPAN2")
span.label <- "Household span"

raceeth.defs <- c("HOUSEHOLDRACEETH", "RACEETH")
raceeth.label <- "Household raceeth"

graph.race.lab <- c(AIAN = "AIAN alone", Asian = "Asian alone", 
                    Black = "Black or African American alone", 
                    NHPI = "NHPI alone", White = "White alone (Non-Hispanic)", 
                    "(Missing)")
graph.span.lab <- c(`Hispanic or Latinx` = "Hispanic of any race", 
                    `Not Hispanic or Latinx` = "Not Hispanic of any race", 
                    "(Missing)")
graph.lab <- c(graph.race.lab, graph.span.lab)

# Select race variable ## EDIT
race.var <- race.defs[1]
span.var <- span.defs[1]

# setting CI level
tstat <- 1.96

# Defining functions ------------------------------------------------------

# generates table of weighted totals by a grouping variable `group_var`
totals_by_variable <- function(df, group_var) {
  group_var <- sym(group_var)
  df %>% filter((!!group_var) != "(Missing)") %>% 
    group_by(!!group_var) %>% summarize(n = survey_total()) -> tmp
  return (tmp)
}

# generates table of weighted proportions by a grouping variable `group_var`
prop_by_variable <- function(df, group_var) {
  group_var <- sym(group_var)
  df %>% filter((!!group_var) != "(Missing)") %>% 
    group_by(!!group_var) %>% summarize(race_prop = survey_mean()) -> tmp
  return (tmp)
}

# generates two way weighted proportion tables by grouping variables 
# `group_var1` and `groupvar2,` with % out of group_var1
twoway_prop <- function(df, group_var1, group_var2) {
  sym.group_var1 <- sym(group_var1)
  sym.group_var2 <- sym(group_var2)
  df %>% filter(((!!sym.group_var1) != "(Missing)" & 
                   !(is.na((!!sym.group_var1)))) & 
                  ((!!sym.group_var2) != "(Missing)" & 
                     !(is.na((!!sym.group_var2))))) %>% 
    group_by((!!sym.group_var1), (!!sym.group_var2)) %>% 
    summarize(prop = survey_mean(),
              n = survey_total()) -> tmp
  tmp <- 
    tmp %>% mutate(CI_lb = prop - tstat * prop_se,
                   CI_ub = prop + tstat * prop_se)
  tmp2 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "prop")
  tmp3 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "prop_se")
  colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
  tmp4 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "CI_lb")
  colnames(tmp4) <- paste(colnames(tmp2), "_CI_lb", sep = "")
  tmp5 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "CI_ub")
  colnames(tmp5) <- paste(colnames(tmp2), "_CI_ub", sep = "")
  tmp6 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "n")
  colnames(tmp6) <- paste(colnames(tmp2), "_n", sep = "")
  colnames(tmp2)[1] <- colnames(tmp3)[1] <- colnames(tmp4)[1] <-
    colnames(tmp5)[1] <- colnames(tmp6)[1] <- "group"
  n.col <- ncol(tmp2)
  final <- Reduce(function(x, y) merge(x, y, by = "group"), 
                  list(tmp2, tmp4, tmp3, tmp5, tmp6))
  return (final)  
}

# generates three way weighted proportion contingency tables by grouping variables 
# `group_var1`, `groupvar2`, and `key`, with % out of group_var1 and group_var2
threeway_prop <- function(df, group_var1, group_var2, key) {
  sym.group_var1 <- sym(group_var1)
  sym.group_var2 <- sym(group_var2)
  sym.key <- sym(key)
  
  df %>% filter(((!!sym.group_var1) != "(Missing)" & 
                   !(is.na((!!sym.group_var1)))) & 
                  ((!!sym.group_var2) != "(Missing)" & 
                     !(is.na((!!sym.group_var2)))) & 
                  ((!!sym.key) != "(Missing)" & 
                     !(is.na((!!key))))) %>% 
    group_by((!!sym.group_var1), (!!sym.group_var2), (!!sym.key)) %>% 
    summarize(prop = survey_mean(),
              n = survey_total()) -> tmp
  tmp <- 
    tmp %>% mutate(CI_lb = prop - tstat * prop_se,
                   CI_ub = prop + tstat * prop_se)
  # tmp_sub <- c(parse(text = group_var1), parse(text = group_var2), parse(text = key))
  # tmp2 <- spread(tmp[,c(eval(tmp_sub), "prop")], key, 
  tmp2 <- spread(tmp[,c(group_var1, group_var2, key, "prop")], key, prop)
  tmp3 <- spread(tmp[,c(group_var1, group_var2, key, "prop_se")], key, prop_se)
  colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
  tmp4 <- spread(tmp[,c(group_var1, group_var2, key, "CI_lb")], key, CI_lb)
  colnames(tmp4) <- paste(colnames(tmp2), "_CI_lb", sep = "")
  tmp5 <- spread(tmp[,c(group_var1, group_var2, key, "CI_ub")], key, CI_ub)
  colnames(tmp5) <- paste(colnames(tmp2), "_CI_ub", sep = "")
  tmp6 <- spread(tmp[,c(group_var1, group_var2, key, "n")], key, n)
  colnames(tmp6) <- paste(colnames(tmp2), "_n", sep = "")
  colnames(tmp2)[1] <- colnames(tmp3)[1] <- colnames(tmp4)[1] <-
    colnames(tmp5)[1] <- colnames(tmp6)[1] <- "group_1"
  colnames(tmp2)[2] <- colnames(tmp3)[2] <- colnames(tmp4)[2] <-
    colnames(tmp5)[2] <- colnames(tmp6)[2] <- "group_2"
  n.col <- ncol(tmp2)
  final <- Reduce(function(x, y) merge(x, y, by = c("group_1", "group_2")), 
                  list(tmp2, tmp4, tmp3, tmp5, tmp6))
  return (final)  
}

# generates median value of `med_var` for each level of `group_var`
twoway_median <- function(df, group_var, med_var) {
  group_var <- sym(group_var)
  med_var <- sym(med_var)
  df %>% filter(((!!group_var) != "(Missing)" & 
                   !(is.na((!!group_var)))) & 
                  ((!!med_var) != "(Missing)" & 
                     !(is.na((!!med_var))))) %>% 
    group_by((!!group_var)) %>% 
    summarize(median_income = survey_median((!!med_var))) -> tmp
  return (tmp)  
}

# applies function `fun.name` to demographic grouping variables `def_var` when only
# grouping by one variable at a time
apply_by_defs_one <- function(df, fun.name, dem.name, def.var) {
  n <- length(def.var)
  ls <- list()
  FUN <- match.fun(fun.name)
  for (i in 1:n) {
    by_def <- FUN(df, def.var[i])
    colnames(by_def)[1] <- dem.name
    ls[[i]] <- by_def
  }
  tmp <- do.call("rbind", ls)
  return(tmp)
}

# applies function `fun.name` to demographic grouping variables `def_var` when only
# grouping by one variable at a time
apply_by_defs_two <- function(df, fun.name, dem.name, def.var, group.var2) {
  n <- length(def.var)
  ls <- list()
  FUN <- match.fun(fun.name)
  for (i in 1:n) {
    by_def <- FUN(df, def.var[i], group.var2)
    colnames(by_def)[1] <- dem.name
    ls[[i]] <- by_def
  }
  tmp <- do.call("rbind", ls)
  return(tmp)
}

# applies function `fun.name` to demographic grouping variables `def_var` when only
# grouping by one variable at a time with a given criteria `criteria`
twoway_prop_criteria <- function(df, group_var1, group_var2, criteria) {
  sym.group_var1 <- sym(group_var1)
  sym.group_var2 <- sym(group_var2)
  df %>% filter(eval(parse(text=criteria)) & 
                  ((!!sym.group_var1) != "(Missing)" & 
                     !(is.na((!!sym.group_var1)))) & 
                  ((!!sym.group_var2) != "(Missing)" & 
                     !(is.na((!!sym.group_var2))))) %>% 
    group_by((!!sym.group_var1), (!!sym.group_var2)) %>% 
    summarize(prop = survey_mean(),
              n = survey_total()) -> tmp
  tmp <- 
    tmp %>% mutate(CI_lb = prop - tstat * prop_se,
                   CI_ub = prop + tstat * prop_se)
  tmp2 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "prop")
  tmp3 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "prop_se")
  colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
  tmp4 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "CI_lb")
  colnames(tmp4) <- paste(colnames(tmp2), "_CI_lb", sep = "")
  tmp5 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "CI_ub")
  colnames(tmp5) <- paste(colnames(tmp2), "_CI_ub", sep = "")
  tmp6 <- reshape2::dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "n")
  colnames(tmp6) <- paste(colnames(tmp2), "_n", sep = "")
  colnames(tmp2)[1] <- colnames(tmp3)[1] <- colnames(tmp4)[1] <-
    colnames(tmp5)[1] <- colnames(tmp6)[1] <- "group"
  n.col <- ncol(tmp2)
  final <- Reduce(function(x, y) merge(x, y, by = "group"), 
                  list(tmp2, tmp4, tmp3, tmp5, tmp6))
  return (final)
}

# generates three way weighted proportion contingency tables by grouping variables 
# `group_var1`, `groupvar2`, and `key`, with % out of group_var1 and group_var2
# by criteria
threeway_prop_criteria <- function(df, group_var1, group_var2, key, criteria) {
  sym.group_var1 <- sym(group_var1)
  sym.group_var2 <- sym(group_var2)
  sym.key <- sym(key)
  
  df %>% filter(eval(parse(text=criteria)) & 
                  ((!!sym.group_var1) != "(Missing)" & 
                     !(is.na((!!sym.group_var1)))) & 
                  ((!!sym.group_var2) != "(Missing)" & 
                     !(is.na((!!sym.group_var2)))) & 
                  ((!!sym.key) != "(Missing)" & 
                     !(is.na((!!key))))) %>% 
    group_by((!!sym.group_var1), (!!sym.group_var2), (!!sym.key)) %>% 
    summarize(prop = survey_mean(),
              n = survey_total()) -> tmp
  tmp <- 
    tmp %>% mutate(CI_lb = prop - tstat * prop_se,
                   CI_ub = prop + tstat * prop_se)
  # tmp_sub <- c(parse(text = group_var1), parse(text = group_var2), parse(text = key))
  # tmp2 <- spread(tmp[,c(eval(tmp_sub), "prop")], key, 
  tmp2 <- spread(tmp[,c(group_var1, group_var2, key, "prop")], key, prop)
  tmp3 <- spread(tmp[,c(group_var1, group_var2, key, "prop_se")], key, prop_se)
  colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
  tmp4 <- spread(tmp[,c(group_var1, group_var2, key, "CI_lb")], key, CI_lb)
  colnames(tmp4) <- paste(colnames(tmp2), "_CI_lb", sep = "")
  tmp5 <- spread(tmp[,c(group_var1, group_var2, key, "CI_ub")], key, CI_ub)
  colnames(tmp5) <- paste(colnames(tmp2), "_CI_ub", sep = "")
  tmp6 <- spread(tmp[,c(group_var1, group_var2, key, "n")], key, n)
  colnames(tmp6) <- paste(colnames(tmp2), "_n", sep = "")
  colnames(tmp2)[1] <- colnames(tmp3)[1] <- colnames(tmp4)[1] <-
    colnames(tmp5)[1] <- colnames(tmp6)[1] <- "group_1"
  colnames(tmp2)[2] <- colnames(tmp3)[2] <- colnames(tmp4)[2] <-
    colnames(tmp5)[2] <- colnames(tmp6)[2] <- "group_2"
  n.col <- ncol(tmp2)
  final <- Reduce(function(x, y) merge(x, y, by = c("group_1", "group_2")), 
                  list(tmp2, tmp4, tmp3, tmp5, tmp6))
  return (final)  
}

# applies function `fun.name` to demographic grouping variables `def_var` when only
# grouping by one variable at a time with specific criteria `criteria`
apply_by_defs_two_criteria <- function(df, fun.name, dem.name, def.var, 
                                       group.var2, criteria) {
  n <- length(def.var)
  ls <- list()
  FUN <- match.fun(fun.name)
  for (i in 1:n) {
    by_def <- FUN(df, def.var[i], group.var2, criteria)
    colnames(by_def)[1] <- dem.name
    ls[[i]] <- by_def
  }
  tmp <- do.call("rbind", ls)
  return(tmp)
}

# GRAPHS ------------------------------------------------------------------

# GRAPH 1: National income limits, by race -----------------------------------------

## DEF: HUD Median Family Income (ACS)
### Note: The race of the household is defined by the race of all members of the household
### The MFI is defined by the HUD ACS estimates, defined here:
### https://www.huduser.gov/portal/datasets/il/il17/Medians2017.pdf

## if want just one of the HH race definitions (householder vs. entire household)
# twoway_prop(housing_weighted, "HHRACE3", "HUDINCLIM_80_FLAG")

## 80% income limit
prop.acs80.byrace <- twoway_prop(housing_weighted, race.var, 
                                 "ACSREG_80_FLAG")
prop.acs80.byspan <- twoway_prop(housing_weighted, span.var,
                                 "ACSREG_80_FLAG")
prop.acs80.byrace$key.0 <- prop.acs80.byspan$key.0 <- "MFI80"

## 50% income limit
prop.acs50.byrace <- twoway_prop(housing_weighted, race.var,
                                 "ACSREG_50_FLAG")
prop.acs50.byspan <- twoway_prop(housing_weighted, span.var, 
                                          "ACSREG_50_FLAG")
prop.acs50.byrace$key.0 <- prop.acs50.byspan$key.0 <- "MFI50"

## 30% income limit
prop.acs30.byrace <- twoway_prop(housing_weighted, race.var,
                                 "ACSREG_30_FLAG")
prop.acs30.byspan <- twoway_prop(housing_weighted, span.var, 
                                          "ACSREG_30_FLAG")
prop.acs30.byrace$key.0 <- prop.acs30.byspan$key.0 <- "MFI30"

## relabeling and combining
# colnames(prop.acs80.byrace)[1] <- colnames(prop.acs30.byrace)[1] <- 
#   colnames(prop.acs50.byrace)[1] <- colnames(prop.acs50.byspan)[1] <-
#   colnames(prop.acs80.byspan)[1] <- colnames(prop.acs30.byspan)[1] <- "group"

levels(prop.acs80.byrace$group) <- 
  levels(prop.acs50.byrace$group) <-
  levels(prop.acs30.byrace$group) <- graph.race.lab

levels(prop.acs80.byspan$group) <- 
  levels(prop.acs50.byspan$group) <- 
  levels(prop.acs30.byspan$group) <- graph.span.lab

# combine into one table
## no CI, only estimate
info.acs.byhh_long <- 
  rbind(prop.acs80.byrace[,c("group", "1", "key.0")],
        prop.acs80.byspan[which(prop.acs80.byspan$group == "Hispanic of any race"),
                                   c("group", "1", "key.0")],
        prop.acs50.byrace[,c("group", "1", "key.0")],
        prop.acs50.byspan[which(prop.acs50.byspan$group == "Hispanic of any race"),
                                   c("group", "1", "key.0")],
        prop.acs30.byrace[,c("group", "1", "key.0")],
        prop.acs30.byspan[which(prop.acs30.byspan$group == "Hispanic of any race"),
                                   c("group", "1", "key.0")])
info.acs.byhh_long <- info.acs.byhh_long %>% na.omit()

## produce table `info.acs.byhh` for estimates only
colnames(info.acs.byhh_long)[2] <- "estimate"
info.acs.byhh_wide <- spread(info.acs.byhh_long, key.0, estimate)
info.acs.byhh <- info.acs.byhh_wide %>% 
  mutate("<30%" = MFI30,
         "30-50%" = MFI50 - MFI30,
         "50-80%" = MFI80 - MFI50)
info.acs.byhh <- info.acs.byhh %>% 
  select(group, `<30%`, `30-50%`, `50-80%`)

## turning into long dataset
# info.acs.byhh_long_2 <- gather(info.acs.byhh, key, estimate, `<30%`:`50-80%`)
## setting `key_order`
# info.acs.byhh_long <- info.acs.byhh_long[order(info.acs.byhh_long$estimate),]
# info.acs.byhh_long$key_order <- nrow(info.acs.byhh_long):1

# SHEET 2: with CIs
## produce table `info.acs.byhh.CI` for CI and estimates
## including CI
info.acs.byhh_long_CI <- 
  rbind(prop.acs80.byrace[,c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs80.byspan[which(prop.acs80.byspan$group == "Hispanic of any race"),
                                         c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs50.byrace[,c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs50.byspan[which(prop.acs50.byspan$group == "Hispanic of any race"),
                                   c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs30.byrace[,c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs30.byspan[which(prop.acs30.byspan$group == "Hispanic of any race"),
                                   c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")])
#info.acs.byhh_long_CI <- info.acs.byhh_long_CI %>% na.omit()
colnames(info.acs.byhh_long_CI)[2] <- "estimate"

# calculating CIs for income limit brackets
info.acs.byhh_CI_lb <- 
  spread(info.acs.byhh_long_CI[,c("group","1_CI_lb","key.0")], key.0, "1_CI_lb")
# %>% mutate("30%" = MFI30,
  #        "50%" = MFI50,
  #        "80%" = MFI80) %>% 
  # select(group, `30%`, `50%`, `80%`)
info.acs.byhh_CI_ub <- 
  spread(info.acs.byhh_long_CI[,c("group","1_CI_ub","key.0")], key.0, "1_CI_ub")
# %>% mutate("30%" = MFI30,
  #        "50%" = MFI50,
  #        "80%" = MFI80) %>% 
  # select(group, `30%`, `50%`, `80%`)

info.income.CI_lb <- gather(info.acs.byhh_CI_lb, key, CI_lb, `MFI30`:`MFI80`, factor_key = TRUE)
info.income.CI_ub <- gather(info.acs.byhh_CI_ub, key, CI_ub, `MFI30`:`MFI80`, factor_key = TRUE)

colnames(info.acs.byhh_long)[2:3] <- c("estimate", "key")
# merge estimates, CI_lb, CI_ub
info.income.CI <- 
  Reduce(function(x, y) merge(x, y, by = c("group", "key")), 
       list(info.acs.byhh_long, info.income.CI_lb, info.income.CI_ub))
info.income.CI <- 
  info.income.CI[order(info.income.CI$key, info.income.CI$estimate),]

# Write to Excel sheet
addWorksheet(wb = excelfile_graph1, sheetName = "by-regmfi-acs", gridLines = TRUE)
writeData(wb = excelfile_graph1, sheet = "by-regmfi-acs", 
          x = info.acs.byhh, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph1, sheetName = "CI-by-regmfi-acs", gridLines = TRUE)
writeData(wb = excelfile_graph1, sheet = "CI-by-regmfi-acs", 
          x = info.income.CI, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph1, "csv files/graphs_final/graph-1-reg.xlsx",  overwrite = TRUE)

# GRAPH 2: How many receiving support? ------------------------------------

# Defining functions

twoway_fedsub_table <- function(group_var, sub_var) {
  prop.full <- twoway_prop(housing_weighted, group_var, sub_var)
  nSub <- length(levels(housing[[sub_var]])[levels(housing[[sub_var]]) != "(Missing)"])
  prop.sub <- prop.full[,1:(nSub+1)]
  
  if(group_var %in% race.defs) {
    index <- which(group_var %in% race.defs)
    prop.span <- twoway_prop(housing_weighted, span.defs[index], sub_var)
    
    ## relabeling and combining
    levels(prop.full$group) <- levels(prop.sub$group) <- graph.race.lab
    levels(prop.span$group) <- graph.span.lab
    
    # combine into one table
    prop.full <- rbind(prop.full, prop.span[1,])
    prop.sub <- rbind(prop.sub[,1:(nSub+1)], prop.span[1,1:(nSub+1)])
  }
  
  ## SHEET 1: estimate
  info.long <- gather(prop.sub, key,
                      estimate, 2:(nSub+1), factor_key = TRUE)
  
  # setting `key_order`
  info.long <- 
    info.long[order(info.long$key,
                    info.long$estimate),]
  n.levels <- ifelse(group_var %in% race.defs, 
                     nlevels(housing[[group_var]]),
                     nlevels(housing[[group_var]]) - 1)
  info.long$key_order <- n.levels:1
  
  ## SHEET 2: with CIs
  col.CIlb <- names(prop.full)[grep("CI_lb", names(prop.full))]
  col.CIub <- names(prop.full)[grep("CI_ub", names(prop.full))]
  info.CI.lb <- 
    prop.full %>% select(group, col.CIlb) %>% 
    gather(., key, CI_lb, col.CIlb, factor_key = TRUE)
  levels(info.CI.lb$key) <- 
    substr(levels(info.CI.lb$key), 1, 
           nchar(levels(info.CI.lb$key))-nchar("_CI_lb"))
  info.CI.ub <- 
    prop.full %>% select(group, col.CIub) %>% 
    gather(., key, CI_ub, col.CIub, factor_key = TRUE)
  levels(info.CI.ub$key) <- 
    substr(levels(info.CI.ub$key),
           1, nchar(levels(info.CI.ub$key))-nchar("_CI_ub"))
  
  info.CI <- 
    Reduce(function(x, y) merge(x, y, by = c("group", "key")), 
           list(info.long[,1:4], info.CI.lb, info.CI.ub)) %>% 
    mutate(MOE_prop = (0.5*(CI_ub-CI_lb)))
  info.CI.order <- 
    info.CI[order(info.CI$key, info.CI$estimate),]
  
  sheetName1 <- paste0("by", group_var)
  sheetName2 <- paste0("CI-by", group_var)
  
  # x <- list(info.long, info.CI.order)
  # return(x)
  # Write to Excel sheet
  addWorksheet(wb = excelfile_graph2, sheetName = sheetName1, gridLines = TRUE)
  writeData(wb = excelfile_graph2, sheet = sheetName1,
            x = info.long, startCol = 1, startRow = 1)
  addWorksheet(wb = excelfile_graph2, sheetName = sheetName2, gridLines = TRUE)
  writeData(wb = excelfile_graph2, sheet = sheetName2,
            x = info.CI.order, startCol = 1, startRow = 1)
  # 
  # print(head(info.long))
  # print(info.long[info.long$key == "Public housing",])
  # print(head(info.CI.order))
  # 
  # # Write only PH to Excel sheet
  # addWorksheet(wb = excelfile_graph3, sheetName = sheetName1, gridLines = TRUE)
  # writeData(wb = excelfile_graph3, sheet = sheetName1,
  #           x = info.long[info.long$key == "Public housing",], startCol = 1, startRow = 1)
  # addWorksheet(wb = excelfile_graph3, sheetName = sheetName2, gridLines = TRUE)
  # writeData(wb = excelfile_graph3, sheet = sheetName2,
  #           x = info.CI.order[info.CI.order$key == "Public housing",], startCol = 1, startRow = 1)
}

threeway_fedsub_table <- function(group_var1, group_var2, sub_var) {
  prop.full <- threeway_prop(housing_weighted, group_var1, group_var2, sub_var)
  nSub <- length(levels(housing[[group_var1]])[levels(housing[[group_var1]]) != "(Missing)"])
  nRow <- length(levels(housing[[group_var2]])[levels(housing[[group_var2]]) != "(Missing)"])
  prop.sub <- prop.full[,1:(nSub+2)]
  
  if(group_var1 %in% race.defs) {
    index <- which(group_var1 %in% race.defs)
    prop.span <- threeway_prop(housing_weighted, span.defs[index], 
                               group_var2, sub_var)
    
    ## relabeling and combining
    levels(prop.full$group_1) <- levels(prop.sub$group_1) <- graph.race.lab
    levels(prop.span$group_1) <- graph.span.lab
    
    # combine into one table
    prop.full <- rbind(prop.full, prop.span[1:nRow,])
    prop.sub <- rbind(prop.sub[,1:(nSub+2)], prop.span[1:nRow,1:(nSub+2)])
    
  }
  
  ## SHEET 1: estimate
  info.long <- gather(prop.sub, key,
                      estimate, 3:(nSub+2), factor_key = TRUE)
  
  # setting `key_order`
  info.long <-
    info.long[order(info.long$key,
                    info.long$estimate),]
  
  n.levels <- ifelse(group_var1 %in% race.defs, 
                     nlevels(housing[[group_var1]]) * nRow,
                     (nlevels(housing[[group_var1]]) - 1) * nRow)
  info.long$key_order <- n.levels:1
  
  ## SHEET 2: with CIs
  info.CI.lb <- 
    prop.full %>%
    select(group_1, group_2, 
           colnames(prop.full)[nSub+3]:colnames(prop.full)[2*nSub+2]) %>% 
    gather(., key, CI_lb, colnames(prop.full)[nSub+3]:colnames(prop.full)[2*nSub+2],
           factor_key = TRUE)
  levels(info.CI.lb$key) <- 
    substr(levels(info.CI.lb$key), 1,
           nchar(levels(info.CI.lb$key))-nchar("_CI_lb"))
  info.CI.ub <-
    prop.full %>%
    select(group_1, group_2,
           colnames(prop.full)[3*nSub+3]:colnames(prop.full)[4*nSub+2]) %>% 
    gather(., key, CI_ub, colnames(prop.full)[3*nSub+3]:colnames(prop.full)[4*nSub+2], 
           factor_key = TRUE)
  levels(info.CI.ub$key) <-
    substr(levels(info.CI.ub$key),
           1, nchar(levels(info.CI.ub$key))-nchar("_CI_ub"))
  
  info.CI <-
    Reduce(function(x, y)
      merge(x, y, by = c("group_1", "group_2", "key")),
      list(info.long[,1:5], info.CI.lb, info.CI.ub)) %>% 
    mutate(MOE_prop = (0.5*(CI_ub-CI_lb)))
  info.CI.order <- 
    info.CI[order(info.CI$key, info.CI$estimate),]
  
  # x <- list(info.long, info.CI.order)
  # return(x)
  sheetName1 <- paste0("by", group_var1, "+", group_var2)
  sheetName2 <- paste0("CI-by", group_var1, "+", group_var2)
  if(nchar(sheetName2) >=31) {
    sheetName2 <- substr(sheetName2, 1, 30)
  }

  # Write to Excel sheet
  addWorksheet(wb = excelfile_graph2, sheetName = sheetName1, gridLines = TRUE)
  writeData(wb = excelfile_graph2, sheet = sheetName1,
            x = info.long, startCol = 1, startRow = 1)
  addWorksheet(wb = excelfile_graph2, sheetName = sheetName2, gridLines = TRUE)
  writeData(wb = excelfile_graph2, sheet = sheetName2,
            x = info.CI.order, startCol = 1, startRow = 1)
  
  # print(head(info.long))
  # print(head(info.CI.order))
  
  # Write only PH to Excel sheet
  addWorksheet(wb = excelfile_graph3, sheetName = sheetName1, gridLines = TRUE)
  writeData(wb = excelfile_graph3, sheet = sheetName1,
            x = info.long[info.long$key == "Public housing",],
            startCol = 1, startRow = 1)
  addWorksheet(wb = excelfile_graph3, sheetName = sheetName2, gridLines = TRUE)
  writeData(wb = excelfile_graph3, sheet = sheetName2,
            x = info.CI.order[info.CI.order$key == "Public housing",],
            startCol = 1, startRow = 1)
}

twoway_fedsub_table_crit <- function(group_var, sub_var, criteria) {
  prop.full <- twoway_prop_criteria(housing_weighted, group_var, 
                                    sub_var, criteria)
  nSub <- length(levels(housing[[sub_var]])[levels(housing[[sub_var]]) != "(Missing)"])
  prop.sub <- prop.full[,1:(nSub+1)]
  
  if(group_var %in% race.defs) {
    index <- which(group_var %in% race.defs)
    prop.span <- twoway_prop_criteria(housing_weighted, span.defs[index], 
                                      sub_var, criteria)
    
    ## relabeling and combining
    levels(prop.full$group) <- levels(prop.sub$group) <- graph.race.lab
    levels(prop.span$group) <- graph.span.lab
    
    # combine into one table
    prop.full <- rbind(prop.full, prop.span[1,])
    prop.sub <- rbind(prop.sub[,1:(nSub+1)], prop.span[1,1:(nSub+1)])
  }
  
  ## SHEET 1: estimate
  info.long <- gather(prop.sub, key,
                      estimate, 2:(nSub+1), factor_key = TRUE)
  
  # setting `key_order`
  info.long <- 
    info.long[order(info.long$key,
                    info.long$estimate),]
  n.levels <- ifelse(group_var %in% race.defs, 
                     nlevels(housing[[group_var]]),
                     nlevels(housing[[group_var]]) - 1)
  info.long$key_order <- n.levels:1
  
  ## SHEET 2: with CIs
  col.CIlb <- names(prop.full)[grep("CI_lb", names(prop.full))]
  col.CIub <- names(prop.full)[grep("CI_ub", names(prop.full))]
  info.CI.lb <- 
    prop.full %>% select(group, col.CIlb) %>% 
    gather(., key, CI_lb, col.CIlb, factor_key = TRUE)
  levels(info.CI.lb$key) <- 
    substr(levels(info.CI.lb$key), 1, 
           nchar(levels(info.CI.lb$key))-nchar("_CI_lb"))
  info.CI.ub <- 
    prop.full %>% select(group, col.CIub) %>% 
    gather(., key, CI_ub, col.CIub, factor_key = TRUE)
  levels(info.CI.ub$key) <- 
    substr(levels(info.CI.ub$key),
           1, nchar(levels(info.CI.ub$key))-nchar("_CI_ub"))
  
  info.CI <- 
    Reduce(function(x, y) merge(x, y, by = c("group", "key")), 
           list(info.long[,1:4], info.CI.lb, info.CI.ub)) %>% 
    mutate(MOE_prop = (0.5*(CI_ub-CI_lb)))
  info.CI.order <- 
    info.CI[order(info.CI$key, info.CI$estimate),]
  
  sheetName1 <- paste0("by", sub_var)
  sheetName2 <- paste0("CI-by", sub_var)
  
  # x <- list(info.long, info.CI.order)
  # return(x)
  # Write to Excel sheet
  addWorksheet(wb = excelfile_graph5, sheetName = sheetName1, gridLines = TRUE)
  writeData(wb = excelfile_graph5, sheet = sheetName1,
            x = info.long, startCol = 1, startRow = 1)
  addWorksheet(wb = excelfile_graph5, sheetName = sheetName2, gridLines = TRUE)
  writeData(wb = excelfile_graph5, sheet = sheetName2,
            x = info.CI.order, startCol = 1, startRow = 1)

  print(head(info.long))
  print(info.long[info.long$key == "Public housing",])
  print(head(info.CI.order))
}

# % out of group_var
twoway_table <- function(group_var, sub_var) {
  prop.full <- twoway_prop(housing_weighted, group_var, sub_var)
  nSub <- length(levels(housing[[sub_var]])[levels(housing[[sub_var]]) != "(Missing)"])
  prop.sub <- prop.full[,1:(nSub+1)]
  
  if(sub_var %in% race.defs) {
    index <- which(sub_var %in% race.defs)
    prop.span <- twoway_prop(housing_weighted, group_var, span.defs[index])
    
    # combine into one table
    hispYes <- !grepl("group|Not Hispanic or Latinx", names(prop.span))
    # names(prop.span)[hispYes]
    prop.full <- cbind(prop.full, prop.span[, hispYes])
    prop.sub <- cbind(prop.sub, `Hispanic or Latinx` = prop.span[, 2])
    
    ## SHEET 1: estimate
    info.long <- gather(prop.sub, key,
                        estimate, 2:(nSub+2), factor_key = TRUE)
  } else {
    ## SHEET 1: estimate
    info.long <- gather(prop.sub, key,
                        estimate, 2:(nSub+1), factor_key = TRUE)
  }
  
  # setting `key_order`
  info.long <- 
    info.long[order(info.long$group,
                    info.long$estimate),]
  n.levels <- length(unique(info.long$key))
  # n.levels <- ifelse(sub_var %in% race.defs, 
  #                    nlevels(housing[[sub_var]]),
  #                    nlevels(housing[[sub_var]]) - 1)
  info.long$key_order <- n.levels:1
  
  ## SHEET 2: with CIs
  col.CIlb <- names(prop.full)[grep("CI_lb", names(prop.full))]
  col.CIub <- names(prop.full)[grep("CI_ub", names(prop.full))]
  
  info.CI.lb <- 
    prop.full %>% select(group, col.CIlb) %>% 
    gather(., key, CI_lb, col.CIlb, factor_key = TRUE)
  levels(info.CI.lb$key) <- 
    substr(levels(info.CI.lb$key), 1, 
           nchar(levels(info.CI.lb$key))-nchar("_CI_lb"))
  info.CI.ub <- prop.full %>% 
    select(group, col.CIub) %>% 
    gather(., key, CI_ub, col.CIub, factor_key = TRUE)
  levels(info.CI.ub$key) <- 
    substr(levels(info.CI.ub$key),
           1, nchar(levels(info.CI.ub$key))-nchar("_CI_ub"))
  
  # Combine lower and upper CIs
  info.CI <- 
    Reduce(function(x, y) merge(x, y, by = c("group", "key")), 
           list(info.long, info.CI.lb, info.CI.ub)) %>% 
    mutate(MOE_prop = (0.5*(CI_ub-CI_lb)))

  if(sub_var %in% race.defs) {
    # info.long
    levels(info.long$key) <- graph.lab[levels(info.long$key)]
    # info CI
    levels(info.CI$key) <- graph.lab[levels(info.CI$key)]
  }
  
  info.CI.order <- 
    info.CI[order(info.CI$group, info.CI$estimate),]
  
  # x <- list(info.long, info.CI.order)
  # return(x)
  
  sheetName1 <- paste0("by", sub_var)
  sheetName2 <- paste0("CI-by", sub_var)
  if(nchar(sheetName2) >=31) {
    sheetName2 <- substr(sheetName2, 1, 30)
  }

  # Write to Excel sheet
  addWorksheet(wb = excelfile_graph4, sheetName = sheetName1, gridLines = TRUE)
  writeData(wb = excelfile_graph4, sheet = sheetName1,
            x = info.long, startCol = 1, startRow = 1)
  addWorksheet(wb = excelfile_graph4, sheetName = sheetName2, gridLines = TRUE)
  writeData(wb = excelfile_graph4, sheet = sheetName2,
            x = info.CI.order, startCol = 1, startRow = 1)

  print(head(info.long))
  print(info.long[info.long$key == "Public housing",])
  print(head(info.CI.order))

  # # Write only PH to Excel sheet
  # addWorksheet(wb = excelfile_graph3, sheetName = sheetName1, gridLines = TRUE)
  # writeData(wb = excelfile_graph3, sheet = sheetName1,
  #           x = info.long[info.long$key == "Public housing",], startCol = 1, startRow = 1)
  # addWorksheet(wb = excelfile_graph3, sheetName = sheetName2, gridLines = TRUE)
  # writeData(wb = excelfile_graph3, sheet = sheetName2,
  #           x = info.CI.order[info.CI.order$key == "Public housing",], startCol = 1, startRow = 1)
}


# creating graphs

# This is twoway:
# (A) by RACE/ETH:
twoway_fedsub_table(race.var, "FEDSUB4") # Universe: each racial group
# twoway_table("FEDSUB4", race.var) # Universe: each type of housing

# (B) by GENDER:
twoway_fedsub_table("HHSEX", "FEDSUB4")
# twoway_table("FEDSUB4", "HHSEX")

# (C) by CITSHP:
twoway_fedsub_table("HOUSEHOLDCITSHP", "FEDSUB4") # Universe: each type citzenship
# twoway_table("FEDSUB4", "HOUSEHOLDCITSHP") # Universe: each type of housing

twoway_fedsub_table("HOUSEHOLDCITSHP2", "FEDSUB4") # Universe: each type citzenship

twoway_fedsub_table_crit(race.var, "HOUSEHOLDCITSHP", # Universe: each racial group in PH
                         "FEDSUB4 == 'Public housing'") 
twoway_fedsub_table_crit(race.var, "HOUSEHOLDCITSHP2", # Universe: each racial group in PH
                         "FEDSUB4 == 'Public housing'") 


# (D) by EDUC:
twoway_fedsub_table("HHGRAD4", "FEDSUB4")
# twoway_table("FEDSUB4", "HHGRAD4")

# This is threeway:
# (A) by RACE & GENDER
threeway_fedsub_table(race.var, "HHSEX", "FEDSUB4")

# (B) by RACE & CITSHP
threeway_fedsub_table(race.var, "HOUSEHOLDCITSHP", "FEDSUB4")

# (C) by RACE & EDUC
threeway_fedsub_table(race.var, "HHGRAD4", "FEDSUB4")


openxlsx::saveWorkbook(excelfile_graph2, "analysis/csv files/graphs_final/graph-2-hh.xlsx",  
                       overwrite = TRUE)
# just public housing
openxlsx::saveWorkbook(excelfile_graph3, "analysis/csv files/graphs_final/graph-3-hh-ph.xlsx",  
                       overwrite = TRUE)

openxlsx::saveWorkbook(excelfile_graph4, "analysis/csv files/graphs_final/graph-4-hh.xlsx",  
                       overwrite = TRUE)

openxlsx::saveWorkbook(excelfile_graph5, "analysis/csv files/graphs_final/graph-5-hh.xlsx",  
                       overwrite = TRUE)
