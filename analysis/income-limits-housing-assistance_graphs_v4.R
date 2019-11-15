# Load relevant libraries
library(tidyverse)
library(survey)
library(srvyr)
library(forcats)
library(reshape2)
library(openxlsx)
library(rlang)
library(tidycensus)
library(tidyr)
library(BSDA)


# DATA --------------------------------------------------------------------


# Loading and cleaning data -----------------------------------------------

## Load data
housing <- read.csv("../raw data/ahs2017_flat_r.csv")

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

# FEDSUB5
## has 5 categories: Owner, Occupied without payment of rent, Public housing, Housing voucher, Other renter
housing <- housing %>% 
  mutate(FEDSUB5 = ifelse(TENURE == "Owner", "Owner", 
                         ifelse(TENURE == "Occupied without payment of rent" & 
                                  !(HUDSUB %in% c("Public housing", "Housing voucher")), 
                                    "Occupied without payment of rent", HUDSUB)))
housing$FEDSUB5 <- as.factor(housing$FEDSUB5)
levels(housing$FEDSUB5)[1:3] <- c("Other renter", "Public housing", "Housing voucher")
# summary(housing$FEDSUB5)

# FEDSUB3
## has 3 categories: Public housing, housing voucher, No rental assistance
housing <- housing %>% 
  mutate(FEDSUB3 = ifelse(TENURE == "Owner", "No rental assistance", 
                          ifelse(HUDSUB == "Public housing", "Public housing",
                                  ifelse(HUDSUB == "Housing voucher", "Housing voucher", 
                                         "No rental assistance"))))
housing$FEDSUB3 <- as.factor(housing$FEDSUB3)
summary(housing$FEDSUB3)

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
                                      "HINCP", "FINCP", "TOTHCAMT", "WEIGHT"))]

housing[,col_factor] <- lapply(housing[,col_factor], function(x) fct_explicit_na(x)) %>% as.data.frame



# Weighting data ----------------------------------------------------------
housing_weighted <- housing %>% as_survey_design(ids = 1, weight = WEIGHT)



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
  tmp2 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "prop")
  tmp3 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "prop_se")
  colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
  tmp4 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "CI_lb")
  colnames(tmp4) <- paste(colnames(tmp2), "_CI_lb", sep = "")
  tmp5 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "CI_ub")
  colnames(tmp5) <- paste(colnames(tmp2), "_CI_ub", sep = "")
  tmp6 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "n")
  colnames(tmp6) <- paste(colnames(tmp2), "_n", sep = "")
  colnames(tmp2)[1] <- colnames(tmp3)[1] <- colnames(tmp4)[1] <-
    colnames(tmp5)[1] <- colnames(tmp6)[1] <- "group"
  n.col <- ncol(tmp2)
  final <- Reduce(function(x, y) merge(x, y, by = "group"), 
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
  tmp2 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                   eval(parse(text = group_var2)), value.var = "prop")
  tmp3 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "prop_se")
  colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
  tmp4 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "CI_lb")
  colnames(tmp4) <- paste(colnames(tmp2), "_CI_lb", sep = "")
  tmp5 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "CI_ub")
  colnames(tmp5) <- paste(colnames(tmp2), "_CI_ub", sep = "")
  tmp6 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "n")
  colnames(tmp6) <- paste(colnames(tmp2), "_n", sep = "")
  colnames(tmp2)[1] <- colnames(tmp3)[1] <- colnames(tmp4)[1] <-
    colnames(tmp5)[1] <- colnames(tmp6)[1] <- "group"
  n.col <- ncol(tmp2)
  final <- Reduce(function(x, y) merge(x, y, by = "group"), 
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

# Setup and definitions ---------------------------------------------------

## Setting up Excel workbooks
excelfile_graph1 <- createWorkbook()
excelfile_graph2 <- createWorkbook()
excelfile_graph3 <- createWorkbook()

## Race definitions and labels
race.defs <- c("HOUSEHOLDRACE", "HHRACE3")
race.label <- "Household race"

span.defs <- c("HOUSEHOLDSPAN", "HHSPAN2")
span.label <- "Household span"

raceeth.defs <- c("HOUSEHOLDRACEETH", "RACEETH")
raceeth.label <- "Household raceeth"

graph.race.lab <- c("AIAN alone", "Asian alone", "Black or African American alone", 
                    "NHPI alone", "White alone (Non-Hispanic)", "(Missing)")
graph.span.lab <- c("Hispanic of any race", "Not Hispanic of any race", "(Missing)")

# setting CI level
tstat <- 1.96

# Creating family income limits ----------------------------

## Defining HUD (ACS) Median Family Income
acs.mfi <- 68000
acs.mfi.80 <- 0.80 * acs.mfi
acs.mfi.50 <- 0.50 * acs.mfi
acs.mfi.30 <- 0.30 * acs.mfi

housing <- housing %>% mutate(ACSMFI_80_FLAG = as.factor(ifelse(FINCP < acs.mfi.80, 1, 0)),
                              ACSMFI_50_FLAG = as.factor(ifelse(FINCP < acs.mfi.50, 1, 0)),
                              ACSMFI_30_FLAG = as.factor(ifelse(FINCP < acs.mfi.30, 1, 0)))

# ## Defining ACS Regional Median Family Incomes
# # sample-info
# finc.byrace <- apply_by_defs_two(housing_weighted, "twoway_median",
#                                  race.label, race.defs, "FINCP")
# finc.byspan <- apply_by_defs_two(housing_weighted, "twoway_median",
#                                  span.label, span.defs, "FINCP")
# 
# # reading in ACS median family income tables by geography
# ## census_api_key("11654cc6c6ee4ff4791e54461c9b48da31c5ff68", install = TRUE)
# fincome_avg_division <- get_acs(table = "B19113", year = 2017, survey = "acs1", 
#                                 geography = "division")
# fincome_avg_division <- fincome_avg_division %>% mutate(inclim_30 = 0.30 * estimate,
#                                                         inclim_80 = 0.80 * estimate)
# fincome_avg_division$DIVISION <- substr(fincome_avg_division$NAME, 1,
#                                         nchar(fincome_avg_division$NAME)-9)
# 
# # merging housing data with ACS median family income data
# housing <- merge(housing, fincome_avg_division[,c("DIVISION", "inclim_80", "inclim_30")], 
#                  by = "DIVISION", all.x = TRUE)
# housing <- housing %>% mutate(REGINCLIM_80_FLAG = as.factor(ifelse(FINCP < inclim_80, 1, 0)),
#                               REGINCLIM_30_FLAG = as.factor(ifelse(FINCP < inclim_30, 1, 0)))
# 
# # comparing HUD federal level income limits with regional income limits (ACS data)
# ## most lenient standard
# summary(housing$HUDINCLIM_80_FLAG)
# summary(housing$REGINCLIM_80_FLAG)
# 
# ## strictest standard
# summary(housing$HUDINCLIM_30_FLAG)
# summary(housing$REGINCLIM_30_FLAG)


# Recleaning and redefining data with new variables ----------------------------
var.flag <- colnames(housing)[grepl("FLAG$", names(housing))]

col_factor <- colnames(housing)[!(colnames(housing) %in% 
                                    c(var.races, var.span, var.num, var.rating,
                                      var.flag, var.pov, "X.1", "X", "HHAGE", 
                                      "HINCP", "FINCP", "TOTHCAMT", "WEIGHT",
                                      "estimate", "inclim_80", "inclim_30"))]

housing[,col_factor] <- lapply(housing[,col_factor], function(x) fct_explicit_na(x)) %>% as.data.frame

# Weighting data 
housing_weighted <- housing %>% as_survey_design(ids = 1, weight = WEIGHT)



# GRAPHS ------------------------------------------------------------------

# GRAPH 1: National income limits, by race -----------------------------------------

## DEF: HUD Median Family Income (ACS)
### Note: The race of the household is defined by the race of all members of the household
### The MFI is defined by the HUD ACS estimates, defined here:
### https://www.huduser.gov/portal/datasets/il/il17/Medians2017.pdf

## if want just one of the HH race definitions (householder vs. entire household)
# twoway_prop(housing_weighted, "HHRACE3", "HUDINCLIM_80_FLAG")

## 80% income limit
prop.acs80.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "ACSMFI_80_FLAG")
prop.acs80.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "ACSMFI_80_FLAG")
prop.acs80.byhouseholdrace$key.0 <- prop.acs80.byhouseholdspan$key.0 <- "MFI80"

## 50% income limit
prop.acs50.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "ACSMFI_50_FLAG")
prop.acs50.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "ACSMFI_50_FLAG")
prop.acs50.byhouseholdrace$key.0 <- prop.acs50.byhouseholdspan$key.0 <- "MFI50"

## 30% income limit
prop.acs30.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "ACSMFI_30_FLAG")
prop.acs30.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "ACSMFI_30_FLAG")
prop.acs30.byhouseholdrace$key.0 <- prop.acs30.byhouseholdspan$key.0 <- "MFI30"

## relabeling and combining
# colnames(prop.acs80.byhouseholdrace)[1] <- colnames(prop.acs30.byhouseholdrace)[1] <- 
#   colnames(prop.acs50.byhouseholdrace)[1] <- colnames(prop.acs50.byhouseholdspan)[1] <-
#   colnames(prop.acs80.byhouseholdspan)[1] <- colnames(prop.acs30.byhouseholdspan)[1] <- "group"

levels(prop.acs80.byhouseholdrace$group) <- 
  levels(prop.acs50.byhouseholdrace$group) <-
  levels(prop.acs30.byhouseholdrace$group) <- graph.race.lab

levels(prop.acs80.byhouseholdspan$group) <- 
  levels(prop.acs50.byhouseholdspan$group) <- 
  levels(prop.acs30.byhouseholdspan$group) <- graph.span.lab

# combine into one table
## no CI, only estimate
info.acs.byhousehold_long <- 
  rbind(prop.acs80.byhouseholdrace[,c("group", "1", "key.0")],
        prop.acs80.byhouseholdspan[which(prop.acs80.byhouseholdspan$group == "Hispanic of any race"),
                                   c("group", "1", "key.0")],
        prop.acs50.byhouseholdrace[,c("group", "1", "key.0")],
        prop.acs50.byhouseholdspan[which(prop.acs50.byhouseholdspan$group == "Hispanic of any race"),
                                   c("group", "1", "key.0")],
        prop.acs30.byhouseholdrace[,c("group", "1", "key.0")],
        prop.acs30.byhouseholdspan[which(prop.acs30.byhouseholdspan$group == "Hispanic of any race"),
                                   c("group", "1", "key.0")])
info.acs.byhousehold_long <- info.acs.byhousehold_long %>% na.omit()

## produce table `info.acs.byhousehold` for estimates only
colnames(info.acs.byhousehold_long)[2] <- "estimate"
info.acs.byhousehold_wide <- spread(info.acs.byhousehold_long, key.0, estimate)
info.acs.byhousehold <- info.acs.byhousehold_wide %>% 
  mutate("<30%" = MFI30,
         "30-50%" = MFI50 - MFI30,
         "50-80%" = MFI80 - MFI50)
info.acs.byhousehold <- info.acs.byhousehold %>% 
  select(group, `<30%`, `30-50%`, `50-80%`)

## turning into long dataset
# info.acs.byhousehold_long_2 <- gather(info.acs.byhousehold, key, estimate, `<30%`:`50-80%`)
## setting `key_order`
# info.acs.byhousehold_long <- info.acs.byhousehold_long[order(info.acs.byhousehold_long$estimate),]
# info.acs.byhousehold_long$key_order <- nrow(info.acs.byhousehold_long):1

# SHEET 2: with CIs
## produce table `info.acs.byhousehold.CI` for CI and estimates
## including CI
info.acs.byhousehold_long_CI <- 
  rbind(prop.acs80.byhouseholdrace[,c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs80.byhouseholdspan[which(prop.acs80.byhouseholdspan$group == "Hispanic of any race"),
                                         c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs50.byhouseholdrace[,c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs50.byhouseholdspan[which(prop.acs50.byhouseholdspan$group == "Hispanic of any race"),
                                   c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs30.byhouseholdrace[,c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")],
        prop.acs30.byhouseholdspan[which(prop.acs30.byhouseholdspan$group == "Hispanic of any race"),
                                   c("group", "1", "1_CI_lb", "1_CI_ub", "key.0")])
#info.acs.byhousehold_long_CI <- info.acs.byhousehold_long_CI %>% na.omit()
colnames(info.acs.byhousehold_long_CI)[2] <- "estimate"

# calculating CIs for income limit brackets
info.acs.byhousehold_CI_lb <- 
  spread(info.acs.byhousehold_long_CI[,c("group","1_CI_lb","key.0")], key.0, "1_CI_lb")
# %>% mutate("30%" = MFI30,
  #        "50%" = MFI50,
  #        "80%" = MFI80) %>% 
  # select(group, `30%`, `50%`, `80%`)
info.acs.byhousehold_CI_ub <- 
  spread(info.acs.byhousehold_long_CI[,c("group","1_CI_ub","key.0")], key.0, "1_CI_ub")
# %>% mutate("30%" = MFI30,
  #        "50%" = MFI50,
  #        "80%" = MFI80) %>% 
  # select(group, `30%`, `50%`, `80%`)

info.income.CI_lb <- gather(info.acs.byhousehold_CI_lb, key, CI_lb, `MFI30`:`MFI80`, factor_key = TRUE)
info.income.CI_ub <- gather(info.acs.byhousehold_CI_ub, key, CI_ub, `MFI30`:`MFI80`, factor_key = TRUE)

colnames(info.acs.byhousehold_long)[2:3] <- c("estimate", "key")
# merge estimates, CI_lb, CI_ub
info.income.CI <- 
  Reduce(function(x, y) merge(x, y, by = c("group", "key")), 
       list(info.acs.byhousehold_long, info.income.CI_lb, info.income.CI_ub))
info.income.CI <- 
  info.income.CI[order(info.income.CI$key, info.income.CI$estimate),]

# Write to Excel sheet
addWorksheet(wb = excelfile_graph1, sheetName = "by-mfi-acs", gridLines = TRUE)
writeData(wb = excelfile_graph1, sheet = "by-mfi-acs", 
          x = info.acs.byhousehold, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph1, sheetName = "CI-by-mfi-acs", gridLines = TRUE)
writeData(wb = excelfile_graph1, sheet = "CI-by-mfi-acs", 
          x = info.income.CI, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph1, "csv files/graphs_v3/graph-1-mfi.xlsx",  overwrite = TRUE)

# GRAPH 2: How many receiving support? ------------------------------------

prop.fedsub.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE",
                                           "FEDSUB5")
prop.fedsub.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN",
                                           "FEDSUB5")

## relabeling and combining
levels(prop.fedsub.byhouseholdrace$group) <- graph.race.lab
levels(prop.fedsub.byhouseholdspan$group) <- graph.span.lab

# combine into one table
info.fedsub.full <- rbind(prop.fedsub.byhouseholdrace,
                          prop.fedsub.byhouseholdspan[1,])
info.fedsub.byhousehold <- rbind(prop.fedsub.byhouseholdrace[,1:6],
                                 prop.fedsub.byhouseholdspan[1,1:6])

info.fedsub.byhousehold_long <- gather(info.fedsub.byhousehold, key,
                                       estimate, 2:6, factor_key = TRUE)

# setting `key_order`
info.fedsub.byhousehold_long <- 
  info.fedsub.byhousehold_long[order(info.fedsub.byhousehold_long$key,
                                     info.fedsub.byhousehold_long$estimate),]
info.fedsub.byhousehold_long$key_order <- 6:1

## SHEET 2: with CIs
fedsub.byrace.CI.lb <- 
  info.fedsub.full %>% 
  select(group, `Other renter_CI_lb`:`Owner_CI_lb`) %>% 
  gather(., key, CI_lb, `Other renter_CI_lb`:`Owner_CI_lb`, factor_key = TRUE)
levels(fedsub.byrace.CI.lb$key) <- substr(levels(fedsub.byrace.CI.lb$key), 
                                       1, nchar(levels(fedsub.byrace.CI.lb$key))-nchar("_CI_lb"))
fedsub.byrace.CI.ub <- 
  info.fedsub.full %>% 
  select(group, `Other renter_CI_ub`:`Owner_CI_ub`) %>% 
  gather(., key, CI_ub, `Other renter_CI_ub`:`Owner_CI_ub`, factor_key = TRUE)
levels(fedsub.byrace.CI.ub$key) <- substr(levels(fedsub.byrace.CI.ub$key), 
                                          1, nchar(levels(fedsub.byrace.CI.ub$key))-nchar("_CI_ub"))

info.fedsub.CI <- 
  Reduce(function(x, y) merge(x, y, by = c("group", "key")), 
         list(info.fedsub.byhousehold_long[,1:3], fedsub.byrace.CI.lb, fedsub.byrace.CI.ub))
info.fedsub.CI <- 
  info.fedsub.CI[order(info.fedsub.CI$key, info.fedsub.CI$estimate),]

# Write to Excel sheet
addWorksheet(wb = excelfile_graph2, sheetName = "fedsub-byrace", gridLines = TRUE)
writeData(wb = excelfile_graph2, sheet = "fedsub-byrace", 
          x = info.fedsub.byhousehold_long, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph2, sheetName = "CI-fedsub-byrace", gridLines = TRUE)
writeData(wb = excelfile_graph2, sheet = "CI-fedsub-byrace", 
          x = info.fedsub.CI, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph2, "csv files/graphs_v3/graph-2-fedsub.xlsx",  overwrite = TRUE)

# GRAPH 3: Of those in need, how many are receiving support? -------------------------------

# tmp <- housing_weighted %>% filter(ACSMFI_30_FLAG == 1) %>% group_by(HOUSEHOLDRACE, FEDSUB5) %>% 
#   summarize(prop = survey_mean())
# tail(tmp)
# 
# prop.table(table(housing[which(housing$ACSMFI_30_FLAG == 1),]$HOUSEHOLDRACE, 
#                  housing[which(housing$ACSMFI_30_FLAG == 1),]$FEDSUB3), 1)

# create qualifying criteria variable - based on ACS
criteria_acs_80 <- "ACSMFI_80_FLAG == '1' & !(is.na(ACSMFI_80_FLAG))"
criteria_acs_30 <- "ACSMFI_30_FLAG == '1' & !(is.na(ACSMFI_30_FLAG))"

## 80% income limit - ACS
fedsub.byhouseholdrace.acs80 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDRACE",
                                                  "FEDSUB3", criteria_acs_80) 
fedsub.byhouseholdspan.acs80 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDSPAN",
                                                  "FEDSUB3", criteria_acs_80)
fedsub.byhouseholdrace.acs80$key <- fedsub.byhouseholdspan.acs80$key <- "<80%"

## 30% income limit - ACS
fedsub.byhouseholdrace.acs30 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDRACE",
                                                  "FEDSUB3", criteria_acs_30)
fedsub.byhouseholdspan.acs30 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDSPAN",
                                                  "FEDSUB3", criteria_acs_30)
fedsub.byhouseholdrace.acs30$key <- fedsub.byhouseholdspan.acs30$key <- "<30%"

## relabeling and combining
# colnames(fedsub.byhouseholdrace.acs80)[1] <- colnames(fedsub.byhouseholdrace.acs30)[1] <- 
#   colnames(fedsub.byhouseholdspan.acs80)[1] <- colnames(fedsub.byhouseholdspan.acs30)[1] <-"group"

levels(fedsub.byhouseholdrace.acs80$group) <- 
  levels(fedsub.byhouseholdrace.acs30$group) <- graph.race.lab

levels(fedsub.byhouseholdspan.acs80$group) <- 
  levels(fedsub.byhouseholdspan.acs30$group) <- graph.span.lab

# combine into one table
info.fedsub.full.acs80 <- rbind(fedsub.byhouseholdrace.acs80,
                                fedsub.byhouseholdspan.acs80[1,])
info.fedsub.full.acs30 <- rbind(fedsub.byhouseholdrace.acs30,
                                fedsub.byhouseholdspan.acs30[1,])

info.fedsub.byhousehold.acs80 <- rbind(fedsub.byhouseholdrace.acs80[,c(1:4)],
                                       fedsub.byhouseholdspan.acs80[1,c(1:4)])
info.fedsub.byhousehold.acs30 <- rbind(fedsub.byhouseholdrace.acs30[,c(1:4)],
                                       fedsub.byhouseholdspan.acs30[1,c(1:4)])

info.fedsub.byhousehold.acs80_long <- gather(info.fedsub.byhousehold.acs80, key, 
                                             estimate_80, 2:4, factor_key = TRUE)
info.fedsub.byhousehold.acs30_long <- gather(info.fedsub.byhousehold.acs30, key,
                                             estimate_30, 2:4, factor_key = TRUE)

# setting `key_order`
info.fedsub.byhousehold.acs80_long <- 
  info.fedsub.byhousehold.acs80_long[order(info.fedsub.byhousehold.acs80_long$key,
                                           info.fedsub.byhousehold.acs80_long$estimate),]
info.fedsub.byhousehold.acs80_long$key_order_80 <- 6:1

info.fedsub.byhousehold.acs30_long <- 
  info.fedsub.byhousehold.acs30_long[order(info.fedsub.byhousehold.acs30_long$key,
                                           info.fedsub.byhousehold.acs30_long$estimate),]
info.fedsub.byhousehold.acs30_long$key_order_30 <- 6:1

info.fedsub.byhousehold.acs <- merge(info.fedsub.byhousehold.acs80_long, 
                                     info.fedsub.byhousehold.acs30_long, 
                                     by = c("group", "key"))

# t-test between White households (x) and Black households (y) for public housing, 
# using weighted values for n
# tsum.test(mean.x=0.01868041, s.x=0.0086587967, n.x=20789.90,
#           mean.y=0.09067740, s.y=0.0039275878, n.y=965650.25)

# SHEET 2: with CIs

## produce table `info.acs.byhousehold.CI` for CI and estimates
## including CI
info.acs80.CI.lb <- 
  info.fedsub.full.acs80 %>% 
    select(group, key, `Housing voucher_CI_lb`:`Public housing_CI_lb`) %>% 
    gather(., key, CI_lb_80, `Housing voucher_CI_lb`:`Public housing_CI_lb`, factor_key = TRUE)
levels(info.acs80.CI.lb$key) <- substr(levels(info.acs80.CI.lb$key), 
                                          1, nchar(levels(info.acs80.CI.lb$key))-nchar("_CI_lb"))
info.acs80.CI.ub <- 
  info.fedsub.full.acs80 %>% 
  select(group, key, `Housing voucher_CI_ub`:`Public housing_CI_ub`) %>% 
  gather(., key, CI_ub_80, `Housing voucher_CI_ub`:`Public housing_CI_ub`, factor_key = TRUE)
levels(info.acs80.CI.ub$key) <- substr(levels(info.acs80.CI.ub$key), 
                                       1, nchar(levels(info.acs80.CI.ub$key))-nchar("_CI_ub"))

info.acs80.CI <- merge(info.acs80.CI.lb, info.acs80.CI.ub, by = c("group", "key"))

info.acs30.CI.lb <- 
  info.fedsub.full.acs30 %>% 
  select(group, key, `Housing voucher_CI_lb`:`Public housing_CI_lb`) %>% 
  gather(., key, CI_lb_30, `Housing voucher_CI_lb`:`Public housing_CI_lb`, factor_key = TRUE)
levels(info.acs30.CI.lb$key) <- substr(levels(info.acs30.CI.lb$key), 
                                       1, nchar(levels(info.acs30.CI.lb$key))-nchar("_CI_lb"))
info.acs30.CI.ub <- 
  info.fedsub.full.acs30 %>% 
  select(group, key, `Housing voucher_CI_ub`:`Public housing_CI_ub`) %>% 
  gather(., key, CI_ub_30, `Housing voucher_CI_ub`:`Public housing_CI_ub`, factor_key = TRUE)
levels(info.acs30.CI.ub$key) <- substr(levels(info.acs30.CI.ub$key), 
                                       1, nchar(levels(info.acs30.CI.ub$key))-nchar("_CI_ub"))

info.acs30.CI <- merge(info.acs30.CI.lb, info.acs30.CI.ub, by = c("group", "key"))

# merge all
info.acs.CI <- 
  Reduce(function(x, y) merge(x, y, by = c("group", "key")), 
         list(info.fedsub.byhousehold.acs80_long, info.acs80.CI,
              info.fedsub.byhousehold.acs30_long, info.acs30.CI))
info.acs.CI <- 
  info.acs.CI[order(info.acs.CI$key, info.acs.CI$estimate),]


# Write to Excel sheet
addWorksheet(wb = excelfile_graph3, sheetName = "fedsub-inc-byrace", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "fedsub-inc-byrace", 
          x = info.fedsub.byhousehold.acs, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph3, sheetName = "CI-fedsub-inc-byrace", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "CI-fedsub-inc-byrace", 
          x = info.acs.CI, startCol = 1, startRow = 1)
openxlsx::saveWorkbook(excelfile_graph3, "csv files/graphs_v3/graph-3-mfi.xlsx",  overwrite = TRUE)

