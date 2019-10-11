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
    summarize(prop = survey_mean()) -> tmp
  tmp2 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "prop")
  tmp3 <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                  eval(parse(text = group_var2)), value.var = "prop_se")
  colnames(tmp3) <- paste(colnames(tmp2), "_se", sep = "")
  n.col <- ncol(tmp2)
  final <- cbind(tmp2, tmp3[,c(2:n.col)])
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
    summarize(prop = survey_mean()) -> tmp
  final <- dcast(tmp, eval(parse(text = group_var1)) ~ 
                   eval(parse(text = group_var2)), value.var = "prop")
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


# Creating family income limits ----------------------------

## Defining AHS Median Family Income
tmp <- housing_weighted %>% filter(FINCP != "(Missing)") %>% 
  summarize(median_fincp = survey_median(FINCP))
ahs.mfi <- tmp[[1]]
ahs.mfi.80 <- 0.80 * tmp[[1]]
ahs.mfi.30 <- 0.30 * tmp[[1]]

housing <- housing %>% mutate(AHSMFI_80_FLAG = as.factor(ifelse(FINCP < ahs.mfi.80, 1, 0)),
                              AHSMFI_30_FLAG = as.factor(ifelse(FINCP < ahs.mfi.30, 1, 0)))

## Defining HUD (ACS) Median Family Income
acs.mfi <- 68000
acs.mfi.80 <- 0.80 * acs.mfi
acs.mfi.30 <- 0.30 * acs.mfi

housing <- housing %>% mutate(ACSMFI_80_FLAG = as.factor(ifelse(FINCP < acs.mfi.80, 1, 0)),
                              ACSMFI_30_FLAG = as.factor(ifelse(FINCP < acs.mfi.30, 1, 0)))

## Defining ACS Regional Median Family Incomes
# sample-info
finc.byrace <- apply_by_defs_two(housing_weighted, "twoway_median",
                                 race.label, race.defs, "FINCP")
finc.byspan <- apply_by_defs_two(housing_weighted, "twoway_median",
                                 span.label, span.defs, "FINCP")

# reading in ACS median family income tables by geography
## census_api_key("11654cc6c6ee4ff4791e54461c9b48da31c5ff68", install = TRUE)
fincome_avg_division <- get_acs(table = "B19113", year = 2017, survey = "acs1", 
                                geography = "division")
fincome_avg_division <- fincome_avg_division %>% mutate(inclim_30 = 0.30 * estimate,
                                                        inclim_80 = 0.80 * estimate)
fincome_avg_division$DIVISION <- substr(fincome_avg_division$NAME, 1,
                                        nchar(fincome_avg_division$NAME)-9)

# merging housing data with ACS median family income data
housing <- merge(housing, fincome_avg_division[,c("DIVISION", "inclim_80", "inclim_30")], 
                 by = "DIVISION", all.x = TRUE)
housing <- housing %>% mutate(REGINCLIM_80_FLAG = as.factor(ifelse(FINCP < inclim_80, 1, 0)),
                              REGINCLIM_30_FLAG = as.factor(ifelse(FINCP < inclim_30, 1, 0)))

# comparing HUD federal level income limits with regional income limits (ACS data)
## most lenient standard
summary(housing$HUDINCLIM_80_FLAG)
summary(housing$REGINCLIM_80_FLAG)

## strictest standard
summary(housing$HUDINCLIM_30_FLAG)
summary(housing$REGINCLIM_30_FLAG)


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
## Note: The race of the household is defined by the race of all members of the household

## DEF 1: AHS Median Family Income

## 80% income limit
prop.ahs80.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "AHSMFI_80_FLAG")[,c(1,3)]
prop.ahs80.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "AHSMFI_80_FLAG")[1,c(1,3)]
prop.ahs80.byhouseholdrace$key <- prop.ahs80.byhouseholdspan$key <- "MFI80"

## 30% income limit
prop.ahs30.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "AHSMFI_30_FLAG")[,c(1,3)]
prop.ahs30.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "AHSMFI_30_FLAG")[1,c(1,3)]
prop.ahs30.byhouseholdrace$key <- prop.ahs30.byhouseholdspan$key <- "MFI30"

## relabeling and combining
colnames(prop.ahs80.byhouseholdrace)[1] <- colnames(prop.ahs30.byhouseholdrace)[1] <- 
  colnames(prop.ahs80.byhouseholdspan)[1] <- colnames(prop.ahs30.byhouseholdspan)[1] <- "group"

levels(prop.ahs80.byhouseholdrace$group) <- 
  levels(prop.ahs30.byhouseholdrace$group) <- graph.race.lab
levels(prop.ahs80.byhouseholdspan$group) <- 
  levels(prop.ahs30.byhouseholdspan$group) <- graph.span.lab

# combine into one table
info.ahs.byhousehold <- rbind(prop.ahs80.byhouseholdrace,
                              prop.ahs80.byhouseholdspan,
                              prop.ahs30.byhouseholdrace,
                              prop.ahs30.byhouseholdspan)

colnames(info.ahs.byhousehold)[2] <- "estimate"

# setting `key_order`
info.ahs.byhousehold <- info.ahs.byhousehold[order(info.ahs.byhousehold$estimate),]
info.ahs.byhousehold$key_order <- nrow(info.ahs.byhousehold):1

## DEF 2: HUD Median Family Income (ACS)
# https://www.huduser.gov/portal/datasets/il/il17/Medians2017.pdf

## if want just one of the HH race definitions (householder vs. entire household)
# twoway_prop(housing_weighted, "HHRACE3", "HUDINCLIM_80_FLAG")

## 80% income limit
prop.acs80.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "ACSMFI_80_FLAG")[,c(1,3)]
prop.acs80.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "ACSMFI_80_FLAG")[1,c(1,3)]
prop.acs80.byhouseholdrace$key <- prop.acs80.byhouseholdspan$key <- "MFI80"

## 30% income limit
prop.acs30.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "ACSMFI_30_FLAG")[,c(1,3)]
prop.acs30.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "ACSMFI_30_FLAG")[1,c(1,3)]
prop.acs30.byhouseholdrace$key <- prop.acs30.byhouseholdspan$key <- "MFI30"

## relabeling and combining
colnames(prop.acs80.byhouseholdrace)[1] <- colnames(prop.acs30.byhouseholdrace)[1] <- 
  colnames(prop.acs80.byhouseholdspan)[1] <- colnames(prop.acs30.byhouseholdspan)[1] <- "group"

levels(prop.acs80.byhouseholdrace$group) <- 
  levels(prop.acs30.byhouseholdrace$group) <- graph.race.lab
levels(prop.acs80.byhouseholdspan$group) <- 
  levels(prop.acs30.byhouseholdspan$group) <- graph.span.lab

# combine into one table
info.acs.byhousehold <- rbind(prop.acs80.byhouseholdrace,
                              prop.acs80.byhouseholdspan,
                              prop.acs30.byhouseholdrace,
                              prop.acs30.byhouseholdspan)

colnames(info.acs.byhousehold)[2] <- "estimate"

# setting `key_order`
info.acs.byhousehold <- info.acs.byhousehold[order(info.acs.byhousehold$estimate),]
info.acs.byhousehold$key_order <- nrow(info.acs.byhousehold):1

# Write to Excel sheet
addWorksheet(wb = excelfile_graph1, sheetName = "by-mfi-ahs", gridLines = TRUE)
writeData(wb = excelfile_graph1, sheet = "by-mfi-ahs", 
          x = info.ahs.byhousehold, startCol = 1, startRow = 1)

addWorksheet(wb = excelfile_graph1, sheetName = "by-mfi-acs", gridLines = TRUE)
writeData(wb = excelfile_graph1, sheet = "by-mfi-acs", 
          x = info.acs.byhousehold, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph1, "csv files/graphs_v2/graph-1-mfi.xlsx",  overwrite = TRUE)


# GRAPH 2: How many receiving support? ------------------------------------

prop.fedsub.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE",
                                           "FEDSUB5")[,1:6]
prop.fedsub.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN",
                                           "FEDSUB5")[,1:6]

## relabeling and combining
colnames(prop.fedsub.byhouseholdrace)[1] <- colnames(prop.fedsub.byhouseholdspan)[1] <-"group"
levels(prop.fedsub.byhouseholdrace$group) <- graph.race.lab
levels(prop.fedsub.byhouseholdspan) <- graph.span.lab

# combine into one table
info.fedsub.byhousehold <- rbind(prop.fedsub.byhouseholdrace,
                                 prop.fedsub.byhouseholdspan[1,])

info.fedsub.byhousehold_long <- gather(info.fedsub.byhousehold, key,
                                       estimate, 2:6, factor_key = TRUE)

# setting `key_order`
info.fedsub.byhousehold_long <- 
  info.fedsub.byhousehold_long[order(info.fedsub.byhousehold_long$key,
                                     info.fedsub.byhousehold_long$estimate),]
info.fedsub.byhousehold_long$key_order <- 6:1

# Write to Excel sheet
addWorksheet(wb = excelfile_graph2, sheetName = "fedsub-byrace", gridLines = TRUE)
writeData(wb = excelfile_graph2, sheet = "fedsub-byrace", 
          x = info.fedsub.byhousehold_long, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph2, "csv files/graphs_v2/graph-2-fedsub.xlsx",  overwrite = TRUE)

# GRAPH 3: Of those in need, how many are receiving support? -------------------------------

# create qualifying criteria variable - based on AHS
criteria_ahs_80 <- "AHSMFI_80_FLAG == '1' & !(is.na(AHSMFI_80_FLAG))"
criteria_ahs_30 <- "AHSMFI_30_FLAG == '1' & !(is.na(AHSMFI_30_FLAG))"
# create qualifying criteria variable - based on ACS
criteria_acs_80 <- "ACSMFI_80_FLAG == '1' & !(is.na(ACSMFI_80_FLAG))"
criteria_acs_30 <- "ACSMFI_30_FLAG == '1' & !(is.na(ACSMFI_30_FLAG))"

## 80% income limit - AHS
fedsub.byhouseholdrace.80 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDRACE",
                                                  "FEDSUB3", criteria_ahs_80)
fedsub.byhouseholdspan.80 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDSPAN",
                                                  "FEDSUB3", criteria_ahs_80)
fedsub.byhouseholdrace.80$key <- fedsub.byhouseholdspan.80$key <- "AHS80"

## 30% income limit - AHS
fedsub.byhouseholdrace.30 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDRACE",
                                                  "FEDSUB3", criteria_ahs_30)
fedsub.byhouseholdspan.30 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDSPAN",
                                                  "FEDSUB3", criteria_ahs_30)
fedsub.byhouseholdrace.30$key <- fedsub.byhouseholdspan.30$key <- "AHS30"

## 80% income limit - ACS
fedsub.byhouseholdrace.acs80 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDRACE",
                                                  "FEDSUB3", criteria_acs_80)
fedsub.byhouseholdspan.acs80 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDSPAN",
                                                  "FEDSUB3", criteria_acs_80)
fedsub.byhouseholdrace.acs80$key <- fedsub.byhouseholdspan.acs80$key <- "ACS80"

## 30% income limit - ACS
fedsub.byhouseholdrace.acs30 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDRACE",
                                                  "FEDSUB3", criteria_acs_30)
fedsub.byhouseholdspan.acs30 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDSPAN",
                                                  "FEDSUB3", criteria_acs_30)
fedsub.byhouseholdrace.acs30$key <- fedsub.byhouseholdspan.acs30$key <- "ACS30"

## relabeling and combining
colnames(fedsub.byhouseholdrace.80)[1] <- colnames(fedsub.byhouseholdrace.30)[1] <- 
  colnames(fedsub.byhouseholdspan.80)[1] <- colnames(fedsub.byhouseholdspan.30)[1] <- 
  colnames(fedsub.byhouseholdrace.acs80)[1] <- colnames(fedsub.byhouseholdrace.acs30)[1] <- 
  colnames(fedsub.byhouseholdspan.acs80)[1] <- colnames(fedsub.byhouseholdspan.acs30)[1] <-"group"

levels(fedsub.byhouseholdrace.80$group) <- 
  levels(fedsub.byhouseholdrace.30$group) <- 
  levels(fedsub.byhouseholdrace.acs80$group) <- 
  levels(fedsub.byhouseholdrace.acs30$group) <- graph.race.lab

levels(fedsub.byhouseholdspan.80$group) <- 
  levels(fedsub.byhouseholdspan.30$group) <- 
  levels(fedsub.byhouseholdspan.acs80$group) <- 
  levels(fedsub.byhouseholdspan.acs30$group) <- graph.span.lab

# combine into one table
info.fedsub.byhousehold.80 <- rbind(fedsub.byhouseholdrace.80,
                                    fedsub.byhouseholdspan.80[1,])
info.fedsub.byhousehold.30 <- rbind(fedsub.byhouseholdrace.30,
                                    fedsub.byhouseholdspan.30[1,])

info.fedsub.byhousehold.80_long <- gather(info.fedsub.byhousehold.80, key,
                                          estimate, 2:4, factor_key = TRUE)
info.fedsub.byhousehold.30_long <- gather(info.fedsub.byhousehold.30, key, 
                                          estimate, 2:4, factor_key = TRUE)

info.fedsub.byhousehold.acs80 <- rbind(fedsub.byhouseholdrace.acs80,
                                       fedsub.byhouseholdspan.acs80[1,])
info.fedsub.byhousehold.acs30 <- rbind(fedsub.byhouseholdrace.acs30,
                                       fedsub.byhouseholdspan.acs30[1,])

info.fedsub.byhousehold.acs80_long <- gather(info.fedsub.byhousehold.acs80, key, 
                                             estimate, 2:4, factor_key = TRUE)
info.fedsub.byhousehold.acs30_long <- gather(info.fedsub.byhousehold.acs30, key,
                                             estimate, 2:4, factor_key = TRUE)

# setting `key_order`
info.fedsub.byhousehold.80_long <- 
  info.fedsub.byhousehold.80_long[order(info.fedsub.byhousehold.80_long$key,
                                        info.fedsub.byhousehold.80_long$estimate),]
info.fedsub.byhousehold.80_long$key_order <- 6:1

info.fedsub.byhousehold.30_long <- 
  info.fedsub.byhousehold.30_long[order(info.fedsub.byhousehold.30_long$key,
                                        info.fedsub.byhousehold.30_long$estimate),]
info.fedsub.byhousehold.30_long$key_order <- 6:1

info.fedsub.byhousehold.acs80_long <- 
  info.fedsub.byhousehold.acs80_long[order(info.fedsub.byhousehold.acs80_long$key,
                                           info.fedsub.byhousehold.acs80_long$estimate),]
info.fedsub.byhousehold.acs80_long$key_order <- 6:1

info.fedsub.byhousehold.acs30_long <- 
  info.fedsub.byhousehold.acs30_long[order(info.fedsub.byhousehold.acs30_long$key,
                                           info.fedsub.byhousehold.acs30_long$estimate),]
info.fedsub.byhousehold.acs30_long$key_order <- 6:1

# Write to Excel sheet
## regional income limits
addWorksheet(wb = excelfile_graph3, sheetName = "ahs-80", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "ahs-80", 
          x = info.fedsub.byhousehold.80_long, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph3, sheetName = "ahs-30", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "ahs-30", 
          x = info.fedsub.byhousehold.30_long, startCol = 1, startRow = 1)

## federal income limits
addWorksheet(wb = excelfile_graph3, sheetName = "acs-80", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "acs-80", 
          x = info.fedsub.byhousehold.acs80_long, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph3, sheetName = "acs-30", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "acs-30", 
          x = info.fedsub.byhousehold.acs30_long, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph3, "csv files/graphs_v2/graph-3-mfi.xlsx",  overwrite = TRUE)

