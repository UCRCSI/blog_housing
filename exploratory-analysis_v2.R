# Load relevant libraries
library(tidyverse)
library(survey)
library(srvyr)
library(forcats)
library(reshape2)
library(openxlsx)
library(rlang)
library(tidycensus)

# Loading and cleaning data -----------------------------------------------
## Load data
housing <- read.csv("raw data/ahs2017_flat_r.csv")

## Clean data
# relabeling HH race and ethnicity levels
levels(housing$HHRACE3) <- c("HH AIAN", "HH Asian", "HH Black", "HH NHPI", "HH White")
levels(housing$HHSPAN2) <- c("HH Hispanic or Latinx", "HH Not Hispanic or Latinx")
levels(housing$RACEETH) <- c("HH AIAN", "HH Asian", "HH Black", "HH NHPI", "HH White NH")

# relabeling column variables
levels(housing$TENURE)[2:3] <- c("Owner", "Renter")
levels(housing$HUDSUB)[1:3] <- c("Other renter", "Public housing", "Voucher recipient")
levels(housing$RENTCNTRL)[1:2] <- c("No rent control", "Rent control")
levels(housing$DBMISSRENT)[1:2] <- c("Not missed rent", "Missed rent")

# converting to characters
housing$HUDINCLIM_80_FLAG <- as.factor(as.character(housing$HUDINCLIM_80_FLAG))
housing$HUDINCLIM_50_FLAG <- as.factor(as.character(housing$HUDINCLIM_50_FLAG))
housing$HUDINCLIM_30_FLAG <- as.factor(as.character(housing$HUDINCLIM_30_FLAG))

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

## Weight data
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

# # applies function `totals_by_variable` to demographic grouping variables
# apply_by_definitions <- function(df, dem.name, def.var) {
#   n <- length(def.var)
#   ls <- list()
#   for (i in 1:n) {
#     by_def <- totals_by_variable(df, sym(def.var[i]))
#     colnames(by_def)[1] <- dem.name
#     ls[[i]] <- by_def
#   }
#   tmp <- do.call("rbind", ls)
#   return(tmp)
# }

# totals_by_variable <- function(df, group_var) {
#   group_var <- enquo(group_var)
#   df %>% filter((!!group_var) != "(Missing)") %>% 
#     group_by(!!group_var) %>% summarize(n = survey_total()) -> tmp
#   return (tmp)
# }
# race_household <- totals_by_variable(housing_weighted, HOUSEHOLDRACE)

# Setup and definitions ---------------------------------------------------

## Setting up Excel workbooks
excelfile <- createWorkbook()
excelfile_hud <- createWorkbook()

## Race definitions and labels
race.defs <- c("HOUSEHOLDRACE", "HHRACE3")
race.label <- "Household race"

span.defs <- c("HOUSEHOLDSPAN", "HHSPAN2")
span.label <- "Household span"

raceeth.defs <- c("HOUSEHOLDRACEETH", "RACEETH")
raceeth.label <- "Household raceeth"

# Sample-info sheets ------------------------------------------------------

## Sheet 1: sample-info-race ----------------------------------------------
tmp <- apply_by_defs_one(housing_weighted, "totals_by_variable", 
                            race.label, race.defs)
addWorksheet(wb = excelfile, sheetName = "sample-info-race", gridLines = TRUE)
writeData(wb = excelfile, sheet = "sample-info-race", 
          x = tmp, startCol = 1, startRow = 1)

## Sheet 2: sample-info-span ----------------------------------------------
tmp <- apply_by_defs_one(housing_weighted, "totals_by_variable", 
                            span.label, span.defs)
addWorksheet(wb = excelfile, sheetName = "sample-info-span", gridLines = TRUE)
writeData(wb = excelfile, sheet = "sample-info-span", 
          x = tmp, startCol = 1, startRow = 1)
# write.csv(tmp, "sample-info-span.csv")

## Sheet 3: sample-info-raceeth -------------------------------------------
tmp <- apply_by_defs_one(housing_weighted, "totals_by_variable", 
                            raceeth.label, raceeth.defs)
addWorksheet(wb = excelfile, sheetName = "sample-info-raceeth", gridLines = TRUE)
writeData(wb = excelfile, sheet = "sample-info-raceeth", 
          x = tmp, startCol = 1, startRow = 1)
# write.csv(tmp, "sample-info-raceeth.csv")

# -----
# DELETE THIS
# write.csv(tmp, "sample-info-race.csv")

# race_household <- totals_by_variable(housing_weighted, sym("HOUSEHOLDRACE"))
# race_HH <- totals_by_variable(housing_weighted, HHRACE3)
# tmp <- list(race_household, race_HH)
# colnames(race_household)[1] <- colnames(race_HH)[1] <- "Household race"
# do.call("rbind", tmp)
# rbind(tmp)

# totals_by_variable <- function(df, group_var) {
#   df %>% group_by_(.dots = lazyeval::lazy(group_var)) %>% summarize(n = survey_total()) -> tmp
#   return (tmp)
# }

# Sheet 4: Analysis --------------------------------------------------------

# Columns:

## Racial proportions ------------------------------------------------------
race_prop <- apply_by_defs_one(housing_weighted, "prop_by_variable",
                                  race.label, race.defs)
span_prop <- apply_by_defs_one(housing_weighted, "prop_by_variable", 
                                  span.label, span.defs)

## Geography (Census division) ---------------------------------------------
division.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop", 
                                     race.label, race.defs, "DIVISION")
division.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop", 
                                     span.label, span.defs, "DIVISION")

## Median income -----------------------------------------------------------
inc.byrace <- apply_by_defs_two(housing_weighted, "twoway_median", 
                                race.label, race.defs, "HINCP")
inc.byspan <- apply_by_defs_two(housing_weighted, "twoway_median", 
                                span.label, span.defs, "HINCP")

## Income as % of poverty level --------------------------------------------
pov.byrace <- apply_by_defs_two(housing_weighted, "twoway_median", 
                                race.label, race.defs, "PERPOVLVL")
pov.byspan <- apply_by_defs_two(housing_weighted, "twoway_median", 
                                span.label, span.defs, "PERPOVLVL")

## Tenure ------------------------------------------------------------------
tenure.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop", 
                                   race.label, race.defs, "TENURE")
tenure.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop", 
                                   span.label, span.defs, "TENURE")

## Housing assistance ------------------------------------------------------

### HUDSUB
hudsub.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop", 
                                   race.label, race.defs, "HUDSUB")
hudsub.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop", 
                                   span.label, span.defs, "HUDSUB")

### RENTCNTRL
rentcntrl.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop",
                                      race.label, race.defs, "RENTCNTRL")
rentcntrl.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop",
                                      span.label, span.defs, "RENTCNTRL")

## Housing delinquency -----------------------------------------------------

### DBMISSRENT
missrent.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop",
                                      race.label, race.defs, "DBMISSRENT")
missrent.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop",
                                      span.label, span.defs, "DBMISSRENT")

# Writing tables into Excel -----------------------------------------------

## by race
all_race_stats <- cbind(race_prop, inc.byrace[,-1], pov.byrace[,-1], tenure.byrace[,-1], 
                        hudsub.byrace[,-1], rentcntrl.byrace[,-1], missrent.byrace[,-1],
                        division.byrace[,-1])
addWorksheet(wb = excelfile, sheetName = "analysis-byrace", gridLines = TRUE)
writeData(wb = excelfile, sheet = "analysis-byrace", x = all_race_stats, 
          startCol = 1, startRow = 1)
# write.csv(all_race_stats, "analysis-byrace.csv")

## by span eth
all_span_stats <- cbind(span_prop, inc.byspan[,-1], pov.byspan[,-1], tenure.byspan[,-1], 
                        hudsub.byspan[,-1], rentcntrl.byspan[,-1], missrent.byspan[,-1],
                        division.byspan[,-1])
addWorksheet(wb = excelfile, sheetName = "analysis-byspan", gridLines = TRUE)
writeData(wb = excelfile, sheet = "analysis-byspan", x = all_span_stats, 
          startCol = 1, startRow = 1)
# write.csv(all_span_stats, "analysis-byspan.csv")

## write into Excel sheet
openxlsx::saveWorkbook(excelfile, "csv files/exploratory-analysis.xlsx",  overwrite = TRUE)

# Federal housing assistance discrimination -------------------------------

# create qualifying criteria variable
# function that creates tables but without _se 
## filter(HOUSEHOLDRACE & CRITERIA) %>% group_by()

criteria_80 <- "HUDINCLIM_80_FLAG == '1' & !(is.na(HUDINCLIM_80_FLAG))"
criteria_50 <- "HUDINCLIM_50_FLAG == '1' & !(is.na(HUDINCLIM_50_FLAG))"
criteria_30 <- "HUDINCLIM_30_FLAG == '1' & !(is.na(HUDINCLIM_30_FLAG))"

# Income limit proportions
## 80% income limit
prop.inclim80.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop",
                                          race.label, race.defs, "HUDINCLIM_80_FLAG")
prop.inclim80.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop",
                                          span.label, span.defs, "HUDINCLIM_80_FLAG")
colnames(prop.inclim80.byrace)[2:3] <- 
  colnames(prop.inclim80.byspan)[2:3] <- c(">80%", "<80%")

## 50% income limit
prop.inclim50.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop",
                                          race.label, race.defs, "HUDINCLIM_50_FLAG")
prop.inclim50.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop",
                                          span.label, span.defs, "HUDINCLIM_50_FLAG")
colnames(prop.inclim50.byrace)[2:3] <- 
  colnames(prop.inclim50.byspan)[2:3] <- c(">50%", "<50%")

## 50% income limit
prop.inclim30.byrace <-apply_by_defs_two(housing_weighted, "twoway_prop",
                                         race.label, race.defs, "HUDINCLIM_30_FLAG")
prop.inclim30.byspan <-apply_by_defs_two(housing_weighted, "twoway_prop",
                                         span.label, span.defs, "HUDINCLIM_30_FLAG")
colnames(prop.inclim30.byrace)[2:3] <- 
  colnames(prop.inclim30.byspan)[2:3] <- c(">30%", "<30%")

info.byrace <- cbind(prop.inclim80.byrace[,1:3], 
                     prop.inclim50.byrace[,2:3], 
                     prop.inclim30.byrace[,2:3])
info.byspan <- cbind(prop.inclim80.byspan[,1:3], 
                     prop.inclim50.byspan[,2:3], 
                     prop.inclim30.byspan[,2:3])

# Write to Excel sheet
addWorksheet(wb = excelfile_hud, sheetName = "sample-info-byrace", gridLines = TRUE)
writeData(wb = excelfile_hud, sheet = "sample-info-byrace", 
          x = info.byrace, startCol = 1, startRow = 1)

addWorksheet(wb = excelfile_hud, sheetName = "sample-info-byspan", gridLines = TRUE)
writeData(wb = excelfile_hud, sheet = "sample-info-byspan", 
          x = info.byspan, startCol = 1, startRow = 1)

# Analysis by income limits ------------------------------------------------

## 80% income limit
huddiscrim.byrace.80 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                race.label, race.defs, "HUDSUB", criteria_80)
huddiscrim.byspan.80 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                   span.label, span.defs, "HUDSUB", criteria_80)
colnames(huddiscrim.byrace.80)[2:4] <- colnames(huddiscrim.byspan.80)[2:4] <- 
  paste(colnames(huddiscrim.byrace.80)[2:4], "_80", sep = "")

## 50% income limit
huddiscrim.byrace.50 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                   race.label, race.defs, "HUDSUB", criteria_50)
huddiscrim.byspan.50 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                   span.label, span.defs, "HUDSUB", criteria_50)
colnames(huddiscrim.byrace.50)[2:4] <- colnames(huddiscrim.byspan.50)[2:4] <- 
  paste(colnames(huddiscrim.byrace.50)[2:4], "_50", sep = "")

## 30% income limit
huddiscrim.byrace.30 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                   race.label, race.defs, "HUDSUB", criteria_30)
huddiscrim.byspan.30 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                   span.label, span.defs, "HUDSUB", criteria_30)
colnames(huddiscrim.byrace.30)[2:4] <- colnames(huddiscrim.byspan.30)[2:4] <- 
  paste(colnames(huddiscrim.byrace.30)[2:4], "_30", sep = "")

# Write to Excel sheet
huddiscrim.byrace <- cbind(huddiscrim.byrace.80, huddiscrim.byrace.50[,-1], 
                           huddiscrim.byrace.30[,-1])
huddiscrim.byspan <- cbind(huddiscrim.byspan.80, huddiscrim.byspan.50[,-1], 
                           huddiscrim.byspan.30[,-1])

addWorksheet(wb = excelfile_hud, sheetName = "huddiscrim-byrace", gridLines = TRUE)
writeData(wb = excelfile_hud, sheet = "huddiscrim-byrace", 
          x = huddiscrim.byrace, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_hud, sheetName = "huddiscrim-byspan", gridLines = TRUE)
writeData(wb = excelfile_hud, sheet = "huddiscrim-byspan", 
          x = huddiscrim.byspan, startCol = 1, startRow = 1)

# Analysis by miss rent ---------------------------------------------------

criteria_MR_80 <- "DBMISSRENT == 'Missed rent' & HUDINCLIM_80_FLAG == '1' & !(is.na(HUDINCLIM_80_FLAG))"
criteria_MR_50 <- "DBMISSRENT == 'Missed rent' & HUDINCLIM_50_FLAG == '1' & !(is.na(HUDINCLIM_50_FLAG))"
criteria_MR_30 <- "DBMISSRENT == 'Missed rent' & HUDINCLIM_30_FLAG == '1' & !(is.na(HUDINCLIM_30_FLAG))"

## 80% income limit
mr.byrace.80 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                           race.label, race.defs, "HUDSUB", criteria_MR_80)
mr.byspan.80 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                           span.label, span.defs, "HUDSUB", criteria_MR_80)
colnames(mr.byrace.80)[2:4] <- colnames(mr.byspan.80)[2:4] <- 
  paste(colnames(mr.byrace.80)[2:4], "_80", sep = "")

## 50% income limit
mr.byrace.50 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                   race.label, race.defs, "HUDSUB", criteria_50)
mr.byspan.50 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                   span.label, span.defs, "HUDSUB", criteria_50)
colnames(mr.byrace.50)[2:4] <- colnames(mr.byspan.50)[2:4] <- 
  paste(colnames(mr.byrace.50)[2:4], "_50", sep = "")

## 30% income limit
mr.byrace.30 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                   race.label, race.defs, "HUDSUB", criteria_30)
mr.byspan.30 <- apply_by_defs_two_criteria(housing_weighted, "twoway_prop_criteria",
                                                   span.label, span.defs, "HUDSUB", criteria_30)
colnames(mr.byrace.30)[2:4] <- colnames(mr.byspan.30)[2:4] <- 
  paste(colnames(mr.byrace.30)[2:4], "_30", sep = "")

# Write to Excel sheet
missrent.byrace <- cbind(mr.byrace.80, mr.byrace.50[,-1], mr.byrace.30[,-1])
missrent.byspan <- cbind(mr.byspan.80, mr.byspan.50[,-1], mr.byspan.30[,-1])

addWorksheet(wb = excelfile_hud, sheetName = "missrent-byrace", gridLines = TRUE)
writeData(wb = excelfile_hud, sheet = "missrent-byrace", 
          x = missrent.byrace, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_hud, sheetName = "missrent-byspan", gridLines = TRUE)
writeData(wb = excelfile_hud, sheet = "missrent-byspan", 
          x = missrent.byspan, startCol = 1, startRow = 1)

## write into Excel
openxlsx::saveWorkbook(excelfile_hud, "csv files/hud-analysis.xlsx",  overwrite = TRUE)

# Housing discrimination by family income ---------------------------------

## sample-info
finc.byrace <- apply_by_defs_two(housing_weighted, "twoway_median",
                                 race.label, race.defs, "FINCP")
finc.byspan <- apply_by_defs_two(housing_weighted, "twoway_median",
                                 span.label, span.defs, "FINCP")
## reading in ACS median income tables by geography
census_api_key("11654cc6c6ee4ff4791e54461c9b48da31c5ff68", install = TRUE)
fincome_avg_division <- get_acs(table = "B19113", year = 2017, survey = "acs1", 
                                geography = "division")
fincome_avg_division <- fincome_avg_division %>% mutate(inclim_30 = 0.30 * estimate,
                                                        inclim_80 = 0.80 * estimate)

# match and fill in table
# have HUDINCLIM_30 and HUDINCLIM_80 flags

# same cross references
  