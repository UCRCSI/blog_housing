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
levels(housing$HUDSUB)[1:3] <- c("Other renter", "Public housing", "Voucher recipient")
levels(housing$RENTCNTRL)[1:2] <- c("No rent control", "Rent control")
levels(housing$DBMISSRENT)[1:2] <- c("Not missed rent", "Missed rent")

# converting to characters
housing$HUDINCLIM_80_FLAG <- as.factor(as.character(housing$HUDINCLIM_80_FLAG))
housing$HUDINCLIM_50_FLAG <- as.factor(as.character(housing$HUDINCLIM_50_FLAG))
housing$HUDINCLIM_30_FLAG <- as.factor(as.character(housing$HUDINCLIM_30_FLAG))

## Creating combined tenure and federal subsidy variable
housing <- housing %>% mutate(FEDSUB = ifelse(TENURE == "Owner", "Owner", HUDSUB))
housing$FEDSUB <- as.factor(housing$FEDSUB)
levels(housing$FEDSUB)[1:3] <- c("Other renter", "Public housing", "Housing voucher")
summary(housing$FEDSUB)

## Creating regional family income limits (ACS)

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
                                      "estimate", "inclim_80", "inclim_30"))]

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
excelfile_graph4 <- createWorkbook()

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

# GRAPHS ------------------------------------------------------------------

# GRAPH 1: Regional income limits, by race -----------------------------------------

## DEF 1: HOUSEHOLDER

## 80% income limit
prop.reg80.byhhrace <- twoway_prop(housing_weighted, "HHRACE3", 
                                   "REGINCLIM_80_FLAG")[,c(1,3)]
prop.reg80.byhhspan <- twoway_prop(housing_weighted, "HHSPAN2", 
                                   "REGINCLIM_80_FLAG")[1,c(1,3)]
prop.reg80.byhhrace$key <- prop.reg80.byhhspan$key <- "REG80"

## 30% income limit
prop.reg30.byhhrace <- twoway_prop(housing_weighted, "HHRACE3", 
                                   "REGINCLIM_30_FLAG")[,c(1,3)]
prop.reg30.byhhspan <- twoway_prop(housing_weighted, "HHSPAN2", 
                                   "REGINCLIM_30_FLAG")[1,c(1,3)]
prop.reg30.byhhrace$key <- prop.reg30.byhhspan$key <- "REG30"

## relabeling and combining
colnames(prop.reg80.byhhrace)[1] <- colnames(prop.reg30.byhhrace)[1] <- 
  colnames(prop.reg80.byhhspan)[1] <- colnames(prop.reg30.byhhspan)[1] <- "group"

levels(prop.reg80.byhhrace$group) <- levels(prop.reg30.byhhrace$group) <- graph.race.lab
levels(prop.reg80.byhhspan$group) <- levels(prop.reg30.byhhspan$group) <- graph.span.lab

# combine into one table
info.reg.byhh <- rbind(prop.reg80.byhhrace,
                       prop.reg80.byhhspan,
                       prop.reg30.byhhrace,
                       prop.reg30.byhhspan)

colnames(info.reg.byhh)[2] <- "estimate"

# setting `key_order`
info.reg.byhh <- info.reg.byhh[order(info.reg.byhh$estimate),]
info.reg.byhh$key_order <- nrow(info.reg.byhh):1

## DEF 2: HOUSEHOLD

## if want just one of the HH race definitions (householder vs. entire household)
# twoway_prop(housing_weighted, "HHRACE3", "HUDINCLIM_80_FLAG")

# Federal income limit proportions - by householder race
## 80% income limit
prop.reg80.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "REGINCLIM_80_FLAG")[,c(1,3)]
prop.reg80.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "REGINCLIM_80_FLAG")[1,c(1,3)]
prop.reg80.byhouseholdrace$key <- prop.reg80.byhouseholdspan$key <- "REG80"

## if want both of the HH race definitions (householder vs. entire household)
# prop.inclim80.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop",
#                                           race.label, race.defs, "HUDINCLIM_80_FLAG")
# prop.inclim80.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop",
#                                           span.label, span.defs, "HUDINCLIM_80_FLAG")

## 30% income limit
prop.reg30.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "REGINCLIM_30_FLAG")[,c(1,3)]
prop.reg30.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "REGINCLIM_30_FLAG")[1,c(1,3)]
prop.reg30.byhouseholdrace$key <- prop.reg30.byhouseholdspan$key <- "REG30"

## relabeling and combining
colnames(prop.reg80.byhouseholdrace)[1] <- colnames(prop.reg30.byhouseholdrace)[1] <- 
  colnames(prop.reg80.byhouseholdspan)[1] <- colnames(prop.reg30.byhouseholdspan)[1] <- "group"

levels(prop.reg80.byhouseholdrace$group) <- 
  levels(prop.reg30.byhouseholdrace$group) <- graph.race.lab
levels(prop.reg80.byhouseholdspan$group) <- 
  levels(prop.reg30.byhouseholdspan$group) <- graph.span.lab

# combine into one table
info.reg.byhousehold <- rbind(prop.reg80.byhouseholdrace,
                              prop.reg80.byhouseholdspan,
                              prop.reg30.byhouseholdrace,
                              prop.reg30.byhouseholdspan)

colnames(info.reg.byhousehold)[2] <- "estimate"

# setting `key_order`
info.reg.byhousehold <- info.reg.byhousehold[order(info.reg.byhousehold$estimate),]
info.reg.byhousehold$key_order <- nrow(info.reg.byhousehold):1

# Write to Excel sheet
addWorksheet(wb = excelfile_graph1, sheetName = "by-def-hh", gridLines = TRUE)
writeData(wb = excelfile_graph1, sheet = "by-def-hh", 
          x = info.reg.byhh, startCol = 1, startRow = 1)

addWorksheet(wb = excelfile_graph1, sheetName = "by-def-household", gridLines = TRUE)
writeData(wb = excelfile_graph1, sheet = "by-def-household", 
          x = info.reg.byhousehold, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph1, "csv files/graphs_v1/graph-1.xlsx",  overwrite = TRUE)


# GRAPH 2: Federal income limits, by race -----------------------------------------
## DEF 1: HOUSEHOLDER

## if want just one of the HH race definitions (householder vs. entire household)
# twoway_prop(housing_weighted, "HHRACE3", "HUDINCLIM_80_FLAG")

# Federal income limit proportions - by householder race
## 80% income limit
prop.fed80.byhhrace <- twoway_prop(housing_weighted, "HHRACE3", 
                                   "HUDINCLIM_80_FLAG")[,c(1,3)]
prop.fed80.byhhspan <- twoway_prop(housing_weighted, "HHSPAN2", 
                                   "HUDINCLIM_80_FLAG")[1,c(1,3)]
prop.fed80.byhhrace$key <- prop.fed80.byhhspan$key <- "HUD80"

## if want both of the HH race definitions (householder vs. entire household)
# prop.inclim80.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop",
#                                           race.label, race.defs, "HUDINCLIM_80_FLAG")
# prop.inclim80.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop",
#                                           span.label, span.defs, "HUDINCLIM_80_FLAG")

## 30% income limit
prop.fed30.byhhrace <- twoway_prop(housing_weighted, "HHRACE3", 
                                   "HUDINCLIM_30_FLAG")[,c(1,3)]
prop.fed30.byhhspan <- twoway_prop(housing_weighted, "HHSPAN2", 
                                   "HUDINCLIM_30_FLAG")[1,c(1,3)]
prop.fed30.byhhrace$key <- prop.fed30.byhhspan$key <- "HUD30"

## relabeling and combining
colnames(prop.fed80.byhhrace)[1] <- colnames(prop.fed30.byhhrace)[1] <- 
  colnames(prop.fed80.byhhspan)[1] <- colnames(prop.fed30.byhhspan)[1] <- "group"

levels(prop.fed80.byhhrace$group) <- levels(prop.fed30.byhhrace$group) <- graph.race.lab
levels(prop.fed80.byhhspan$group) <- levels(prop.fed30.byhhspan$group) <- graph.span.lab

# combine into one table
info.fed.byhh <- rbind(prop.fed80.byhhrace,
                       prop.fed80.byhhspan,
                       prop.fed30.byhhrace,
                       prop.fed30.byhhspan)

colnames(info.fed.byhh)[2] <- "estimate"

# setting `key_order`
info.fed.byhh <- info.fed.byhh[order(info.fed.byhh$estimate),]
info.fed.byhh$key_order <- nrow(info.fed.byhh):1

## DEF 2: HOUSEHOLD

## if want just one of the HH race definitions (householder vs. entire household)
# twoway_prop(housing_weighted, "HHRACE3", "HUDINCLIM_80_FLAG")

# Federal income limit proportions - by householder race
## 80% income limit
prop.fed80.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "HUDINCLIM_80_FLAG")[,c(1,3)]
prop.fed80.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "HUDINCLIM_80_FLAG")[1,c(1,3)]
prop.fed80.byhouseholdrace$key <- prop.fed80.byhouseholdspan$key <- "HUD80"

## if want both of the HH race definitions (householder vs. entire household)
# prop.inclim80.byrace <- apply_by_defs_two(housing_weighted, "twoway_prop",
#                                           race.label, race.defs, "HUDINCLIM_80_FLAG")
# prop.inclim80.byspan <- apply_by_defs_two(housing_weighted, "twoway_prop",
#                                           span.label, span.defs, "HUDINCLIM_80_FLAG")

## 30% income limit
prop.fed30.byhouseholdrace <- twoway_prop(housing_weighted, "HOUSEHOLDRACE", 
                                          "HUDINCLIM_30_FLAG")[,c(1,3)]
prop.fed30.byhouseholdspan <- twoway_prop(housing_weighted, "HOUSEHOLDSPAN", 
                                          "HUDINCLIM_30_FLAG")[1,c(1,3)]
prop.fed30.byhouseholdrace$key <- prop.fed30.byhouseholdspan$key <- "HUD30"

## relabeling and combining
colnames(prop.fed80.byhouseholdrace)[1] <- colnames(prop.fed30.byhouseholdrace)[1] <- 
  colnames(prop.fed80.byhouseholdspan)[1] <- colnames(prop.fed30.byhouseholdspan)[1] <- "group"

levels(prop.fed80.byhouseholdrace$group) <- 
  levels(prop.fed30.byhouseholdrace$group) <- graph.race.lab
levels(prop.fed80.byhouseholdspan$group) <- 
  levels(prop.fed30.byhouseholdspan$group) <- graph.span.lab

# combine into one table
info.fed.byhousehold <- rbind(prop.fed80.byhouseholdrace,
                          prop.fed80.byhouseholdspan,
                          prop.fed30.byhouseholdrace,
                          prop.fed30.byhouseholdspan)

colnames(info.fed.byhousehold)[2] <- "estimate"

# setting `key_order`
info.fed.byhousehold <- info.fed.byhousehold[order(info.fed.byhousehold$estimate),]
info.fed.byhousehold$key_order <- nrow(info.fed.byhousehold):1

# Write to Excel sheet
addWorksheet(wb = excelfile_graph2, sheetName = "by-def-hh", gridLines = TRUE)
writeData(wb = excelfile_graph2, sheet = "by-def-hh", 
          x = info.fed.byhh, startCol = 1, startRow = 1)

addWorksheet(wb = excelfile_graph2, sheetName = "by-def-household", gridLines = TRUE)
writeData(wb = excelfile_graph2, sheet = "by-def-household", 
          x = info.fed.byhousehold, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph2, "csv files/graphs_v1/graph-2.xlsx",  overwrite = TRUE)

# GRAPH 3: Federal housing assistance discrimination (regional) -------------------------------

# create qualifying criteria variable
criteria_80 <- "REGINCLIM_80_FLAG == '1' & !(is.na(REGINCLIM_80_FLAG))"
criteria_30 <- "REGINCLIM_30_FLAG == '1' & !(is.na(REGINCLIM_30_FLAG))"

## DEF 1: HOUSEHOLDER

## 80% income limit - regional
fedsub.byhhrace.80 <- twoway_prop_criteria(housing_weighted, "HHRACE3", 
                                           "FEDSUB", criteria_80)
fedsub.byhhrace.80 <- cbind(graph.race.lab[1:5], 
                            fedsub.byhhrace.80[,2:5]*prop.reg80.byhhrace[,2])
fedsub.byhhspan.80 <- twoway_prop_criteria(housing_weighted, "HHSPAN2", 
                                           "FEDSUB", criteria_80)
fedsub.byhhspan.80 <- cbind(graph.span.lab[1:2], 
                            fedsub.byhhspan.80[,2:5]*prop.reg80.byhhspan[,2])
fedsub.byhhrace.80$key <- fedsub.byhhspan.80$key <- "REG80"

## 30% income limit - regional
fedsub.byhhrace.30 <- twoway_prop_criteria(housing_weighted, "HHRACE3", 
                                           "FEDSUB", criteria_30)
fedsub.byhhrace.30 <- cbind(graph.race.lab[1:5], 
                            fedsub.byhhrace.30[,2:5]*prop.reg30.byhhrace[,2])
fedsub.byhhspan.30 <- twoway_prop_criteria(housing_weighted, "HHSPAN2", 
                                           "FEDSUB", criteria_30)
fedsub.byhhspan.30 <- cbind(graph.span.lab[1:2], 
                            fedsub.byhhspan.30[,2:5]*prop.reg30.byhhspan[,2])
fedsub.byhhrace.30$key <- fedsub.byhhspan.30$key <- "REG30"

## 80% income limit - federal
fedsub.byhhrace.hud80 <- cbind(graph.race.lab[1:5], 
                               fedsub.byhhrace.80[,2:5]*prop.fed80.byhhrace[,2])
fedsub.byhhspan.hud80 <- cbind(graph.span.lab[1:2], 
                               fedsub.byhhspan.80[,2:5]*prop.fed80.byhhspan[,2])
fedsub.byhhrace.hud80$key <- fedsub.byhhspan.hud80$key <- "HUD80"

## 30% income limit - federal
fedsub.byhhrace.hud30 <- cbind(graph.race.lab[1:5], 
                               fedsub.byhhrace.30[,2:5]*prop.fed30.byhhrace[,2])
fedsub.byhhspan.hud30 <- cbind(graph.span.lab[1:2], 
                               fedsub.byhhspan.30[,2:5]*prop.fed30.byhhspan[,2])
fedsub.byhhrace.hud30$key <- fedsub.byhhspan.hud30$key <- "HUD30"

## relabeling and combining
colnames(fedsub.byhhrace.80)[1] <- colnames(fedsub.byhhrace.30)[1] <- 
  colnames(fedsub.byhhspan.80)[1] <- colnames(fedsub.byhhspan.30)[1] <- 
  colnames(fedsub.byhhrace.hud80)[1] <- colnames(fedsub.byhhrace.hud30)[1] <- 
  colnames(fedsub.byhhspan.hud80)[1] <- colnames(fedsub.byhhspan.hud30)[1] <-"group"

# combine into one table
info.fedsub.byhh.80 <- rbind(fedsub.byhhrace.80,fedsub.byhhspan.80[2,])
info.fedsub.byhh.30 <- rbind(fedsub.byhhrace.30,fedsub.byhhspan.30[2,])

info.fedsub.byhh.80_long <- gather(info.fedsub.byhh.80, key, estimate, 
                                   2:5, factor_key = TRUE)
info.fedsub.byhh.30_long <- gather(info.fedsub.byhh.30, key, estimate, 
                                   2:5, factor_key = TRUE)

info.fedsub.byhh.hud80 <- rbind(fedsub.byhhrace.hud80,fedsub.byhhspan.hud80[2,])
info.fedsub.byhh.hud30 <- rbind(fedsub.byhhrace.hud30,fedsub.byhhspan.hud30[2,])

info.fedsub.byhh.hud80_long <- gather(info.fedsub.byhh.hud80, key, estimate, 
                                      2:5, factor_key = TRUE)
info.fedsub.byhh.hud30_long <- gather(info.fedsub.byhh.hud30, key, estimate, 
                                      2:5, factor_key = TRUE)

# setting `key_order`
info.fedsub.byhh.80_long <- 
  info.fedsub.byhh.80_long[order(info.fedsub.byhh.80_long$key,
                                 info.fedsub.byhh.80_long$estimate),]
info.fedsub.byhh.80_long$key_order <- rep(6:1)

info.fedsub.byhh.30_long <- 
  info.fedsub.byhh.30_long[order(info.fedsub.byhh.30_long$key, 
                                 info.fedsub.byhh.30_long$estimate),]
info.fedsub.byhh.30_long$key_order <- rep(6:1)

info.fedsub.byhh.hud80_long <- 
  info.fedsub.byhh.hud80_long[order(info.fedsub.byhh.hud80_long$key, 
                                    info.fedsub.byhh.hud80_long$estimate),]
info.fedsub.byhh.hud80_long$key_order <- rep(6:1)

info.fedsub.byhh.hud30_long <- 
  info.fedsub.byhh.hud30_long[order(info.fedsub.byhh.hud30_long$key,
                                    info.fedsub.byhh.hud30_long$estimate),]
info.fedsub.byhh.hud30_long$key_order <- rep(6:1)

# Write to Excel sheet
## regional income limits
addWorksheet(wb = excelfile_graph3, sheetName = "reg-80", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "reg-80", 
          x = info.fedsub.byhh.80_long, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph3, sheetName = "reg-30", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "reg-30", 
          x = info.fedsub.byhh.30_long, startCol = 1, startRow = 1)

## federal income limits
addWorksheet(wb = excelfile_graph3, sheetName = "hud-80", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "hud-80", 
          x = info.fedsub.byhh.hud80_long, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph3, sheetName = "hud-30", gridLines = TRUE)
writeData(wb = excelfile_graph3, sheet = "hud-30", 
          x = info.fedsub.byhh.hud30_long, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph3, "csv files/graphs_v1/graph-3.xlsx",  overwrite = TRUE)

# GRAPH 4: Federal housing assistance discrimination (federal) -------------------------------

## DEF 2: HOUSEHOLD

## 80% income limit - regional
fedsub.byhouseholdrace.80 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDRACE",
                                                  "FEDSUB", criteria_80)
fedsub.byhouseholdrace.80 <- cbind(graph.race.lab[1:5],
                                   fedsub.byhouseholdrace.80[,2:5] * prop.reg80.byhhrace[,2])
fedsub.byhouseholdspan.80 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDSPAN",
                                                  "FEDSUB", criteria_80)
fedsub.byhouseholdspan.80 <- cbind(graph.span.lab[1:2],
                                   fedsub.byhouseholdspan.80[, 2:5] * prop.reg80.byhhspan[,2])
fedsub.byhouseholdrace.80$key <- fedsub.byhouseholdspan.80$key <- "REG80"

## 30% income limit - regional
fedsub.byhouseholdrace.30 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDRACE",
                                                  "FEDSUB", criteria_30)
fedsub.byhouseholdrace.30 <- cbind(graph.race.lab[1:5],
                                   fedsub.byhouseholdrace.30[,2:5]*prop.reg30.byhouseholdrace[,2])
fedsub.byhouseholdspan.30 <- twoway_prop_criteria(housing_weighted, "HOUSEHOLDSPAN",
                                                  "FEDSUB", criteria_30)
fedsub.byhouseholdspan.30 <- cbind(graph.span.lab[1:2],
                                   fedsub.byhouseholdspan.30[, 2:5] * prop.reg30.byhouseholdspan[,2])
fedsub.byhouseholdrace.30$key <- fedsub.byhouseholdspan.30$key <- "REG30"

## 80% income limit - federal
fedsub.byhouseholdrace.hud80 <- cbind(graph.race.lab[1:5], 
                               fedsub.byhouseholdrace.80[,2:5]*prop.fed80.byhouseholdrace[,2])
fedsub.byhouseholdspan.hud80 <- cbind(graph.span.lab[1:2], 
                               fedsub.byhouseholdspan.80[,2:5]*prop.fed80.byhouseholdspan[,2])
fedsub.byhouseholdrace.hud80$key <- fedsub.byhouseholdspan.hud80$key <- "HUD80"

## 30% income limit - federal
fedsub.byhouseholdrace.hud30 <- cbind(graph.race.lab[1:5],
                                      fedsub.byhouseholdrace.30[,2:5]*prop.fed30.byhouseholdrace[,2])
fedsub.byhouseholdspan.hud30 <- cbind(graph.span.lab[1:2], 
                               fedsub.byhouseholdspan.30[,2:5]*prop.fed30.byhouseholdspan[,2])
fedsub.byhouseholdrace.hud30$key <- fedsub.byhouseholdspan.hud30$key <- "HUD30"

## relabeling and combining
colnames(fedsub.byhouseholdrace.80)[1] <- colnames(fedsub.byhouseholdrace.30)[1] <- 
  colnames(fedsub.byhouseholdspan.80)[1] <- colnames(fedsub.byhouseholdspan.30)[1] <- 
  colnames(fedsub.byhouseholdrace.hud80)[1] <- colnames(fedsub.byhouseholdrace.hud30)[1] <- 
  colnames(fedsub.byhouseholdspan.hud80)[1] <- colnames(fedsub.byhouseholdspan.hud30)[1] <-"group"

# combine into one table
info.fedsub.byhousehold.80 <- rbind(fedsub.byhouseholdrace.80,fedsub.byhouseholdspan.80[2,])
info.fedsub.byhousehold.30 <- rbind(fedsub.byhouseholdrace.30,fedsub.byhouseholdspan.30[2,])

info.fedsub.byhousehold.80_long <- gather(info.fedsub.byhousehold.80, key, estimate, 
                                   2:5, factor_key = TRUE)
info.fedsub.byhousehold.30_long <- gather(info.fedsub.byhousehold.30, key, estimate, 
                                   2:5, factor_key = TRUE)

info.fedsub.byhousehold.hud80 <- rbind(fedsub.byhouseholdrace.hud80,
                                       fedsub.byhouseholdspan.hud80[2,])
info.fedsub.byhousehold.hud30 <- rbind(fedsub.byhouseholdrace.hud30,
                                       fedsub.byhouseholdspan.hud30[2,])

info.fedsub.byhousehold.hud80_long <- gather(info.fedsub.byhousehold.hud80, key, 
                                             estimate, 2:5, factor_key = TRUE)
info.fedsub.byhousehold.hud30_long <- gather(info.fedsub.byhousehold.hud30, key,
                                             estimate, 2:5, factor_key = TRUE)

# setting `key_order`
info.fedsub.byhousehold.80_long <- 
  info.fedsub.byhousehold.80_long[order(info.fedsub.byhousehold.80_long$key,
                                        info.fedsub.byhousehold.80_long$estimate),]
info.fedsub.byhousehold.80_long$key_order <- 6:1

info.fedsub.byhousehold.30_long <- 
  info.fedsub.byhousehold.30_long[order(info.fedsub.byhousehold.30_long$key,
                                        info.fedsub.byhousehold.30_long$estimate),]
info.fedsub.byhousehold.30_long$key_order <- 6:1

info.fedsub.byhousehold.hud80_long <- 
  info.fedsub.byhousehold.hud80_long[order(info.fedsub.byhousehold.hud80_long$key,
                                           info.fedsub.byhousehold.hud80_long$estimate),]
info.fedsub.byhousehold.hud80_long$key_order <- 6:1

info.fedsub.byhousehold.hud30_long <- 
  info.fedsub.byhousehold.hud30_long[order(info.fedsub.byhousehold.hud30_long$key,
                                           info.fedsub.byhousehold.hud30_long$estimate),]
info.fedsub.byhousehold.hud30_long$key_order <- 6:1

# Write to Excel sheet
## regional income limits
addWorksheet(wb = excelfile_graph4, sheetName = "reg-80", gridLines = TRUE)
writeData(wb = excelfile_graph4, sheet = "reg-80", 
          x = info.fedsub.byhousehold.80_long, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph4, sheetName = "reg-30", gridLines = TRUE)
writeData(wb = excelfile_graph4, sheet = "reg-30", 
          x = info.fedsub.byhousehold.30_long, startCol = 1, startRow = 1)

## federal income limits
addWorksheet(wb = excelfile_graph4, sheetName = "hud-80", gridLines = TRUE)
writeData(wb = excelfile_graph4, sheet = "hud-80", 
          x = info.fedsub.byhousehold.hud80_long, startCol = 1, startRow = 1)
addWorksheet(wb = excelfile_graph4, sheetName = "hud-30", gridLines = TRUE)
writeData(wb = excelfile_graph4, sheet = "hud-30", 
          x = info.fedsub.byhousehold.hud30_long, startCol = 1, startRow = 1)

openxlsx::saveWorkbook(excelfile_graph4, "csv files/graphs_v1/graph-4.xlsx",  overwrite = TRUE)

