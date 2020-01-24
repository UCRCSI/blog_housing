threeway_fedsub_table <- function(group_var1, group_var2, sub_var) {
  prop.full <- threeway_prop(housing_weighted, group_var1, group_var2, sub_var)
  nSub <- nlevels(housing[[sub_var]]) - 1
  nRow <- nlevels(housing[[group_var2]]) - 1
  prop.sub <- prop.full[,1:(nSub+1)]
  
  if(group_var %in% race.defs) {
    index <- which(race.var %in% race.defs)
    prop.span <- threeway_prop(housing_weighted, span.defs[index], 
                               group_var2, sub_var)
    
    ## relabeling and combining
    levels(prop.full$group) <- levels(prop.sub$group) <- graph.race.lab
    levels(prop.span$group) <- graph.span.lab
    
    # combine into one table
    prop.full <- rbind(prop.full, prop.span[1:nRow,])
    prop.sub <- rbind(prop.sub[,1:(nSub+2)], prop.span[1:nRow,1:(nSub+2)])
  
  ## SHEET 1: estimate
  info.long <- gather(prop.sub, key,
                      estimate, 3:(nSub+2), factor_key = TRUE)
  
  # setting `key_order`
  info.long <- 
    info.long[order(info.long$key,
                    info.long$estimate),]
  n.levels <- ifelse(group_var1 %in% race.defs, 
                     nlevels(housing[[group_var1]])*nRow,
                     (nlevels(housing[[group_var1]])-1)*nRow)
  info.long$key_order <- n.levels:1
  
  ## SHEET 2: with CIs
  info.CI.lb <- 
    prop.full %>% 
    select(group1, group2, 
           colnames(prop.full)[nSub+3]:colnames(prop.full)[2*nSub+2]) %>% 
    gather(., key, CI_lb, colnames(prop.full)[nSub+3]:colnames(prop.full)[2*nSub+2],
           factor_key = TRUE)
  levels(info.CI.lb$key) <- 
    substr(levels(info.CI.lb$key), 1, 
           nchar(levels(info.CI.lb$key))-nchar("_CI_lb"))
  info.CI.ub <- 
    prop.full %>% 
    select(group1, group2,
           colnames(prop.full)[3*nSub+3]:colnames(prop.full)[4*nSub+2]) %>% 
    gather(., key, CI_ub, colnames(prop.full)[3*nSub+3]:colnames(prop.full)[4*nSub+2], 
           factor_key = TRUE)
  levels(info.CI.ub$key) <- 
    substr(levels(info.CI.ub$key),
           1, nchar(levels(info.CI.ub$key))-nchar("_CI_ub"))
  
  info.CI <- 
    Reduce(function(x, y) merge(x, y, by = c("group", "key")), 
           list(info.long[,1:4], info.CI.lb, info.CI.ub))
  info.CI.order <- 
    info.CI[order(info.CI$key, info.CI$estimate),]
  
  sheetName1 <- paste0("by", group_var1, "+", group_var2)
  sheetName2 <- paste0("CI-by", group_var1, "+", group_var2)
  
  # Write to Excel sheet
  addWorksheet(wb = excelfile_graph2, sheetName = sheetName1, gridLines = TRUE)
  writeData(wb = excelfile_graph2, sheet = sheetName1, 
            x = info.long, startCol = 1, startRow = 1)
  addWorksheet(wb = excelfile_graph2, sheetName = sheetName2, gridLines = TRUE)
  writeData(wb = excelfile_graph2, sheet = sheetName2, 
            x = info.CI.order, startCol = 1, startRow = 1)
  # print(head(info.long))
  # print(head(info.CI.order))
}