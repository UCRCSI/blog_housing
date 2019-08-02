## setup a workbook with 3 worksheets
age <- readr::read_rds("dta/age_racial.rds")
excelfile <- createWorkbook()
addWorksheet(wb = excelfile, sheetName = "raw_data", gridLines = TRUE)
# img <- system.file("einstein.jpg", package = "openxlsx")
insertImage(excelfile, "raw_data", "/Users/sunnyshao/Dropbox/plotter/examples/logo_aapidata.png", 
            startRow = 1,  startCol = 1, width = 2.5, height = 1.5)
# addWorksheet(wb = excelfile, sheetName = "raw_data", gridLines = TRUE)
writeData(wb = excelfile, sheet = "raw_data", x = age, startCol = 1, startRow = 9)
saveWorkbook(excelfile, "dta/final2.xlsx",  overwrite = TRUE)