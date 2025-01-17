#Function input 
#df is the dataframe with ID so that we can keep track

library(tableone)

pathway_tableone = function(full_data) {
  
  low = full_data$B1SORIEN >= 6 & full_data$B1SORIEN <= 22
  medium = full_data$B1SORIEN >= 23 & full_data$B1SORIEN <= 26
  high = full_data$B1SORIEN >= 27 & full_data$B1SORIEN <= 30
  
  full_data.low = full_data[low,]
  full_data.medium = full_data[medium,]
  full_data.high = full_data[high,]
  
  
  truncate_decimal <- function(x, digits) {
    factor <- 10^digits
    ifelse(x >= 0, floor(x * factor) / factor, ceiling(x * factor) / factor)
  }
  
  # generate table 1
  # smoking
  
  optimism <- ifelse(full_data$B1SORIEN <= 22, "low",
                     ifelse(full_data$B1SORIEN >= 27, "high", "moderate"))
  
  tb_smoke = table(full_data$smoking_status,optimism)
  tb_smoke = tb_smoke[,c(2,3,1)]
  
  row_prop_smoke = tb_smoke
  for(i in 1:3){
    row_prop_smoke[i,] = tb_smoke[i,]/sum(tb_smoke[i,])
  }
  row.names(row_prop_smoke) = c("Current smoker row %","Past smoker row %","Never smoker row %")
  row_prop_smoke = truncate_decimal(row_prop_smoke * 100,2)
  row_prop_smoke
  
  col_prop_smoke = tb_smoke
  for(i in 1:3){
    col_prop_smoke[,i] = tb_smoke[,i]/sum(tb_smoke[,i])
  }
  row.names(col_prop_smoke) = c("Current smoker col %","Past smoker col %","Never smoker col %")
  col_prop_smoke = truncate_decimal(col_prop_smoke * 100,2)
  col_prop_smoke
  
  
  
  
  
  # generate table 1
  
  
  full_data$optimism = ifelse(full_data$B1SORIEN >= 6 & full_data$B1SORIEN <= 22, "low", 
                              ifelse(medium, "medium", 
                                     ifelse(high, "high", NA)))
  
  full_data$drinks_per_day = as.numeric(full_data$drinks_per_day)
  
  display = c("drinks_per_day", "smoking_status", "prudent_diet_score")
  tab <- CreateTableOne(vars = display, data = full_data, strata = "optimism", includeNA = TRUE)
  kableone(tab)
  
  display2 = c("optimism")
  tab2 <- CreateTableOne(vars = display2, data = full_data, strata = "smoking_status", includeNA = TRUE)
  kableone(tab2)
  
  
  return(list(tbsmoke = tb_smoke, smokerow=row_prop_smoke, smokecol = col_prop_smoke, drinksPrudent = kableone(tab), smokeRow = kableone(tab2)))
  
}

exercise_tbl = function(df) {
  
  low = full_data$B1SORIEN >= 6 & full_data$B1SORIEN <= 22
  medium = full_data$B1SORIEN >= 23 & full_data$B1SORIEN <= 26
  high = full_data$B1SORIEN >= 27 & full_data$B1SORIEN <= 30
  
  full_data.low = full_data[low,]
  full_data.medium = full_data[medium,]
  full_data.high = full_data[high,]
  
  ### Regular Exercise ###
  
  # Column Percentages for '(1) Yes' and '(2) No'
  col_yes_low <- mean(full_data.low$regular_exercise == '(1) Yes', na.rm = TRUE)
  col_yes_medium <- mean(full_data.medium$regular_exercise == '(1) Yes', na.rm = TRUE)
  col_yes_high <- mean(full_data.high$regular_exercise == '(1) Yes', na.rm = TRUE)
  
  col_no_low <- mean(full_data.low$regular_exercise == '(2) No', na.rm = TRUE)
  col_no_medium <- mean(full_data.medium$regular_exercise == '(2) No', na.rm = TRUE)
  col_no_high <- mean(full_data.high$regular_exercise == '(2) No', na.rm = TRUE)
  
  # Row Percentages for '(1) Yes'
  optimism_yes <- full_data[full_data$regular_exercise == '(1) Yes',]$B1SORIEN
  row_yes_low <- mean(optimism_yes >= 6 & optimism_yes <= 22, na.rm = TRUE)
  row_yes_medium <- mean(optimism_yes >= 23 & optimism_yes <= 26, na.rm = TRUE)
  row_yes_high <- mean(optimism_yes >= 27 & optimism_yes <= 30, na.rm = TRUE)
  
  # Row Percentages for '(2) No'
  optimism_no <- full_data[full_data$regular_exercise == '(2) No',]$B1SORIEN
  row_no_low <- mean(optimism_no >= 6 & optimism_no <= 22, na.rm = TRUE)
  row_no_medium <- mean(optimism_no >= 23 & optimism_no <= 26, na.rm = TRUE)
  row_no_high <- mean(optimism_no >= 27 & optimism_no <= 30, na.rm = TRUE)
  
  # Construct the transposed table for better readability
  exercise_table <- data.frame(
    Metric = c("Yes Column %", "Yes Row %", "No Column %", "No Row %"),
    Low = c(col_yes_low * 100, row_yes_low * 100, col_no_low * 100, row_no_low * 100),
    Medium = c(col_yes_medium * 100, row_yes_medium * 100, col_no_medium * 100, row_no_medium * 100),
    High = c(col_yes_high * 100, row_yes_high * 100, col_no_high * 100, row_yes_high * 100)
  )
  
  return(exercise_table)
}

