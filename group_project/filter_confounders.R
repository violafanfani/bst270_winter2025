## helper function for computing months between optimism and serum lipid measures
cleanVisitInterval <- function(Df){
  Df$B4ZCOMPM <- as.numeric(Df$B4ZCOMPM) #B4ZCOMPM is a factor; convert to months
  #number of full years between visits
  years <- ifelse(Df$B1PIDATE_YR==Df$B4ZCOMPY,0,
                  Df$B4ZCOMPY-(Df$B1PIDATE_YR+1))
  #visit interval in months
  Df$visitInterval <- ifelse(Df$B1PIDATE_YR==Df$B4ZCOMPY,
                             Df$B4ZCOMPM - Df$B1PIDATE_MO,
                               years*12 + (12-Df$B1PIDATE_MO) + Df$B4ZCOMPM)
  return(Df)
}

## main function for getting the final confounder columns
Filter_confounders = function(Df){
  ## input: main dataframe 
  ## output: confounder columns as a dataframe
  
  ## get months between optimism and serum lipid measures
  Df = cleanVisitInterval(Df)
  
  ## remove all NA for selected confounders
  ## income variable was changed to B1STINC1 (household only) from B1SRINC1 (sum)
  confounders.ls = c('B1PB1','B1STINC1','B1PF7A','B1PAGE_M2','B1PRSEX','B1SCHROX','B1SNEGPA')
  Df = Df[complete.cases(Df[,confounders.ls]), ]
  
  ## convert the education to numeric 
  Df = Df %>% mutate(B1PB1 = as.integer(B1PB1))
  
  ## the blood pressure variable processing
  ## B1PA24 Yes/Suspects -> look at B1PA24B answer; else=no
  ## B1PA24B should be used instead of B1PA24C (too much missing values)
  Df$BPmed <- Df$B1PA24B
  Df$BPmed <- ifelse(is.na(Df$BPmed) & Df$B1PA24 == '(2) No', 2, Df$BPmed)
  Df = Df[!is.na(Df$BPmed),]
  
  ## outputting a data frame called confounder_columns
  confounder_columns = data.frame(
    M2ID = Df$M2ID,
    household_income = Df$B1STINC1/1000,
    race = ifelse(Df$B1PF7A=="(1) White","White","Nonwhite"),
    visit_interval = Df$visitInterval,
    age = Df$B1PAGE_M2,
    sex = Df$B1PRSEX,
    chronic_condition = Df$B1SCHROX,
    education_categorical = case_when(Df$B1PB1 < 4 ~ 1,
                                      Df$B1PB1 == 4 | Df$B1PB1 == 5 ~ 2,
                                      Df$B1PB1 == 6 | Df$B1PB1 == 7 ~ 3,
                                      Df$B1PB1 > 7 ~ 4),
    blood_pressure_med = Df$BPmed,
    negative_affect = Df$B1SNEGPA
  )
  return(confounder_columns)
}




