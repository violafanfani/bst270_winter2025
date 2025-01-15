#Function input 
#df is the dataframe with filtered pathway variables, with ID so that we can keep track

filter_pathway = function(df){
  
  require(tidyverse)
  #####################################
  ## Calculating the prudent diet score
  #Grab fruit/veggie intake 
  fruit_veggie = df$B4H21 
  
  #Compute points
  point_veggie = as.numeric(fruit_veggie == levels(fruit_veggie)[3] | fruit_veggie == levels(fruit_veggie)[4])
  
  #grab grain intake
  whole_grain = df$B4H22
  
  #compute points
  point_whole.grain = as.numeric(whole_grain == levels(whole_grain)[3] | whole_grain == levels(whole_grain)[4])
  
  #grab fish intake
  oily_fish = df$B4H23A 
  
  #compute points 
  point_fish = as.numeric(oily_fish == levels(oily_fish)[3] | oily_fish == levels(oily_fish)[4] | 
                            oily_fish == levels(oily_fish)[5])
  
  #grab lean meat intake
  lean_meat = df$B4H23C 
  
  #compute points
  point_lean.meat = as.numeric(lean_meat == levels(lean_meat)[3] | lean_meat == levels(lean_meat)[4] | 
                                 lean_meat == levels(lean_meat)[5])
  
  #grab sugar bev intake
  sugar_bev = df$B4H20
  
  #compute points
  point_sugar.bev = as.numeric(sugar_bev == levels(sugar_bev)[1])
  
  #grab beef fat intake 
  beef_fat = df$B4H23B
  
  #compute points
  point_beef.fat = as.numeric(beef_fat == levels(beef_fat)[1] | beef_fat == levels(beef_fat)[2] | 
                                beef_fat == levels(beef_fat)[3])
  #grab fast food intake
  fast_food = df$B4H24
  
  #compute points
  point_fast.food = as.numeric(fast_food == levels(fast_food)[1] | fast_food == levels(fast_food)[2])
  
  #Compute total points across all categories
  total_points = point_veggie + point_whole.grain + point_fish + point_lean.meat + point_sugar.bev + point_beef.fat + point_fast.food
  #############################
  
  ## Creating smoking status
  
  smoking_status = rep(NA,nrow(df))
  smoking_status[df$B4H26 == "(2) No"] = "(3) never smoker"
  smoking_status[df$B4H26A == "(2) No"] = "(2) past smoker"
  smoking_status[df$B4H26A == "(1) Yes"] = "(1) current smoker"
  
  #####################################
  
  ## Calculating drinking variable: average number of drinks consumed/day in the past month
  
  dInMo = 30
  wksInMo = 4
  # rename columns
  colnames(df)[grep("B4H33", colnames(df))] = "bin"
  
  colnames(df)[grep("B4H34", colnames(df))] = "often1"
  
  colnames(df)[grep("B4H35", colnames(df))] = "often2"
  
  colnames(df)[grep("B4H36", colnames(df))] = "onDrinkDays"
  df$onDrinkDays = as.numeric(df$onDrinkDays)
  
  look = df %>%
    mutate(often2 = recode(often2, "(1) 3 Or 4 days/month" = 3.5, "(2) 1 Or 2 days/month" = 1.2, "(3) Less than one day/mo" = 0.5, "(4) Never drinks" = 0 ))
  
  #table(df$bin, useNA = "always") # no missing data for bin var
  df$bin = ifelse(df$bin == "(1) Yes", 1, 0)
  
  # coding "Inapplicable as 1 because these are for people with 0 for "Past month, have you had at least one drink..." so multiplying bin and often1and2 will yield 0. And no truly missing data for often1, only in often2.
  df$often1and2 = ifelse(df$bin == 0, 0,
                         ifelse(df$often1 == "(1) Everyday", dInMo,
                                ifelse(df$often1 == "(2) 5 Or 6 days/week", 5.5*wksInMo,
                                       ifelse(df$often1 == "(3) 3 Or 4 days/week", 3.5*wksInMo,
                                              ifelse(df$often1 == "(4) 1 Or 2 days/week", 1.5*wksInMo, 
                                                     ifelse(df$often1 == "(5) Less than one day/week", df$often2,
                                                            ifelse(df$often1 == "(6) Never drinks", 0, 1)))))))
  
  
  # coding "Inapplicable as 1 because these are for people with 0 for "Past month, have you had at least one drink..." or said "Never Drinks" or the like for the other variables so the final multiplication will yield 0. And there is no truly missing data for this variable https://www.icpsr.umich.edu/web/NACDA/studies/29282/datasets/0001/variables/B4H36?archive=NACDA
  df$onDrinkDays = ifelse(is.na(df$onDrinkDays), 1, df$onDrinkDays)
  
  # multiply
  DrinksPerDay = df$bin * df$often1and2 * df$onDrinkDays /30
  
  #####################################
  
  ## Combining all the results
  
  out = data.frame(M2ID = df$M2ID, prudent_diet_score = total_points, smoking_status = smoking_status,
                   drinks_per_day = DrinksPerDay, regular_exercise = df$B4H25, BMI = df$B4PBMI)
  out = na.omit(out)
  
  return(out)
  
}
