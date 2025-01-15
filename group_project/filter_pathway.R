#Function input 
#df is original dataframe with missing data filtered out 

filter_pathway = function(df){
  
  require(tidyverse)
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
  
  smoking_status = rep(NA,nrow(df))
  smoking_status[df$B4H26 == "(2) No"] = "(3) never smoker"
  smoking_status[df$B4H26A == "(2) No"] = "(2) past smoker"
  smoking_status[df$B4H26A == "(1) Yes"] = "(1) current smoker"
  
  out = data.frame(M2ID = df$M2ID, prudent_diet_score = total_points, smoking = smoking_status,
                   drinking = df$B4H36, exercise = df$B4H25, BMI = df$B4PBMI)
  out = na.omit(out)
  
  #Return ID and Point Total
  return(out)
  
}
