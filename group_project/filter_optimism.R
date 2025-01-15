Filter_optimism <- function(Df) { 
  # require necessary packages
  require(tidyverse)
  
  # filter for lipids and remove any missing or inapp values
  Df_optimism <- Df %>% select(M2ID, B1SORIEN) %>% 
    filter(B1SORIEN != -1.00) %>% 
    filter(B1SORIEN != 98.00) %>% 
    filter(B1SORIEN %in% 6:30)
  # remove any NA rows to obtain complete df
    Df_optimism <- na.omit(Df_optimism)
  
  # return the filtered df
  return(Df_optimism)
}
