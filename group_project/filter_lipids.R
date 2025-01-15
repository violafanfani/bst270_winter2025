Filter_lipids <- function(Df) { 
  # require necessary packages
  require(tidyverse)
  
  # filter for lipids and remove any missing or inapp values
  Df_lipids <- Df %>% select(M2ID, B4BCHOL, B4BTRIGL, B4BHDL, B4BLDL) %>% 
    filter(B4BCHOL != 999) %>% 
    filter(B4BTRIGL != 9999) %>% 
    filter(!B4BHDL %in% c(999, 998)) %>% 
    filter(!B4BLDL %in% c(999, 998))
  # remove any NA rows to obtain complete df
  Df_lipids <- na.omit(Df_lipids)
  
  # return the filtered df
  return(Df_lipids)
}