Filter_lipids_bad <- function(Df) { 
  # require necessary packages
  require(tidyverse)
  
  # filter for lipids and remove any missing or inapp values
  Df_lipids <- Df %>% select(M2ID, B4BCHOL, B4BTRIGL, B4BHDL, B4BLDL,B4H1HD,B4XTCS_19,B1PRSEX) %>% 
    filter(B4BCHOL != 999) |> 
    filter(B4BTRIGL != 9999) |> 
    filter(!B4BHDL %in% c(999, 998)) |>
    filter(!B4BLDL %in% c(999, 998)) |>
    mutate(bad_chol=B4BCHOL>=240,
           bad_hdl=(B4BHDL<40&B1PRSEX==1)|(B4BHDL<50&B1PRSEX==2),
           bad_ldl=B4BLDL>=160,
           bad_tri=B4BTRIGL>=200,
           bad_lipid_drug=B4XTCS_19==1,
           bad_lipid_disease=B4H1HD==1)|>
    group_by(M2ID)|>
    mutate(bad_lipid_condition=any(bad_chol,bad_ldl,bad_tri,bad_lipid_drug,bad_lipid_disease))|>
    ungroup()
  # return the filtered df
  return(Df_lipids)
}

