library(gtsummary)
library(tidyverse)
#Generating table for low, medium, and high optimism levels for corresponding lipid variables
#Visualize the mean and standard deviation using one-way ANOVA
table_1_lipid<-function(df){
  df$Optimism<-cut(df$B1SORIEN,
                   breaks=c(-Inf,
                            22,26,
                            Inf),
                   labels=c("Low","Moderate","High"))
  table_1<-df|>
    select(Optimism,B4BCHOL,B4BHDL,B4BLDL,B4BTRIGL)|>
    tbl_summary(by=Optimism,
                label = list(
                  B4BCHOL ~ "Total cholesterol",
                  B4BTRIGL ~ "Triglycerides",
                  B4BHDL ~ "High-density lipoprotein cholesterol",
                  B4BLDL ~ "Low-density lipoprotein cholesterol"),
                statistic = all_continuous() ~ "{mean} ± {sd}",
                digits = list(all_continuous() ~ c(2, 2)))|>
    add_p(test = list(
      all_continuous() ~ "oneway.test")) |> 
    modify_spanning_header(all_stat_cols() ~"**Optimism**") #adds spanning header
    return(table_1)
}

