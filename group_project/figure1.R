
#Generating figure 1 histogram of the frequency of optimism score values for filtered data
figure1 <- function(Df){
  #bars are colored based on low, medium, and high values
  Df$colors <- cut(Df$B1SORIEN, breaks = c(-Inf, 
                                           22,26, Inf), labels = c("Low", "Med", "High"))
  Df$colors <- factor(Df$colors, levels = c("Low", "Med", "High"))
  print(table(Df$colors))
  #generate histogram for optimism score values 
  histogram <- ggplot(Df, aes(x = B1SORIEN, fill = colors)) +
    geom_bar(width = 0.75, colour = "black") +
    scale_x_continuous(breaks = seq(6, 30, by = 2)) +
    scale_fill_manual(values = c( "Low" = "black",  "Med" = "gray", "High" = "white")) + 
    labs(x = "Optimism Score", y = "Frequency") + theme_classic() + theme(legend.position = "none") +
    scale_y_continuous(expand = c(0, 0))
  
  return(histogram)
}


