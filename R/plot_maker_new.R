##' function to plot proxzyme data
##'
##' @param df a dataframe of data containing each condition minus background, avg, and sd
##' @param bar.chart if you want the bar chart, bar.chart = TRUE, leave blank for scatterplot
##' @description 
##' @details 
##' 
##' @export
##'
plot_maker <- function(df, bar.chart = FALSE, plot.time = NULL, save.fn = NULL) {
  
  #the no DNA condition is still present in the data frame, but isn't necessary 
  #for the plot so it can be filtered out
  no_no_dna <- df %>%
    filter(dna != "No DNA")
  
  #if the user only wants to plot scpecific time points
  if(!is.null(plot.time)) {
    no_no_dna <- no_no_dna %>%
      filter(time %in% plot.time)
  }
  
  if(bar.chart) {
    no_no_dna2 <- no_no_dna #make a copy of the data frame so it doesn't change the original
    no_no_dna2$treatment <- factor(no_no_dna2$treatment) #the x variable has to be a factor
    #in order for the error bars to work
    plot <- ggplot(no_no_dna2, aes(x = treatment, y = mean, fill = dna)) +
      geom_bar(stat = "identity", position = position_dodge()) + 
      #no idea what "identity" is, but position_dodge() prevents the bars from stacking
      geom_errorbar(aes(ymin = mean - std, ymax = mean + std), 
                    #upper bounds and lower bounds of the error bars
                    position = position_dodge()) +
      facet_wrap(vars(time)) +
      scale_y_continuous(name = "Absorbance due to DNA at 655 nm") +
      scale_x_discrete(name = "Treatment Group") #edit so that they don't overlap
  } else {
    plot <- ggplot(no_no_dna) +
      geom_point(aes(x = treatment, y = abs.minus.bg, color = dna)) +
      # add color, and make bars narrower
      geom_errorbar(aes(x = treatment, y = abs.minus.bg, ymin = mean, ymax = mean, 
                        color = dna), width = 0.5) +
      facet_wrap(vars(time)) +
      scale_y_continuous(name = "Absorbance due to DNA at 655 nm") +
      scale_x_discrete(name = "Treatment Group") #edit so that they don't overlap
  }
  
  if(!is.null(save.fn)) {
    ggsave(save.fn, width = 6, height = 4, dpi = 300)
  }
  plot
}
