##' function to plot proxzyme data
##'
##' @param df a dataframe of data containing each condition minus background, avg, and sd
##' @param bar.chart if you want the bar chart, bar.chart = TRUE, leave blank for scatterplot
##' @param plot.time the time(s) that will be plotted by the function
##' @param save.fn the name of the plot image to be saved
##' @description This function first filters a dataframe to remove all No DNA values, then
##' uses an if statement to let the user determine which time(s) will be plotted, then
##' uses another if statement to let the user determine if a bar chart or a scatterplot should
##' be made, and the last is statement give the user the option to assign a file name for the
##' plot to be saved.
##' @details
##' bar.chart must be TRUE in order to generate a bar chart, default is FALSE and will
##' generate a scatterplot.
##' plot.time must be either a single numerical timepoint or in the form c(#, #, ...) for
##' multiple timepoints, the default is NULL and will generate a plot with all timepoints.
##' print.plot must be FALSE if the user does not wish to have the plot printed. The default
##' is TRUE.
##' save.fn mus be the name of the file, including the file type (.png, .jpg, etc.). This
##' should contain the path for the file if the user wants it to go to a different directory.
##' The default is NULL, so if no file name is given, the plot will not be saved.
##'
##'
##' @export
##'
plot_maker <- function(df, bar.chart = FALSE, plot.time = NULL,
                       print.plot = TRUE, save.fn = NULL) {

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
      scale_x_discrete(name = "Treatment Group") +
      theme(axis.text.x = element_text(angle = 45, hjust=1))

  } else {
    plot <- ggplot(no_no_dna) +
      geom_point(aes(x = treatment, y = abs.minus.bg, color = dna)) +
      geom_errorbar(aes(x = treatment, y = abs.minus.bg, ymin = mean, ymax = mean,
                        color = dna), width = 0.5) +
      facet_wrap(vars(time)) +
      scale_y_continuous(name = "Absorbance due to DNA at 655 nm") +
      scale_x_discrete(name = "Treatment Group") +
      theme(axis.text.x = element_text(angle = 45, hjust=1))

  }

  if(print.plot) {
    print(plot)

  }

  if(!is.null(save.fn)) {
    ggsave(save.fn, width = 10, height = 10, dpi = 300)

  }
  plot
}
