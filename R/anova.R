##' function to run ANOVA test on proxzyme data
##'
##' @param df a dataframe of data containing each condition minus background, avg, and sd
##' should be the same df used in plot_maker
##' @param stat.time anova test of data at that time point
##' @param stat.dna input for the dna condition of interest, default is "ssDNA T",
##' for another condition, name must be in "" in input.
##' @description This function filters the processed data by time and dna condition,
##' creates a linear model of absorbance with respect to treatment, and then runs aov test.
##'
##' @export
##'
anova <- function(df, stat.time = NULL, stat.dna = "ssDNA T") {

  timepoint <- df %>%
    filter(time == stat.time, dna == stat.dna)

  mod <- lm(absorbance ~ treatment, data = timepoint)
  summary(mod)
  tuk <- TukeyHSD(aov(mod))
  print(tuk)

}
