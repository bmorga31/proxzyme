##' function to subtract background based on whether there is a reference time
##' 
##' @param df a dataframe of processed proxzyme data
##' @param ref.time time at which the rxn has progressed the most from the previous time point
##' @description This function uses an if statement to choose which process to run based on 
##' whether or not a reference time is assigned in the input. Either process will subtract the 
##' No DNA condition from each treatment to remove background reaction and give absorbance due to 
##' DNA.
##' @details The reference time must be determined beforehand by looking at the change in absorbance
##' between each timepoint. If there is no reference time, this function will subtract the average
##' of the No DNA condition for each treatment at each time from each replicate. When a reference 
##' time is assigned, the function will subtract the average No DNA condition for each treatment at the reference time
##' from each replicate at every other time.
##' The output of that decision process is mutated to add the average and standard deviation 
##' of each time, DNA, and treatment group to the data frame. This is done to prepare the data for 
##' the next step in the analysis process--visualization.
##' @export 
##'
minus_bg <- function(df, ref.time = NULL) {
  
  if(is.null(ref.time)) {
    blank_corrected<- df %>%
      group_by(time, treatment) %>% 
      mutate(abs.minus.bg = absorbance - mean(absorbance[dna == "No DNA"]))
    
  } else {
    #add code to check that ref.time passed in is actually in the data frame
    ref_points <- df %>%
      filter(time == ref.time, dna == "No DNA") %>%
      group_by(treatment) %>%
      summarize(ref.mean = mean(absorbance))
    blank_corrected <- left_join(df, ref_points, by = ("treatment" = "treatment")) %>%
      mutate(abs.minus.bg = absorbance - ref.mean)
  } 
  
  abs_minus_bg <- blank_corrected %>%
    group_by(time, dna, treatment) %>% 
    mutate(mean = mean(abs.minus.bg), std = sd(absorbance))
}
