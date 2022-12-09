##' function to read and pivot raw proxzyme absorbance data
##'
##' @param raw.fn a csv file containing raw absorbance data from a 96 well plate
##' @param key.fn a csv file with the treatments in each well
##' @description This function will read the raw data, convert it to long-form data,
##' read the plate key, and join the two in order to assign each absorbance value a DNA treatment
##' and a buffer treatment.
##' @details All excel formats must be in csv format and raw data file should have no more than
##' 14 columns, with the first column being the time at which the reading was taken and the
##' second column being the lettered rows of the plate. The plate key must be made in Excel
##' in long form in order to have separate columns for DNA treatment and buffer/rxn condition.
##'
##' @export
##'
##'
preprocess_proxzyme <- function(raw.fn, key.fn) {
  # has to be in specific format specific to the package folder
  raw_data <- read_csv(raw.fn) %>%
    pivot_longer(cols = 3:14, #as long as column 1 is time and column 2 is row letters, this will work
                 names_to = "column.number",
                 values_to = "absorbance")

  plate_key <- read_csv(key.fn) %>%
    mutate(col = as.character(col)) #this is done so that the column numbers are treated as labels instead of values

  # to keep all values in the original raw data, use left join to combine with plate key
  # if the plate key is made in the same way as the original, this doesn't really matter as
  # there will be a DNA and  buffer label for each well that was used to get the raw data
  new_data <- left_join(raw_data, plate_key, by = c("row" = "row", "column.number" = "col"))

  new_data
}
