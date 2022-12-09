#' Plate Key for Tween Experiment
#'
#' Key for plate layout for tween experiment specifying the treatment/condition
#' of each well
#'
#' @format a data frame with 72 rows and 4 columns
#' \describe{
#' \item{row}{Lettered rows of the 96 well plate, A-F}
#' \item{col}{Numbered columns of the 96 well plate, 1-12}
#' \item{dna}{The DNA treatment for the corresponding well,
#' No DNA, contains no DNA, only buffer,
#' No Target, contains proxzyme DNA, but no target sequence,
#' P2 Only, contains only proxzyme 2, with 41 bp ssDNA target
#' ssDNA T, contains both proxzymes, with 41 bp ssDNA target}
#' \item{treatment}{the buffer treatment/reaction condition for the corresponding well,
#' water, contained no additional buffer components (TE buffer, *Salt, 5mg/50ml Hemin, DMSO),
#' buffer, contains PCR buffer in addition to standard buffer components
#' 0.05%, 0.005%, 0.0005%, 0.00005%, concentration of Tween added to buffer}
#' }
#'
#'
"tween_package_key"
