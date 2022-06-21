#' Get useful info on all countries
#' @description Get a dataframe of useful information on all countries such as
#' FIFA codes, Dial codes, ISO codes, currency, official name, and a whole lot
#' more. See https://datahub.io/core/country-codes
#' @usage countries()
#' @return A dataframe.
#' @export
countries <- function() {
  c <- read.csv(url("https://datahub.io/core/country-codes/r/country-codes.csv"))
  return(c)
}
