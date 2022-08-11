#' A global var.
#' @description A global var for the package
#' @keywords internal
webtools <- new.env(parent=emptyenv())
webtools$ran <- 0

#' A global var.
#' @description A global var for the package
#' @keywords internal
scripts <- ''

#' Get a random Seinfeld quote
#' @description Get a random Seinfeld quote. Simple.
#' @usage yada()
#' @return A str quote.
#' @examples yada()
#' @export
yada <- function() {
  yada <- scripts
  ran <- stats::runif(1, 1, length(yada$dialogue))
  seas <- yada$season[[ran]]
  epi <- yada$episode[[ran]]
  charact <- yada$character[[ran]]
  dialog <- yada$dialogue[[ran]]
  both <- paste0('S', seas, '_E', epi, ', ', charact, ': ', dialog)
  return(both)
}

#' Get two random Seinfeld quotes in a row (guessing game)
#' @description Get a random Seinfeld quote, followed by the very next line.
#' Designed to be played in a terminal/console, otherwise it just returns the
#' consecutive quotes.
#' @usage yadayada()
#' @return A vector of the two quotes
#' @examples yadayada()
#' @export
yadayada <- function() {
  yada <- scripts
  webtools$ran <- as.integer(stats::runif(1, 1, length(yada$dialogue)-1))
  yada_1 <- yadasay(yada)
  if(interactive()) {
    writeLines(crayon::blue(yada_1))
    readline(prompt="Guess the line that follows ([enter] to continue): ")
  }
  webtools$ran <- webtools$ran + 1
  yada_2 <- yadasay(yada)
  if(interactive()) {
    writeLines(crayon::blue(yada_2))
    return('Did you guess correctly?')
  }
  return(c(yada_1, yada_2))
}

#' A global func.
#' @description A global func for the package
#' @keywords internal
yadasay <- function(scr) {
  seas <- scr$season[[webtools$ran]]
  epi <- scr$episode[[webtools$ran]]
  charact <- scr$character[[webtools$ran]]
  dialog <- scr$dialogue[[webtools$ran]]
  both <- paste0('S', seas, '_E', epi, ', ', charact, ': ', dialog)
  return(both)
}
