#' Duckduckgo Instant-search
#' @description Perform a DDG Instant search on a query term.
#' @param q The search string
#' @param max_rel (optional) Max. number of related results (default 3), or the
#' number of results returned, which ever is smaller
#' @param get_result (optional) Return the first result (either the `Instant
#' Result`` or, if none, the first related result)
#' @usage ddg(q, max_rel, get_result)
#' @examples ddg('ngrams')
#' @examples ddg('ngrams', 2)
#' @examples ddg('ngrams', get_result=TRUE)
#' @return A "safe" search URL, or the first result if get_result=TRUE passed
#' @export
ddg <- function(q, max_rel=3, get_result=FALSE) {

  q <- paste0(q) # String it
  url <- paste0('https://api.duckduckgo.com/?format=json&q=', q)
  url_safe <- paste0('https://api.duckduckgo.com/?q=', q)
  mydata <- jsonlite::fromJSON(url)
  inst_txt <- mydata$AbstractText
  header <- paste0(crayon::bgYellow('\nDDG >>'), ' Instant Result:\n')
  writeLines(header)

  qry.out <- crayon::blue$bold(q)
  result <- ifelse(inst_txt=='', '(No *Instant Result* !)', inst_txt)
  writeLines(paste0(qry.out, ' - ', result))

  writeLines(paste0(crayon::bgYellow('\n>>'), ' Related topics:\n'))

  i <- 1
  for(u in mydata$RelatedTopics$FirstURL) {
    if(i <= max_rel && !is.na(u) && stringr::str_starts(u, 'http')) {
      writeLines(mydata$RelatedTopics[['Text']][[i]])
      writeLines(c(crayon::italic(u), ''))
      i <- i + 1
    }
  }

  if(i == 1) {
    writeLines('(No *Related Topics* !)\n')
  } else if(inst_txt=='') {
    result <- mydata$RelatedTopics[['Text']][[1]]
  }

  return(ifelse(get_result==TRUE, result, url_safe))

}

