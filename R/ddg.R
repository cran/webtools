#' Duckduckgo Instant-search
#' @description Perform a DDG Instant search on a query term.
#' @param x The search string
#' @param max_rel (optional) Max. number of related results (default 3), or the
#' number of results returned, which ever is smaller
#' @usage ddg(x, max_rel)
#' @examples ddg('ngrams')
#' @examples ddg('ngrams', 2)
#' @return The search URL used
#' @export
ddg <- function(x, max_rel=3) {

  q <- paste0(x) # String it
  url <- paste0('https://api.duckduckgo.com/?format=json&q=', q)
  mydata <- jsonlite::fromJSON(url)
  inst_txt <- mydata$AbstractText
  header <- paste0(crayon::bgYellow('\nDDG >>'), ' Instant Result:\n')
  writeLines(header)

  qry.out <- crayon::blue$bold(q)

  if(inst_txt == '') {
    writeLines(paste0(qry.out, ' - (No *Instant Result* !)'))
  } else {
    result <- paste0(qry.out, ' - ', inst_txt)
    writeLines(result)
  }

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
  }

  return(url)

}
