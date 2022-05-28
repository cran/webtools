#' Get a unique GUID
#' @description Get a unique GUID. Simple. See: `ddg('guid')`.
#' @usage guid()
#' @return A unique GUID.
#' @examples guid()
#' @export
guid <- function() {
  guid <- RCurl::httpGET('http://givemeguid.com')
  writeLines(crayon::blue(guid[[1]]))
  return(guid[[1]])
}
