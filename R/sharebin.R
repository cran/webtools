#' "Paste" a file for web sharing
#' @description "Paste" a TEXT based file to a "bin" on the web for sharing.
#' ~~(DO NOT POST SENSITIVE DATA!)~~.
#' @param file_name (Optional) - `sharebin()` will paste the file in the current
#' active editor window. Otherwise `sharebin('file.suff')`, where 'file.suff'
#' is a file name that must exist in the working dir (getwd()) and is TEXT based
#'  (.R, .Rmd, .csv, or any other code or text file)
#'  ~~(DO NOT POST SENSITIVE DATA!)~~
#' @usage sharebin(file_name)
#' @return A URL you can share.
#' @export
sharebin <- function(file_name='-1') {

  err <- FALSE

  if(file_name != '-1') {
    file_name <- paste0(getwd(), .Platform$file.sep, file_name)
  } else {
    tryCatch(
      file_name <- rstudioapi::getSourceEditorContext()$path,
         error = function(e) {
           err <<- TRUE
           writeLines(paste0(crayon::red("NOTICE: "),
                             "`sharebin()` can only be called from RStudio.\n",
                             "Use `sharebin('file.suff')` - where 'file.suff' ",
                             "exists under the working dir. \n",
                             "[ Useful: `list.files()` and `setwd('dir')` ]"))
       }
    )
  }

  if(err == FALSE) {
    t <- readr::read_file(file_name)
    r <- RCurl::postForm('https://clbin.com', 'clbin'=t)
    r <- stringr::str_replace_all(r[[1]], "[\r\n]" , "")
    writeLines(crayon::blue$underline(r))
    return(r)
  }

}
