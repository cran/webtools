.onLoad <- function(libname, pkgname) {
  assign("webtools", NULL, envir=new.env(parent=emptyenv()))
}
