% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dsearch.R
\name{dinfo}
\alias{dinfo}
\title{Get dataset info on a package (all of CRAN) and fetch/load the dataset/s}
\usage{
dinfo(pkg)
}
\arguments{
\item{pkg}{The package name}
}
\value{
A tibble of datasets belonging to the package.
}
\description{
Get dataset info on CRAN packages with datasets, installed
locally or not. And fetch the datasets. Use dsearch('query') to find
packages with datasets.
Note: file types may vary, use the full file name of the dataset you
require, as displayed. Package & dataset names are case-sensitive.
}
\examples{
dinfo('TSrepr')
}
