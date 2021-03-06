% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPP.func.R
\name{SPP.func}
\alias{SPP.func}
\title{Returns Species SPPtype, Shade Tolerance, Standard Gravity, wd, and Waterlog Values}
\usage{
SPP.func(SPP)
}
\arguments{
\item{SPP}{Species: use FVS species codes}
}
\value{
This function returns a vector of length 5.
SPtype 1, : shade 2, : sg 3, : wd 4, : waterlog 5,
}
\description{
This function returns species values which are dependencies for other
more complex functions. These values include whether a species is a hardwood or softwood,
a species standard gravity, and coefficients for shade, drought, waterlog, and wd.
Some of these coefficients may be valuable on their own, but they are grouped together to
simplify function calls for other more complex functions in this inventory package.
}
\examples{
x <- c("BF", "BF", "RO", "RS", "RO")
sppinfo <- sapply(x, SPP.func)
HW_SW <- as.vector(sppinfo[1,])

SPP.func("BA")

}
