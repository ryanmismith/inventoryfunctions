% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KozakTaper.R
\name{KozakTaper}
\alias{KozakTaper}
\title{Kozak Taper Equation}
\usage{
KozakTaper(Bark, SPP, DHT, DBH, HT, Planted = FALSE)
}
\arguments{
\item{Bark}{specify if you want measurement inside or outside bark: 'ob' = outside, 'ib' = inside}

\item{SPP}{Species: use FVS species codes: example 'RO' - Red Oak, 'WS' = White Spruce}

\item{DHT}{Diameter at Height in cm where diameter measurment is desired}

\item{DBH}{Diameter Breast Height in cm}

\item{HT}{Tree Height in meters}

\item{Planted}{specify if the tree is planted, specify as TRUE or FALSE. Defaults to FALSE.}
}
\description{
This function allows you to calculate the diameter at any point on tree
for common tree species found in the northeastern and acadian forests.
All measurements must be entered as metric. Function requires
you to gives the specified height on tree for which you want diameter calculated.
}
\examples{
KozakTaper('ob', 'RO', 12, 40, 20, FALSE)
KozakTaper('ib', 'RS', 8, 28, 16, TRUE)

}
\references{
Kozak, A. (2004). My last words on taper equations. The Forestry Chronicle,
80(4), 507–515. https://doi.org/10.5558/tfc80507-4

MacFarlane, D. W., & Weiskittel, A. R. (2016). A new method for capturing stem taper
variation for trees of diverse morphological types. Canadian Journal of Forest Research,
46(6), 804–815. https://doi.org/10.1139/cjfr-2016-0018
}
