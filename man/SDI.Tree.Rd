% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SDI.Tree.R
\name{SDI.Tree}
\alias{SDI.Tree}
\title{Single Tree SDI Function}
\usage{
SDI.Tree(DBH)
}
\arguments{
\item{DBH}{Diameter at Breast Height}
}
\description{
This function computes the SDI value of a single tree with
no regard for its expansion factor or the influence of the tree
on a plot or stand level.
}
\details{
This internal function is used by the SDI.Plot function to calculate Stand Density Index values
using the summation method.
}
\examples{

SDI.Tree(25)

}
\references{
Woodall, C. W., Miles, P. D., & Vissage, J. S. (2005). Determining maximum stand density index
in mixed species stands for strategic-scale stocking assessments.
Forest Ecology and Management, 216(1–3), 367–377. https://doi.org/10.1016/j.foreco.2005.05.050
}
\author{
Ryan Smith
}
\keyword{internal}
