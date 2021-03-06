% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ASG.R
\name{ASG}
\alias{ASG}
\title{ASG Merchandising}
\usage{
ASG(Stand, Plot, Tree, SPP, DBH, HT, GS = "USG")
}
\arguments{
\item{Stand}{The Unique Stand Identification Number}

\item{Plot}{The Unique Plot Identification Number}

\item{Tree}{The Unique Tree Identification Number}

\item{SPP}{The species identification using FVS codes: ex 'RO' = Red Oak}

\item{DBH}{Diameter at breast height in cm}

\item{HT}{Height of tree in meters}

\item{GS}{Record the growing stock of the tree as ASG or USG, defaults to USG}
}
\value{
Metric, with the exception of Board Feet which is returned with imperial values.
\subsection{}{

data.frame(Stand, Plot, Tree, Method, SPP, LogLength, LogCount, Saw.BF.ASG, Saw.Vol.ASG,
Pulp.Vol.ASG, Cull.Vol.ASG, Total.Vol, Merch.Vol, Percent.Sawlog.ASG)
}
}
\description{
This function calculates the total tree volume, merchantable volume,
sawlog volume, pulp volume, cull volume, and saw board feet for trees
using Acceptable or Unacceptable Sawlog Grade Observations Inputs should be in metric.
}
\details{
ASG is any tree tree with at least one 8 foot sawlog grade section.
USG trees have no sawlogs and are marked as either Pulp or Cull.

ASG/USG can have multiple definitions, but this function assumes all
portions of the tree to be sawlog to minimum outside bark merchantable diameter, pulp
to minimum outside bark merchantable diameter, and cull for the remainder for all 'ASG'
trees. All USG trees will be calculated as Pulp or CULL.
\subsection{All trees default to USG when no value is input.}{

Volumes determined using Kozak Taper Equations and Smalians Volume Formula.
Merch diameters establish by the MerchDiam function.

Sawlog board feet is estimated using the international 1/4 inch rule. The sawlog portion of the
stem is broken into 2.4384m sections and the international 1/4 inch rule is applied
to each section. If the final section is longer than 2.4384m but smaller than 4.8768m
then that entire length will be used as the final log for calculating board feet.

df <- df \%>\% rownames_to_column()
\%>\% gather(variable, value, -rowname) \%>\% spread(rowname, value)
is a useful pipe for unnesting the lists into dataframe when used with mapply.
}
}
\examples{
ASG(1, 1, 1, 'RS', 30, 14, 'ASG')
ASG(1, 1, 2, 'RO', 25, 12, 'USG')
ASG(1, 1, 3, 'SM', 40, 18)

}
\seealso{
\link{KozakTreeVol}

\link{KozakTaper}

\link{MerchDiam}

Other Merchandising Functions: 
\code{\link{Form.Risk}()},
\code{\link{MerchHT}()},
\code{\link{Sawlog.Likelihood}()},
\code{\link{Stick.Cruise}()},
\code{\link{ValueEstimate}()}
}
\author{
Ryan Smith
}
\concept{Merchandising Functions}
