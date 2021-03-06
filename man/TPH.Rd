% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TPH.R
\name{TPH}
\alias{TPH}
\title{Trees Per Hectare}
\usage{
TPH(Stand, Plot, DBH, EXPF, CUTOFF = TRUE)
}
\arguments{
\item{Stand}{The unique Stand ID for each Plot.}

\item{Plot}{The unique Plot ID for each Plot.}

\item{DBH}{Diameter at breast height in cm.}

\item{EXPF}{Expansion factor for each tree.}

\item{CUTOFF}{The minimum diameter in cm of a tree to be included in the analysis. If left blank the default CUTOFF is 10cm.}
}
\value{
Returns a vector of the length N with the trees per hectare for each plot.
}
\description{
This function calculates the Trees Per Hectare for each plot within your data set. The minimum diameter of trees included
in the analysis is 10cm. This diameter can be changed using the CUTOFF parameter in this function. You can use cutoff to
either include trees smaller than 10cm or to identify how many larger trees are in your stand.
}
\details{
This function requires that you calculate the Expansion Factor for each tree in
your inventory. See the EXP.F function if you need to do this.
\subsection{}{
}
}
\section{TPH(Stand, Plot, DBH, EXPF, 20) <- this is an example of a CUTOFF that only includes trees >= 20cm.}{
\subsection{}{

Trees Per Hectare is a dependency for a number of other functions that may be useful in your analysis, so
using a standard naming convention, while always good practice, will help when applying to additional functions.
(ex: TPH, TreePH).
}
}

\examples{
Stand <- c(1,1,1,1,1,1,1,1,1)
Plot  <- c(1,1,1,1,1,2,2,2,2)
DBH   <- c(5, 6, 33, 23, 11, 22, 4, 17, 40)
EXPF  <- c(15, 15, 15, 15, 15, 15, 15, 15, 15)
TPH(Stand, Plot, DBH, EXPF)
TPH(Stand, Plot, DBH, EXPF, 7)
TPH(Stand, Plot, DBH, EXPF, 20)

}
\seealso{
\link{EXP.F}

Other Plot Level Functions: 
\code{\link{BA.Larger.Trees}()},
\code{\link{BAPH}()},
\code{\link{CCF.Larger}()},
\code{\link{CrownCompF}()},
\code{\link{HeightPredict}()},
\code{\link{RD}()},
\code{\link{SDI.Max}()},
\code{\link{SDI.Plot}()},
\code{\link{TallestTrees}()},
\code{\link{Unique.ID}()}
}
\author{
Ryan Smith
}
\concept{Plot Level Functions}
