\name{GDELTtools}
\alias{GDELTtools}
\docType{package}
\title{
Download, slice, and normalize GDELT data
}
\description{
The GDELT data set is over 60 GB now and growing 100 MB a month. 
The number of source articles has increased over time and unevenly across countries.
This package makes it easy to download a subset of that data, then normalize
that data to facilitate valid timeseries analysis.
}
\details{
\tabular{ll}{
Package: \tab GDELTtools\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2014-02-17\cr
License: \tab MIT + file LICENSE\cr
}
\code{\link{GetGDELT}} is used to download and subset data.

\code{\link{NormEventCounts}} takes the output from \code{GetGDELT} and normalizes the 
counts appropriately for conducting time series analysis.
}
\author{
  \tabular{ll}{
    Stephen R. Haptonstahl \tab \email{srh@haptonstahl.org}\cr
    Thomas Scherer \tab \email{tscherer@princeton.edu}\cr
    Oskar N.T. Thoms \tab \email{othoms@princeton.edu}\cr
    Patrick Wheatley \tab \email{pvwheatley@gmail.com}\cr
  }

Maintainer: Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\references{
  GDELT: Global Data on Events, Location and Tone,
  1979-2012. Presented at the 2013 meeting of the
  International Studies Association in San Francisco, CA.
  \url{http://gdelt.utdallas.edu/}
}
\keyword{ ts }
\keyword{ survival }
\keyword{ spatial }
