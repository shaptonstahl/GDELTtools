\name{GdeltZipToDataframe}
\alias{GdeltZipToDataframe}
\title{Reads a ZIPed GDELT file into a data.frame}
\usage{
  GdeltZipToDataframe(f)
}
\arguments{
  \item{f}{character, full path and name of zip file}
}
\value{
  data.frame
}
\description{
  Given a local GDELT .zip file returns a data.frame with
  all of the data.
}
\examples{
\dontrun{my.df <- GdeltZipToDataframe("c:/gdeltdata/1979.zip")}
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\references{
  \url{http://gdelt.utdallas.edu/}
}

