\name{FilterGdeltDataframe}
\alias{FilterGdeltDataframe}
\title{Filter a data.frame by field name}
\usage{
  FilterGdeltDataframe(x, filter, allow.wildcards = FALSE,
    use.regex = FALSE)
}
\arguments{
  \item{x}{data.frame, imported from a GDELT zip file.}

  \item{filter}{list, see details.}

  \item{allow.wildcards}{logical, Do you want to use
  wildcards in \code{filter}?}

  \item{use.regex}{logical, Do you want to use regular
  expressions in \code{filter}?}
}
\value{
  data.frame, filtered on selected values
}
\description{
  This is not specific to Gdelt
}
\examples{
\dontrun{df.filtered <- GdeltZipToDataframe(df, filter=list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US"))}
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\references{
  \url{http://gdelt.utdallas.edu/}
}

