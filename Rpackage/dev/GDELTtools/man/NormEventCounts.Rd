\name{NormEventCounts}
\alias{NormEventCounts}
\title{Scale event counts}
\usage{
  NormEventCounts(x, unit.analysis,
    var.name = "norming_vars")
}
\arguments{
  \item{x}{data.frame, a GDELT data.frame.}

  \item{unit.analysis}{character, default is country.day;
  other options: country.day, country.month, country.year,
  day, month, year}

  \item{var.name}{character, base name for the new count
  variables}
}
\value{
  data.frame
}
\description{
  Scale event counts based on the unit of analysis.
}
\details{
  For \code{unit.analysis}, day and country-day put out a
  data set where date is of class \sQuote{date}.  All other
  options put out a data set where year or month is integer
  (this needs to be unified in a later version).
}
\examples{
\dontrun{
GDELT.subset.data <- GetGDELT("2013-06-01", "2013-06-07", allow.wildcards=TRUE,
  filter=list(ActionGeo_CountryCode=c("AF", "US"), EventCode="14*"),
  local.folder="~/gdeltdata")
GDELT.normed.data <- NormEventCounts(x = GDELT.subset.data,
  unit.analysis="day",
  var.name="protest")}
}
\author{
  \tabular{ll}{ Oskar N.T. Thoms \tab
  \email{othoms@princeton.edu}\cr Stephen R. Haptonstahl
  \tab \email{srh@haptonstahl.org}\cr John Beieler \tab
  \email{jub270@psu.edu}\cr }
}
\references{
  GDELT: Global Data on Events, Location and Tone,
  1979-2012. Presented at the 2013 meeting of the
  International Studies Association in San Francisco, CA.
  \url{http://gdelt.utdallas.edu/}
}

