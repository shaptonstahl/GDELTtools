\name{NormEventCounts}
\alias{NormEventCounts}
\title{Scale event counts}
\usage{
  NormEventCounts(data, include.events, condition,
    unit.analysis, var.name)
}
\arguments{
  \item{data}{data.frame, a GDELT dataframe.}

  \item{include.events}{character, vector with CAMEO event
  codes to include in the count.}

  \item{condition}{list, any additional conditions for
  coding the event count, such as, e.g., restricting to
  particular actor codes. this needs to refer to dataframe
  declared above.}

  \item{unit.analysis}{character, default is country.day;
  other options: country.day, country.month, country.year,
  day, month, year}

  \item{var.name,}{character, base name for the new count
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
  dataset where date is of class \sQuote{date}.
}
\examples{
\dontrun{
new.data.frame <- NormEventCounts(data = GDELT.subset.data,
  include.events = c(141, 145, 140, 143, 142, 144, 1412, 1413, 1411, 1431, 1414),
  condition = (GDELT.subset.data $ActionGeo_CountryCode == "AF"),
  unit.analysis = "country.year",
  var.name = "protest")}
}
\author{
  \tabular{ll}{ Timo Thoms \tab
  \email{othoms@princeton.edu}\cr Stephen R. Haptonstahl
  \tab \email{srh@haptonstahl.org}\cr }
}
\references{
  GDELT: Global Data on Events, Location and Tone,
  1979-2012. Presented at the 2013 meeting of the
  International Studies Association in San Francisco, CA.
  \url{http://gdelt.utdallas.edu/}
}

