\name{GetGDELT}
\alias{GetGDELT}
\title{Download and subset GDELT data}
\usage{
  GetGDELT(start.date, end.date = start.date, filter,
    local.folder = tempdir(), max.local.mb = Inf,
    allow.wildcards = FALSE, use.regex = FALSE,
    historical.url.root = "http://gdelt.umn.edu/data/backfiles/",
    daily.url.root = "http://gdelt.umn.edu/data/dailyupdates/",
    verbose = TRUE)
}
\arguments{
  \item{start.date}{character, just about any
  human-readable form of the earliest date to include.}

  \item{end.date}{character, just about any human-readable
  form of the latest date to include.}

  \item{filter}{list, named list encoding the values to
  include for specified fields. See Details.}

  \item{local.folder}{character, if specified, where
  downloaded files will be saved.}

  \item{max.local.mb}{numeric, the maximum size in MB of
  the downloaded files that will be retained.}

  \item{allow.wildcards}{logical, must be TRUE to use * in
  \code{filter} to specify 'any character(s)'.}

  \item{use.regex}{logical, if TRUE then \code{filter} will
  be processed as a \code{\link{regular expression}}.}

  \item{historical.url.root}{character, URL from which
  historical files will be downloaded.}

  \item{daily.url.root}{character, URL from which daily
  files will be downloaded.}

  \item{verbose}{logical, if TRUE then indications of
  progress will be displayed.}
}
\value{
  data.frame
}
\description{
  Download the GDELT files necessary for a data set, import
  them, filter on various crieteria, and return a
  data.frame.
}
\details{
  If \code{local.folder} is not specified then downloaded
  files are stored in \code{tempdir()}. If a needed file
  has already been downloaded to \code{local.folder} then
  this file is used instead of being downloaded. This can
  greatly speed up future

  Dates are parsed with \code{dateParse} in the TimeWarp
  package. Years must be given with four digits.
}
\section{Filtering Results}{
  This is how you write the \code{filter}.
}
\examples{
\dontrun{
test.filter <- list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US")
test.results <- GetGDELT(start.date="1979-01-01", end.date="1979-12-31",
  filter=test.filter)
table(test.results$ActionGeo_ADM1Code)
table(test.results$ActionGeo_CountryCode)}

# Specify a local folder to store the downloaded files
\dontrun{
test.results <- GetGDELT(start.date="1979-01-01", end.date="1979-12-31",
                         filter=test.filter,
                         local.folder="~/gdeltdata",
                         max.local.mb=500)}
}
\author{
  \tabular{ll}{ Stephen R. Haptonstahl \tab
  \email{srh@haptonstahl.org}\cr Thomas Scherer \tab
  \email{tscherer@princeton.edu}\cr John Beieler \tab
  \email{jub270@psu.edu}\cr }
}
\references{
  GDELT: Global Data on Events, Location and Tone,
  1979-2012. Presented at the 2013 meeting of the
  International Studies Association in San Francisco, CA.
  \url{http://gdelt.utdallas.edu/}
}

