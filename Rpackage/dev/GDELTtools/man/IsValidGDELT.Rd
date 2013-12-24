\name{IsValidGDELT}
\alias{IsValidGDELT}
\title{Verify the integrity of a GDELT data file}
\usage{
  IsValidGDELT(f, local.folder)
}
\arguments{
  \item{f}{character, file name of GDELT file to be
  validated.}

  \item{local.folder}{character, path to the file to be
  validated.}
}
\value{
  logical, TRUE if the file is intact.
}
\description{
  Compares the MD5 hash of a downloaded file to the known
  hash provided on the server.
}
\examples{
\dontrun{
IsValidGDELT("1979.zip", "c:/gdeltdata")}
}
\author{
  \tabular{ll}{ Stephen R. Haptonstahl \tab
  \email{srh@haptonstahl.org}\cr }
}
\references{
  GDELT: Global Data on Events, Location and Tone,
  1979-2012. Presented at the 2013 meeting of the
  International Studies Association in San Francisco, CA.
  \url{http://gdelt.utdallas.edu/}
}

