% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetGDELT.R
\name{GetGDELT}
\alias{GetGDELT}
\title{Download and subset GDELT data}
\usage{
GetGDELT(
  start_date,
  end_date = start_date,
  row_filter,
  ...,
  local_folder = tempdir(),
  max_local_mb = Inf,
  data_url_root = "http://data.gdeltproject.org/events/",
  verbose = TRUE
)
}
\arguments{
\item{start_date}{character, earliest date to include in "YYYY-MM-DD" format.}

\item{end_date}{character, latest date to include in "YYYY-MM-DD" format.}

\item{row_filter}{<data-masking> Row selection. Expressions that return a logical value, and are defined in terms of the variables in GDELT. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.}

\item{...}{<tidy-select>, Column selection. This takes the form of one or more unquoted expressions separated by commas. Variable names can be used as if they were positions in the data frame, so expressions like x:y can be used to select a range of variables.}

\item{local_folder}{character, if specified, where downloaded files will be saved.}

\item{max_local_mb}{numeric, the maximum size in MB of the downloaded files that will be retained.}

\item{data_url_root}{character, URL for the folder with GDELT data files.}

\item{verbose}{logical, if TRUE then indications of progress will be displayed_}
}
\value{
data.frame
}
\description{
Download the GDELT files necessary for a data set, import them, filter on various criteria, and return a data.frame.
}
\details{
Dates are parsed with \code{guess_datetime} in the datetimeutils package. 
The recommended format is "YYYY-MM-DD".

If \code{local_folder} is not specified then downloaded files are stored in
\code{tempdir()}. If a needed file has already been downloaded to \code{local_folder}
then this file is used instead of being downloaded. This can greatly speed up future
downloads.
}
\section{Filtering Results}{


The \code{row_filter} is passed to \code{\link[dplyr]{filter}}. This is a very flexible way to filter
the rows. It's well worth checking out the \code{\link[dplyr]{filter}} documentation.
}

\section{Selecting Columns}{


The \code{...} is passed to \code{\link[dplyr]{select}}. This is a very flexible way to choose
which columns to return. It's well worth checking out the \code{\link[dplyr]{select}} documentation.
}

\examples{
\dontrun{
df1 <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31")

df2 <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31",
                row_filter=ActionGeo_CountryCode=="US")

df3 <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31",
                row_filter=Actor2Geo_CountryCode=="RS" & NumArticles==2 & is.na(Actor1CountryCode), 
                1:5)

df4 <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31",
                row_filter=Actor2Code=="COP" | Actor2Code=="MED", 
                contains("date"), starts_with("actor"))

# Specify a local folder to store the downloaded files
df5 <- GetGDELT(start_date="1979-01-01", end_date="1979-12-31",
                row_filter=ActionGeo_CountryCode=="US",
                local_folder = "~/gdeltdata")
}
}
\references{
GDELT: Global Data on Events, Location and Tone, 1979-2013.  
Presented at the 2013 meeting of the International Studies Association
in San Francisco, CA.
\url{https://www.gdeltproject.org/}
}
\author{
\tabular{ll}{
  Stephen R. Haptonstahl \tab \email{srh@haptonstahl.org}\cr
  Thomas Scherer \tab \email{tscherer@princeton.edu}\cr
  John Beieler \tab \email{jub270@psu.edu}\cr
}
}
