pkgname <- "GDELTtools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "GDELTtools-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('GDELTtools')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("GetGdelt")
### * GetGdelt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GetGDELT
### Title: Download and subset GDELT data
### Aliases: GetGDELT

### ** Examples

## Not run: 
##D test.filter <- list(ActionGeo_ADM1Code=c("NI", "US"), ActionGeo_CountryCode="US")
##D test.results <- GetGDELT(start.date="1979-01-01", end.date="1979-12-31", filter=test.filter)
##D table(test.results$ActionGeo_ADM1Code)
##D table(test.results$ActionGeo_CountryCode
## End(Not run)

# Specify a local folder to store the downloaded files
## Not run: 
##D test.results <- GetGDELT(start.date="1979-01-01", end.date="1979-12-31",
##D                          filter=test.filter,
##D                          local.folder="c:/gdeltdata",
##D                          max.local.mb=500)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GetGdelt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NormEventCounts")
### * NormEventCounts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NormEventCounts
### Title: Scale event counts
### Aliases: NormEventCounts

### ** Examples

## Not run: 
##D GDELT.subset.data <- GetGDELT("2012-01-01", "2012-12-31", allow.wildcards=TRUE,
##D                               filter=list(ActionGeo_CountryCode=c("AF", "US"), EventCode="14*"))
##D GDELT.normed.data <- NormEventCounts(x = GDELT.subset.data,
##D                                      unit.analysis="country.year",
##D                                      var.name="protest")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NormEventCounts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NormEventCountsData")
### * NormEventCountsData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NormEventCountsData
### Title: Normalization Factors for GDELT data, 1979-2012
### Aliases: NormEventCountsData
### Keywords: datasets

### ** Examples

data(NormEventCountsData)
str(NormEventCountsData)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NormEventCountsData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
