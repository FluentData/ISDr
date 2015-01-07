\name{summaryISDdb}
\alias{summaryISDdb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summaryISDdb(example = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{example}{
%%     ~~Describe \code{example} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (example = F) 
{
    if (example == T) {
        db.folder <- normalizePath(system.file("database", package = "ISDr"))
        db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
            dbname = paste0(db.folder, "\\exampleISDdatabase"))
        db.info <- file.info(paste0(db.folder, "\\exampleISDdatabase"))
        stations.df <- stationsISDdb(example = T)
        download.years <- downloadedYearsISD(example = T)
    }
    else {
        db.folder <- normalizePath(system.file("database", package = "ISDr"))
        db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
            dbname = paste0(db.folder, "\\ISDdatabase"))
        db.info <- file.info(paste0(db.folder, "\\ISDdatabase"))
        stations.df <- stationsISDdb()
        download.years <- downloadedYearsISD()
    }
    db.matrix <- matrix(c(row.names(db.info), humanReadable(db.info$size)), 
        ncol = 1, dimnames = list(c("Location", "Size"), "ISD Database"))
    cat("\n")
    print(db.matrix, quote = F)
    cat("\n\n\t\t\tStations in Database:\n\n")
    print(as.matrix(stations.df), quote = F)
    cat("\n\n\t\t\t\t\tStation-Years Downloaded:\n\n")
    print(download.years, quote = F)
    invisible(dbDisconnect(db.con))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
