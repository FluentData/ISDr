\name{stationsISDdb}
\alias{stationsISDdb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stationsISDdb()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    if (example == T) {
        db.folder <- normalizePath(system.file("database", package = "ISDr"))
        connect <- dbConnect(dbDriver("SQLite", max.con = 25), 
            dbname = paste0(db.folder, "/exampleISDdatabase"))
    }
    else {
        db.folder <- normalizePath(system.file("database", package = "ISDr"))
        connect <- dbConnect(dbDriver("SQLite", max.con = 25), 
            dbname = paste0(db.folder, "/ISDdatabase"))
    }
    stations <- dbReadTable(connect, "Stations")
    dbDisconnect(connect)
    if (nrow(stations) > 0) {
        stations[, 12:13] = apply(stations[, 12:13], 2, function(x) as.integer(x))
    }
    stations
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
