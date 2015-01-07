\name{downloadedYearsISD}
\alias{downloadedYearsISD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
downloadedYearsISD(example = F, db.table = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{example}{
%%     ~~Describe \code{example} here~~
}
  \item{db.table}{
%%     ~~Describe \code{db.table} here~~
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
function (example = F, db.table = F) 
{
    if (example == T) {
        db.folder <- normalizePath(system.file("database", package = "ISDr"))
        connection <- dbConnect(dbDriver("SQLite", max.con = 25), 
            dbname = paste0(db.folder, "/exampleISDdatabase"))
    }
    else {
        db.folder <- normalizePath(system.file("database", package = "ISDr"))
        connection <- dbConnect(dbDriver("SQLite", max.con = 25), 
            dbname = paste0(db.folder, "/ISDdatabase"))
    }
    downloads <- dbReadTable(connection, "Downloads")
    downloads.monitors <- sort(unique(downloads$ID))
    downloads.years <- sort(unique(downloads$year))
    downloads.matrix <- matrix(rep("NA", length(downloads.monitors) * 
        length(downloads.years)), ncol = length(downloads.years), 
        dimnames = list(downloads.monitors, downloads.years))
    insertYes <- function(column.name, row.name) {
        downloads.matrix[[as.character(row.name), as.character(column.name)]] <<- "Yes"
    }
    invisible(mapply(FUN = insertYes, column.name = downloads$year, 
        row.name = downloads$ID))
    downloads$nrows[downloads$nrows == 0] <- 1
    downloads$nrows[downloads$nrows > 365 * 24 * 3] <- 365 * 
        24 * 3
    downloads[, "nrows"] <- (downloads$nrows/(365 * 24 * 3)) * 
        100
    names(downloads)[3] <- "Percent_Complete"
    p1 <- ggplot(downloads, aes(x = as.factor(year), y = as.factor(ID))) + 
        geom_tile(aes(fill = Percent_Complete)) + scale_fill_gradient2(low = "white", 
        high = "blue") + xlab("Downloaded Years") + ylab("Downloaded Stations")
    print(p1)
    dbDisconnect(connection)
    if (db.table == F) {
        return(downloads.matrix)
    }
    else {
        return(downloads)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
