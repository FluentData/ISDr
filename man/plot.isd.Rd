\name{plot.isd}
\alias{plot.isd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.isd(isd.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{isd.object}{
%%     ~~Describe \code{isd.object} here~~
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
function (isd.object) 
{
    cat("\n\t\tPlease wait. This may take a while...\n")
    attribs <- attributes(isd.object)[4:11]
    shiny2.folder <- normalizePath(system.file("shiny2", package = "ISDr"))
    save(attribs, file = paste0(shiny2.folder, "\\attributes.Rdata"))
    if (attr(isd.object, "period") != "daily") {
        isd.object[, "hour"] <- paste0(sprintf("\%02d", as.numeric(isd.object$hour)), 
            ":", sprintf("\%02d", as.numeric(isd.object$minute)))
        write.csv(isd.object, paste0(shiny2.folder, "\\windrose.csv"))
        ts.data <- convertToDaily(isd.object, attribs$period)
        write.csv(ts.data, paste0(shiny2.folder, "\\timeseries.csv"))
    }
    else {
        write.csv(isd.object, paste0(shiny2.folder, "\\timeseries.csv"))
        invisible(try(file.remove(paste0(shiny2.folder, "\\windrose.csv"))))
    }
    shinyISD(name = "shiny2")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
