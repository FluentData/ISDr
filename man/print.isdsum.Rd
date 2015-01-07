\name{print.isdsum}
\alias{print.isdsum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.isdsum(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    stations <- matrix(x$station.ids, ncol = 1, dimnames = list(rep("", 
        length(x$station.ids)), "Stations queried:"))
    cat("\n\n\t\t\tISD Query Summary\n\nQueried on:", paste0(x$time), 
        "\n\nFrom", paste0(as.Date(as.character(x$date.range[1]), 
            "\%Y\%m\%d"), " to ", as.Date(as.character(x$date.range[2]), 
            "\%Y\%m\%d")), "\n\nPeriod:", x$period[1], "\n\n")
    print(stations, quote = F)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
