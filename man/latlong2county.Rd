\name{latlong2county}
\alias{latlong2county}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
latlong2county(pointsDF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pointsDF}{
%%     ~~Describe \code{pointsDF} here~~
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
function (pointsDF) 
{
    counties <- map("county", fill = TRUE, col = "transparent", 
        plot = FALSE)
    IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
    counties_sp <- map2SpatialPolygons(counties, IDs = IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
    pointsSP <- SpatialPoints(pointsDF, proj4string = CRS("+proj=longlat +datum=WGS84"))
    indices <- over(pointsSP, counties_sp)
    countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
    countyNames[indices]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
