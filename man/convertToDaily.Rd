\name{convertToDaily}
\alias{convertToDaily}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convertToDaily(isd.object, period)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{isd.object}{
%%     ~~Describe \code{isd.object} here~~
}
  \item{period}{
%%     ~~Describe \code{period} here~~
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
function (isd.object, period) 
{
    if (period == "sub-hourly") {
        isd.object <- group_by(isd.object, ID, date, hour)
        isd.object <- filter(isd.object, minute == min(minute))
    }
    clmn.names <- c("temp_max", "temp_min", "temp_mean", "temp_valid_hours", 
        "dew_point_max", "dew_point_min", "dew_point_mean", "dew_point_valid_hours", 
        "atmospheric_pressure_max", "atmospheric_pressure_min", 
        "atmospheric_pressure_mean", "atmospheric_pressure_valid_hours")
    isd.object <- ddply(isd.object, .(ID, date), function(df) c(max(df[, 
        "temperature"], na.rm = T), min(df[, "temperature"], 
        na.rm = T), mean(df[, "temperature"], na.rm = T), sum(as.integer(df[, 
        "temperature_quality"])), max(df[, "dew_point_temp"], 
        na.rm = T), min(df[, "dew_point_temp"], na.rm = T), mean(df[, 
        "dew_point_temp"], na.rm = T), sum(as.integer(df[, "dew_point_temp_quality"])), 
        max(df[, "atmospheric_pressure"], na.rm = T), min(df[, 
            "atmospheric_pressure"], na.rm = T), mean(df[, "atmospheric_pressure"], 
            na.rm = T), sum(as.integer(df[, "atmospheric_pressure_quality"]))))
    colnames(isd.object)[3:14] <- clmn.names
    isd.object
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
