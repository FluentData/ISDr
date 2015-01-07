\name{makeFormats}
\alias{makeFormats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeFormats(length, type, decimals)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{length}{
%%     ~~Describe \code{length} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{decimals}{
%%     ~~Describe \code{decimals} here~~
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
function (length, type, decimals) 
{
    if (type == "numeric") {
        paste0("F", as.character(length(seq(as.numeric(strsplit(length, 
            "--")[[1]][1]), as.numeric(strsplit(length, "--")[[1]][2])))), 
            ".", decimals)
    }
    else if (type == "integer") {
        paste0("I", as.character(length(seq(as.numeric(strsplit(length, 
            "--")[[1]][1]), as.numeric(strsplit(length, "--")[[1]][2])))))
    }
    else if (type == "character") {
        paste0("A", as.character(length(seq(as.numeric(strsplit(length, 
            "--")[[1]][1]), as.numeric(strsplit(length, "--")[[1]][2])))))
    }
    else if (type == "skip") {
        paste0("X", as.character(length(seq(as.numeric(strsplit(length, 
            "--")[[1]][1]), as.numeric(strsplit(length, "--")[[1]][2])))))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
