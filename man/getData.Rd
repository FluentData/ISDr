\name{getData}
\alias{getData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getData(station.years, fixed.length.formats, col.names, pb.con)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{station.years}{
%%     ~~Describe \code{station.years} here~~
}
  \item{fixed.length.formats}{
%%     ~~Describe \code{fixed.length.formats} here~~
}
  \item{col.names}{
%%     ~~Describe \code{col.names} here~~
}
  \item{pb.con}{
%%     ~~Describe \code{pb.con} here~~
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
function (station.years, fixed.length.formats, col.names, pb.con) 
{
    station.ids <- substr(station.years, 1, 12)
    years <- substr(station.years, 14, 17)
    percent.complete <- (1:length(station.ids)/length(station.ids)) * 
        0.8 + 0.15
    db.folder <- normalizePath(system.file("database", package = "ISDr"))
    downLoad <- function(year, id, progress) {
        url <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", 
            year, "/", id, "-", year, ".gz")
        temp <- tempfile(fileext = ".gz")
        try(download.file(url = url, temp))
        fl.name <- paste0(db.folder, strsplit(id, "-")[[1]][1], 
            "_", strsplit(id, "-")[[1]][2], "_", year, ".csv")
        print(fl.name)
        try(gunzip(temp, fl.name))
        unlink(temp)
        try(dframe <- read.fortran(file = fl.name, format = fixed.length.formats))
        if (exists("dframe")) {
            colnames(dframe) <- col.names
            print(names(dframe))
            dframe <- data.frame(ID = paste(dframe[, "USAF.id"], 
                dframe[, "WBAN.id"], sep = "-"), dframe)
            data.colnames <- c("wind.direction", "wind.speed", 
                "ceiling.height", "visibility.distance", "visibility.variability.code", 
                "temperature", "dew.point.temp", "atmospheric.pressure")
            data.missing <- c(999, 9999, 99999, 999999, 9, 9999, 
                9999, 9999.9)
            for (i in 1:length(data.colnames)) {
                dframe[, data.colnames[i]][dframe[, data.colnames[i]] == 
                  data.missing[i]] <- NA
                dframe[, data.colnames[i]][dframe[, paste0(data.colnames[i], 
                  ".quality")] == 2] <- NA
                dframe[, data.colnames[i]][dframe[, paste0(data.colnames[i], 
                  ".quality")] == 3] <- NA
                dframe[, data.colnames[i]][dframe[, paste0(data.colnames[i], 
                  ".quality")] == 6] <- NA
                dframe[, data.colnames[i]][dframe[, paste0(data.colnames[i], 
                  ".quality")] == 7] <- NA
            }
            dframe <- mutate(dframe, hour = as.integer(substr(sprintf("\%04d", 
                as.numeric(dframe$time)), 1, 2)), minute = as.integer(substr(sprintf("\%04d", 
                as.numeric(dframe$time)), 3, 4)))
            dframe <- select(dframe, ID:date, hour, minute, source.flag:atmospheric.pressure.quality)
            con <- dbConnect(dbDriver("SQLite", max.con = 25), 
                dbname = paste0(db.folder, "/ISDdatabase"))
            dbWriteTable(conn = con, name = "Data", value = dframe, 
                row.names = FALSE, header = TRUE, append = TRUE)
            dbDisconnect(con)
            rows <- nrow(dframe)
            remove(dframe)
            unlink(fl.name)
            setTxtProgressBar(pb.con, progress)
            print(rows)
            return(rows)
        }
        else {
            unlink(fl.name)
            setTxtProgressBar(pb.con, progress)
            print(0)
            return(0)
        }
    }
    n.rows <- mapply(FUN = downLoad, year = years, id = station.ids, 
        progress = percent.complete)
    return(n.rows)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
