\name{downloadISD}
\alias{downloadISD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
downloadISD(state, years)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state}{
%%     ~~Describe \code{state} here~~
}
  \item{years}{
%%     ~~Describe \code{years} here~~
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
function (state, years) 
{
    pb <- txtProgressBar(min = 0, max = 1, initial = 0, char = "=", 
        width = NA, title, label, style = 3, file = "")
    setTxtProgressBar(pb, 0.05)
    db.folder <- normalizePath(system.file("database", package = "ISDr"))
    stations.file <- paste0(db.folder, "/stations.csv")
    download.file("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv", 
        stations.file)
    setTxtProgressBar(pb, 0.1)
    stations.df <- read.csv(stations.file, header = T, colClasses = rep_len("character", 
        12), stringsAsFactors = F)
    stations.df[, "LAT"] <- as.numeric(paste(substr(stations.df[, 
        "LAT"], 1, 3), substr(stations.df[, "LAT"], 4, 6), sep = "."))
    stations.df[, "LON"] <- as.numeric(paste(substr(stations.df[, 
        "LON"], 1, 4), substr(stations.df[, "LON"], 5, 7), sep = "."))
    stations.df[, "BEGIN"] <- as.numeric(substr(stations.df[, 
        "BEGIN"], 1, 4))
    stations.df[, "END"] <- as.numeric(substr(stations.df[, "END"], 
        1, 4))
    colnames(stations.df)[c(3, 10)] <- c("NAME", "ELEV")
    if (length(years) == 1) 
        years <- rep.int(years, 2)
    stations.df <- filter(stations.df, STATE == state, BEGIN <= 
        years[2] & END >= years[1])
    COUNTY <- matrix(unlist(strsplit(latlong2county(stations.df[c(9, 
        8)]), ",", fixed = T)), ncol = 2, byrow = T)[, 2]
    stations.df <- data.frame(ID = paste(stations.df[, 1], stations.df[, 
        2], sep = "-"), stations.df[, 1:6], COUNTY, stations.df[, 
        7:12])
    unlink(stations.file)
    db.con <- dbConnect(dbDriver("SQLite", max.con = 25), dbname = paste0(db.folder, 
        "/ISDdatabase"))
    file.formats <- read.csv(paste0(db.folder, "/ISD_fixed_format.csv"), 
        header = T, stringsAsFactors = F)
    formats <- mapply(makeFormats, length = file.formats[, "positions"], 
        type = file.formats[, "class"], decimals = file.formats[, 
            "decimal.places"])
    column.names <- file.formats[file.formats[, "class"] != "skip", 
        "description"]
    downloads.old <- dbReadTable(db.con, "Downloads")
    print("Old Downloads Table")
    print(downloads.old)
    station.years.old <- paste(downloads.old$ID, downloads.old$year, 
        sep = "-")
    print("Old station-years")
    print(station.years.old)
    station.years.new <- expand.grid(ID = stations.df$ID, year = years[1]:years[2])
    station.years.new <- paste(station.years.new$ID, station.years.new$year, 
        sep = "-")
    print("New station-years")
    print(station.years.new)
    station.years.diff <- station.years.new[!station.years.new \%in\% 
        station.years.old]
    print("Station-years difference")
    print(station.years.diff)
    setTxtProgressBar(pb, 0.15)
    download.years.nrows <- getData(station.years = station.years.diff, 
        fixed.length.formats = formats, col.names = column.names, 
        pb.con = pb)
    downloads.new <- data.frame(ID = substr(station.years.diff, 
        1, 12), year = substr(station.years.diff, 14, 17), nrows = download.years.nrows)
    print("New Downloads")
    print(downloads.new)
    dbWriteTable(conn = db.con, name = "Downloads", value = downloads.new, 
        row.names = FALSE, header = TRUE, append = TRUE)
    stations.old <- dbReadTable(db.con, "Stations")
    print("Old Stations Table")
    print(stations.old)
    print("Stations Table for Download")
    print(stations.df)
    stations.merged <- merge(stations.old, stations.df, all = T)
    print("Merged Stations Table")
    print(stations.merged)
    dbWriteTable(conn = db.con, name = "Stations", value = stations.merged, 
        row.names = FALSE, header = TRUE, overwrite = TRUE)
    setTxtProgressBar(pb, 1)
    dbDisconnect(db.con)
    close(pb)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
