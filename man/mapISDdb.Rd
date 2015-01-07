\name{mapISDdb}
\alias{mapISDdb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mapISDdb(example = F)
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
        cn <- dbConnect(dbDriver("SQLite", max.con = 25), dbname = paste0(db.folder, 
            "/exampleISDdatabase"))
    }
    else {
        db.folder <- normalizePath(system.file("database", package = "ISDr"))
        cn <- dbConnect(dbDriver("SQLite", max.con = 25), dbname = paste0(db.folder, 
            "/ISDdatabase"))
    }
    stations <- dbReadTable(cn, "Stations")
    downloads <- dbReadTable(cn, "Downloads")
    dbDisconnect(cn)
    print(stations)
    print(downloads)
    d.loads <- ddply(downloads, .(ID), summarize, rows = sum(nrows))
    d.loads <- arrange(d.loads, desc(rows))
    d.loads$rows[d.loads$rows == 0] <- 1
    d.loads$rows[d.loads$rows > 26279] <- 26279
    print(d.loads)
    colfunc <- colorRamp(c("white", "blue"))
    col.channels <- colfunc(d.loads$rows/(length(levels(as.factor(downloads$year))) * 
        365 * 24 * 3))
    print(levels(as.factor(downloads$year)))
    fillColor <- rgb(col.channels, maxColorValue = 256)
    dl.cols <- data.frame(d.loads, fillColor)
    stations.cols <- merge(stations, dl.cols, all = T)
    makePopup <- function(row, df) {
        paste0("<table><tr><td>Name</td><td>", df[, "NAME"][row], 
            "</td></tr><tr><td>State</td><td>", df[, "STATE"][row], 
            "</td></tr><tr><td>County</td><td>", df[, "COUNTY"][row], 
            "</td></tr><tr><td>ID</td><td>", df[, "ID"][row], 
            "</td></tr><tr><td>Begin</td><td>", df[, "BEGIN"][row], 
            "</td></tr><tr><td>End</td><td>", df[, "END"][row], 
            "</td></tr></table>")
    }
    popup <- sapply(1:nrow(stations.cols), makePopup, df = stations.cols)
    stations.cols.popups <- data.frame(stations.cols, popup)
    data_json <- toJSONArray2(stations.cols.popups, json = F)
    data_geojson <- toGeoJSON(data_json, lat = "LAT", lon = "LON")
    map = Leaflet$new()
    center <- c(mean(c(max(stations[, "LAT"]), min(stations[, 
        "LAT"]))), mean(c(max(stations[, "LON"]), min(stations[, 
        "LON"]))))
    map$setView(center, 7)
    for (i in 1:nrow(stations.cols.popups)) {
        map$geoJson(data_geojson, onEachFeature = "#! function(feature, layer){\n                 layer.bindPopup(feature.properties.popup)\n  } !#", 
            pointToLayer = "#! function(feature, latlng){\n                 return L.circleMarker(latlng, {\n                 radius: 7,\n                 fillColor: feature.properties.fillColor || 'blue',\n                 color: '#000',\n                 weight: 1,\n                 fillOpacity: 0.6\n                 })\n} !#")
    }
    map
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
