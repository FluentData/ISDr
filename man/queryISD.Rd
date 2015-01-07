\name{queryISD}
\alias{queryISD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
queryISD(date.range = "NULL", station.ids = "NULL", states = "NULL", counties = "NULL", shape = c("long", "wide"), period = c("sub-hourly", "hourly", "daily"), percent.complete = "NULL", example = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date.range}{
%%     ~~Describe \code{date.range} here~~
}
  \item{station.ids}{
%%     ~~Describe \code{station.ids} here~~
}
  \item{states}{
%%     ~~Describe \code{states} here~~
}
  \item{counties}{
%%     ~~Describe \code{counties} here~~
}
  \item{shape}{
%%     ~~Describe \code{shape} here~~
}
  \item{period}{
%%     ~~Describe \code{period} here~~
}
  \item{percent.complete}{
%%     ~~Describe \code{percent.complete} here~~
}
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
function (date.range = "NULL", station.ids = "NULL", states = "NULL", 
    counties = "NULL", shape = c("long", "wide"), period = c("sub-hourly", 
        "hourly", "daily"), percent.complete = "NULL", example = F) 
{
    if (example == T) {
        db.folder <- normalizePath(system.file("database", package = "ISDr"))
        db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
            dbname = paste0(db.folder, "/exampleISDdatabase"))
    }
    else {
        db.folder <- normalizePath(system.file("database", package = "ISDr"))
        db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
            dbname = paste0(db.folder, "/ISDdatabase"))
    }
    if ("Query" \%in\% dbListTables(db.con)) {
        query.exists = T
    }
    else {
        query.exists = F
    }
    stations.table <- dbReadTable(db.con, "Stations")
    downloads.table <- dbReadTable(db.con, "Downloads")
    if (date.range[1] == "NULL") {
        date.range <- c(min(as.integer(downloads.table[, "year"])), 
            max(as.integer(downloads.table[, "year"])))
    }
    if (station.ids[1] != "NULL") {
        query.df <- stations.table[stations.table$ID \%in\% station.ids, 
            ]
        dbWriteTable(conn = db.con, name = "Query", value = query.df, 
            overwrite = query.exists, row.names = FALSE, header = TRUE)
    }
    else if (state[1] != "NULL") {
        query.df <- stations.table[stations.table$STATE \%in\% 
            states, ]
        if (counties[1] != "NULL") {
            query.df <- query.df[query.df$COUNTY \%in\% counties, 
                ]
        }
        dbWriteTable(conn = db.con, name = "Query", value = query.df, 
            overwrite = query.exists, row.names = FALSE, header = TRUE)
    }
    if (period[1] == "sub-hourly") {
        sql.statement <- paste0("SELECT * FROM Data JOIN Query ON Data.ID=Query.ID WHERE Data.date BETWEEN ", 
            date.range[1], " AND ", date.range[2], " ORDER BY Data.ID, date, hour, minute")
    }
    else if (period[1] == "hourly") {
        colnames <- dbListFields(db.con, "Data")
        colnames <- paste0(sapply(colnames, function(x) paste0("h.", 
            x)), collapse = ", ")
        sql.statement <- paste0("SELECT ", colnames, " FROM \n                             (SELECT *, MIN(minute) AS minute_min FROM Data WHERE Data.date BETWEEN ", 
            date.range[1], " AND ", date.range[2], " GROUP BY ID, date, hour) AS h\n                             JOIN Query AS q\n                               ON h.ID=q.ID ORDER BY h.ID, h.date, h.hour")
    }
    else if (period[1] == "daily") {
        sql.statement <- paste0("SELECT h.ID, h.date, MAX(temperature) AS temp_max, MIN(temperature) AS temp_min,\n                              AVG(temperature) AS temp_mean, COUNT(temperature) AS temp_valid_hours,\n                              MAX(dew_point_temp) AS dew_point_max, MIN(dew_point_temp) AS dew_point_min,\n                              AVG(dew_point_temp) AS dew_point_mean, COUNT(dew_point_temp) AS dew_point_valid_hours, \n                              MAX(atmospheric_pressure) AS atmospheric_pressure_max,\n                              MIN(atmospheric_pressure) AS atmospheric_pressure_min,\n                              AVG(atmospheric_pressure) AS atmospheric_pressure_mean,\n                              COUNT(atmospheric_pressure) AS atmospheric_pressure_valid_hours FROM\n                              \n                               (SELECT *, MIN(minute) AS minute_min FROM Data WHERE Data.date BETWEEN ", 
            date.range[1], " AND ", date.range[2], " GROUP BY ID, date, hour) AS h\n                                  JOIN Query as q\n                                    ON h.ID = q.ID GROUP BY h.ID, h.date")
    }
    res <- dbGetQuery(db.con, statement = sql.statement)
    if (period[1] == "daily" & shape[1] == "wide") {
        list.dfs <- vector("list", length(clmn.names))
        for (i in 1:length(clmn.names)) {
            list.dfs[[i]] <- dcast(res, date ~ ID, value.var = clmn.names[i])
            station.ids <- colnames(list.dfs[[i]])[-1]
            colnames(list.dfs[[i]])[2:(1 + length(station.ids))] <- sapply(station.ids, 
                function(x) paste(clmn.names[i], x, sep = "_"))
        }
        x <- list.dfs[[1]]
        for (i in 2:(length(list.dfs))) {
            y <- list.dfs[[i]]
            x <- merge(x, y, all = T)
        }
        res <- x
    }
    dbDisconnect(db.con)
    attr(res, "date.range") <- date.range
    attr(res, "station.ids") <- station.ids
    attr(res, "counties") <- counties
    attr(res, "shape") <- shape[1]
    attr(res, "period") <- period[1]
    attr(res, "percent.complete") <- percent.complete
    attr(res, "time") <- Sys.time()
    attr(res, "example") <- example
    class(res) <- c("isd", "data.frame")
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
