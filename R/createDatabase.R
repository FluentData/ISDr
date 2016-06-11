#' # Takes data frame with columns "description", "positions",
#' # "unit", and "decimal.places" and makes formats for the
#' # read.fortran() function
#' makeFormats <- function(length, type, decimals){
#'   
#'   if(type=="numeric"){paste0("F", as.character(length(seq(as.numeric(strsplit(length, "--")[[1]][1]), 
#'                                                           as.numeric(strsplit(length, "--")[[1]][2])))), ".", decimals)} else
#'                                                             if(type=="integer"){paste0("I", as.character(length(seq(as.numeric(strsplit(length, "--")[[1]][1]), 
#'                                                                                                                     as.numeric(strsplit(length, "--")[[1]][2])))))} else
#'                                                                                                                       if(type=="character"){paste0("A", as.character(length(seq(as.numeric(strsplit(length, "--")[[1]][1]), 
#'                                                                                                                                                                                 as.numeric(strsplit(length, "--")[[1]][2])))))} else
#'                                                                                                                                                                                   if(type=="skip"){paste0("X", as.character(length(seq(as.numeric(strsplit(length, "--")[[1]][1]), 
#'                                                                                                                                                                                                                                        as.numeric(strsplit(length, "--")[[1]][2])))))}
#'   
#' }
#' 
#' 
#' 
#' 
#' # This function takes one, or a vector of, station id(s) and returns a data frame based on the 
#' # range of years provided
#' getData <- function(station.years, fixed.length.formats, col.names, pb.con){
#'   
#'   
#'   station.ids <- substr(station.years, 1, 12)
#'   
#'   #   print("Station IDs in getData()")
#'   #   print(station.ids)
#'   
#'   years <- substr(station.years, 14, 17)
#'   #   print("Years in getData()")
#'   #   print(unique(years))
#'   
#'   percent.complete <- (1:length(station.ids)/length(station.ids)) * 0.8 + 0.15
#'   
#'   # get file path for database folder in library ISDr
#'   db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'   
#'   downLoad <- function(year, id, progress){
#'     
#'     url <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", year, "/", id, "-", year, ".gz")
#'     temp <- tempfile(fileext = ".gz")
#'     try(download.file(url=url, temp))
#'     fl.name <- paste0(db.folder, strsplit(id, "-")[[1]][1], "_", strsplit(id, "-")[[1]][2], "_", year, ".csv")
#'     print(fl.name)
#'     try(gunzip(temp, fl.name))
#'     unlink(temp)
#'     try(dframe <- read.fortran(file=fl.name, format=fixed.length.formats))
#'     if(exists("dframe")){
#'       colnames(dframe) <- col.names
#'       
#'       print(names(dframe))
#'       
#'       #add ID column
#'       dframe <- data.frame(ID = paste(dframe[, "USAF.id"], dframe[, "WBAN.id"], sep = "-"), dframe)
#'       
#'       
#'       # Column names for processing
#'       data.colnames <- c("wind.direction", "wind.speed", "ceiling.height", "visibility.distance",
#'                          "visibility.variability.code", "temperature", "dew.point.temp", 
#'                          "atmospheric.pressure")
#'       data.missing <- c(999, 9999, 99999, 999999, 9, 9999, 9999, 9999.9)
#'       
#'       # insert NAs for cells with "missing" codes or with error codes
#'       for(i in 1:length(data.colnames)){
#'         dframe[, data.colnames[i]][dframe[, data.colnames[i]] == data.missing[i]] <- NA
#'         dframe[, data.colnames[i]][dframe[, paste0(data.colnames[i], ".quality")] == 2] <- NA
#'         dframe[, data.colnames[i]][dframe[, paste0(data.colnames[i], ".quality")] == 3] <- NA
#'         dframe[, data.colnames[i]][dframe[, paste0(data.colnames[i], ".quality")] == 6] <- NA
#'         dframe[, data.colnames[i]][dframe[, paste0(data.colnames[i], ".quality")] == 7] <- NA
#'       }
#'       
#'       dframe <- mutate(dframe, hour = as.integer(substr(sprintf("%04d", as.numeric(dframe$time)), 1, 2)),
#'                        minute = as.integer(substr(sprintf("%04d", as.numeric(dframe$time)), 3, 4)))
#'       
#'       dframe <- select(dframe, ID:date, hour, minute, source.flag:atmospheric.pressure.quality)
#'       
#'       # create a connection to ISDdatabase
#'       con <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                        dbname=paste0(db.folder, "/ISDdatabase"))
#'       
#'       # append the data to the Data table
#'       dbWriteTable(conn = con, name = "Data", value = dframe,
#'                    row.names = FALSE, header = TRUE, append = TRUE)
#'       dbDisconnect(con)
#'       
#'       rows <- nrow(dframe)
#'       
#'       remove(dframe)
#'       unlink(fl.name)
#'       
#'       setTxtProgressBar(pb.con, progress)
#'       
#'       print(rows)
#'       
#'       return(rows)
#'       
#'     } else {
#'       unlink(fl.name)
#'       setTxtProgressBar(pb.con, progress)
#'       print(0)
#'       return(0)
#'     }
#'   }
#'   
#'   
#'   
#'   n.rows <- mapply(FUN = downLoad, year = years, id = station.ids, progress = percent.complete)
#'   
#'   return(n.rows)
#'   
#'   
#' }
#' 
#' # The single argument to this function, pointsDF, is a data.frame in which:
#' #   - column 1 contains the longitude in degrees (negative in the US)
#' #   - column 2 contains the latitude in degrees
#' 
#' # Code lifted from Stack Overflow http://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
#' latlong2county <- function(pointsDF) {
#'   # Prepare SpatialPolygons object with one SpatialPolygon
#'   # per county
#'   counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
#'   IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
#'   counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
#'                                      proj4string=CRS("+proj=longlat +datum=WGS84"))
#'   
#'   # Convert pointsDF to a SpatialPoints object 
#'   pointsSP <- SpatialPoints(pointsDF, 
#'                             proj4string=CRS("+proj=longlat +datum=WGS84"))
#'   
#'   # Use 'over' to get _indices_ of the Polygons object containing each point 
#'   indices <- over(pointsSP, counties_sp)
#'   
#'   # Return the county names of the Polygons object containing each point
#'   countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
#'   countyNames[indices]
#' }
#' 
#' 
#' #' @title Download Integrated Surface Data
#' #' @description Download integrated surface data from NOAA/NCDC into SQLite database
#' #' @param state two letter abbreviation of state to be downloaded 
#' #' @param years a numeric vector of beginning and ending years (e.g. c(2005, 2010))
#' #' @examples
#' #' downloadISD(state = "IN", years = c(2007, 2012))
#' downloadISD <- function(state, years){
#'   
#'   pb <- txtProgressBar(min = 0, max = 1, initial = 0, char = "=",
#'                  width = NA, title, label, style = 3, file = "")
#'   
#'   setTxtProgressBar(pb, 0.05)
#'   
#'   # get file path for database folder in library ISDr
#'   db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'   
#'   # create file path for csv document with station information
#'   stations.file <- normalizePath(paste0(db.folder, "/stations.csv"))
#'   
#'   # download table with staion information
#'   download.file("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv", stations.file)
#'   
#'   
#'   setTxtProgressBar(pb, 0.1)
#'   
#'   # read in the station information
#'   stations.df <- read.csv(stations.file, colClasses = "character",
#'                           stringsAsFactors=F)
#'   
#'   # insert decimal into lat and lon columns
#'   stations.df[, "LAT"] <- as.numeric(paste(substr(stations.df[, "LAT"], 1, 3),
#'                                            substr(stations.df[, "LAT"], 4, 6), sep = "."))
#'   stations.df[, "LON"] <- as.numeric(paste(substr(stations.df[, "LON"], 1, 4),
#'                                            substr(stations.df[, "LON"], 5, 7), sep = "."))
#'   
#'   # chop off month and day from BEGIN and END values
#'   stations.df[, "BEGIN"] <- as.numeric(substr(stations.df[, "BEGIN"], 1, 4))
#'   stations.df[, "END"] <- as.numeric(substr(stations.df[, "END"], 1, 4))
#'   
#'   colnames(stations.df)[c(3, 10)] <- c("NAME", "ELEV")
#'   
#'   if(length(years) == 1) years <- rep.int(years, 2)
#'   
#'   # subset based on state
#'   stations.df <- filter(stations.df, STATE == state, BEGIN <= years[2] & END >= years[1])
#'   # quick but inaccurate version:
#'   #stations.df <- filter(stations.df, STATE == state, BEGIN %in% years[1]:years[2] | END %in% years[1]:years[2])
#'   
#'   
#'   # create COUNTY column
#'   COUNTY <- matrix(unlist(strsplit(latlong2county(stations.df[c(9, 8)]), ",", fixed = T)), 
#'                    ncol = 2, byrow = T)[, 2]
#'   
#'   # insert ID and COUNTY columns into data frame
#'   stations.df <- data.frame(ID = paste(stations.df[, 1], stations.df[, 2], sep = "-"), 
#'                             stations.df[, 1:6], COUNTY, stations.df[, 7:12])
#'   
#'   # delete the original station csv file
#'   unlink(stations.file)
#'   
#'   # create a connection to ISDdatabase
#'   db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                       dbname=paste0(db.folder, "/ISDdatabase"))
#'   #   
#'   #   # add the station dataframe to the Stations table
#'   #   dbWriteTable(conn = db.con, name = "Stations", value = stations.df,
#'   #                row.names = FALSE, header = TRUE)
#'   #   
#'   #     print(dbListTables(db.con))              # The tables in the database
#'   #     print(dbListFields(db.con, "Stations"))    # The columns in a table
#'   #     print(head(dbReadTable(db.con, "Stations")))     # The data in a table
#'   
#'  
#'   
#'   # load dataframe called formats for read.fortran() function
#'   file.formats <- read.csv(paste0(db.folder, "/ISD_fixed_format.csv"), 
#'                            header=T, stringsAsFactors=F)
#'   
#'   formats <- mapply(makeFormats, length=file.formats[, "positions"], 
#'                     type=file.formats[, "class"], 
#'                     decimals=file.formats[, "decimal.places"])
#' 
#'   
#'   column.names <- file.formats[file.formats[, "class"]!="skip", "description"]
#'   
#'   # get Dowloads table in database
#'   downloads.old <- dbReadTable(db.con, "Downloads")
#'   print("Old Downloads Table")
#'   print(downloads.old)
#'   
#'   # create vector of existing station/years
#'   station.years.old <- paste(downloads.old$ID, downloads.old$year, sep = "-")
#'   print("Old station-years")
#'   print(station.years.old)
#'   
#'   
#'   # create vector of the new station/years
#'   station.years.new <- expand.grid(ID = stations.df$ID, year = years[1]:years[2])
#'   station.years.new <- paste(station.years.new$ID, station.years.new$year, sep = "-")
#'   print("New station-years")
#'   print(station.years.new)
#'   
#'   # create vector of station/years that do not already exist in database
#'   station.years.diff <- station.years.new[!station.years.new %in% station.years.old]
#'   print("Station-years difference")
#'   print(station.years.diff)
#'   
#'   setTxtProgressBar(pb, 0.15)
#'   
#'   download.years.nrows <- getData(station.years = station.years.diff, fixed.length.formats = formats,
#'                                   col.names = column.names, pb.con = pb)
#'   
#'   downloads.new <- data.frame(ID = substr(station.years.diff, 1, 12), year = substr(station.years.diff, 14, 17),
#'                               nrows = download.years.nrows)
#'   
#'   print("New Downloads")
#'   print(downloads.new)
#'   
#'   dbWriteTable(conn = db.con, name = "Downloads", value = downloads.new,
#'                row.names = FALSE, header = TRUE, append = TRUE)
#'   
#'   # get Stations table in database
#'   stations.old <- dbReadTable(db.con, "Stations")
#'   
#'   print("Old Stations Table")
#'   print(stations.old)
#'   
#'   print("Stations Table for Download")
#'   print(stations.df)
#'   
#'   stations.merged <- merge(stations.old, stations.df, all = T)
#'   
#'   print("Merged Stations Table")
#'   print(stations.merged)
#'   
#'   dbWriteTable(conn = db.con, name = "Stations", value = stations.merged,
#'                row.names = FALSE, header = TRUE, overwrite = TRUE)
#'   
#'   setTxtProgressBar(pb, 1)
#'   
#'   dbDisconnect(db.con)
#'   
#'   close(pb)
#'   
#' }
#' 
#' #' Stations in SQLite Database
#' #' 
#' stationsISDdb <- function(){
#'   
#'   if(example == T){
#'     db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'     connect <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                          dbname=paste0(db.folder, "/exampleISDdatabase"))
#'   } else {
#'     db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'     connect <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                          dbname=paste0(db.folder, "/ISDdatabase"))
#'   }
#'   
#'   stations <- dbReadTable(connect, "Stations")
#'   dbDisconnect(connect)
#'   if(nrow(stations) > 0){stations[, 12:13] = apply(stations[, 12:13], 2, function(x) as.integer(x))}
#'   stations
#' }
#' 
#' mapISDdb <- function(example = F){
#'   if(example == T){
#'     db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'     cn <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                     dbname=paste0(db.folder, "/exampleISDdatabase"))
#'   } else {
#'     db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'     cn <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                     dbname=paste0(db.folder, "/ISDdatabase"))
#'   }
#'   
#'   stations <- dbReadTable(cn, "Stations")
#'   downloads <- dbReadTable(cn, "Downloads")
#'   dbDisconnect(cn)
#'   
#'   print(stations)
#'   print(downloads)
#' 
#'   d.loads <- ddply(downloads, .(ID), summarize, rows = sum(nrows))
#'   d.loads <- arrange(d.loads, desc(rows))
#'   d.loads$rows[d.loads$rows == 0] <- 1
#'   d.loads$rows[d.loads$rows > 26279] <- 26279
#'   
#'   print(d.loads)
#'   
#'   colfunc <- colorRamp(c("white", "blue"))
#'   col.channels <- colfunc(d.loads$rows/(length(levels(as.factor(downloads$year)))*365*24*3))
#'   print(levels(as.factor(downloads$year)))
#'   fillColor <- rgb(col.channels, maxColorValue=256)
#'   
#'   dl.cols <- data.frame(d.loads, fillColor)
#'   stations.cols <- merge(stations, dl.cols, all = T)
#'   
#'   makePopup <- function(row, df){
#'     paste0("<table><tr><td>Name</td><td>", df[, 'NAME'][row],
#'            "</td></tr><tr><td>State</td><td>", df[, 'STATE'][row],
#'            "</td></tr><tr><td>County</td><td>", df[, 'COUNTY'][row],
#'            "</td></tr><tr><td>ID</td><td>", df[, 'ID'][row],
#'            "</td></tr><tr><td>Begin</td><td>", df[, 'BEGIN'][row],
#'            "</td></tr><tr><td>End</td><td>", df[, 'END'][row],
#'            "</td></tr></table>")
#'   }
#'   
#'   popup <- sapply(1:nrow(stations.cols), makePopup, df = stations.cols)
#'   
#'   stations.cols.popups <- data.frame(stations.cols, popup)
#'   
#'   data_json <- toJSONArray2(stations.cols.popups, json = F)  ## use this function to convert your lat longs and popu info to a json array##
#'   data_geojson <- toGeoJSON(data_json,lat="LAT",lon="LON") ##Convert your json array to GeoJson##
#'   
#'   map = Leaflet$new()
#'   center <- c(mean(c(max(stations[, "LAT"]), min(stations[, "LAT"]))), 
#'               mean(c(max(stations[, "LON"]), min(stations[, "LON"]))))
#'   map$setView(center, 7)
#'   
#'   
#'   for(i in 1:nrow(stations.cols.popups)){
#'     map$geoJson(data_geojson, onEachFeature = '#! function(feature, layer){
#'                  layer.bindPopup(feature.properties.popup)
#'   } !#',
#'                  pointToLayer = "#! function(feature, latlng){
#'                  return L.circleMarker(latlng, {
#'                  radius: 7,
#'                  fillColor: feature.properties.fillColor || 'blue',
#'                  color: '#000',
#'                  weight: 1,
#'                  fillOpacity: 0.6
#'                  })
#' } !#")
#'     }
#' 
#'   map
#' }
#' 
#' downloadedYearsISD <- function(example = F, db.table = F){
#'   if(example == T){
#'     db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'     connection <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                             dbname=paste0(db.folder, "/exampleISDdatabase"))
#'   } else {
#'     db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'     connection <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                             dbname=paste0(db.folder, "/ISDdatabase"))
#'   }
#'   
#'   downloads <- dbReadTable(connection, "Downloads")
#'   
#'   downloads.monitors <- sort(unique(downloads$ID))
#'   
#'   downloads.years <- sort(unique(downloads$year))
#'   
#'   downloads.matrix <- matrix(rep("NA", length(downloads.monitors) * length(downloads.years)), 
#'                              ncol = length(downloads.years), 
#'                              dimnames = list(downloads.monitors, downloads.years))
#'   
#'   insertYes <- function(column.name, row.name){
#'     downloads.matrix[[as.character(row.name), as.character(column.name)]] <<- "Yes"
#'   }
#'   
#'   
#'   invisible(mapply(FUN=insertYes, column.name = downloads$year, row.name = downloads$ID))
#'   
#'   downloads$nrows[downloads$nrows == 0] <- 1
#'   downloads$nrows[downloads$nrows > 365*24*3] <- 365*24*3
#'   downloads[, "nrows"] <- (downloads$nrows/(365*24*3))*100
#'   names(downloads)[3] <- "Percent_Complete"
#'   
#'   p1 <- ggplot(downloads, aes(x = as.factor(year), y = as.factor(ID))) +
#'     geom_tile(aes(fill = Percent_Complete)) +
#'     scale_fill_gradient2(low="white", high="blue") +
#'     xlab("Downloaded Years") +
#'     ylab("Downloaded Stations") 
#'   print(p1)
#'   
#'   
#'   
#'   dbDisconnect(connection)
#'   
#'   if(db.table == F){return(downloads.matrix)} else {return(downloads)}
#'   
#' }
#' 
#' 
#' 
#' 
#' summaryISDdb <- function(example = F){
#'   
#'   if(example == T){
#'     # get file path for database folder in library ISDr
#'     db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'     db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                         dbname=paste0(db.folder, "\\exampleISDdatabase"))
#'     db.info <- file.info(paste0(db.folder, "\\exampleISDdatabase"))
#'     stations.df <- stationsISDdb(example = T)
#'     download.years <- downloadedYearsISD(example = T)
#'   } else {
#'     db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#'     db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
#'                         dbname=paste0(db.folder, "\\ISDdatabase"))
#'     db.info <- file.info(paste0(db.folder, "\\ISDdatabase"))
#'     stations.df <- stationsISDdb()
#'     download.years <- downloadedYearsISD()
#'   }
#'   
#'   db.matrix <- matrix(c(row.names(db.info), humanReadable(db.info$size)),
#'                       ncol = 1, dimnames = list(c("Location", "Size"), 
#'                                                 "ISD Database"))
#'   
#'   
#'   cat("\n")
#'   print(db.matrix, quote = F)
#'   cat("\n\n\t\t\tStations in Database:\n\n")
#'   print(as.matrix(stations.df), quote = F)
#'   cat("\n\n\t\t\t\t\tStation-Years Downloaded:\n\n")
#'   print(download.years, quote = F)
#'   
#'   
#'   invisible(dbDisconnect(db.con))
#' }
#' 
#' 
