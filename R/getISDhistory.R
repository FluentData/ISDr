#' Download the latest version of the station history file
#' 
#' @description Downloads the file found at \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv}.
#' The table lists all stations, their location, and their period of activity.
#' @export
getISDhistory <- function(){
  read.table("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv",
             header = TRUE, sep = ",", stringsAsFactors = FALSE,
             colClasses = c("character", "character", rep(NA, 9)))
}
