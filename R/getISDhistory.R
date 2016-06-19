#' @export
getISDhistory <- function(){
  read.table("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv",
             header = TRUE, sep = ",", stringsAsFactors = FALSE,
             colClasses = c("character", "character", rep(NA, 9)))
}
