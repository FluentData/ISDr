getISDhistory <- function(){
  read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv",
           stringsAsFactors = FALSE)
}