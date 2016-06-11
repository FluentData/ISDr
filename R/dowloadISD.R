downloadISD <- function(usaf, wban, year, history = getISDhistory()){
  # usaf = 720266; wban = 54809; year = 2015
  if(!usaf %in% history$USAF | !wban %in% history$WBAN){
    stop(paste(usaf, wban, sep = "-"), " is not a station")
  }
  url <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", year, "/", usaf,
                "-", wban, "-", year, ".gz")
  temp1 <- tempfile(fileext = ".gz")
  try(download.file(url=url, temp1))
  temp2 <- tempfile(fileext = ".txt")
  try(gunzip(temp1, temp2))
  try(df <- read.fortran(file=temp2, format=ISD_documentation$FIXED_FORMAT))
  unlink(c(temp1, temp2))
}

