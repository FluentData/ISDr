downloadISD <- function(usaf, wban, year, history = getISDhistory(),
                        parameters){
  # usaf = 720266; wban = 54809; year = 2015
  if(!usaf %in% history$USAF | !wban %in% history$WBAN){
    stop(paste(usaf, wban, sep = "-"), " is not a station")
  }
  
  formats <- ISD_documentation$FIXED_FORMAT
  
  if(!missing(parameters)){
    if(sum(!parameters %in% formats) > 0){
      stop("Invalid parameter(s)")
    }
    
    skip <- !formats %in% parameters
    
    skip_length <- ISD_documentation[skip, "LENGTH"]
    
    skip_formats <- paste0("X", skip_length)
    
    formats[skip] <- skip_formats
  
    }else{
    formats <- ISD_documentation$FIXED_FORMAT
  }
  
  url <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", year, "/", usaf,
                "-", wban, "-", year, ".gz")
  
  temp1 <- tempfile(fileext = ".gz")
  
  try_download <- try(download.file(url = url, temp1))
  
  if(class(try_download) == "try-error"){
    error_df <- data.frame(TYPE = "download_error",
                           MESSAGE = attr(try_download, "condition")$message,
                           DATE_TIME = Sys.time(),
                           stringsAsFactors = FALSE)
    class(error_df) <- c("data.frame", "error")
    return(error_df)
  }
  
  temp2 <- tempfile(fileext = ".txt")
  
  try_unzip <- try(gunzip(temp1, temp2))
  
  if(class(try_unzip) == "try-error"){
      error_df <- data.frame(TYPE = "unzip_error",
                             MESSAGE = attr(try_unzip, "condition")$message,
                             DATE_TIME = Sys.time(),
                             stringsAsFactors = FALSE)
    class(error_df) <- c("data.frame", "error")
    unlink(temp1)
    return(error_df)
  }
  
  try_read_fortran <- try(df <- read.fortran(file = temp2, format = formats))
  unlink(c(temp1, temp2))
  
  if(class(try_read_fortran) == "try-error"){
    error_df <- data.frame(TYPE = "read_fortran_error",
                           MESSAGE = attr(try_unzip, "condition")$message,
                           DATE_TIME = Sys.time(),
                           stringsAsFactors = FALSE)
    class(error_df) <- c("data.frame", "error")
    return(error_df)
  }
  
  df

}

