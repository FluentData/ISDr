#' Download Integrated Surface Data from NOAA
#' 
#' @param usaf The USAF master station catalog identifier.
#' @param wban The NCEI WBAN station identifier.
#' @param year The year of data.
#' @param parameters A vector of parameters to download. For a list of parameters, 
#' see the \code{SHORT_NAME} column in \code{data(ISD_documentation)}. If missing,
#' all parameters are downloaded.
#' @export
downloadISD <- function(usaf, wban, year, parameters){
  # usaf = 720266; wban = 54809; year = 2015; parameters = c("USAF", "WBAN", "DATE", "TIME", "LATITUDE", "LONGITUDE", "WIND_DIR", "WIND_SPEED", "TEMP", "DEW_POINT_TEMP", "SEA_LEVEL_PRESSURE")
  
  formats <- ISD_documentation$FIXED_FORMAT
  short_names <- ISD_documentation$SHORT_NAME
  
  if(!missing(parameters)){
    if(sum(!parameters %in% short_names) > 0){
      stop("Invalid parameter(s)")
    }
    
    skip <- !short_names %in% parameters
    
    skip_length <- ISD_documentation[skip, "LENGTH"]
    
    skip_formats <- paste0("X", skip_length)
    
    formats[skip] <- skip_formats
  
    }else{
    formats <- ISD_documentation$FIXED_FORMAT
    parameters <- ISD_documentation$SHORT_NAME
  }
  
  url <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", year, "/", usaf,
                "-", wban, "-", year, ".gz")
  
  temp1 <- tempfile(fileext = ".gz")
  
  try_download <- try(download.file(url = url, temp1))
  
  if(class(try_download) == "try-error"){
    error_df <- data.frame(TYPE = "download_error",
                           MESSAGE = attr(try_download, "condition")$message,
                           USAF = usaf, WBAN = wban, YEAR = year,
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
                             USAF = usaf, WBAN = wban, YEAR = year,
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
                           USAF = usaf, WBAN = wban, YEAR = year,
                           DOWNLOAD_TIME = Sys.time(),
                           stringsAsFactors = FALSE)
    class(error_df) <- c("data.frame", "error")
    return(error_df)
  }
  
  names(df) <- parameters
  
  df <- addMissing(df)
  
  return(df)

}

addMissing <- function(data, doc = ISD_documentation){
  doc <- doc %>%
    filter(SHORT_NAME %in% names(data), !is.na(MISSING))
  for(i in doc$SHORT_NAME){
    # i = doc$SHORT_NAME[1]
    doc_row <- doc[doc$SHORT_NAME == i, ]
    if(doc_row$CLASS == "numeric"){
      missing <- as.numeric(doc_row$MISSING)
      if(!is.na(doc_row$SCALING_FACTOR)){
        missing <- missing/doc_row$SCALING_FACTOR
      }
    }else{
      missing <- doc_row$MISSING
    }
    nas <- as.character(data[[i]]) == as.character(missing) 
    data[nas, i] <- NA
  }
  data
}



