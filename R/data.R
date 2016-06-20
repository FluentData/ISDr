#' Documentation for NOAA's Integrated Surface Data
#' 
#' The \code{ISDr} package facilitates the downloading of fixed length files
#' from the Integrated Surface Database via \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa}.
#' This data set contains documentation for the fields that are available to 
#' download. The \code{SHORT_NAME} column of this data set corresponds to 
#' column names in the ISD files.
#' @source \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf}
#' \describe{
#'   \item{SHORT_NAME}{The column name in the ISD file.}
#'   \item{START}{The starting position in the fixed width file.}
#'   \item{STOP}{The stopping position in the fixed width file.}
#'   \item{LENGTH}{The length of character positions.}
#'   \item{LONG_NAME}{The name of the column in the NOAA documentation.}
#'   \item{DESCRIPTION}{The description of the column in the NOAA documentation.}
#'   \item{DOM}{The domain.}
#'   \item{MIN}{The minimum value allowed for the column.}
#'   \item{MAX}{The maximum value allowed for the column.}
#'   \item{UNITS}{Unit of measurement.}
#'   \item{SCALING_FACTOR}{Scaling factor for a numeric column.}
#'   \item{MISSING}{Character value for the column that signifies a missing value.}
#'   \item{CLASS}{The class of the column when it is read into R.}
#'   \item{FIXED_FORMAT}{The format for the column that is used in the \code{read.fortran()}}
#' }
"ISD_documentation"

#' Lookup table for coded values in NOAA's Integrated Surface Data
#' 
#' The \code{ISDr} package facilitates the downloading of fixed length files
#' from the Integrated Surface Database via \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa}.
#' This data set contains the key for coded values in the downloaded data.
#' @source \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf}
#' \describe{
#'   \item{FIELD}{The name of the column in the downloaded weather data.}
#'   \item{CODE}{The character value found in the coded weather data.}
#'   \item{DESCRIPTION}{The corresponding value.}
#' }
"ISD_lookup"