#' Recode Integrated Surface Data (ISD)
#'
#' @description Some columns of the data are coded. The codes can be found in
#'  \code{data(ISD_lookup)}. This function will replace the codes with the values
#'  found in the \code{DESCRIPTION} column of \code{ISD_lookup}.
#'  @param ISD_readings A \code{data.frame} of Integrated Surface Data.
#'  @param columns A character vector of columns to be recoded. If missing,
#'  all coded columns will be recoded.
#' @export
recodeISD <- function(ISD_readings, columns){
  # ISD_readings = df
  if(missing(columns)){
    columns <- names(ISD_readings)[names(ISD_readings) %in% unique(ISD_lookup$FIELD)]
  }else{
    if(sum(!columns %in% unique(ISD_lookup)) > 0){
      stop("columns do not match coded data: ",
           columns[columns %in% unique(ISD_lookup)])
    }
  }
  
  for(i in columns){
    # i = columns[3]
    lookup <- ISD_lookup %>% filter(FIELD == i)
    ISD_readings[[i]] <- lookup[match(ISD_readings[[i]], lookup[, "CODE"]), "DESCRIPTION"]
  }
  
  return(ISD_readings)
}