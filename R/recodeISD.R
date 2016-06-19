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