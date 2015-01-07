
summary.isd <- function(x){

  sum.list <- attributes(x)[4:11]
  
  class(sum.list) <- c("isdsum", "list")
  
  return(sum.list)
  
}

print.isdsum <- function(x){
  stations <- matrix(x$station.ids, ncol = 1, 
                     dimnames = list(rep("", length(x$station.ids)), "Stations queried:"))
  
  cat("\n\n\t\t\tISD Query Summary\n\nQueried on:", paste0(x$time), "\n\nFrom",
      paste0(as.Date(as.character(x$date.range[1]), "%Y%m%d"), " to ",
             as.Date(as.character(x$date.range[2]), "%Y%m%d")), "\n\nPeriod:", x$period[1],"\n\n")
  print(stations, quote = F)
  
}

plot.isd <- function(isd.object){
  cat("\n\t\tPlease wait. This may take a while...\n")

  attribs <- attributes(isd.object)[4:11]
  
  shiny2.folder <- normalizePath(system.file("shiny2" , package = "ISDr"))
  save(attribs, file = paste0(shiny2.folder, "\\attributes.Rdata"))
  
  if(attr(isd.object, "period") != "daily"){
    #isd.object[, "date"] <- format(as.Date(as.character(isd.object$date), "%Y%m%d"), "%d/%m/%Y")
    isd.object[, "hour"] <- paste0(sprintf("%02d", as.numeric(isd.object$hour)), ":",
                                   sprintf("%02d", as.numeric(isd.object$minute)))
    
    write.csv(isd.object, paste0(shiny2.folder, "\\windrose.csv"))
    
    ts.data <- convertToDaily(isd.object, attribs$period)
    write.csv(ts.data, paste0(shiny2.folder, "\\timeseries.csv"))
  } else {
    write.csv(isd.object, paste0(shiny2.folder, "\\timeseries.csv"))
    invisible(try(file.remove(paste0(shiny2.folder, "\\windrose.csv"))))
  }
  
  shinyISD(name = "shiny2")
}