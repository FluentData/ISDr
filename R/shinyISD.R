shinyISD <- function(name = "shiny"){
  runApp(normalizePath(system.file(name , package = "ISDr")))
}

convertToDaily <- function(isd.object, period){
  if(period == "sub-hourly"){
    # subset down to readings at the beginning of the hour
    isd.object <- group_by(isd.object, ID, date, hour)
    isd.object <- filter(isd.object, minute == min(minute))
  }
  
  # create column names
  clmn.names <- c("temp_max", "temp_min", "temp_mean", "temp_valid_hours", "dew_point_max",
                  "dew_point_min", "dew_point_mean", "dew_point_valid_hours", "atmospheric_pressure_max",
                  "atmospheric_pressure_min", "atmospheric_pressure_mean", "atmospheric_pressure_valid_hours")
  
  # create daily summarized columns
  isd.object <- ddply(isd.object, .(ID, date), 
               function(df) c(max(df[, "temperature"], na.rm=T), min(df[, "temperature"], na.rm=T),
                              mean(df[, "temperature"], na.rm=T), sum(as.integer(df[, "temperature_quality"])),
                              max(df[, "dew_point_temp"], na.rm=T), min(df[, "dew_point_temp"], na.rm=T),
                              mean(df[, "dew_point_temp"], na.rm=T), sum(as.integer(df[, "dew_point_temp_quality"])),
                              max(df[, "atmospheric_pressure"], na.rm=T), min(df[, "atmospheric_pressure"], na.rm=T),
                              mean(df[, "atmospheric_pressure"], na.rm=T), sum(as.integer(df[, "atmospheric_pressure_quality"]))))
  
  # attach column names
  colnames(isd.object)[3:14] <- clmn.names
  
  isd.object
}