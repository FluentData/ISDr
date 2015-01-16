# .onAttach <- function(...){
#   if(!require(rCharts)){install_github("rCharts", "ramnathv")}
#   library(rCharts)
# }
# 
# 
# 
# queryISD <- function(date.range = "NULL", station.ids = "NULL", states = "NULL", counties = "NULL", 
#                      shape = c("long", "wide"), period = c("sub-hourly", "hourly", "daily"),
#                      percent.complete = "NULL", example = F){
#   
#   if(example == T){
#     db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#     db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
#                         dbname=paste0(db.folder, "/exampleISDdatabase"))} else {
#                           # get file path for database folder in library ISDr
#                           db.folder <- normalizePath(system.file("database" , package = "ISDr"))
#                           # create a connection to the database
#                           db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
#                                               dbname=paste0(db.folder, "/ISDdatabase"))
#                         }
#   
#   # check if query table already exists 
#   if("Query" %in% dbListTables(db.con)){query.exists = T} else{query.exists = F}
#   
#   stations.table <- dbReadTable(db.con, "Stations")
#   downloads.table <- dbReadTable(db.con, "Downloads")
#   
#   if(date.range[1] == "NULL"){date.range <- c(min(as.integer(downloads.table[, "year"])),
#                                               max(as.integer(downloads.table[, "year"])))}
#   
#   # create Query table based on monitor ids
#   if(station.ids[1] != "NULL"){
#     
#     query.df <- stations.table[stations.table$ID %in% station.ids, ] 
#  
#     # make the query dataframe a table in the database
#     dbWriteTable(conn = db.con, name = "Query", value = query.df,
#                  overwrite = query.exists, row.names = FALSE, header = TRUE)
#     
#     # create Query table based on counties
#   } else if(state[1] != "NULL"){
#     
#     query.df <- stations.table[stations.table$STATE %in% states, ]
#     
#     if (counties[1] != "NULL"){
#       query.df <- query.df[query.df$COUNTY %in% counties, ]
#     }
#     
#     # make the query dataframe a table in the database
#     dbWriteTable(conn = db.con, name = "Query", value = query.df,
#                  overwrite = query.exists, row.names = FALSE, header = TRUE)    
#   } 
#   
#   if(period[1] == "sub-hourly"){
#     sql.statement <- paste0("SELECT * FROM Data JOIN Query ON Data.ID=Query.ID WHERE Data.date BETWEEN ",
#                             date.range[1], " AND ", date.range[2], " ORDER BY Data.ID, date, hour, minute")
#   } else if(period[1] == "hourly"){
#     colnames <- dbListFields(db.con, "Data")
#     colnames <- paste0(sapply(colnames, function(x) paste0("h.", x)), collapse = ", ")
#         
#     sql.statement <- paste0("SELECT ", colnames, " FROM 
#                              (SELECT *, MIN(minute) AS minute_min FROM Data WHERE Data.date BETWEEN ",
#                             date.range[1], " AND ", date.range[2], " GROUP BY ID, date, hour) AS h
#                              JOIN Query AS q
#                                ON h.ID=q.ID ORDER BY h.ID, h.date, h.hour") 
#   } else if(period[1] == "daily"){
#     sql.statement <- paste0("SELECT h.ID, h.date, MAX(temperature) AS temp_max, MIN(temperature) AS temp_min,
#                               AVG(temperature) AS temp_mean, COUNT(temperature) AS temp_valid_hours,
#                               MAX(dew_point_temp) AS dew_point_max, MIN(dew_point_temp) AS dew_point_min,
#                               AVG(dew_point_temp) AS dew_point_mean, COUNT(dew_point_temp) AS dew_point_valid_hours, 
#                               MAX(atmospheric_pressure) AS atmospheric_pressure_max,
#                               MIN(atmospheric_pressure) AS atmospheric_pressure_min,
#                               AVG(atmospheric_pressure) AS atmospheric_pressure_mean,
#                               COUNT(atmospheric_pressure) AS atmospheric_pressure_valid_hours FROM
#                               
#                                (SELECT *, MIN(minute) AS minute_min FROM Data WHERE Data.date BETWEEN ",
#                                 date.range[1], " AND ", date.range[2], " GROUP BY ID, date, hour) AS h
#                                   JOIN Query as q
#                                     ON h.ID = q.ID GROUP BY h.ID, h.date")
#   }
#   
#   # send query  
#   res <- dbGetQuery(db.con, statement = sql.statement)
#   
#   
#   if(period[1] == "daily" & shape[1] == "wide"){
#     
#     # create empty list
#     list.dfs <- vector("list", length(clmn.names))
#     
#     for(i in 1:length(clmn.names)){
#       
#       # convert to wide
#       list.dfs[[i]] <- dcast(res, date ~ ID, value.var = clmn.names[i])
#       
#       # attach station ID to column names
#       station.ids <- colnames(list.dfs[[i]])[-1]
#       colnames(list.dfs[[i]])[2:(1 + length(station.ids))] <- sapply(station.ids, function(x) paste(clmn.names[i], x, sep = "_"))
#     }
#     
#     # create initial dataframe to be used in for loop
#     x <- list.dfs[[1]]
#     
#     # loop through and merge all dataframes
#     for(i in 2:(length(list.dfs))){
#       y <- list.dfs[[i]]
#       x <- merge(x, y, all=T)
#     }
#     
#     res <- x
#     
#   }
#   
#   dbDisconnect(db.con)
#   
#   attr(res, "date.range") <- date.range
#   attr(res, "station.ids") <- station.ids
#   attr(res, "counties") <- counties
#   attr(res, "shape") <- shape[1]
#   attr(res, "period") <- period[1]
#   attr(res, "percent.complete") <- percent.complete
#   attr(res, "time") <- Sys.time()
#   attr(res, "example") <- example
#   
#   class(res) <- c("isd", "data.frame")
#   
#   return(res)
# }
# 
# 
