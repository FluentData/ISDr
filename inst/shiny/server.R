


shinyServer(function(input, output, session){
  
  shinyGetData <- function(station.years, fixed.length.formats, col.names){
    
    station.ids <- substr(station.years, 1, 12)
    
    print("Station IDs in getData()")
    print(station.ids)
    
    years <- substr(station.years, 14, 17)
    print("Years in getData()")
    print(years)
    
    percent.complete <- (1:length(station.ids)/length(station.ids))*100
    
    # get file path for database folder in library ISDr
    db.folder <- normalizePath(system.file("database" , package = "ISDr"))
    
    downLoad <- function(year, id, progress){
      
      url <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", year, "/", id, "-", year, ".gz")
      temp <- tempfile(fileext = ".gz")
      try(download.file(url=url, temp))
      fl.name <- paste0(db.folder, strsplit(id, "-")[[1]][1], "_", strsplit(id, "-")[[1]][2], "_", year, ".csv")
      print(fl.name)
      try(gunzip(temp, fl.name))
      unlink(temp)
      try(dframe <- read.fortran(file=fl.name, format=fixed.length.formats))
      if(exists("dframe")){
        colnames(dframe) <- col.names
        
        #add ID column
        dframe <- data.frame(ID = paste(dframe[, "USAF.id"], dframe[, "WBAN.id"], sep = "-"), dframe)
        
        #print(colnames(dframe))
        
        # create a connection to ISDdatabase
        con <- dbConnect(dbDriver("SQLite", max.con = 25), 
                         dbname=paste0(db.folder, "/ISDdatabase"))
        
        # append the data to the Data table
        dbWriteTable(conn = con, name = "Data", value = dframe,
                     row.names = FALSE, header = TRUE, append = TRUE)
        dbDisconnect(con)
        
        remove(dframe)
        unlink(fl.name)
        
        print(progress)
        
        updateProgressBar(session, "pb", progress, animate = T, visible = T)
        
      } else {unlink(fl.name)
              print(progress)
              updateProgressBar(session, "pb", progress, animate = T, visible = T)}
    }
    
    
    
    mapply(FUN = downLoad, year = years, id = station.ids, progress = percent.complete)
    
    #   loopYears <- function(id){
    #     sapply(years, downLoad, id=id, db.con=con)
    #   }
    #   
    #   sapply(station.ids, loopYears)
    
  }
  
  # Initialize database of integrated surface data from NOAA/NCDC
  shinyDownloadISD <- function(state="IN", years=c(2008), example = F){
    
    # get file path for database folder in library ISDr
    db.folder <- normalizePath(system.file("database" , package = "ISDr"))
    
    # create file path for csv document with station information
    stations.file <- paste0(db.folder, "/stations.csv")
    
    # download table with staion information
    download.file("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-history.csv", stations.file)
    
    # read in the station information
    stations.df <- read.csv(stations.file, header = T, colClasses = rep_len("character", 12),
                            stringsAsFactors=F)
    
    # insert decimal into lat and lon columns
    stations.df[, "LAT"] <- as.numeric(paste(substr(stations.df[, "LAT"], 1, 3),
                                             substr(stations.df[, "LAT"], 4, 6), sep = "."))
    stations.df[, "LON"] <- as.numeric(paste(substr(stations.df[, "LON"], 1, 4),
                                             substr(stations.df[, "LON"], 5, 7), sep = "."))
    
    # chop off month and day from BEGIN and END values
    stations.df[, "BEGIN"] <- as.numeric(substr(stations.df[, "BEGIN"], 1, 4))
    stations.df[, "END"] <- as.numeric(substr(stations.df[, "END"], 1, 4))
    
    colnames(stations.df)[c(3, 10)] <- c("NAME", "ELEV")
    
    if(length(years) == 1) years <- rep.int(years, 2)
    
    # subset based on state
    stations.df <- filter(stations.df, STATE == state, BEGIN <= years[2] & END >= years[1])
    #stations.df <- filter(stations.df, STATE == state, BEGIN %in% years[1]:years[2] | END %in% years[1]:years[2])
    
    # create COUNTY column
    COUNTY <- matrix(unlist(strsplit(latlong2county(stations.df[c(9, 8)]), ",", fixed = T)), 
                     ncol = 2, byrow = T)[, 2]
    
    # insert ID and COUNTY columns into data frame
    stations.df <- data.frame(ID = paste(stations.df[, 1], stations.df[, 2], sep = "-"), 
                              stations.df[, 1:6], COUNTY, stations.df[, 7:12])
    
    # delete the original station csv file
    unlink(stations.file)
    
    # create a connection to ISDdatabase
    db.con <- dbConnect(dbDriver("SQLite", max.con = 25), 
                        dbname=paste0(db.folder, "/ISDdatabase"))
    #   
    #   # add the station dataframe to the Stations table
    #   dbWriteTable(conn = db.con, name = "Stations", value = stations.df,
    #                row.names = FALSE, header = TRUE)
    #   
    #     print(dbListTables(db.con))              # The tables in the database
    #     print(dbListFields(db.con, "Stations"))    # The columns in a table
    #     print(head(dbReadTable(db.con, "Stations")))     # The data in a table
    
    # load dataframe called formats for read.fortran() function
    file.formats <- read.csv(paste0(db.folder, "/ISD_fixed_format.csv"), 
                             header=T, stringsAsFactors=F)
    
    formats <- mapply(makeFormats, length=file.formats[, "positions"], 
                      type=file.formats[, "class"], 
                      decimals=file.formats[, "decimal.places"])
    
    column.names <- file.formats[file.formats[, "class"]!="skip", "description"]
    
    print(stations.df)
    
    # get Dowloads table in database
    downloads.old <- dbReadTable(db.con, "Downloads")
    print("Old Downloads Table")
    print(downloads.old)
    
    # create vector of existing station/years
    station.years.old <- paste(downloads.old$ID, downloads.old$year, sep = "-")
    print("Old station-years")
    print(station.years.old)
    
    
    # create vector of the new station/years
    station.years.new <- expand.grid(ID = stations.df$ID, year = years[1]:years[2])
    station.years.new <- paste(station.years.new$ID, station.years.new$year, sep = "-")
    print("New station-years")
    print(station.years.new)
    
    # create vector of station/years that do not already exist in database
    station.years.diff <- station.years.new[!station.years.new %in% station.years.old]
    print("Station-years difference")
    print(station.years.diff)
    
    
    
    shinyGetData(station.years = station.years.diff, fixed.length.formats = formats,
                 col.names = column.names)
    
    
    downloads.new <- data.frame(ID = substr(station.years.diff, 1, 12), year = substr(station.years.diff, 14, 17))
    
    dbWriteTable(conn = db.con, name = "Downloads", value = downloads.new,
                 row.names = FALSE, header = TRUE, append = TRUE)
    
    # get Stations table in database
    stations.old <- dbReadTable(db.con, "Stations")
    
    stations.merged <- merge(stations.old, stations.df, all = T)
    
    dbWriteTable(conn = db.con, name = "Stations", value = stations.merged,
                 row.names = FALSE, header = TRUE, overwrite = TRUE)
    
    updateProgressBar(session, "pb", visible = F)
    
    dbDisconnect(db.con)
    
  }
  
  
  values <- reactiveValues(x = 0)
  
  observe({
    if(input$download > 0) {
      
      state = isolate(input$state)
      range = isolate(input$range)
      x = isolate(values$x)
      
      updateProgressBar(session, "pb", value = 2, animate = T, visible = T)
      
      shinyDownloadISD(state = state, years = c(first(range), last(range)))
      
      
      values$x <- x + 1
      
    }
  })
  
  output$stations <- renderTable({
    values$x 
    return(stationsISDdb(example = input$example))
    
  })
  
  queryOutput <- reactive({
    dates <- sapply(input$date.range, function(x) gsub("-", "", x))
    data <- queryISD(date.range = dates, station.ids = input$monitor, period = input$period,
                     example = input$example)
    return(data)
  })
  
  output$data <- renderTable({
    return(head(queryOutput(), 100))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$monitor, dates[1], "through", dates[2], '.csv', sep='_') },
    content = function(file) {
      write.csv(queryOutput(), file)})
  
#   # Time series plot using the NVD3 library from rCharts
#   output$timeNvd3 <- renderChart ({
#     
#     dates <- sapply(input$date.range, function(x) gsub("-", "", x))
#     data <- queryISD(date.range = dates, station.ids = input$monitor, period = "daily")
#     
#     melted <- melt(data, id.vars = c("ID", "date"))
#     melted <- subset(melted, grepl(input$stat, melted[, "variable"], fixed = T))
#     melted <- subset(melted, grepl("\\d", melted[, "value"]))
#     melted$date <- as.Date(as.character(melted$date), format="%Y%m%d")
#     melted$date <- as.POSIXlt(melted$date, origin = "1970-01-01")
#     melted$date <- as.double(melted$date)*1000
#     
#     print(head(melted))
#     
#     n1 = nPlot(value ~ date, group = "variable", 
#                data = melted, type = "lineWithFocusChart")
#     n1$xAxis(
#       tickFormat=
#         "#!function(d) {return d3.time.format('%Y %m %d')(new Date(d));}!#"
#     )
#     n1$x2Axis(
#       tickFormat=
#         "#!function(d) {return d3.time.format('%Y %m %d')(new Date(d));}!#"
#     )
#     n1$set(dom="timeNvd3")
#     
#     return(n1)  
#   })
#   
#   addPopover(session, id="timeNvd3", title="Time Series", 
#              content = "Daily values of temperature and dew point temperature from queried data.",
#              placement = "left", trigger = "hover")
  
  #   observe({
  #     updateTabsetPanel(session, inputId = "tabset", selected = input$collapse1)
  #   })
  
  
  observe({
    if(values$x > 0) {
      
      print(stationsISDdb()[, "ID"])
      updateTypeAhead(session, "monitor", choices = stationsISDdb()[, "ID"])
      
    } else if(input$example == T){
      print(stationsISDdb(example = T)[, "ID"])
      updateTypeAhead(session, "monitor", choices = stationsISDdb(example = T)[, "ID"])
    }
  })
  
  getDownloads <- reactive({
    data <- downloadedYearsISD(example = input$example, db.table = T)
    return(data)
  })
  
  output$heatmap <- renderPlot({
    downloads <- getDownloads()
    p1 <- ggplot(downloads, aes(x = as.factor(year), y = as.factor(ID))) +
      geom_tile(aes(fill = nrows)) +
      scale_fill_gradient2(low="white", high="blue") +
      xlab("Downloaded Years") +
      ylab("Downloaded Stations")
    print(p1)
  })
  
  output$leafletmap <- renderMap({
    mapISDdb(example = input$example)
  })
  
  
  
})




