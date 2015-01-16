load("attributes.Rdata")
if(attribs$period != "daily"){
  invisible(wr.data <- import("windrose.csv", date.format = "%Y%m%d", time = "hour",
                    time.format = "%H:%M", ws = "wind_speed", wd = "wind_direction"))
  wr.data <- select(wr.data, V1:hour, wd, ws)
} 

ts.data <- read.csv("timeseries.csv")
ts.data <- melt(ts.data, id.vars = c("ID", "date"))
ts.data <- subset(ts.data, grepl("\\d", ts.data[, "value"]))
ts.data$date <- as.Date(as.character(ts.data$date), format="%Y%m%d")
ts.data$date <- as.POSIXlt(ts.data$date, origin = "1970-01-01")
ts.data$date <- as.double(ts.data$date)*1000

cat("\n\tTo close web plot, press Esc.")

################################################################################################
shinyServer(function(input, output, session){
  
  getTSdata <- reactive({
    data <- subset(ts.data, ID == input$station)
    data <- subset(data, grepl(input$stat, data[, "variable"], fixed = T))
    data
  })
  
  # Time series plot using the NVD3 library from rCharts
  output$timeNvd3 <- renderChart ({
    data <- getTSdata()
    
    n1 = nPlot(value ~ date, group = "variable", 
               data = data, type = "lineWithFocusChart")
    n1$xAxis(
      tickFormat=
        "#!function(d) {return d3.time.format('%Y %m %d')(new Date(d));}!#"
    )
    n1$x2Axis(
      tickFormat=
        "#!function(d) {return d3.time.format('%Y %m %d')(new Date(d));}!#"
    )
    n1$set(dom="timeNvd3")
    
    return(n1)  
  })
  
  addPopover(session, id="timeNvd3", title="Time Series", 
             content = "Daily values of temperature, dew point temperature, and barometric pressure.",
             placement = "top", trigger = "hover")
  
  
  getWR <- reactive({
    data <- subset(wr.data, ID == input$station)
    data
  })
  
  output$windrose <- renderPlot({
    data <- getWR()
    windRose(data, type = input$condition, grid.line=5,annotate=FALSE,
             key.position="right",offset=3, breaks=5,paddle=FALSE)   
  })
  
  createAlert(session, inputId = "aa",
              message = "The wind rose graph can only be displayed if the queried data contains hourly or sub-hourly data.",
              type = "warning",
              dismiss = T,
              block = T,
              append = TRUE
  )
  
})