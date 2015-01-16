shinyUI(pageWithSidebar(
  
  headerPanel(h2("Integrated Surface Data"),windowTitle="Integrated Surface Data")
  
  ,
  
  sidebarPanel(
    
    bsCollapse(multiple = T, open = "col1", id = "collapse1",
               bsCollapsePanel(h4("Download Options"), selectInput("state", "State:", state.abb),
                               selectInput("range", "Range of Years:", as.numeric(substr(Sys.Date(), 1, 4)):1901, multiple = T),
                               actionButton("download", "Download"),
                               bsProgressBar("pb", animate = T, visible = F),
                               checkboxInput("example", "Use Example Database:", FALSE),
                               id="col1", value="download"),
               bsCollapsePanel(h4("Query Options"), 
                               #textInput("monitor", "Monitor ID:"),
                               bsNavTypeAhead(inputId = "monitor", label = "Monitor ID:", choices = ""),
                               dateRangeInput("date.range", "Date Range:", format = "yyyy-mm-dd"),
                               selectInput("period", "Averaging Period:", c("raw", "hourly", "daily")), 
                               id="col2", value="query"),
               bsCollapsePanel(h4("Display Options"), 
                               selectInput("stat", "Daily Value:", list("Mean" = "mean", "Maximum" = "max",
                                                                        "Minimum" = "min")), 
                               id="col3", value="display")
    )                
   
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Download",   #showOutput("leafletmap", "leaflet")
        
        bsCollapse(multiple = T, open = "colC", id = "collapse2",
                   bsCollapsePanel(h3("Visualize Database"), plotOutput("heatmap"), id = "colA"),
                   #bsCollapsePanel(h3("Stations Map"), showOutput("leafletmap", "leaflet"), id = "colB"),
                   bsCollapsePanel(h3("Stations in Database"), tableOutput("stations"), id = "colC")
          ), value = "download"
        
      ),
      
      tabPanel("Map", showOutput("leafletmap", "leaflet")),
        
      tabPanel("Query", downloadButton('downloadData', 'Download'), 
               bsTooltip(id = "downloadData", title = "Download query as CSV",
                         placement = "top"),
               div(""),
               tableOutput("data"), value = "query")
#       ,
#       tabPanel("Display", showOutput("timeNvd3", "nvd3"), value = "display")
      ,
      id = "tabset"
    )
    
  )
)
)

