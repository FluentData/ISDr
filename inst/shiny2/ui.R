load("attributes.Rdata")

shinyUI(pageWithSidebar(
  
  headerPanel(h2("Integrated Surface Data"),windowTitle="Integrated Surface Data"),
  
  sidebarPanel(
    
    bsCollapse(multiple = T, open = "col3", id = "collapse1",
               
               bsCollapsePanel(h4("Display Options"), 
                               selectInput("stat", "Daily Value:", 
                                           list("Mean" = "mean", "Maximum" = "max",
                                                "Minimum" = "min")),
                               selectInput("station", "Stations:", attribs$station.ids),
                               id="col3", value="display")
    )                
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Time Series", showOutput("timeNvd3", "nvd3")),
      tabPanel("Wind Rose",
               
               bsNavBar("Options", brand = "shinyBS", 
                        #bsNavLink("nbLink1", "Link", href="#"),
                        bsNavDropDown("condition", "Split by", c("default", "season", "year", "weekday"), selected = "default")
                        #bsNavDivider(), 
                        #bsNavToggleLink("nbLink2", "Toggle", value=TRUE),        
                        #rightItems = list(
                        #  bsNavTextInput("nbText", "Text Input", width=75),
                        #  bsNavButton("nbButton", "Button")
                        #)
               ),
               
               bsAlert(inputId="aa"),                        
               plotOutput("windrose")
      )
    )
  )
  
)
)

