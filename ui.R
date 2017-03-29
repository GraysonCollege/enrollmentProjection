# name: shiny app for projections
# author: Anderson Zhu
# email: zhua@grayson.edu

fluidPage(
    titlePanel("Projections for Following Semesters"),
    sidebarLayout(
        sidebarPanel(

            # select years
            # h5("1. Select years of data to be included for projection"),
            sliderInput("years", 
                        label = "Year", 
                        min = 2000, # if any data before 2000 are available, 
                                    # please update this number and the number below!
                        max = as.integer(format(Sys.Date(), "%Y")), 
                        value = c(2000, as.integer(format(Sys.Date(), "%Y"))), 
                        step = 1),
            
            tags$hr(),
            # h5("2. Select a projection variable "),
            # select target to predict
            radioButtons(inputId = "target",
                        label = "Target",
                        choices = c("Enrollment",
                                    "Attempted Credits",
                                    "Contact Hours"),
                        selected = "Enrollment"),
            

            tags$hr(),
            
            # select models
            radioButtons(inputId = "model", 
                         label = "Forecast Models", 
                         choices = c("Basic Linear Regression", 
                                     "Prior Moving Average (3yrs)", 
                                     "Prior Moving Average (5yrs)"), 
                         selected = "Basic Linear Regression"),
            
            tags$hr(),
            #h5("Show historical data?"),
            checkboxInput(inputId = "showData",
                          label = "Show Historical Enrollment Data?",
                          value = FALSE),            
            tags$hr(),
            
            # select report format
            radioButtons('format', 'Document format', c('Word', 'HTML', 'PDF'), inline = TRUE),
            
            # download report
            downloadButton('downloadReport')
        ),
        mainPanel(
            plotOutput('plot')
            , dataTableOutput('contents')
            
        )
    )
)