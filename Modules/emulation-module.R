## Emulate module UI

EmulateModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(10,
           uiOutput(ns("UserName")),
           br(),
           
           fluidRow(style="margin-left:100px;", class="well",
                    h3("Data saved by user"),        
                    dataTableOutput(ns("SymbolsSavedByUser"))
           ),
           br(),
           h2("Simulation", class="title-black", style="margin-left:100px;"),
           fluidRow(style="margin-left:100px;", class="well",
                    fluidRow(
                      column(4, numericInput(ns("money"),  p("Money",class="white-text"),10000, min=1, max=100000)),
                      column(4, dateRangeInput(ns("dataRange"), p("Date range:",class="white-text"),
                                               start  = "2001-01-01",
                                               end    = "2010-12-31",
                                               min    = "2001-01-01",
                                               max    = "2021-09-30",
                                               format = "mm/dd/yy",
                                               separator = " - ")),
                      column(4, actionButton(ns("emulate"), "Emulate"), style="margin-top:18px;")
                    ),
                    br(),
                    fluidRow(
                      plotlyOutput(ns("emulateGraph"))
                    )
           )
    )
  )
}

## Emulate Module Server
EmulateModuleServer <- function(input, output, session ) {
  ## Reactive values to save the data about stock
  emulateData<- reactiveValues()
  
  ##Render the user name in the main window 
  output$UserName<- renderUI({
    req(UserData()$user)
    h2(paste("Welcome", UserData()$user),class="title-black" ,style= "margin-left:100px;")
  })
  
  ## Get the data about stocks saved by the user
  output$SymbolsSavedByUser<-renderDataTable({
    req(credentials()$user_auth)
    symbolsSaved<-SymbolsSavedByUser$Symbols[SymbolsSavedByUser$Symbols$user %in% UserData()$user,]
    datatable(symbolsSaved, selection = "single")
  })
  
  
  ### Emulate a performance with the data provided by the stock, the money and the time. And return the emulate data
  observeEvent(input$emulate,{
    req(credentials()$user_auth)
    if(is.null(input$SymbolsSavedByUser_rows_selected)){
      showNotification(h3("Select a row on table"), duration = 5,
                       id = NULL, type = c("error"))
      return()
    }
    symbol<- SymbolsSavedByUser$Symbols[SymbolsSavedByUser$Symbols$user %in% UserData()$user,]
    
    symbol<- symbol[input$SymbolsSavedByUser_rows_selected, "symbol"]
    
    
    
    responseGetSymbol = GET('https://www.alphavantage.co/query',
                query = list("function" = "TIME_SERIES_MONTHLY", "symbol" = symbol, "apikey"="0PYUTT0O5MG98O0P", "datatype" = "csv"))
    
    symbolData <- content(responseGetSymbol, type="text/csv")
    
    if(ncol(symbolData) < 3){
      showNotification(
        h4("This symbol does not exists on database"), 
        action = NULL, duration = 5, type = "warning")
      
      return()
    }
    
    
    dataToPlot<- symbolData
    dataToPlot<- dataToPlot[(dataToPlot$timestamp > input$dataRange[1] & dataToPlot$timestamp < input$dataRange[2]),]
    
    
    if(nrow(dataToPlot) < 1){
      showNotification(h3("No data available on this date"), duration = 5,
                       id = NULL, type = c("error"))
      return()
    }
    
    
    StockNumber <- input$money/dataToPlot[1,"close"]
    
    dataToPlot$inversion <- as.vector(dataToPlot$open) * as.numeric(StockNumber)
    
    emulateData$dataToPlot<-dataToPlot
    
  })
  
  
  ## Render a plot with the emulate data 
  output$emulateGraph<-renderPlotly({
    
    req(credentials()$user_auth)
    req(emulateData$dataToPlot)
    dataPlot<-emulateData$dataToPlot
    ggplotTimeSeries<-dataPlot %>%
      ggplot(aes(x= timestamp, y= inversion))+
      geom_line(color ="blue") + 
      scale_y_log10() +
      labs(title = "Line Chart", 
           subtitle = "Log Scale", 
           y = "Closing Price", x = "") + 
      theme_linedraw()+
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)
      )
    
    ggplotly(ggplotTimeSeries) %>%config(displaylogo=FALSE)
  })
  
  
}