###### Symbol search Module UI inputs

SearchSymbolModuleUI <- function(id) {
  ns <- NS(id)
  
  sidebarPanel(id="lateralLeftPanel", width = 3, style = "position:fixed;width:20%;",
               h3("Select Inputs"),
               selectInput(ns("timeSeries"), p("Time series",class="white-text"), 
                           choices = list("Daily" = "TIME_SERIES_DAILY", 
                                          "Weekly" = "TIME_SERIES_WEEKLY", "Monthly" = "TIME_SERIES_MONTHLY")),
               
               tags$div(id="autocomplete", class="dropdown",
                        textInput(ns("tradeSymbol"),  p("Symbol Search",class="white-text"), value="AMZN"),
                        tags$div(id="dropdown-content" ,class="dropdown-content")
               ),
               actionButton(ns("search"), "Search"),
               h3("Save Data"),
               actionButton(ns("save"), "Save"),
               h3("Download data"),
               uiOutput(ns("downloadButton"))
               
  )
}




###### Symbol search Module UI plots
SearchSymbolModuleUIplot <- function(id) {
  ns <- NS(id)
  
  div(
    fluidRow(
      uiOutput(ns("title")),
      column(6,
             plotlyOutput(ns('timeseriesPlot'))
             
      ), 
      column(6,
             plotlyOutput(ns('volumenPlot'))
      )
    ),
    fluidRow( class="well",
              dataTableOutput(ns("StockDataPlot"))
    )
  )
  
}


###### Symbol search Module Server
SearchSymbolModuleServer <- function(input, output, session) {
  
  ns <- session$ns
  
  ##
  StockData<- reactiveValues()
  ## Render autocomplete field by typing
  observe({
    req(credentials()$user_auth)
    req(input$tradeSymbol)
    
    
    if(nchar(input$tradeSymbol) %in% 0){
      return()
    }
    responseAutocompleteData = GET('https://www.alphavantage.co/query',
              query = list("function" = "SYMBOL_SEARCH", keywords = input$tradeSymbol, apikey="0PYUTT0O5MG98O0P"))
    
    autocompleteData <- fromJSON(rawToChar(responseAutocompleteData$content))
    
    
    if(class(autocompleteData$bestMatches) != "data.frame"){
      StockData$symbols = "none"
    } else {
      StockData$symbols = autocompleteData$bestMatches[,1]
    }
    ## Run js function from the "www/js/script-shinyjs.js" file
    js$AddListContent(StockData$symbols)
    
  })
  
  ### Run the code one time to get one symbol by deafault
  
  responseStockDefault = GET('https://www.alphavantage.co/query',
              query = list("function" = "time_series_daily", "symbol" = "AMZN", "apikey"="0PYUTT0O5MG98O0P", "datatype" = "csv"))
  
  symbolData<- content(responseStockDefault, type="text/csv")
  
  
  StockData$symbolData<-symbolData[1:100,]
  
  
  
  
  ## Table with tranding data
  output$tabla<-renderDataTable({
    req(credentials()$user_auth)
    req(StockData$symbolData)
    
    datatable(StockData$symbolData)
  })
  
  
  ## Take the inputs and call the API and save the data in the reactive values
  
  observeEvent(input$search,{
    req(credentials()$user_auth)
    req(input$timeSeries)
    req(input$tradeSymbol)
    
    
    responseSymbolData = GET('https://www.alphavantage.co/query',
               query = list("function" = input$timeSeries, "symbol" = input$tradeSymbol, "apikey"="0PYUTT0O5MG98O0P", "datatype" = "csv"))
    
    
    
    symbolData<- content(responseSymbolData, type="text/csv")
    
    if(ncol(symbolData) < 3){
      showNotification(
        h4("This symbol does not exists in database"), 
        action = NULL, duration = 5, type = "warning")
      
      return()
    } ## Need to be improved
    
    
    
    StockData$symbolData<-symbolData[1:100,]
    
    StockData$symbolName<-input$tradeSymbol
  })
  
  
  ## Render a time series plot with the reactive values
  output$timeseriesPlot<-renderPlotly({
    req(credentials()$user_auth)
    req(StockData$symbolData)
    
    dataToPlot<-StockData$symbolData
    names(dataToPlot)<-c("Date", "Open", "High", "low", "close", "volume")
    
    ggplotTimeseries<-dataToPlot %>%
      ggplot(aes(x= Date, y= Open))+
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
    
    ggplotly(ggplotTimeseries) %>%config(displaylogo=FALSE)
  })

  
  ## Render a valumen plot with the reactive values
  output$volumenPlot<-renderPlotly({
    req(credentials()$user_auth)
    req(StockData$symbolData)
    
    dataToPlot<-StockData$symbolData
    names(dataToPlot)<-c("Date", "Open", "High", "low", "close", "volumen")
    
    ggplotVolumen<-dataToPlot %>%
      ggplot(aes(x = Date, y = volumen)) +
      geom_segment(aes(xend = Date, yend = 0, color = volumen)) + 
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = "Volume Chart", 
           subtitle = "Charting  Volume", 
           y = "Volume", x = "") +
      theme_linedraw() +
      theme(legend.position = "none", 
            panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA)
      ) 
    
    ggplotly(ggplotVolumen) %>%config(displaylogo=FALSE)
  })
  
  ## Render a title 
  output$title<-renderUI({
    req(credentials()$user_auth)
    req(StockData$symbolName)
    
    h2(paste("Symbol - ",input$tradeSymbol), class="title-black")
  })
  
  ## Render datatable with tranding data
  output$StockDataPlot<-renderDataTable({
    req(credentials()$user_auth)
    req(StockData$symbolData)
    datosc<-StockData$symbolData
    names(datosc)<-c("Date", "Open", "High", "low", "close", "volumen")
    datatable(datosc)
  })
  
  ## Save data in the database
  observeEvent(input$save,{
    req(credentials()$user_auth)
    req(input$tradeSymbol)
    
    newSymbolSqlScript <- 'INSERT INTO symbols (user,symbol, date) 
                    VALUES (?user,?sym, ?dat);'
    
    
    newSymbolSqlQuery <- sqlInterpolate(DbConnection, newSymbolSqlScript, user= UserData()$user, 
                                 sym=input$tradeSymbol, dat=Sys.Date())
    
    dbGetQuery(DbConnection, newSymbolSqlQuery)
    
    SymbolsByUserSqlScript <- "SELECT * FROM symbols"
    newSymbolSqlQuery_tranding <- sqlInterpolate(DbConnection, SymbolsByUserSqlScript)
    SymbolsSavedByUser$Symbols<-dbGetQuery(DbConnection, newSymbolSqlQuery_tranding)
    
    showNotification(h3("Data saved"), duration = 5,
                     id = NULL, type = c("message"))
  })
  
  
  ## Render download button
  output$downloadButton<-renderUI(expr = if(!is.null(StockData$symbolData)){
    downloadButton(ns("dnw"), "Donwload")
  } else {
    NULL
  })
  
  ## Download data
  output$dnw<-downloadHandler(
    filename="trandingData.csv",
    content=function(file){
      req(credentials()$user_auth)
      req(StockData$symbolData)
      datosd<-StockData$symbolData
      names(datosd)<-c("Date", "Open", "High", "low", "close", "volumen")
      write.csv(datosd,file, row.names = FALSE)
    }
  )
}