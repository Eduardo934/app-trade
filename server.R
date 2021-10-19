source("global.R")
source("Modules/admin-module.R")
source("Modules/search-symbol-module.R")
source("Modules/emulation-module.R")


shinyServer(function(input, output) {

    
    source("Rfunctions/authentication_functions.R")
    
    ## Lateral Panel on tabPanel Main
    output$lateralLeftPanel<-renderUI({
        req(credentials()$user_auth)
        req(UserData()$license%in%"customer")
        SearchSymbolModuleUI("SymbolModuleID")
        
    })
    
    ##Main panel on tabPanel Main
    output$mainPage<- renderUI(expr = if(credentials()$user_auth){
        if(UserData()$license%in%"customer"){
            SearchSymbolModuleUIplot("SymbolModuleID")
        } else {
            AdminModuleUI("AdminModuleID")
        }
    })
    
    ## tabPanel Data
    output$tabPanelData<-renderUI(expr = if(credentials()$user_auth){
        req(UserData()$license%in%"customer")
        EmulateModuleUI("EmulateModuleID")
    })
    
    
    ## Reactive values and module for Admin (Create and delete users)
    callModule(AdminModuleServer, "AdminModuleID")
    
    
    
    ###genera los resultados de busqueda de symbol_trade
    callModule(SearchSymbolModuleServer, "SymbolModuleID")
    
    
    ##Emulator module
    callModule(EmulateModuleServer, "EmulateModuleID")
    
    

})
