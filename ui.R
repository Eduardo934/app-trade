source("global.R")
source("Modules/admin-module.R")

##############Main UI shiny########################

shinyUI(
  bootstrapPage(
    useShinyjs(),
    extendShinyjs(script="js/script-shinyjs.js", functions = c("AddListContent")),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "CSS/styles.css"),
        tags$script(src = "js/script-extensions.js")
    ),
    theme = shinytheme("cerulean"),
    
    navbarPage(title = appName, id="tabs", collapsible = TRUE,
               tabPanel("Main",
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            uiOutput("lateralLeftPanel"),
                            
                            # Show main panel rendered by server
                            mainPanel(
                                fluidRow(style="margin-left:50%;",
                                         shinyauthr::loginUI(id = "login", cookie_expiry = cookieValue) 
                                ),
                                
                                uiOutput("mainPage")
                            )
                        )
                        
               ),
               tabPanel("Data",
                        uiOutput("tabPanelData")
               )
    ),
    div(shinyauthr::logoutUI(id = "logout", label="Log out"), style="position: absolute; top: 8px; right: 20px; z-index: 1000;")
    
    
))
