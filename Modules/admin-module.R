##module UI
AdminModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(style="align-content: center;",
        column(width = 10, class="well", style="margin-left:30%;",
               dataTableOutput(ns("tableUsers")),
               br(),
               br(),
               fluidRow(
                 column(width = 2, actionButton(ns("deleteUsers"), label = "Delete",  class = "btn-new")),
                 column(width = 2, actionButton(ns("newUser"), label = "New",  class = "btn-new"))
               )
        )
    )
  )
}



##Module server
AdminModuleServer <- function(input, output, session) {
  
  ns <- session$ns
  
  ## Get all user saved in the database and are saved on a reactive expretion
  users<- reactiveValues()
  GetUsersSqlScript <- "SELECT * FROM users"
  GetUsersQuery<-dbGetQuery(DbConnection, GetUsersSqlScript)
  
  users$data<-GetUsersQuery
  
  ## Render the datatable with the users
  output$tableUsers<- renderDataTable({
    req(credentials()$user_auth)
    req(UserData()$license %in% "administrator")
    
    users_plot<- users$data
    
    datatable(users_plot[,c("user", "license", "names")], options = list(
      pageLength = 10,
      scrollX = TRUE,
      searching = TRUE
    ))
  })
  
  ## Close modal when input cancel is clicked
  observeEvent(input$cancel,{
    removeModal()
  })
  
  ## Dispay the input Modal to type the data to create a new user
  observeEvent(input$newUser,{
    showModal(
      modalDialog(
        title = "New User",
        fluidRow(
          column(width = 12,
                 textInput(ns("userNew"), label = h4("User")),
                 passwordInput(ns("passwordNew"),label = h4("Password")),
                 radioButtons(ns("licenseNew"), label = h4("License"),
                              choiceNames = c("Customer","Administrator"),
                              choiceValues = c("customer","administrator")
                 ),
                 textInput(ns("namesNew"),label = h4("Name"))
          )
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("cancel"),"Cancel"),
          actionButton(ns("saveNewUser"),"Save")
        )
      )
    )
  })
  
  ## Save the new user on the database
  observeEvent(input$saveNewUser,{
    req(credentials()$user_auth)
    req(UserData()$license%in%"administrator")
    
    if(nchar(input$userNew)<1 || nchar(input$passwordNew)<1 ||
       nchar(input$namesNew)<1){
      showNotification(
        h4("You must fill all fields"), 
        action = NULL, duration = 5, type = "warning")
    } else {
      GetUsersSqlScript <- "SELECT * FROM users"
      UserData<-dbGetQuery(DbConnection, GetUsersSqlScript)
      
      if(length(grep(input$userNew,UserData$user))>0){
        showNotification(
          h4("This user already exists"), 
          action = NULL, duration = 5, type = "warning")   
      } else {
        newUserSqlScript <- 'INSERT INTO users (user,password,license,names) 
                    VALUES (?iduser,?idpassword,?idpermiso,?idnames);'
        
        GetUsersQuery <- sqlInterpolate(DbConnection, newUserSqlScript, iduser= input$userNew, 
                                     idpassword=password_store(input$passwordNew),
                                     idpermiso=input$licenseNew,idnames=input$namesNew)
        
        dbGetQuery(DbConnection, GetUsersQuery)
        
        removeModal()
        
        showNotification(
          h4("Succes query"), 
          action = NULL, duration = 5, type = "message")
        
        GetUsersSqlScript <- "SELECT * FROM users"
        users$data<-dbGetQuery(DbConnection, GetUsersSqlScript)
      }
      
    }
  })
  
  
  ## Create reactive values for the rows selected by the user 
  RowsSelectedByUser<- reactiveValues()
  ## Show a modal to confirm deleting users  
  observeEvent(input$deleteUsers,{
    
    RowsSelectedByUser$row<-input$tableUsers_rows_selected
    
    if(length(RowsSelectedByUser$row)>0){
      showModal(
        modalDialog(title = "Borrar",
                    fluidPage(column(12,h3("Warning: You're removing an user from de database"),style="color:red;")),
                    easyClose = FALSE,
                    size = "m",
                    footer = tagList(
                      actionButton(ns("cancel"),"Cancel"),
                      actionButton(ns("deleteUsers_fromDB"),"Delete")
                    ) 
        )
      )
    } else {
      showNotification(
        h4("Select a row"), 
        action = NULL, duration = 5, type = "warning") 
    }
    
  })
  
  ## Remove users regarding the rows selected on the table
  observeEvent(input$deleteUsers_fromDB,{
    req(credentials()$user_auth)
    req(UserData()$license%in%"administrator")
    
    RowsSelectedByUser$row<-input$tableUsers_rows_selected
    users_a_borrar<-users$data[RowsSelectedByUser$row,"user"]
    
    sql_borrar_usuario <- paste("DELETE FROM users WHERE user IN (",
                                paste0(sprintf("'%s'",users_a_borrar),collapse = ","),")")
    
    dbGetQuery(DbConnection, sql_borrar_usuario)
    removeModal()
    
    showNotification(
      h4("User removed"), 
      action = NULL, duration = 5, type = "message")
    
    GetUsersSqlScript <- "SELECT * FROM users"
    
    users$data<-dbGetQuery(DbConnection, GetUsersSqlScript) 
    
  })
}