source("./global.R")


#################shihyauthr funcitons #############
## save session data on the database
AddSessionIdToDb <- function(user, sessionid, conn = DbConnection) {
  tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE, row.names = FALSE)
}

## Return a dataframe with session data
getSessionidsFromDb <- function(conn = DbConnection, expiry = cookieValue) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}

## Get user data from the database
GetUsersSqlScript <- "SELECT * FROM users"
GetUsersQuery <- sqlInterpolate(DbConnection, GetUsersSqlScript)
usersData<-dbGetQuery(DbConnection, GetUsersQuery)

##Get the symbols saved by Users 
SymbolsSavedByUser<-reactiveValues()
GetSymbolsSqlScript <- "SELECT * FROM symbols"
GetSymbolsQuery <- sqlInterpolate(DbConnection, GetSymbolsSqlScript)
SymbolsSavedByUser$Symbols<-dbGetQuery(DbConnection, GetSymbolsQuery)



##Call shinyauthr Module from the package and creates de time to session expiry
logout_init <- callModule(
  shinyauthr::logout,
  id = "logout",
  active = reactive(credentials()$user_auth)
)

## Call shinyauthr module to read the credentiales and the time regarding cookies
credentials <- callModule(
  shinyauthr::login,
  id = "login",
  data = usersData,
  user_col = user, 
  pwd_col = password,
  sessionid_col = sessionid, 
  cookie_getter = getSessionidsFromDb,
  cookie_setter = AddSessionIdToDb,
  sodium_hashed = TRUE,
  log_out = reactive(logout_init())
)


##Read the info saved on the database about the users (names, credential)
UserData <- reactive({
  credentials()$info
})