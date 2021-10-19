#########################Packages##################
##Shiny 
library(shiny)
library(shinythemes)
library(shinyalert)
library(shinyauthr)
library(shinyjs)

library(sodium)

##API connection
library(jsonlite)
library(httr)

## Database connection
library(RMySQL)
library(DBI)
library(pool)

## Visualizations
library(DT)
library(ggplot2)
library(plotly)

## Data processing
library(dplyr)
library(lubridate)



###############Global variables###############
## App name
appName<-"Tript"
## Cookie value
cookieValue<- 1

DbConnection <- dbPool(
  drv = dbDriver("MySQL", max.con = 1000),
  dbname = "Tript",
  host = "database-pruebas.cayjegc6zikj.us-east-2.rds.amazonaws.com",
  user = 'admin',
  password = 'admin_admin',
  idleTimeout = 3600000 ###una hora
)
