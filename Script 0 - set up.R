
### Check and install packages ----
packages <- data.frame(installed.packages())

if (!("jsonlite" %in% packages$Package)) {
  install.packages("jsonlite")
}
if (!("httr" %in% packages$Package)) {
  install.packages("httr")
}
if (!("tidyverse" %in% packages$Package)) {
  install.packages("tidyverse")
}
if (!("dplyr" %in% packages$Package)) {
  install.packages("dplyr")
}
if (!("readxl" %in% packages$Package)) {
  install.packages("readxl")
}
if (!("writexl" %in% packages$Package)) {
  install.packages("writexl")
}
if (!("googlesheets4" %in% packages$Package)) {
  install.packages("googlesheets4")
}
if (!("gargle" %in% packages$Package)) {
  install.packages("gargle")
}
if (!("openxlsx" %in% packages$Package)) {
  install.packages("openxlsx")
} # for adding hyperlinks and formatting to output Excel reports
if (!("DBI" %in% packages$Package)) {
  install.packages("DBI")
} # for read/writing to Excel database
if (!("odbc" %in% packages$Package)) {
  install.packages("odbc")
} 
if (!("countrycode" %in% packages$Package)) {
  install.packages("countrycode")
} 
if (!("testthat" %in% packages$Package)) {
  install.packages("testthat")
} 

library(jsonlite)
library(httr)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(googlesheets4)
library(gargle)
library(openxlsx)
library(DBI)
library(odbc)
library(countrycode)
library(testthat)

### Set authentication
load_dot_env()
api_key = Sys.getenv("API_KEY")
authentication = add_headers(`Ocp-Apim-Subscription-Key` = api_key)

### IATI functions ### ----

## 1. Function to extract activity data for a given activity identifier

# As flat JSON
activity_extract_flat <- function(activity_id) {

path <- paste0(
  'https://api.iatistandard.org/datastore/activity/select?',
  'q=iati_identifier:"',
  activity_id,
  '"&wt=json',
  "&fl=other_identifier*,reporting_org*,location*,default_flow_type*,activity_date*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*",
  "&wt=json"
)
request <- GET(url = path, authentication)
response <- content(request, as = "text", encoding = "UTF-8")
response <- fromJSON(response, flatten = TRUE) 
activity_json <- response$response$docs

return(activity_json)

}


# As hierarchical JSON
activity_extract_hierarchy <- function(activity_id) {
  
  path <- paste0(
    'https://api.iatistandard.org/datastore/activity/iati_json?',
    'q=iati_identifier:"',activity_id,'"'
  )
  request <- GET(url = path, authentication)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  activity_json <- response$response$docs$`iati_json.iati-activity`[[1]]
  
  return(activity_json)
  
}


# Tests
id <- "GB-1-203166-103"
result_flat <- activity_extract_flat(id)
result_hierarchy <- activity_extract_hierarchy(id)




## 2. Function to return a list of the activities from a specified reporting org

org_activity_extract <- function(page, org_code) {
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 org_code,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,activity_date*,reporting_org*,activity_status*,title*,description*")
  request <- GET(url = path, authentication)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  }
  
  return(new_data)
  
}

# Tests
org_id <- "GB-GOV-7"
result_orgs <- org_activity_extract(1, org_id)


## 3. Function to extract transactions for a specified IATI activity ID
transactions_extract <- function(page, activity_id) {
  
  rows = 1000
  start <- (page - 1) * rows
  path  <- paste0('https://api.iatistandard.org/datastore/transaction/select?',
                  'q=transaction_provider_org_provider_activity_id:"',
                  activity_id,
                  '"&wt=json',
                  '&rows=',rows,
                  '&start=',start,
                  "&fl=iati_identifier,transaction_provider_org_provider_activity_id")
  request <- GET(url = path, authentication)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- unique(response$response$docs)
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  } 
  
  return(new_data)
}

id <- "GB-1-203166-103"
id <- "GB-GOV-3-CSSF-01-000005"
activity_transactions <- transactions_extract(1,id)




