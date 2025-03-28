
### Set end of quarter data for update ----

quarter_end_date <- as.Date("2025-03-31")


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
file_path <- "token.txt"
API_KEY = readLines(file_path)
authentication = add_headers(`Ocp-Apim-Subscription-Key` = API_KEY)


### IATI functions ### ----

countrycode_list <- read.csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Country.csv")
regioncode_list <- read.csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Region.csv")
sector_list <- read.csv("https://iatistandard.org/reference_downloads/203/codelists/downloads/clv3/csv/en/Sector.csv")


iati_activity_extract <- function(activity_id) {
  
  # Reformat ID if it contains spaces (for API)
  rows <- 1000
  page <- 1
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=iati_identifier:"',
                 activity_id,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,reporting_org*,location*,sector_code*,default_flow_type*,recipient_country_code,recipient_region_code,activity_date*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
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

debug(iati_activity_extract)
result1 <- iati_activity_extract("CA-CRA_ACR-869974816-4901-4902")


# Function to extract IATI activity IDs for a specified org code

org_activity_extract <- function(page, org_code) {
  rows = 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 org_code,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,recipient_country_code,recipient_region_code,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  }
  return(new_data)
  
}

org_activity_extract <- function(page, org_code) {
  rows <- 1000
  start <- (page - 1) * rows
  path <- paste0('https://api.iatistandard.org/datastore/activity/select?',
                 'q=reporting_org_ref:"',
                 org_code,
                 '"&wt=json',
                 '&rows=',rows,
                 '&start=',start,
                 "&fl=iati_identifier,other_identifier*,activity_date*,reporting_org*,sector_code*,location*,recipient_country_code,recipient_region_code,default_flow_type*,budget*,policy_marker*,activity_status*,hierarchy*,title*,description*,participating_org*,related_activity*,tag*")
  request <- GET(url = path, authentication)
  message(request$status_code)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$response$docs
  numb_data <- response$response$numFound
  
  if(start >= numb_data){
    return(NULL)
  }
  return(new_data)
  
}

debug(org_activity_extract)
result2 <- org_activity_extract(1, "GB-GOV-1")


# Function to extract transactions for a specified IATI activity ID
transactions_extract <- function(activity_id, page, output_data) {
  
  # Reformat ID if it contains spaces (for API)
  activity_id <- str_replace_all(activity_id, " ", "%20")
  
  path <- paste0("https://iati.cloud/api/transactions/?iati_identifier=", activity_id, "&fields=value,transaction_date,description,currency,receiver_organisation&format=json&page_size=20&page=", page)
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results
  
  if(length(new_data) > 0) {
    output <- plyr::rbind.fill(output_data, new_data)
  } else {
    output <- output_data
  }
  
  return(output)
}


# Function to extract activity names from an IATI activity ID

extract_iati_activity_name <- function(activity_id) {
  
  # Reformat ID if it contains spaces (for API)
  activity_id <- str_replace_all(activity_id, " ", "%20")
  
  path <- paste0("https://iati.cloud/api/activities/?iati_identifier=", activity_id, "&format=json&fields=title")
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  response <- fromJSON(response, flatten = TRUE) 
  new_data <- response$results 
  
  if(length(new_data) > 0) {
    new_data <- new_data %>% 
      unnest(col = title.narrative) %>% 
      select(funder_iati_id = iati_identifier, funder_programme = text)
    
    result <- new_data$funder_programme
    
  } else {
    result <- NA_character_
  }
  
  return(result)
  
}



