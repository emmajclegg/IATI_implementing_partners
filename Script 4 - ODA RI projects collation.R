# --------------------------------------------------------------- #
# Script 4 
# Extract and collate ODA R&I award level data from
# - IATI Registry (UK government funder and external partner activities)
# - UKRI Gateway to Research
# - NIHR Open Data
# - Wellcome Trust (spreadsheet input)
# - DHSC Global Health Security (non-UKRI projects) (spreadsheet input)
# - BEIS GCRF and Newton Fund (spreadsheet input)
# --------------------------------------------------------------- #

# Read in org names and countries from previous script
org_names_and_locations_1 <- readRDS(file = "Outputs/org_names_and_locations_1.rds")

# 1) Extract IATI projects ------------------------------------------------

# Read in list of IATI activities (from UK gov funders and select delivery partners)
gov_iati_list <- readRDS(file = "Outputs/gov_list_final.rds")
partner_iati_list <- readRDS(file = "Outputs/partner_activity_list.rds")
gov_non_iati_ids <- paste0(gov_non_iati_programmes$iati_identifier, collapse = "|")

# Filter gov department records for project-level activities
iati_projects <- gov_iati_list %>%
  filter(str_detect(iati_identifier, "GB-GOV-3|GB-GOV-7") |     # include ex-FCO and Defra activities
         str_detect(iati_identifier, gov_non_iati_ids)         # keep FCDO/DHSC programmes funding out of scope of IATI 
         ) %>%    
  mutate(fund = if_else(is.na(fund), "Unknown", fund)) %>% 
  plyr::rbind.fill(partner_iati_list) # Add partner activities

# Identify UKRI projects (by "RI" IATI tag)
ukri_iati_projects <- gov_iati_list %>% 
  filter(extending_org == "UK Research & Innovation") %>% 
  mutate(gtr_id = str_replace(iati_identifier, "GB-GOV-13-FUND--GCRF-", "")) %>% 
  mutate(gtr_id = str_replace(gtr_id, "GB-GOV-13-FUND--Newton-", "")) %>% 
  mutate(gtr_id = str_replace_all(gtr_id, "_", "/")) %>%
  select(gtr_id, iati_identifier, recipient_country) %>% 
  unique()

# Add on beneficiary countries for FCDO non-IATI programmes
iati_projects <- iati_projects %>% 
  # remove any FCDO component numbers
  mutate(programme_iati_id = if_else(reporting_org_ref == "GB-GOV-1" &
                                      substr(iati_identifier, nchar(iati_identifier)-3, nchar(iati_identifier)-3) == "-",
                                     substr(iati_identifier, 1, nchar(iati_identifier)-4), iati_identifier)) %>% 
  left_join(gov_non_iati_programmes, by = c("programme_iati_id" = "iati_identifier")) %>% 
  mutate(recipient_country = coalesce(recipient_country, str_to_title(fcdo_geocoding_countries))) %>% 
  select(-programme_name, -fcdo_geocoding_countries, -programme_iati_id)


# Keep required fields
iati_projects_final <- iati_projects %>% 
  mutate(Funder = coalesce(gov_funder, reporting_org),
         partner_org_name = partner,
         partner_org_country = partner_country,         
         lead_org_name = coalesce(extending_org, reporting_org),
         lead_org_country = reporting_org_country,
         extending_org = coalesce(extending_org, reporting_org),
         status = if_else(!is.na(end_date),
                          if_else(Sys.Date() <= end_date, "Active", "Closed"), "Unknown"),
         iati_id = coalesce(programme_id, iati_identifier),
         last_updated = quarter_end_date) %>% 
  select(id = iati_identifier,
         title = activity_title, 
         abstract = activity_description,
         start_date,
         end_date,
         amount,
         period_start,
         period_end,
         currency,
         extending_org,
         lead_org_name,
         lead_org_country,
         partner_org_name,
         partner_org_country,
         iati_id,
         Fund = fund,
         Funder, 
         recipient_country,
         subject = sector_name,
         status,
         last_updated
  ) 

# Add IATI link to awards
iati_projects_final <- iati_projects_final %>% 
  mutate(link = paste0("https://d-portal.org/ctrack.html#view=act&aid=", id))

# Clear environment
rm(gov_iati_list, partner_iati_list, iati_projects)



# 7) Join funder datasets together ----------------------------------------------

all_projects <- iati_projects_final %>% 
  unique() %>% 
  ungroup()


# 8) Manual exclusions and formatting -------------------------------------------

# Manually edit country info for Chevening Scholarships
all_projects_tidied <- all_projects %>% 
  mutate(lead_org_country = if_else(Fund == "FCDO - Chevening Scholarships", "United Kingdom", lead_org_country),
         start_date = if_else(Fund == "FCDO - Chevening Scholarships", NA_character_, start_date))

# Name BEIS delivery partners fully
all_projects_tidied <- all_projects_tidied %>% 
  mutate(extending_org = case_when(
              extending_org == "AMS" ~ "Academy of Medical Sciences", 
              extending_org == "BA" ~ "British Academy",
              extending_org %in% c("BC", "BRITISH COUNCIL") ~ "British Council",
              extending_org == "MO" ~ "Met Office",
              extending_org == "RAE" ~ "Royal Academy of Engineering",
              extending_org == "RS" ~ "Royal Society",
              extending_org == "UKSA" ~ "UK Space Agency",
              TRUE ~ extending_org
              ))

# Add FCDO DevTracker links in absence of other public source
all_projects_tidied <- all_projects_tidied %>% 
  mutate(link = if_else((str_detect(iati_id, "GB-GOV-1-") | str_detect(iati_id, "GB-1-")) & is.na(link),
                        paste0("https://devtracker.fcdo.gov.uk/projects/", iati_id, "/summary"), link))


# 9) Save datasets -------------------------------------------

saveRDS(all_projects_tidied, file = "Outputs/all_projects_tidied.rds")
# all_projects_tidied <- readRDS("Outputs/all_projects_tidied.rds") 

# Save org names and countries to file
org_names_and_locations <- rbind(org_names_and_locations_1, org_names_and_locations_2, 
                                 org_names_and_locations_3) %>% 
  mutate(organisation_name = str_trim(organisation_name)) %>% 
  filter(!is.na(organisation_name)) %>% 
  unique()

saveRDS(org_names_and_locations, file = "Outputs/org_names_and_locations.rds")


# Clear environment
rm(iati_projects_final)



