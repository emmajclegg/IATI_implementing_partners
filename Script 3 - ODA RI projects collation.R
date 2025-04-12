# --------------------------------------------------------------- #
# Script 3 
# Extract and collate IATI Registry (UK partner activities)
# --------------------------------------------------------------- #

# Read in org names and countries from previous script
org_names_and_locations_1 <- readRDS(file = "Outputs/org_names_and_locations_1.rds")

# 1) Extract IATI projects ------------------------------------------------

# Read in list of IATI activities (from UK gov funders and select delivery partners)
gov_iati_list <- readRDS(file = "Outputs/gov_list_final.rds")

# Keep required fields
iati_projects_final <- iati_projects %>% 
  mutate(department = reporting_org,
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




