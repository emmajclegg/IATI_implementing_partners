# --------------------------------------------------------------- #
# Script 2
# Extract IATI partner activities linked to UK gov funders' IATI activities
# by an incoming fund transaction
# --------------------------------------------------------------- #

# 1) Extract linked R&I partner IATI activities -----

# Read in list of UK gov funder activities from Script 1
uk_gov_list <- readRDS(file = "Outputs/uk_gov_list.rds")

uk_gov_list_filtered <- uk_gov_list %>% 
  filter(activity_status_code == 2) %>% 
  slice_sample(n=100)

# Extract linked partner activity IDs ----

linked_activities_completed <- lapply(uk_gov_list_filtered$iati_identifier, linked_activity_extract)

test <- t(data.frame(unlist(linked_activities_completed)))

# Extract activity data for each government department

linked_activities <- data.frame()

for (act in uk_gov_list_filtered$iati_identifier) {

    new_activities <- linked_activity_extract(act)
    linked_activities <- rbind(linked_activities, new_activities)
  
}


# Extract activity data for each government department
for (act in uk_gov_list$iati_identifier) {
  new_rows <- 0
  page <- 1
  
  while (page == 1 | new_rows > 0) {
    print(act)
    x <- nrow(linked_activities)
    new_activities <- linked_activity_extract(page, act)
    
    linked_activities <- rbind(linked_activities, new_activities)
    
    page <- page + 1
    y <- nrow(linked_activities)
    new_rows = y - x
  }
}


# Save output data
saveRDS(linked_activities, file = "Outputs/linked_activities.rds")
# linked_activities <- readRDS(file = "Outputs/linked_activities.rds")


# Extract full activity data for the partner activities

for (activity in linked_activities_completed$) {
  new_rows <- 0
  page <- 1
  
  while (page == 1 | new_rows > 0) {
    print(paste0(activity, "-", page))
    x <- nrow(uk_gov_list)
    print(x)
    new_activities <- activity_extract_hierarchy(page, org)
    uk_gov_list <- rbind(uk_gov_list, new_activities)
    page <- page + 1
    y <- nrow(uk_gov_list)
    print(y)
    new_rows = y - x
    print(new_rows)
  }
}


# 4) Unnest activity information -----------

# Extract basic activity information - hierarchy and status
gov_list_base <- uk_gov_list %>% 
  select(reporting_org_ref,
         reporting_org_narrative,
         iati_identifier, 
         title_narrative,
         activity_status_code,
         description_narrative) %>% 
  unique()


# 3) Keep partner activity IDs only (not duplicate gov funder ones) ----

ri_linked_activites <- transactions_dataset %>% 
  filter(iati_identifier != activity_id) %>% 
  select(activity_id,
         iati_identifier) %>% 
  unique()


# Save to Rdata file
saveRDS(ri_linked_activites, file = "Outputs/ri_linked_activites.rds")
# ri_linked_activites <- readRDS(file = "Outputs/ri_linked_activites.rds")

# Clear environment
rm(ri_iati_activities, new_rows, page, path, id, x, y, request, response, new_data, transactions_dataset, 
   ri_linked_activites)
