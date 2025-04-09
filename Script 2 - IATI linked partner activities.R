# --------------------------------------------------------------- #
# Script 2
# Extract IATI partner activities linked to UK gov funders' IATI activities
# by an incoming fund transaction
# --------------------------------------------------------------- #

# 1) Extract linked R&I partner IATI activities -----

# Read in list of UK gov funder activities from Script 1
uk_gov_list <- readRDS(file = "Outputs/uk_gov_list.rds")

# Extract linked partner activity IDs ----

linked_activities <- data.frame()

# Extract activity data for each government department
for (act in uk_gov_list$iati_identifier) {
  new_rows <- 0
  page <- 1
  
  while (page == 1 | new_rows > 0) {
    print(paste0(act, "-", page))
    x <- nrow(linked_activities)
    new_activities <- transactions_extract(page, act)
    
    if (nrow(new_activities) > 0) {
      linked_activities <- rbind(linked_activities, new_activities)
    }
    
    page <- page + 1
    y <- nrow(linked_activities)
    print(y)
    new_rows = y - x
    print(new_rows)
  }
}


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
