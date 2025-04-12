# --------------------------------------------------------------- #
# Script 1
# Extract UK government departments' IATI data 
# --------------------------------------------------------------- #


# 1) Extract all IATI activities from UK government departments --------

# Define UK government department IATI org IDs
organisation_codes <- c("GB-GOV-1")

# Prepare output data frame
uk_gov_list <- data.frame()

# Extract activity data for each government department
for (org in organisation_codes) {
  new_rows <- 0
  page <- 1

  while (page == 1 | new_rows > 0) {
    print(paste0(org, "-", page))
    x <- nrow(uk_gov_list)
    print(x)
    new_activities <- org_activity_extract(page, org)
    uk_gov_list <- rbind(uk_gov_list, new_activities)
    page <- page + 1
    y <- nrow(uk_gov_list)
    print(y)
    new_rows = y - x
    print(new_rows)
  }
}

# Save output data
saveRDS(uk_gov_list, file = "Outputs/uk_gov_list.rds")
# uk_gov_list <- readRDS(file = "Outputs/uk_gov_list.rds")
