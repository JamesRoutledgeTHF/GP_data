
CQC_Datasets <- dbGetQuery(con, "
  SELECT *
  FROM CQC.Locations_SCD
  WHERE Is_Latest = 1 AND careHome = 'N'
")

CQC_Datasets_filtered <- dbGetQuery(con, "
  SELECT ownershipType, odsCode, providerId
  FROM CQC_Providers.Providers_SCD
  WHERE Is_Latest = 1
")


CQC_Datasets_filtered_renamed <- CQC_Datasets_filtered %>%
  rename(odsCode_filtered = odsCode)

# Perform the join
CQC_Datasets_joined <- CQC_Datasets %>%
  inner_join(CQC_Datasets_filtered_renamed, by = "providerId") %>%
  select(-odsCode_filtered) %>%  # Drop the renamed odsCode from the filtered dataset
  arrange(odsCode, desc(as.Date(deregistrationDate)), desc(as.Date(registrationDate))) %>%  # Sort by odsCode and most recent dates
  group_by(odsCode) %>%  # Group by odsCode to handle multiple rows per provider
  filter(as.Date(registrationDate) <= as.Date("2024-03-31")) %>%  # Exclude rows with registrationDate after March 31, 2024
  # Now, apply logic based on registrationStatus
  mutate(
    keep_registered = registrationStatus == "Registered",
    keep_deregistered = registrationStatus == "Deregistered"
  ) %>%
  # If there is at least one "Registered" row, keep them. Otherwise, keep the most recent "Deregistered" row.
  filter(
    (keep_registered) | 
      (keep_deregistered & !any(keep_registered))
  ) %>%
  slice(1) %>%  # Keep only the most recent row per odsCode
  ungroup()  # Ungroup to avoid issues in future operations


