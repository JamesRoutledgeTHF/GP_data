#Load the CQC datasets

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
  select(-odsCode_filtered) %>%  
  arrange(odsCode, desc(as.Date(deregistrationDate)), desc(as.Date(registrationDate))) %>%  
  group_by(odsCode) %>%  
  filter(as.Date(registrationDate) <= as.Date("2024-03-31")) %>% #as data only goes up to here don't want newer practices 
  mutate(
    keep_registered = registrationStatus == "Registered",
    keep_deregistered = registrationStatus == "Deregistered"
  ) %>%
  filter(
    (keep_registered) | 
      (keep_deregistered & !any(keep_registered))
  ) %>%
  slice(1) %>%  # Keep only the most recent row per odsCode
  ungroup()  


