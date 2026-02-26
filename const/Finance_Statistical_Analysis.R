library(writexl)

#join IMD onto and then group by year and imd, add the avg registered and weighted columns for each group as well 
finances_IMD_PRACTICE <- finances_GROUPED_TOTAL %>%
  left_join(prac_imd, by = c("YEAR", "PRACTICE_CODE")) %>%
  filter(!is.na(IMD_QUINTILE)) %>%
  mutate(IMD_QUINTILE = as.factor(IMD_QUINTILE)) %>%
  group_by(PRACTICE_CODE, IMD_QUINTILE, YEAR) %>%
  summarise(
    Global_Sum          = sum(Total.Global.Sum, na.rm = TRUE),
    IT_Premises         = sum(Total.IT.Premises, na.rm = TRUE),
    Other_And_PCOAdmin  = sum(Total.OtherAndPCOAdmin, na.rm = TRUE),
    Directed_Enhanced_Services = sum(Total.DirectedEnhancedServices, na.rm = TRUE),
    ACCESSANDTRANSFORMATION = sum(Total.AccessAndTransFormation, na.rm = TRUE),
    Local_Incentive_Schemes = sum(LIS, na.rm = TRUE),
    Prescribing         = sum(PRESCRIBING_FEE, na.rm = TRUE),
    Dispensing          = sum(DISPENSING_FEE, na.rm = TRUE),
    DrugReinbursement   = sum(REINBURSMENT_FEE, na.rm = TRUE),
    QOF                 = sum(QOF_PAYMENT, na.rm = TRUE),
    Deductions          = sum(DEDUCTIONS, na.rm = TRUE),
    PCNPARTICIPATION          = sum(PCNP, na.rm = TRUE),
    REGISTERED          = sum(REGISTERED_PATIENTS, na.rm = TRUE),
    WEIGHTED            = sum(WEIGHTED_PATIENTS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(
    c(Global_Sum, IT_Premises, Other_And_PCOAdmin, Directed_Enhanced_Services, Local_Incentive_Schemes, Prescribing, Dispensing, DrugReinbursement, QOF, Deductions, PCNPARTICIPATION, ACCESSANDTRANSFORMATION),
    list(
      "Avg (Registered)" = ~ . / REGISTERED,
      "Avg (Weighted)"   = ~ . / WEIGHTED
    ),
    .names = "{.col} {fn}"
  )) 

finances_IMD_PRACTICE$IMD_QUINTILE <- factor(finances_IMD_PRACTICE$IMD_QUINTILE,
                                             levels = c('5', '1', '2', '3', '4'))


options(scipen = 0)  # This makes R avoid scientific notation

Global_Sum_Model <- lm(`Global_Sum Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE_Global_Weight)
Presc_Model <- lm(`Prescribing Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
IT_Model <- lm(`IT_Premises Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE_IT_Premises)
QOF_Model <- lm(`QOF Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE_QOF)
Other_And_PCOAdmin_Model <- lm(`Other_And_PCOAdmin Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE_Other_And_PCOAdmin)
LIS_Model <- lm(`Local_Incentive_Schemes Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE_LIS)
DES_Model <- lm(`Directed_Enhanced_Services Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE_DES)
Dispensing_Model <- lm(`Dispensing Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
DrugReinbursement <- lm(`DrugReinbursement Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE_DrugReinbursement)
PCNParticipation <- lm(`PCNPARTICIPATION Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE_PCNParticipation)
AccessAndTransformation <- lm(`ACCESSANDTRANSFORMATION Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE_ACCESS_TRANSFORMATION)

summary(Global_Sum_Model)
summary(Presc_Model)
summary(IT_Model)
summary(QOF_Model)
summary(Other_And_PCOAdmin_Model)
summary(LIS_Model)
summary(DES_Model)
summary(Dispensing_Model)
summary(DrugReinbursement)
summary(PCNParticipation)
summary(AccessAndTransformation)


model <- glm( `Global_Sum Avg (Weighted)` ~ `IMD_QUINTILE`, family = quasipoisson(link = "identity"), data = finances_IMD_PRACTICE)
model_gamma <- glm( `Global_Sum Avg (Weighted)` ~ `IMD_QUINTILE`, family = Gamma(link = "identity"), data = finances_IMD_PRACTICE)

summary(model)

Global_Sum_Model_reg <- lm(`Global_Sum Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
Presc_Model_reg <- lm(`Prescribing Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
IT_Model_reg <- lm(`IT_Premises Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
QOF_Model_reg <- lm(`QOF Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
Other_And_PCOAdmin_Model <- lm(`Other_And_PCOAdmin Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
LIS_Model_reg <- lm(`Local_Incentive_Schemes Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
DES_Model_reg <- lm(`Directed_Enhanced_Services Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)

summary(Global_Sum_Model_reg)
summary(Presc_Model_reg)
summary(IT_Model_reg)
summary(QOF_Model_reg)
summary(Other_And_PCOAdmin_Model)
summary(LIS_Model_reg)
summary(DES_Model_reg)




finance_imd_wide_regression <- finances_GROUPED_TOTAL %>%
  left_join(prac_imd, by = c("YEAR", "PRACTICE_CODE")) %>%
  filter(!is.na(IMD_QUINTILE)) %>%
  group_by(PRACTICE_CODE, IMD_QUINTILE, YEAR) %>%
  summarise(
    # keep practice characteristics
    Contract_Type        = first(CONTRACT_TYPE),
    Dispensing_Practice  = first(Dispensing_Practice),
    Practice_Rurality    = first(Practice_Rurality),
    
    # financial totals
    Global_Sum          = sum(Total.Global.Sum, na.rm = TRUE),
    IT_Premises         = sum(Total.IT.Premises, na.rm = TRUE),
    Other_And_PCOAdmin  = sum(Total.OtherAndPCOAdmin, na.rm = TRUE),
    Directed_Enhanced_Services = sum(Total.DirectedEnhancedServices, na.rm = TRUE),
    ACCESSANDTRANSFORMATION = sum(Total.AccessAndTransFormation, na.rm = TRUE),
    Local_Incentive_Schemes = sum(LIS, na.rm = TRUE),
    Prescribing         = sum(PRESCRIBING_FEE, na.rm = TRUE),
    Dispensing          = sum(DISPENSING_FEE, na.rm = TRUE),
    DrugReinbursement   = sum(REINBURSMENT_FEE, na.rm = TRUE),
    QOF                 = sum(QOF_PAYMENT, na.rm = TRUE),
    Deductions          = sum(DEDUCTIONS, na.rm = TRUE),
    PCNPARTICIPATION    = sum(PCNP, na.rm = TRUE),
    
    REGISTERED          = sum(REGISTERED_PATIENTS, na.rm = TRUE),
    WEIGHTED            = sum(WEIGHTED_PATIENTS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Contract_Type       = factor(Contract_Type),
    Dispensing_Practice = factor(Dispensing_Practice),
    Practice_Rurality   = factor(Practice_Rurality),
    IMD_QUINTILE        = factor(IMD_QUINTILE),
    
    `Avg (Registered)` =
      (Global_Sum + IT_Premises + Other_And_PCOAdmin +
         Directed_Enhanced_Services + ACCESSANDTRANSFORMATION +
         Local_Incentive_Schemes + Prescribing + Dispensing +
         DrugReinbursement + QOF + PCNPARTICIPATION) / REGISTERED,
    
    `Avg (Weighted)` =
      (Global_Sum + IT_Premises + Other_And_PCOAdmin +
         Directed_Enhanced_Services + ACCESSANDTRANSFORMATION +
         Local_Incentive_Schemes + Prescribing + Dispensing +
         DrugReinbursement + QOF + PCNPARTICIPATION) / WEIGHTED,
    
    `Avg (Weighted) Including deductions, excluding premises and reimbursment` =
      (Global_Sum + Other_And_PCOAdmin +
         Directed_Enhanced_Services + ACCESSANDTRANSFORMATION +
         Local_Incentive_Schemes + Prescribing + Dispensing
       + QOF + PCNPARTICIPATION + Deductions) / WEIGHTED,
    
    `Avg (Registered) Including deductions, excluding premises and reimbursment` =
      (Global_Sum + Other_And_PCOAdmin +
         Directed_Enhanced_Services + ACCESSANDTRANSFORMATION +
         Local_Incentive_Schemes + Prescribing + Dispensing
       + QOF + PCNPARTICIPATION + Deductions) / REGISTERED,
    
    `Avg (Registered) excluding reimbursment` =
      (Global_Sum + Other_And_PCOAdmin +
         Directed_Enhanced_Services + ACCESSANDTRANSFORMATION +
         Local_Incentive_Schemes + Prescribing + Dispensing
       + QOF + PCNPARTICIPATION + IT_Premises) / REGISTERED,
    
    `Avg (Registered) excluding dispensing, prescribing, reinbusement` =
      (Global_Sum + IT_Premises + Other_And_PCOAdmin +
         Directed_Enhanced_Services + ACCESSANDTRANSFORMATION +
         Local_Incentive_Schemes + QOF + PCNPARTICIPATION) / REGISTERED,
    
    `Avg (Weighted) excluding dispensing, prescribing, reinbusement` =
      (Global_Sum + IT_Premises + Other_And_PCOAdmin +
         Directed_Enhanced_Services + ACCESSANDTRANSFORMATION +
         Local_Incentive_Schemes + QOF + PCNPARTICIPATION) / WEIGHTED,
  )


finance_imd_wide_regression <- finance_imd_wide_regression %>%
  group_by(YEAR, PRACTICE_CODE) %>%
  filter(any( `Avg (Registered)` >= 0)) %>%
  ungroup()

#outliers_005pct <- finance_imd_wide_regression %>%
#  group_by(YEAR) %>%
#  mutate(
 #   p005 = quantile( `Avg (Registered)`, 0.005, na.rm = TRUE),
 #   p995 = quantile( `Avg (Registered)`, 0.995, na.rm = TRUE)
#  ) %>%
#  ungroup() %>%
 # filter( `Avg (Registered)` <= p005 |  `Avg (Registered)` >= p995) %>%
 # distinct(YEAR, PRACTICE_CODE)

#finance_imd_wide_regression <- finance_imd_wide_regression %>%
  #anti_join(outliers_005pct, by = c("YEAR", "PRACTICE_CODE"))

finance_imd_wide_regression$IMD_QUINTILE <- factor(finance_imd_wide_regression$IMD_QUINTILE,
                                             levels = c('5', '1', '2', '3', '4'))

finance_imd_wide_regression$Practice_Rurality <- factor(finance_imd_wide_regression$Practice_Rurality,
                                                   levels = c('Urban', 'Rural'))

finance_imd_wide_regression$Contract_Type <- factor(finance_imd_wide_regression$Contract_Type,
                                                        levels = c('GMS', 'APMS', "PMS"))

finance_imd_wide_regression$Dispensing_Practice <- factor(finance_imd_wide_regression$Dispensing_Practice,
                                                    levels = c("No", "Yes"))

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  mutate(
    # Grouping by the number of registered patients
    Registered_Patient_Group = cut(
      `REGISTERED`,  # This is the column for grouping
      breaks = c(0, 5000, 10000, 20000, 50000, Inf),  # Added Inf for practices with more than 50,000
      right = FALSE,                           # Ensure bins are closed on the left
      include.lowest = TRUE,                   # Include the lowest value in the first bin
      labels = c("0-5000", "5001-10000", "10001-20000", "20001-50000", "50001+")  # New label for > 50,000
    )
  )

finance_imd_wide_regression$Registered_Patient_Group <- factor(finance_imd_wide_regression$Registered_Patient_Group,
                                                    levels = c("0-5000", "5001-10000", "10001-20000", "20001-50000", "50001+"))

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  inner_join(Over65_quintile %>%
               select(PRACTICE_CODE, Pct_Over_65, YEAR), 
             by = c("PRACTICE_CODE", "YEAR"))

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  mutate(
    # Grouping by `Pct_Over_65` into terciles
    Pct_Over_65_Tercile = ntile(Pct_Over_65, 3)  # Divide the data into 3 equal parts
  )

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  inner_join(GPPS_Datasets %>%
               select(Practice_Code, Field_Name, Field_Value), 
             by = c("PRACTICE_CODE" = "Practice_Code")) 

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  filter(Field_Value >= 0)  # Remove rows where Field_Value is less than 0

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  inner_join(CQC_Datasets_joined %>%
               select(odsCode, ownershipType), 
             by = c("PRACTICE_CODE" = "odsCode"))

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  mutate(
    # Grouping by `Pct_Over_65` into terciles
    Field_Value_Tercile = ntile(Field_Value, 3)  # Divide the data into 3 equal parts
  )

finance_imd_wide_regression$ownershipType <- factor(finance_imd_wide_regression$ownershipType,
                                                    levels = c('Partnership', 'Individual', "Organisation", "NHS Body"))

finance_imd_wide_regression$Field_Value_Tercile <- factor(finance_imd_wide_regression$Field_Value_Tercile,
                                                   levels = c('1', '2', '3'))

finance_imd_wide_regression$Pct_Over_65_Tercile <- factor(finance_imd_wide_regression$Pct_Over_65_Tercile,
                                                          levels = c('1', '2', '3'))

#removed_rows <- finance_imd_wide_regression %>%
#  left_join(CQC_Datasets_joined %>%
       #       select(odsCode, ownershipType), 
          #  by = c("Practice_Code" = "odsCode")) %>%
  #filter(is.na(ownershipType))  # Filter rows where no match was found (ownershipType is NA)

#also join the GPPS data onto the dataset..

All_regression_registered <- lm( `Avg (Registered)` ~ IMD_QUINTILE + Contract_Type + Dispensing_Practice + Practice_Rurality + Registered_Patient_Group + Pct_Over_65_Tercile + Field_Value_Tercile + ownershipType, data = finance_imd_wide_regression)
summary(All_regression_registered)

All_regression_weighted <- lm( `Avg (Weighted)` ~ IMD_QUINTILE + Contract_Type + Dispensing_Practice + Practice_Rurality + Registered_Patient_Group + Pct_Over_65_Tercile + Field_Value_Tercile + ownershipType, data = finance_imd_wide_regression)
summary(All_regression_weighted)

All_regression_registered_including_deductions_excluding_premises_and_reimbursemnt <- lm( `Avg (Registered) Including deductions, excluding premises and reimbursment` ~ IMD_QUINTILE + Contract_Type + Dispensing_Practice + Practice_Rurality + Registered_Patient_Group + Pct_Over_65_Tercile + Field_Value_Tercile + ownershipType, data = finance_imd_wide_regression)
summary(All_regression_registered_including_deductions_excluding_premises_and_reimbursemnt)

All_regression_weighted_including_deductions_excluding_premises_and_reimbursemnt <- lm( `Avg (Weighted) Including deductions, excluding premises and reimbursment` ~ IMD_QUINTILE + Contract_Type + Dispensing_Practice + Practice_Rurality + Registered_Patient_Group + Pct_Over_65_Tercile + Field_Value_Tercile + ownershipType, data = finance_imd_wide_regression)
summary(All_regression_weighted_including_deductions_excluding_premises_and_reimbursemnt)

All_regression_registered_excluding_dispensing
All_regression_weighted_excluding_dispensing <- lm( `Avg (Weighted) excluding dispensing` ~ IMD_QUINTILE + Contract_Type + Dispensing_Practice + Practice_Rurality + Registered_Patient_Group + Pct_Over_65_Tercile + Field_Value_Tercile + ownershipType, data = finance_imd_wide_regression)
summary(All_regression_weighted_excluding_dispensing)


library(writexl)
write_xlsx(finance_imd_wide_regression, "Practice.xlsx")

library(corrplot)
 
numeric_data <- finance_imd_wide_regression[sapply(finance_imd_wide_regression, is.numeric)]
corr_matrix <- cor(numeric_data, use = "complete.obs", method = "spearman")
corrplot(corr_matrix, method = "circle", tl.cex = 0.1)

library(vcd)
library(colorspace)


# Convert ownershipType to numeric (ordinal, treated as factors)
finance_imd_wide_regression$ownershipType_num <- as.numeric(finance_imd_wide_regression$ownershipType)

# Convert IMD_QUINTILE to numeric (ordinal: 1 to 5)
finance_imd_wide_regression$IMD_QUINTILE_num <- as.numeric(as.character(finance_imd_wide_regression$IMD_QUINTILE))

# Convert Practice_Rurality to numeric (ordered: Urban=1, Rural=2)
finance_imd_wide_regression$Practice_Rurality_num <- ifelse(finance_imd_wide_regression$Practice_Rurality == 'Urban', 1, 2)

# Convert Contract_Type to numeric (nominal, treated as factors)
finance_imd_wide_regression$Contract_Type_num <- as.numeric(finance_imd_wide_regression$Contract_Type)

# Convert Dispensing_Practice to numeric (binary: No=0, Yes=1)
finance_imd_wide_regression$Dispensing_Practice_num <- ifelse(finance_imd_wide_regression$Dispensing_Practice == 'No', 0, 1)

# Include the numeric variables you already have
numeric_data <- finance_imd_wide_regression[, c(
  "ownershipType_num",           # Converted from ownershipType
  "IMD_QUINTILE_num",            # Converted from IMD_QUINTILE
  "Practice_Rurality_num",       # Converted from Practice_Rurality
  "Contract_Type_num",           # Converted from Contract_Type
  "Dispensing_Practice_num",     # Converted from Dispensing_Practice
  "Pct_Over_65",                 # Already numeric
  "Field_Value",                 # Already numeric
  "REGISTERED"                   # Already numeric
)]

# Calculate Spearman correlation
corr_matrix <- cor(numeric_data, use = "complete.obs", method = "spearman")

# Create the correlation matrix
corr_matrix <- cor(numeric_data, use = "complete.obs", method = "spearman")

# Create the correlation plot with correlation coefficients
library(corrplot)
corrplot(corr_matrix, 
         method = "circle",          # Use circles to represent correlations
         tl.cex = 0.8,               # Adjust the size of the text labels
         addCoef.col = "black",      # Add the correlation coefficients in black
         number.cex = 0.7)           # Adjust the size of the correlation numbers

