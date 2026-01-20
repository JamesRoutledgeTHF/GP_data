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

finances_IMD_PRACTICE <- finances_IMD_PRACTICE %>% filter( 
  `Global_Sum Avg (Weighted)` >= 0, 
  `Prescribing Avg (Weighted)` >= 0, 
  `IT_Premises Avg (Weighted)` >= 0, 
  `QOF Avg (Weighted)` >= 0, 
  `Other_And_PCOAdmin Avg (Weighted)` >= 0, 
  `Local_Incentive_Schemes Avg (Weighted)` >= 0, 
  `Directed_Enhanced_Services Avg (Weighted)` >= 0, 
  `Dispensing Avg (Weighted)` >= 0, 
  `DrugReinbursement Avg (Weighted)` >= 0, 
  `PCNPARTICIPATION Avg (Weighted)` >= 0, 
  `ACCESSANDTRANSFORMATION Avg (Weighted)` >= 0 )

#write_xlsx(finances_IMD_PRACTICE, "finances_IMD_PRACTICE.xlsx")

#finances_IMD_PRACTICE <- finances_IMD_PRACTICE %>%
 # mutate(
    #`PCO_Payments Avg (Weighted)` = as.numeric(`PCO_Payments Avg (Weighted)`)  # Convert column to numeric
#  )

options(scipen = 999)  # This makes R avoid scientific notation

finances_IMD_PRACTICE$IMD_QUINTILE <- factor(finances_IMD_PRACTICE$IMD_QUINTILE,
                                             levels = c('5', '1', '2', '3', '4'))

Global_Sum_Model <- lm(`Global_Sum Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
Presc_Model <- lm(`Prescribing Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
IT_Model <- lm(`IT_Premises Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
QOF_Model <- lm(`QOF Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
Other_And_PCOAdmin_Model <- lm(`Other_And_PCOAdmin Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
LIS_Model <- lm(`Local_Incentive_Schemes Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
DES_Model <- lm(`Directed_Enhanced_Services Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
Dispensing_Model <- lm(`Dispensing Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
DrugReinbursement <- lm(`DrugReinbursement Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
PCNParticipation <- lm(`PCNPARTICIPATION Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
AccessAndTransformation <- lm(`ACCESSANDTRANSFORMATION Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)

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




finance_imd_wide_regression <- finance_imd_wide_regression %>%
  mutate(
    Contract_Type = factor(Contract_Type),
    Dispensing_Practice = factor(Dispensing_Practice),
    Practice_Rurality = factor(Practice_Rurality),
    IMD_QUINTILE = factor(IMD_QUINTILE)
  )

finance_imd_wide_regression$IMD_QUINTILE <- factor(finance_imd_wide_regression$IMD_QUINTILE,
                                             levels = c('5', '1', '2', '3', '4'))

finance_imd_wide_regression$Practice_Rurality <- factor(finance_imd_wide_regression$Practice_Rurality,
                                                   levels = c('Urban', 'Rural'))

finance_imd_wide_regression$Contract_Type <- factor(finance_imd_wide_regression$Contract_Type,
                                                        levels = c('GMS', 'APMS', "PMS"))

#+ Contract_Type + Dispensing_Practice + Practice_Rurality + `Average Number of Registered Patients` + Pct_Over_65

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  mutate(
    # Grouping by the number of registered patients
    Registered_Patient_Group = cut(
      `Average Number of Registered Patients`,  # This is the column for grouping
      breaks = c(0, 10000, 20000, 50000),      # Define the bins: 0-10,000, 10,001-20,000, 20,001-50,000
      right = FALSE,                           # Ensure bins are closed on the left
      include.lowest = TRUE,                   # Include the lowest value in the first bin
      labels = c("0-10,000", "10,001-20,000", "20,001-50,000") # Assign labels for the bins
    )
  )

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  mutate(
    # Grouping by `Pct_Over_65`
    Pct_Over_65_Group = cut(
      Pct_Over_65,                           # This is the column for grouping
      breaks = c(0, 10, 20, 30, 40, 55),     # Define the bins: 0-10%, 11-20%, 21-30%, 31-40%, 41-55%
      right = FALSE,                         # Ensure bins are closed on the left
      include.lowest = TRUE,                 # Include the lowest value in the first bin
      labels = c("0-10%", "11-20%", "21-30%", "31-40%", "41-55%") # Assign labels for the bins
    )
  )

All_regression_registered <- lm(`Average payments per registered patient` ~ IMD_QUINTILE + Contract_Type + Dispensing_Practice + Practice_Rurality + Registered_Patient_Group + Pct_Over_65_Group, data = finance_imd_wide_regression)
summary(All_regression_registered)

All_regression_weighted <- lm(`Average payments per weighted patient` ~ IMD_QUINTILE + Contract_Type + Dispensing_Practice + Practice_Rurality + Registered_Patient_Group + Pct_Over_65_Group, data = finance_imd_wide_regression)
summary(All_regression_weighted)


library(writexl)
write_xlsx(finances_IMD_PRACTICE, "Practice.xlsx")


