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




finance_imd_wide_regression <- finance_imd_wide_regression %>%
  mutate(
    Contract_Type = factor(Contract_Type),
    Dispensing_Practice = factor(Dispensing_Practice),
    Practice_Rurality = factor(Practice_Rurality),
    IMD_QUINTILE = factor(IMD_QUINTILE)
  )

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  group_by(YEAR, Practice_Code) %>%
  filter(any(`Average payments per registered patient` >= 0)) %>%
  ungroup()

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
      breaks = c(0, 5000, 10000, 20000, 50000),      # Define the bins: 0-10,000, 10,001-20,000, 20,001-50,000
      right = FALSE,                           # Ensure bins are closed on the left
      include.lowest = TRUE,                   # Include the lowest value in the first bin
      labels = c("0-5000", "5001-10000", "10001-20000", "20001-50000" ) # Assign labels for the bins
    )
  )

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  mutate(
    # Grouping by `Pct_Over_65` into terciles
    Pct_Over_65_Tercile = ntile(Pct_Over_65, 3)  # Divide the data into 3 equal parts
  )

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  inner_join(GPPS_Datasets %>%
               select(Practice_Code, Field_Name, Field_Value), 
             by = "Practice_Code") 

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  filter(Field_Value >= 0)  # Remove rows where Field_Value is less than 0

finance_imd_wide_regression <- finance_imd_wide_regression %>%
  inner_join(CQC_Datasets_joined %>%
               select(odsCode, ownershipType), 
             by = c("Practice_Code" = "odsCode"))


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

All_regression_registered <- lm(`Average payments per registered patient` ~ IMD_QUINTILE + Contract_Type + Dispensing_Practice + Practice_Rurality + Registered_Patient_Group + Pct_Over_65_Tercile + Field_Value_Tercile + ownershipType, data = finance_imd_wide_regression)
summary(All_regression_registered)

All_regression_weighted <- lm(`Average payments per weighted patient` ~ IMD_QUINTILE + Contract_Type + Dispensing_Practice + Practice_Rurality + Registered_Patient_Group + Pct_Over_65_Tercile + Field_Value_Tercile + ownershipType, data = finance_imd_wide_regression)
summary(All_regression_weighted)


library(writexl)
write_xlsx(finances_IMD_PRACTICE, "Practice.xlsx")

# Check the levels of each factor variable in the dataset
sapply(finance_imd_wide_regression[, c("IMD_QUINTILE", "Contract_Type", "Dispensing_Practice", 
                                       "Practice_Rurality", "Registered_Patient_Group", 
                                       "Pct_Over_65_Group", "Field_Value", "ownershipType")], 
       function(x) length(unique(x)))


