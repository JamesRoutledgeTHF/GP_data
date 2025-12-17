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
    PCO_Payments        = sum(Total.PCO, na.rm = TRUE),
    Directed_Enhanced_Services = sum(Total.DirectedEnhancedServices, na.rm = TRUE),
    Local_Incentive_Schemes = sum(Total.LocalIncentiveSchemes, na.rm = TRUE),
    Prescribing         = sum(Total.Prescribing, na.rm = TRUE),
    COVID_19            = sum(Total.COVID, na.rm = TRUE),
    QOF                 = sum(QOF_PAYMENT, na.rm = TRUE),
    REGISTERED          = sum(REGISTERED_PATIENTS, na.rm = TRUE),
    WEIGHTED            = sum(WEIGHTED_PATIENTS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(
    c(Global_Sum, IT_Premises, PCO_Payments, Directed_Enhanced_Services, Local_Incentive_Schemes, Prescribing, COVID_19, QOF),
    list(
      "Avg (Registered)" = ~ . / REGISTERED,
      "Avg (Weighted)"   = ~ . / WEIGHTED
    ),
    .names = "{.col} {fn}"
  )) 

write_xlsx(finances_IMD_PRACTICE, "finances_IMD_PRACTICE.xlsx")

finances_IMD_PRACTICE <- finances_IMD_PRACTICE %>%
  mutate(
    `PCO_Payments Avg (Weighted)` = as.numeric(`PCO_Payments Avg (Weighted)`)  # Convert column to numeric
  )

# Ensure no scientific notation is displayed
options(scipen = 999)  # This makes R avoid scientific notation

# If you need to format it manually without scientific notation
finances_IMD_PRACTICE <- finances_IMD_PRACTICE %>%
  mutate(
    Prescribing_Weighted = format(PCO_Payments, scientific = FALSE)
  )

finances_IMD_PRACTICE$IMD_QUINTILE <- factor(finances_IMD_PRACTICE$IMD_QUINTILE,
                                             levels = c('5', '1', '2', '3', '4'))

Global_Sum_Model <- lm(`Global_Sum Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
Presc_Model <- lm(`Prescribing Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
IT_Model <- lm(`IT_Premises Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
QOF_Model <- lm(`QOF Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
PCO_Model <- lm(`PCO_Payments Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
LIS_Model <- lm(`Local_Incentive_Schemes Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
DES_Model <- lm(`Directed_Enhanced_Services Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)

summary(Global_Sum_Model)
summary(Presc_Model)
summary(IT_Model)
summary(QOF_Model)
summary(PCO_Model)
summary(LIS_Model)
summary(DES_Model)

Global_Sum_Model_reg <- lm(`Global_Sum Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
Presc_Model_reg <- lm(`Prescribing Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
IT_Model_reg <- lm(`IT_Premises Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
QOF_Model_reg <- lm(`QOF Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
PCO_Model_reg <- lm(`PCO_Payments Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
LIS_Model_reg <- lm(`Local_Incentive_Schemes Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
DES_Model_reg <- lm(`Directed_Enhanced_Services Avg (Registered)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)

summary(Global_Sum_Model_reg)
summary(Presc_Model_reg)
summary(IT_Model_reg)
summary(QOF_Model_reg)
summary(PCO_Model_reg)
summary(LIS_Model_reg)
summary(DES_Model_reg)

df_cat <- Weighted_Patients_Finance_Categories %>%
  filter(PayCat_base == "Prescribing Avg")

anova <- aov(`Total (£)` ~ IMD_QUINTILE, data = df_cat)

summary(anova)
