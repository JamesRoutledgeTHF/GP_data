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

# Summarizing data to calculate average payments for each IMD quintile
finances_IMD_PRACTICE_avg <- finances_IMD_PRACTICE %>%
  group_by(IMD_QUINTILE) %>%
  summarise(
    Global_Sum_Avg = mean(`Global_Sum Avg (Weighted)`, na.rm = TRUE),
    IT_Premises_Avg = mean(`IT_Premises Avg (Weighted)`, na.rm = TRUE),
    Other_And_PCOAdmin_Avg = mean(`Other_And_PCOAdmin Avg (Weighted)`, na.rm = TRUE),
    Directed_Enhanced_Services_Avg = mean(`Directed_Enhanced_Services Avg (Weighted)`, na.rm = TRUE),
    ACCESSANDTRANSFORMATION_Avg = mean(`ACCESSANDTRANSFORMATION Avg (Weighted)`, na.rm = TRUE),
    Local_Incentive_Schemes_Avg = mean(`Local_Incentive_Schemes Avg (Weighted)`, na.rm = TRUE),
    Prescribing_Avg = mean(`Prescribing Avg (Weighted)`, na.rm = TRUE),
    Dispensing_Avg = mean(`Dispensing Avg (Weighted)`, na.rm = TRUE),
    DrugReinbursement_Avg = mean(`DrugReinbursement Avg (Weighted)`, na.rm = TRUE),
    QOF_Avg = mean(`QOF Avg (Weighted)`, na.rm = TRUE),
    Deductions_Avg = mean(`Deductions Avg (Weighted)`, na.rm = TRUE),
    PCNPARTICIPATION_Avg = mean(`PCNPARTICIPATION Avg (Weighted)`, na.rm = TRUE),
    .groups = "drop"
  )

# Reshape the data to long format for plotting
finances_IMD_PRACTICE_avg_long <- finances_IMD_PRACTICE_avg %>%
  pivot_longer(
    cols = starts_with("Global_Sum_Avg"):starts_with("PCNPARTICIPATION_Avg"),
    names_to = "Payment_Category",
    values_to = "Avg_Payment"
  )

# Create the horizontal bar chart
ggplot(finances_IMD_PRACTICE_avg_long, aes(x = Avg_Payment, y = IMD_QUINTILE, fill = Payment_Category)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Average Payment per Registered Patient",
    y = "IMD Quintile",
    fill = "Payment Category",
    title = "Average Payment per Category by IMD Quintile (Registered Patients)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),  # Adjust Y axis text size for readability
    axis.text.x = element_text(size = 10),  # Adjust X axis text size for better readability
    axis.title.x = element_text(size = 12), # Size for x axis title
    axis.title.y = element_text(size = 12), # Size for y axis title
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


#write_xlsx(finances_IMD_PRACTICE, "finances_IMD_PRACTICE.xlsx")

#finances_IMD_PRACTICE <- finances_IMD_PRACTICE %>%
 # mutate(
    #`PCO_Payments Avg (Weighted)` = as.numeric(`PCO_Payments Avg (Weighted)`)  # Convert column to numeric
#  )

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
Other_And_PCOAdmin_Model <- lm(`Other_And_PCOAdmin Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
LIS_Model <- lm(`Local_Incentive_Schemes Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)
DES_Model <- lm(`Directed_Enhanced_Services Avg (Weighted)` ~ `IMD_QUINTILE`, data = finances_IMD_PRACTICE)

summary(Global_Sum_Model)
summary(Presc_Model)
summary(IT_Model)
summary(QOF_Model)
summary(Other_And_PCOAdmin_Model)
summary(LIS_Model)
summary(DES_Model)

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

df_cat <- Weighted_Patients_Finance_Categories %>%
  filter(PayCat_base == "Prescribing Avg")

anova <- aov(`Total (£)` ~ IMD_QUINTILE, data = df_cat)

summary(anova)
