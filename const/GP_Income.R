# Run the SQL query
GP_income_range <- dbGetQuery(con, "
  SELECT 
    [GP_Type],
    [Contract_Type],
    [Country],
    [Practice_Type],
    [Gender],
    [Age],
    [Rurality],
    [Region],
    [Practice_Registered_Patients],
    [Weekly_Working_Hours],
    [Range_Of_Gross_Earnings_From_Self_Employment],
    [Range_Of_Total_Expenses_From_Self_Employment],
    [Range_Of_Income_Before_Tax_From_Self_Employment],
    [Range_Of_Total_Income_Before_Tax],
    [Measure],
    [Value],
    [Effective_Snapshot_Date],
    [Ethnicity]
  FROM [GP_Earnings_And_Expenses_Estimates].[Data1]
  WHERE 
        [Contract_Type] = 'GPMS' AND 
        [Country] = 'England' AND
        ([Range_Of_Income_Before_Tax_From_Self_Employment] IS NOT NULL OR
         [Range_Of_Total_Income_Before_Tax] IS NOT NULL)
")


# Filter the dataframe for Count_Of_GPs
GP_income_range_count <- GP_income_range %>%
  filter(Measure == "Count_of_GPs")

# Filter the dataframe for Percentage_of_GPs_%
GP_income_range_percentage <- GP_income_range %>%
  filter(Measure == "Percentage_of_GPs_%")

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the order of income ranges for Salaried GPs
ordered_income_ranges_salaried <- c(
  "0 - 25,000", "25,000 - 50,000", "50,000 - 75,000", "75,000 - 100,000", 
  "100,000 - 125,000", "125,000+"
)

# Define the order of income ranges for Contractor GPs
ordered_income_ranges_contractor <- c(
  "0 - 50,000", "50,000 - 75,000", "75,000 - 100,000", "100,000 - 125,000",
  "125,000 - 150,000", "150,000 - 175,000", "175,000 - 200,000", 
  "200,000 - 225,000", "225,000 - 250,000", "250,000 - 275,000",
  "275,000 - 300,000", "300,000 - 400,000", "400,000+"
)

# Reorder the factors in both datasets using the predefined order
GP_income_range_count_filtered_salaried <- GP_income_range_count_filtered_salaried %>%
  mutate(Range_Of_Total_Income_Before_Tax = factor(Range_Of_Total_Income_Before_Tax, 
                                                   levels = ordered_income_ranges_salaried))

GP_income_range_count_filtered_contractor <- GP_income_range_count_filtered_contractor %>%
  mutate(Range_Of_Income_Before_Tax_From_Self_Employment = factor(Range_Of_Income_Before_Tax_From_Self_Employment, 
                                                                  levels = ordered_income_ranges_contractor))

# Apply the same reordering to the percentage datasets
GP_income_range_percentage_filtered_salaried <- GP_income_range_percentage_filtered_salaried %>%
  mutate(Range_Of_Total_Income_Before_Tax = factor(Range_Of_Total_Income_Before_Tax, 
                                                   levels = ordered_income_ranges_salaried))

GP_income_range_percentage_filtered_contractor <- GP_income_range_percentage_filtered_contractor %>%
  mutate(Range_Of_Income_Before_Tax_From_Self_Employment = factor(Range_Of_Income_Before_Tax_From_Self_Employment, 
                                                                  levels = ordered_income_ranges_contractor))

# Plot 1: Salaried GP with Range_Of_Income_Before_Tax for Count
plot1 <- ggplot(GP_income_range_count_filtered_salaried, aes(x = Range_Of_Total_Income_Before_Tax, y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Count of Salaried GPs by Income Range",
       x = "Range of Income Before Tax",
       y = "Count of GPs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Contractor GP with Range_Of_Income_Before_Tax_From_Self_Employment for Count
plot2 <- ggplot(GP_income_range_count_filtered_contractor, aes(x = Range_Of_Income_Before_Tax_From_Self_Employment, y = Value)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Count of Contractor GPs by Income Range",
       x = "Range of Income Before Tax (Self Employment)",
       y = "Count of GPs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: Salaried GP with Range_Of_Income_Before_Tax for Percentage
plot3 <- ggplot(GP_income_range_percentage_filtered_salaried, aes(x = Range_Of_Total_Income_Before_Tax, y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Percentage of Salaried GPs by Income Range (23/24)",
       x = "Range of Income Before Tax",
       y = "Percentage of GPs (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4: Contractor GP with Range_Of_Income_Before_Tax_From_Self_Employment for Percentage
plot4 <- ggplot(GP_income_range_percentage_filtered_contractor, aes(x = Range_Of_Income_Before_Tax_From_Self_Employment, y = Value)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Percentage of Contractor GPs by Income Range (23/24)",
       x = "Range of Income Before Tax (Self Employment)",
       y = "Percentage of GPs (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Now, let's print the plots to view them
plot1
plot2
plot3
plot4





