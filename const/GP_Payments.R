
library(readxl)

GP_Workforce <- read_excel("GPW Bulletin Tables - October 2025.xlsx", sheet = "1a", skip = 4)
GP_Workforce_headcount <- read_excel("GPW Bulletin Tables - October 2025.xlsx", sheet = "1b", skip = 4)

# Load necessary libraries
library(lubridate)
library(dplyr)

# Clean column names
clean_colnames <- gsub("\r\n", " ", colnames(GP_Workforce))  # Remove newline characters
clean_colnames <- gsub("\\s*\\[.*", "", clean_colnames)      # Remove everything after '[' (including the notes part)
clean_colnames <- trimws(clean_colnames)                     # Trim any leading/trailing spaces

clean_colnames2 <- gsub("\r\n", " ", colnames(GP_Workforce_headcount))  # Remove newline characters
clean_colnames2 <- gsub("\\s*\\[.*", "", clean_colnames2)      # Remove everything after '[' (including the notes part)
clean_colnames2 <- trimws(clean_colnames2)    

# Manually fix any discrepancies by replacing full month names with an accepted date format
# Ensure months and years are in the format "Month YYYY"
clean_colnames <- gsub("(January|February|March|April|May|June|July|August|September|October|November|December)\\s+(\\d{4})", 
                       "\\1 \\2", clean_colnames)

clean_colnames2 <- gsub("(January|February|March|April|May|June|July|August|September|October|November|December)\\s+(\\d{4})", 
                       "\\1 \\2", clean_colnames2)

# Add "01" to the start of each month-year (to make it a valid date)
clean_colnames <- paste("01", clean_colnames)
clean_colnames2 <- paste("01", clean_colnames2)

# Convert to Date format using lubridate's mdy function
clean_colnames <- dmy(clean_colnames)
clean_colnames2 <- dmy(clean_colnames2)

# Check if the conversion worked
colnames(GP_Workforce) <- clean_colnames
colnames(GP_Workforce_headcount) <- clean_colnames2

# View the updated column names
colnames(GP_Workforce)

GP_Workforce <- GP_Workforce[-c(1:3), ]
GP_Workforce_headcount <- GP_Workforce_headcount[-c(1:3), ]

# Keep only rows 1 to 10 and discard the others
GP_Workforce <- GP_Workforce[1:10, ]
GP_Workforce_headcount <- GP_Workforce_headcount[1:10, ]

GP_Workforce <- GP_Workforce[-c(2), ]
GP_Workforce_headcount <- GP_Workforce_headcount[-c(2), ]

# Remove the last column
GP_Workforce <- GP_Workforce[, -ncol(GP_Workforce)]
GP_Workforce_headcount <- GP_Workforce_headcount[, -ncol(GP_Workforce_headcount)]

# Check the updated column names
colnames(GP_Workforce_headcount)

# Fix NA column names before renaming
colnames(GP_Workforce_headcount)[is.na(colnames(GP_Workforce_headcount))] <- "Unknown_Column"

# Verify the updated column names
colnames(GP_Workforce_headcount)

# Rename the first column to "GP_Group"
GP_Workforce <- GP_Workforce %>%
  rename(GP_Group = 1)  # Rename the first column to GP_Group

# Rename the first column to "GP_Group"
GP_Workforce_headcount <- GP_Workforce_headcount %>%
  rename(GP_Group = 1)  # Rename the first column to GP_Group
GP_Workforce_headcount <- GP_Workforce_headcount %>%
  rename("2017-03-01" = 6)  # Rename the first column to GP_Group

# Convert columns 2 onwards to numeric without affecting the headers
GP_Workforce[ , 2:ncol(GP_Workforce)] <- sapply(GP_Workforce[ , 2:ncol(GP_Workforce)], as.numeric)
GP_Workforce_headcount[ , 2:ncol(GP_Workforce_headcount)] <- sapply(GP_Workforce_headcount[ , 2:ncol(GP_Workforce_headcount)], as.numeric)

# Pivot GP_Workforce to long format with columns 'Date' and 'FTE'
GP_Workforce_long <- GP_Workforce %>%
  pivot_longer(cols = -GP_Group, 
               names_to = "Date", 
               values_to = "Values")

# Pivot GP_Workforce_headcount to long format with columns 'Date' and 'Headcount'
GP_Workforce_headcount_long <- GP_Workforce_headcount %>%
  pivot_longer(cols = -GP_Group, 
               names_to = "Date", 
               values_to = "Values")

# Add a new column identifying the source of the data
# Add a new column identifying the source of the data in GP_Workforce_long (FTE)
GP_Workforce_long <- GP_Workforce_long %>%
  mutate(Group = "FTE")

# Add a new column identifying the source of the data in GP_Workforce_headcount_long (Headcount)
GP_Workforce_headcount_long <- GP_Workforce_headcount_long %>%
  mutate(Group = "Headcount")

# Combine the two datasets by stacking the rows
GP_Workforce_combined <- bind_rows(GP_Workforce_long, GP_Workforce_headcount_long)


# Load necessary libraries
library(dplyr)
library(tidyr)
library(lubridate)

GP_Workforce_combined <- GP_Workforce_combined %>%
  mutate(Date = as.Date(Date),  # Convert the Date column to Date format
         Year = year(Date),     # Extract the year
         Month = month(Date),   # Extract the month
         Financial_Year = ifelse(Month >= 4, Year, Year - 1),  # Create Financial Year
         Financial_Year = paste0(Financial_Year, "/", substr(Financial_Year + 1, 3, 4)))  # Format as '2020/21'



GP_Workforce_summary <- GP_Workforce_combined %>%
  group_by(Financial_Year, GP_Group, Group) %>%
  summarise(Average_Value = mean(Values, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Pivot the data to get FTE and Headcount in separate columns
GP_Workforce_pivoted <- GP_Workforce_summary %>%
  pivot_wider(names_from = Group, values_from = Average_Value)

# Step 3: Calculate the FTE/Headcount ratio
GP_Workforce_pivoted <- GP_Workforce_pivoted %>%
  mutate(FTE_Ratio = FTE / Headcount)

# Filter the dataset to include only GP Partners and Salaried GPs
GP_Workforce_pivoted <- GP_Workforce_pivoted %>%
  filter(GP_Group %in% c("GP Partners", "Salaried GPs"))

# Rename GP_Group to Type and update the values within the column
GP_Workforce_pivoted <- GP_Workforce_pivoted %>%
  rename(Year = Financial_Year) %>%  # Rename the column
  rename(Type = GP_Group) %>%  # Rename the column
  mutate(Type = recode(Type, 
                       "GP Partners" = "Contractor", 
                       "Salaried GPs" = "Salaried"))  # Recode the values



# View the results
head(GP_Workforce_avg)

# Load dplyr library
library(dplyr)

GP_Salary_Contractor <- read_excel("gpearnextime_202324 V2.xlsx", sheet = "1a. Contractor earn. by country", skip = 7)

GP_Salary_Contractor <- GP_Salary_Contractor%>%
  filter(Country == "England") %>%
  filter(`Practice Type \r\n[note 5]` == "All practices") %>%
  filter(Category == "Income Before Tax") %>%
  filter(`Contract Type\r\n[note 6]` == "GPMS")

# Find the column that matches "2014/15 [note 4]" exactly and remove it
GP_Salary_Contractor <- GP_Salary_Contractor[, !grepl("^2014/15 \\[note 4\\]$", colnames(GP_Salary_Contractor))]

# Remove any text within square brackets (including the brackets) from the column names
colnames(GP_Salary_Contractor) <- gsub("\\[.*?\\]", "", colnames(GP_Salary_Contractor))

# Optionally, you can also remove extra spaces that may have been left
colnames(GP_Salary_Contractor) <- gsub("\\s+", " ", colnames(GP_Salary_Contractor))

# Check the updated column names
colnames(GP_Salary_Contractor)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(lubridate)

# Check the column names and structure of the GP_Workforce dataset
colnames(GP_Workforce)
str(GP_Workforce)




# Convert the columns 5 onwards (which contain '2002/03' style values) to a proper year format
##colnames(GP_Salary_Contractor)[5:ncol(GP_Salary_Contractor)] <- gsub("(\\d{4})/\\d{2}", "\\1", colnames(GP_Salary_Contractor)[5:ncol(GP_Salary_Contractor)])

# Check the updated column names
#colnames(GP_Salary_Contractor)

GP_Salary_Salaried <- read_excel("gpearnextime_202324 V2.xlsx", sheet = "8a. Salaried by Country ", skip = 7 )

GP_Salary_Salaried <- GP_Salary_Salaried%>%
  filter(Country == "England") %>%
  filter(`Category [note 22] \r\n[note 23] [note 24]` == "Total Income Before Tax")%>%
  filter(`Contract Type\r\n[note 6]` == "GPMS")

# Remove the text inside square brackets (including the brackets) from the column names
colnames(GP_Salary_Salaried) <- gsub("\\[.*?\\]", "", colnames(GP_Salary_Salaried))

# Optionally, remove extra spaces that may have been left
colnames(GP_Salary_Salaried) <- gsub("\\s+", " ", colnames(GP_Salary_Salaried))

# Check the updated column names
colnames(GP_Salary_Salaried)

# Remove the trailing space in the "Category " column name
colnames(GP_Salary_Salaried) <- gsub("Category \\s*$", "Category", colnames(GP_Salary_Salaried))

# Check the updated column names
colnames(GP_Salary_Salaried)


# Convert all columns to character in both dataframes
GP_Salary_Contractor[] <- lapply(GP_Salary_Contractor, as.character)
GP_Salary_Salaried[] <- lapply(GP_Salary_Salaried, as.character)

# Add a new column to label the dataset
GP_Salary_Contractor$Type <- "Contractor"
GP_Salary_Salaried$Type <- "Salaried"

# Combine the datasets
GP_Salary_Combined <- bind_rows(GP_Salary_Contractor, GP_Salary_Salaried)

# Check the result
head(GP_Salary_Combined)

# Load necessary libraries
library(tidyr)
library(ggplot2)
library(dplyr)

# Pivot the data (assuming `GP_Salary_Contractor` has your year columns as column names)
GP_Salary_Combined_long <- GP_Salary_Combined %>%
  pivot_longer(cols = starts_with("20"),  # Adjust this if your years are in different columns
               names_to = "Year",
               values_to = "Value")

# Convert the "Value" column to numeric (if it's not already)
GP_Salary_Combined_long$Value <- as.numeric(GP_Salary_Combined_long$Value)

# Remove rows where 'Value' is NA
GP_Salary_Combined_long_clean <- GP_Salary_Combined_long %>%
  filter(!is.na(Value))  # This filters out rows where Value is NA

# Create the line plot with Type as the group and set y-axis to start at 0
ggplot(GP_Salary_Combined_long_clean, aes(x = Year, y = Value, group = Type, color = Type)) +
  geom_line(size = 1) +  # Increase line thickness
  geom_point(size = 2) +   # Add dots at each year
  labs(title = "Value over Years by Type",
       x = "Year",
       y = "Value") +
  scale_y_continuous(limits = c(0, 200000), expand = c(0, 0)) +  # Set y-axis to start at 0
  theme_minimal()




# Join the two datasets by 'Type' and 'Financial_Year'
GP_Salary_Workforce_combined <- GP_Salary_Combined_long_clean %>%
  left_join(GP_Workforce_pivoted, by = c("Type", "Year"))

# Calculate the payment-to-FTE ratio
GP_Salary_Workforce_combined <- GP_Salary_Workforce_combined %>%
  mutate(Payment_to_FTE = Value / FTE_Ratio)

library(ggplot2)

# Plot the Payment-to-FTE ratio by Financial Year and GP Type
ggplot(GP_Salary_Workforce_combined, aes(x = Year, y = Payment_to_FTE, color = Type, group = Type)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +  # Add dots at each year point
  labs(title = "Payment to FTE Ratio by Financial Year and GP Type",
       x = "Financial Year",
       y = "Payment to FTE Ratio") +
  scale_y_continuous(limits = c(0, NA)) +  # Set Y-axis to start at 0
  scale_x_discrete(limits = c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")) +  # Adjust X-axis limits
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability




