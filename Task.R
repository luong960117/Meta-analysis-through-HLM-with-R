
##############################################################
#                                                             #
#                                                             #
#   # Importing libraries                         #
#                                                             #
#                                                             #
##############################################################

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(lme4)

##############################################################
#                                                                                          #
#                                                                                          #
#   # Importing the data and merge the four data files        #
#                                                                                           #
#                                                                                           #
##############################################################

# Import mathematics grade 4 data
mathematics_grade_4 <- read_excel("mathematics_grade_4.xlsx")

# rename columns
mathematics_grade_4 <- mathematics_grade_4 %>%
  rename(ME_S_20 = `...3`, ME_A_20 = `...5`, ME_S_10 = `...7`, ME_A_10 = `...9`,
         ME_S_5 = `...11`, ME_A_5 = `...13`, ME_S_0 = `...15`, ME_A_0 = `...17`,
         ME_E = `...19`)
# check structure of the data
str(mathematics_grade_4)

# Replace all occurrences of '~' with NA
mathematics_grade_4[mathematics_grade_4 == "~"] <- NA

# Remove all rows containing NA values
mathematics_grade_4 <- na.omit(mathematics_grade_4)



# import Science grade 4 data
Science_grade_4 <- read_excel("Science_grade_4.xlsx")

# rename columns
Science_grade_4 <- Science_grade_4 %>%
  rename(ME_S_20 = `...3`, ME_A_20 = `...5`, ME_S_10 = `...7`, ME_A_10 = `...9`,
         ME_S_5 = `...11`, ME_A_5 = `...13`, ME_S_0 = `...15`, ME_A_0 = `...17`,
         ME_E = `...19`)

# Replace all occurrences of '~' with NA
Science_grade_4[Science_grade_4 == "~"] <- NA

# Remove all rows containing NA values
Science_grade_4 <- na.omit(Science_grade_4)



# Import mathematics grade 8 data
mathematics_grade_8 <- read_excel("mathematics_grade_8.xlsx")

# rename columns
mathematics_grade_8 <- mathematics_grade_8 %>%
  rename(ME_S_20 = `...3`, ME_A_20 = `...5`, ME_S_10 = `...7`, ME_A_10 = `...9`,
         ME_S_5 = `...11`, ME_A_5 = `...13`, ME_S_0 = `...15`, ME_A_0 = `...17`,
         ME_E = `...19`)

# Replace all occurrences of '~' with NA
mathematics_grade_8[mathematics_grade_8 == "~"] <- NA

# Remove all rows containing NA values
mathematics_grade_8 <- na.omit(mathematics_grade_8)
#import Science grade 8 data
Science_grade_8 <- read_excel("Science_grade_8.xlsx")

# rename columns
Science_grade_8 <- Science_grade_8 %>%
  rename(ME_S_20 = `...3`, ME_A_20 = `...5`, ME_S_10 = `...7`, ME_A_10 = `...9`,
         ME_S_5 = `...11`, ME_A_5 = `...13`, ME_S_0 = `...15`, ME_A_0 = `...17`,
         ME_E = `...19`)

# Replace all occurrences of '~' with NA
Science_grade_8[Science_grade_8 == "~"] <- NA

# Remove all rows containing NA values
Science_grade_8 <- na.omit(Science_grade_8)

# Clean the column names to remove any leading/trailing spaces
colnames(mathematics_grade_4) <- str_trim(colnames(mathematics_grade_4))
colnames(mathematics_grade_8) <- str_trim(colnames(mathematics_grade_8))
colnames(Science_grade_4) <- str_trim(colnames(Science_grade_4))
colnames(Science_grade_8) <- str_trim(colnames(Science_grade_8))

# Convert 'Average_Achievement to numeric_0 for all data frames
mathematics_grade_4$Average_Achievement_0 <- as.numeric(mathematics_grade_4$Average_Achievement_0)
mathematics_grade_8$Average_Achievement_0 <- as.numeric(mathematics_grade_8$Average_Achievement_0)
Science_grade_4$Average_Achievement_0 <- as.numeric(Science_grade_4$Average_Achievement_0)
Science_grade_8$Average_Achievement_0 <- as.numeric(Science_grade_8$Average_Achievement_0)

# Convert Average_Achievement_5 to numeric for all data frames
mathematics_grade_4$Average_Achievement_5 <- as.numeric(mathematics_grade_4$Average_Achievement_5)
mathematics_grade_8$Average_Achievement_5 <- as.numeric(mathematics_grade_8$Average_Achievement_5)
Science_grade_4$Average_Achievement_5 <- as.numeric(Science_grade_4$Average_Achievement_5)
Science_grade_8$Average_Achievement_5 <- as.numeric(Science_grade_8$Average_Achievement_5)

# Convert Average_Achievement_10 to numeric for all data frames
mathematics_grade_4$Average_Achievement_10 <- as.numeric(mathematics_grade_4$Average_Achievement_10)
mathematics_grade_8$Average_Achievement_10 <- as.numeric(mathematics_grade_8$Average_Achievement_10)
Science_grade_4$Average_Achievement_10 <- as.numeric(Science_grade_4$Average_Achievement_10)
Science_grade_8$Average_Achievement_10 <- as.numeric(Science_grade_8$Average_Achievement_10)

# Convert Average_Achievement_20 to numeric for all data frames
mathematics_grade_4$Average_Achievement_20 <- as.numeric(mathematics_grade_4$Average_Achievement_20)
mathematics_grade_8$Average_Achievement_20 <- as.numeric(mathematics_grade_8$Average_Achievement_20)
Science_grade_4$Average_Achievement_20 <- as.numeric(Science_grade_4$Average_Achievement_20)
Science_grade_8$Average_Achievement_20 <- as.numeric(Science_grade_8$Average_Achievement_20)

# Convert 'ME_A_0 to numeric for all data frames
mathematics_grade_4$ME_A_0 <- as.numeric(mathematics_grade_4$ME_A_0)
mathematics_grade_8$ME_A_0 <- as.numeric(mathematics_grade_8$ME_A_0)
Science_grade_4$ME_A_0 <- as.numeric(Science_grade_4$ME_A_0)
Science_grade_8$ME_A_0 <- as.numeric(Science_grade_8$ME_A_0)

# Convert 'ME_A_5 to numeric for all data frames
mathematics_grade_4$ME_A_5 <- as.numeric(mathematics_grade_4$ME_A_5)
mathematics_grade_8$ME_A_5 <- as.numeric(mathematics_grade_8$ME_A_5)
Science_grade_4$ME_A_5 <- as.numeric(Science_grade_4$ME_A_5)
Science_grade_8$ME_A_5 <- as.numeric(Science_grade_8$ME_A_5)

# Convert 'ME_A_10 to numeric for all data frames
mathematics_grade_4$ME_A_10 <- as.numeric(mathematics_grade_4$ME_A_10)
mathematics_grade_8$ME_A_10 <- as.numeric(mathematics_grade_8$ME_A_10)
Science_grade_4$ME_A_10 <- as.numeric(Science_grade_4$ME_A_10)
Science_grade_8$ME_A_10 <- as.numeric(Science_grade_8$ME_A_10)

# Convert 'ME_A_20 to numeric for all data frames
mathematics_grade_4$ME_A_20 <- as.numeric(mathematics_grade_4$ME_A_20)
mathematics_grade_8$ME_A_20 <- as.numeric(mathematics_grade_8$ME_A_20)
Science_grade_4$ME_A_20 <- as.numeric(Science_grade_4$ME_A_20)
Science_grade_8$ME_A_20 <- as.numeric(Science_grade_8$ME_A_20)



# Add Subject and Grade columns to each data frame before combining
mathematics_grade_4$Subject <- "Mathematics"
mathematics_grade_4$Grade <- 4

mathematics_grade_8$Subject <- "Mathematics"
mathematics_grade_8$Grade <- 8

Science_grade_4$Subject <- "Science"
Science_grade_4$Grade <- 4

Science_grade_8$Subject <- "Science"
Science_grade_8$Grade <- 8

# Combine all data frames into one
combined_data <- bind_rows(
  mathematics_grade_4,
  mathematics_grade_8,
  Science_grade_4,
  Science_grade_8
)

combined_data$Grade = as.factor(combined_data$Grade)


colnames(combined_data)

str(combined_data)
# View the structure of the combined data
str(combined_data)

# Print the first few rows of the combined data
head(combined_data)

##############################################################
#                                                             #
#                                                             #
#                 # Data Codding                   #
#                                                             #
#                                                             #
##############################################################


combined_data <- combined_data %>%
  mutate(Experience_20_Category = case_when(
    Percent_Students_20 >= 75 ~ "Very High",
    Percent_Students_20 >= 50 ~ "High",
    Percent_Students_20 >= 25 ~ "Moderate",
    Percent_Students_20 > 0   ~ "Low"
  ))

combined_data <- combined_data %>%
  mutate(Region = case_when(
    Country %in% c("United States", "Ontario, Canada", "Quebec, Canada", "Canada") ~ "North America",
    Country %in% c("Germany", "Belgium (Flemish)", "France", "UK",
                   "Austria", "Czech Republic", "Denmark", "England", "Finland", 
                   "France", "Georgia", "Germany", "Hungary", "Ireland", "Italy",
                   "Kosovo", "Latvia", "Lithuania", "Malta", "Netherlands", "Northern Ireland",
                   "Norway (5)", "Norway (9)", "Poland", "Portugal", "Slovak Republic", "Spain", 
                   "Madrid, Spain","Sweden", "Romania") ~ "Eastern Europe",
    Country %in% c("China", "Japan", "India","armenia", "Azerbaijan", "Japan",
                   "Kazakhstan", "Korea", "Korea, Rep. of", "Pakistan", "Philippines", "Russian Federation",
                   "Singapore", "Turkey (5)", "Armenia", "Moscow City, Russian Fed.", 
                   "Russian Fed.", "Malaysia") ~ "Asia",
    Country %in% c("Australia", "New Zealand") ~ "Ocenia",
    Country %in% c("Bahrain", "Cyprus", "Iran, Islamic Rep. of", "Kuwait", "Oman", "Qatar", "Saudi Arabia",
                   "United Arab Emirates", "Dubai", "Abu Dhabi, UAE", "United Arab Emirates",
                   "Abu Dhabi", "Dubai, UAE", "Turkey", "Israel",
                   "Jordan", "Lebanon") ~ "Middle East",
    Country %in% c("Bosnia and Herzegovina", "Bulgaria", "Croatia", "Montenegro", 
                   "North Macedonia", "Serbia", "Albania") ~ "Balkans",
    Country %in% c("Chile") ~ "South America",
    Country %in% c("Chinese Taipei") ~ "Taiwan",
    Country %in% c("Hong Kong SAR") ~ "China",
    Country %in% c("Morocco", "Egypt") ~ "North Africa",
    Country %in% c("South Africa (5)", "South Africa (9)", "Western Cape, RSA (9)",
                   "Gauteng, RSA (9)")~ "South Africa",
    TRUE ~ "Other"
  ))

# move age to first column 
combined_data = combined_data %>% dplyr::select("Region",  
                                                everything()) 
combined_data$Region = as.factor(combined_data$Region)
colnames(combined_data)

# Calculate mean Average_Experience by Region
mean_experience <- combined_data %>%
  group_by(Region) %>%
  summarise(Mean_Average_Experience = mean(Average_Experience, na.rm = TRUE))

# Bar chart of mean Average_Experience by Region
ggplot(mean_experience, aes(x = reorder(Region, -Mean_Average_Experience), y = Mean_Average_Experience)) +
  geom_bar(stat = "identity", fill ="skyblue" , color = "black") +
  labs(title = "Mean Average Experience by Region",
       x = "Region",
       y = "Mean Average Experience") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate counts and percentages for each region
mean_experience <- mean_experience %>%
  mutate(Percentage = (Mean_Average_Experience / sum(Mean_Average_Experience)) * 100)

# Bar chart with percentages on top of the bars
ggplot(mean_experience, aes(x = reorder(Region, -Mean_Average_Experience), y = Mean_Average_Experience)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, color = "black") +  # Add percentage labels
  labs(title = "Mean Average Experience by Region",
       x = "Region",
       y = "Mean Average Experience") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Box plot of Average_Experience by Region
ggplot(combined_data, aes(x = Region, y = Average_Experience)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Average Experience by Region",
       x = "Region",
       y = "Average Experience") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot of Average_Achievement_20 by Region
ggplot(combined_data, aes(x = Region, y = Average_Achievement_20)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Studen't Performance  by Region",
       x = "Region",
       y = "Average Achievement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############################################################
#                                                             #
#                                                             #
#                 # Regional Variations                       #
#                                                             #
#                                                             #
##############################################################

# Fit ANOVA model
anova_model <- aov(Average_Achievement_20 ~ Region, data = combined_data)

# Summary of the ANOVA model
summary(anova_model)

##############################################################
#                                                             #
#                                                             #
#                 # Descriptive Statistics                    #
#                                                             #
#                                                             #
##############################################################

# Structure of the data
str(combined_data)

# column names in the data
colnames(combined_data)

# Filter numeric columns
numeric_data <- combined_data %>% 
  select_if(is.numeric)
summary(numeric_data)

# Loop through each numeric column and calculate its standard deviation
for (col in colnames(numeric_data)) {
  cat("The standard deviation for", col, "is:", sd(numeric_data[[col]], na.rm = TRUE), "\n")
}


# Distributions of the regions
table(combined_data$Region)

# Distributions of the subject
table(combined_data$Subject)

# Distributions of grade
table(combined_data$Grade)


##############################################################
#                                                             #
#                                                             #
#                 # Data Transformation                       #
#                                                             #
#                                                             #
##############################################################


# Function to remove outliers based on IQR for a specific column
remove_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  df <- df[df[[column]] >= lower_bound & df[[column]] <= upper_bound, ]
  return(df)
}

# Filter numeric columns
numeric_data <- combined_data %>% 
  select_if(is.numeric)

# Standardize the numeric columns
standardized_data <- as.data.frame(scale(numeric_data))

# Extract non-numeric columns
non_numeric_data <- combined_data[sapply(combined_data, is.numeric) == FALSE]
# Combine standardized numeric data with non-numeric data
combined_data_standardized <- bind_cols(non_numeric_data, standardized_data)

# Apply the function to the numeric columns with potential outliers
numeric_columns <- c("Average_Achievement_20", "Percent_Students_20", 
                     "Average_Achievement_10", "Percent_Students_10", 
                     "Average_Achievement_5", "Percent_Students_5", 
                     "Average_Achievement_0", "Percent_Students_0")

# Loop through each numeric column and remove outliers
for (column in numeric_columns) {
  combined_data <- remove_outliers(combined_data, column)
}

# Check the dataset after removing outliers
summary(combined_data)


# Histogram of Average Achievement (20 Years Experience)
ggplot(combined_data, aes(x = Average_Achievement_20)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Student's Average Achievement",
       x = "Student's Average Achievement",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))


# Scatter Plot of Teacher Experience vs. Average Achievement
ggplot(combined_data, aes(x = Average_Experience, y = Average_Achievement_20)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Teacher Experience vs. Student Achievement",
       x = "Average Experience in Years",
       y = "Student's Average Achievement") +
  theme(plot.title = element_text(hjust = 0.5)) 




##############################################################
#                                                             #
#                                                             #
#                 # Visualization                             #
#                                                             #
#                                                             #
##############################################################

# Box plot of Average Achievement by Subject
ggplot(combined_data, aes(x = Subject, y = Average_Achievement_20)) +
  geom_boxplot(fill="slateblue", alpha=0.2) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Students Average Achievement by Subject",
       x = "Subject",
       y = "Student's Average Achievement")


# Histogram of Average Achievement (20 Years Experience)
ggplot(combined_data, aes(x = Average_Achievement_20)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Student's Average Achievement",
       x = "Student's Average Achievement",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Scatter Plot of Teacher Experience vs. Average Achievement
ggplot(combined_data, aes(x = Average_Experience, y = Average_Achievement_20)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Teacher Experience vs. Student Achievement",
       x = "Teacher's Average Experience",
       y = "Student's Average Achievement") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Bar Plot of Average Achievement by Grade
ggplot(combined_data, aes(x = Grade, y = Average_Achievement_20, fill = Grade)) +
  geom_bar(stat = "identity") +
  labs(title = "Student's Average Achievement by Grade",
       x = "Grade",
       y = "Student's Average Achievement") +
  theme(plot.title = element_text(hjust = 0.5))  

# Line Plot of Percent Students Reaching Achievement Levels by Experience
ggplot(combined_data, aes(x = as.factor(Average_Experience))) +
  geom_line(aes(y = Percent_Students_20, color = "20 Years"), size = 1) +
  geom_line(aes(y = Percent_Students_10, color = "10 Years"), size = 1) +
  geom_line(aes(y = Percent_Students_5, color = "5 Years"), size = 1) +
  geom_line(aes(y = Percent_Students_0, color = "0 Years"), size = 1) +
  labs(title = "Percent of Students Reaching Achievement Levels by Teacher Experience",
       x = "Teacher Experience (Years)",
       y = "Percent of Students Reaching Achievement Level") +
  scale_color_manual(name = "Experience", values = c("red", "blue", "green", "purple")) +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Density Plot of Average Achievement by Subject
ggplot(combined_data, aes(x = Average_Achievement_20, fill = Subject)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Student's Average Achievement by Subject",
       x = "Student's Average Achievement",
       y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Violin Plot of Average Achievement by Grade
ggplot(combined_data, aes(x = Grade, y = Average_Achievement_20, fill = Grade)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of Student's Average Achievement by Grade",
       x = "Grade",
       y = "Student's Average Achievement") +
  theme(plot.title = element_text(hjust = 0.5))

# Facet Plot of Achievement by Grade and Subject
ggplot(combined_data, aes(x = Subject, y = Average_Achievement_20)) +
  geom_boxplot(fill="slateblue", alpha=0.2) +
  facet_wrap(~ Grade) +
  labs(title = "Student's Average Achievement by Subject and Grade",
       x = "Subject",
       y = "Student's Average Achievement") +
  theme(plot.title = element_text(hjust = 0.5)) 


##############################################################
#                                                             #
#                                                             #
#                 # Modelling                                 #
#                                                             #
#                                                             #
##############################################################

# fitting HLM model
hlm_model <- lmer(Average_Achievement_20 ~ Percent_Students_20 + Average_Experience + 
                    Average_Experience+
                    (1 | Region), data = combined_data_standardized)

hlm_model2 <- lmer(Average_Achievement_20 ~ Percent_Students_20 + Average_Experience + 
                     Subject + Grade + (1 | Region), data = combined_data_standardized)

hlm_model3 <- lmer(Average_Achievement_20 ~ Percent_Students_20 * Average_Experience + 
                     Subject + Grade + (1 | Region), data = combined_data_standardized)

hlm_model4 <- lmer(Average_Achievement_20 ~ Percent_Students_20 + Average_Experience + 
                     ME_E + ME_A_0 + ME_S_20 + (1 | Region), data = combined_data_standardized)

hlm_model5 <- lmer(Average_Achievement_20 ~ Percent_Students_20 + Average_Experience + 
                     (Percent_Students_20 | Region), data = combined_data_standardized)

hlm_model6 <- lmer(Average_Achievement_20 ~ Percent_Students_20 + Average_Experience + 
                     ME_E + ME_A_0 + ME_S_20 + ME_S_10 + ME_S_5 + (1 | Region), data = combined_data_standardized)


# Summarize the model
summary(hlm_model)
summary(hlm_model2)
summary(hlm_model3)
summary(hlm_model4)
summary(hlm_model5)
summary(hlm_model6)

# Visualize the random effects
ranef(hlm_model4)

# Diagnostics plot
plot(hlm_model4)

# Calculate AIC and BIC
aic_model1 <- AIC(hlm_model)
aic_model2 <- AIC(hlm_model2)
aic_model3 <- AIC(hlm_model3)
aic_model4 <- AIC(hlm_model4)
aic_model5 <- AIC(hlm_model5)
aic_model6 <- AIC(hlm_model6)

bic_model1 <- BIC(hlm_model)
bic_model2 <- BIC(hlm_model2)
bic_model3 <- BIC(hlm_model3)
bic_model4 <- BIC(hlm_model4)
bic_model5 <- BIC(hlm_model5)
bic_model6 <- BIC(hlm_model6)
# Combine the results into a data frame for better visualization
comparison <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
  AIC = c(aic_model1, aic_model2, aic_model3, aic_model4, aic_model5, aic_model6),
  BIC = c(bic_model1, bic_model2, bic_model3, bic_model4, bic_model5, bic_model6)
)

print(comparison)


# Generate predicted values (y-hat)
combined_data_standardized <- combined_data_standardized %>%
  mutate(fixed_prediction = predict(hlm_model4, re.form = NA)) 

# Create the ggplot
ggplot(data = combined_data_standardized, aes(x = Percent_Students_20, y = Average_Achievement_20, color = Region)) +
  geom_point() + 
  geom_line(aes(y = fixed_prediction)) +  
  labs(title = "Predicted vs Actual: Average Achievement by Percent of Students",
       x = "Percent of Students)",
       y = "Student's Average Achievement") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 

