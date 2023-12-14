# Load the necessary libraries
library(tidyverse)
library(googlesheets4)
library(sjPlot)
library(likert)
library(dplyr)
library(lme4)        # for mixed-effects models
library(lmerTest)    # for p-values in mixed-effects models
library(knitr)
library(ltm)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(janitor) # Load the janitor package
library(ggthemes) # Load the ggthemes package
library(patchwork) # Load the patchwork package
library(psych)
library(knitr)

# Authenticate your Google account
gs4_auth()

# Read the Google sheet data in R
survey_data <- read_sheet("https://docs.google.com/spreadsheets/d/12HrHClwlinGBj-zpIPYBi14shBsk1Tk08RTvRtyKuk0/edit?usp=drive_link", sheet = "data", range = "A1:AG61")

# Rename columns and make them snake_case
survey_data <- survey_data %>%
  rename(profession = `What is your profession?`,
         work_experience = `Your years of work experience`
  ) %>%
  clean_names()

# Select columns starting with "q"
questions_columns <- grep("^q", names(survey_data), value = TRUE)
questions_data <- survey_data[, c("profession", questions_columns)]

# Define the levels for Likert items
levels <- c("Disagree", "Strongly Disagree", "Neutral", "Agree", "Strongly Agree")

# Convert the character columns to ordered factors
questions_data_factor <- questions_data %>%
  mutate(across(starts_with("q"), ~factor(.x, levels = levels, ordered = TRUE)))

# Convert the ordered factors to numeric
questions_data_numeric <- questions_data_factor %>%
  mutate(across(starts_with("q"), as.numeric))

# Exclude the 'profession' column
questions_data_numeric <- questions_data_numeric[,-1]

# Cronbach's alpha analysis
cronbachs_alpha_result <- psych::alpha(questions_data_numeric)
print(cronbachs_alpha_result$total)

# Print Cronbach's Alpha Result
kable(cronbachs_alpha_result$total, digits = 2, caption = "Cronbach's Alpha Result")

# Factor Analysis
factor_analysis_result <- stats::factanal(questions_data_numeric, factors = 3, rotation = "varimax")
print(factor_analysis_result)

# Extract Factor Analysis Loadings
loadings <- factor_analysis_result$loadings[, 1:3]

# Create a data frame for loadings
loadings_df <- as.data.frame(loadings)

# Add a factor variable to loadings_df
loadings_df$Factor <- rep(c("Factor1", "Factor2", "Factor3"), each = nrow(loadings_df))

# Print Factor Analysis Loadings
kable(loadings_df, digits = 2, caption = "Factor Analysis Loadings")

# Reshape data for ggplot
loadings_long <- reshape2::melt(loadings_df, id.vars = "Factor", variable.name = "Question", value.name = "Loading")

# Create bar charts for each factor
bar_plot <- ggplot(loadings_long, aes(x = Question, y = Loading, fill = Factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Factor, scales = "free_y") +
  labs(title = "Factor Analysis Loadings Bar Chart",
       x = "Questions",
       y = "Loadings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Print the bar chart
print(bar_plot)
