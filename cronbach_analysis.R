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

# Now you can compute Cronbach's alpha
cronbachs_alpha_result <- psych::alpha(questions_data_numeric)
print(cronbachs_alpha_result)


# Load the necessary packages
library(psych)

# Perform Cronbach's alpha analysis
cronbachs_alpha_result <- psych::alpha(questions_data_numeric)

# Print the result in a nice table format for Markdown
kable(cronbachs_alpha_result$total, digits = 2, caption = "Cronbach's Alpha Result")




