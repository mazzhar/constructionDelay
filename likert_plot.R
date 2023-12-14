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

# Convert Likert items to factors with specified levels
survey[, 2:32] <- lapply(survey[, 2:32], function(x) factor(x, levels = levels))

# Create a Likert plot
likert_plot <- likert(survey[, 2:32])

# Plot the Likert plot
plot(likert_plot, wrap = 60)
