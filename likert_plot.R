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

gs4_auth()

survey_data <- read_sheet("https://docs.google.com/spreadsheets/d/12HrHClwlinGBj-zpIPYBi14shBsk1Tk08RTvRtyKuk0/edit?usp=drive_link", sheet = "data", range = "A1:AG61")

survey_data <- survey_data %>%
  rename(profession = `What is your profession?`,
         work_experience = `Your years of work experience`
  )

questions_columns <- grep("^q", names(survey_data), value = TRUE)
questions_data <- survey_data[, c("profession", questions_columns)]

levels <- c("Disagree", "Strongly Disagree", "Neutral", "Agree", "Strongly Agree")

survey<- lapply(survey_data[, 3:32], function(x) factor(x, levels = levels))

# Convert the list to a data frame
survey_df <- as.data.frame(survey)

# Create a Likert plot
likert_plot <- likert(survey_df)

# Plot the Likert plot
plot(likert_plot, wrap = 60) +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 16))
