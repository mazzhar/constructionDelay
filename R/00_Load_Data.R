# Author: Salman Mazhar
# Date: 19-12-2023

# This will load, clean, and export in csv for local df.This will avoid getting approval from google every time I want to run an analysis

library(tidyverse)
library(googlesheets4)
library(dplyr)

# Authenticate your Google account
gs4_auth()

# Read the Google sheet data in R
survey_data <- read_sheet("https://docs.google.com/spreadsheets/d/10aARA5ROB-L7Dr47OCH5ryZVkIOgh9H2Un78c_83UJo/edit?usp=sharing", sheet = "data", range = "A1:AG61")

# Clean column names
colnames(survey_data)[4:ncol(survey_data)] <- paste0("Q", str_pad(1:(ncol(survey_data)-3), 2, "left", "0"), ": ", colnames(survey_data)[4:ncol(survey_data)]) %>%
  stringr::str_replace_all("\\.", " ") %>%
  stringr::str_squish() %>%
  stringr::str_replace_all("$", "?")

# Drop some columns
survey_data_cleaned <- survey_data %>%
  select(-c(Timestamp))

# Function to convert different formats to years
convert_to_years <- function(duration) {
  # Convert to lowercase for case-insensitive matching
  duration <- tolower(duration)
  
  if (grepl("year", duration)) {
    # Extract numeric part and convert to numeric, rounded to 2 decimal places
    return(round(as.numeric(gsub("[^0-9.]", "", duration)), 1))
  } else if (grepl("month", duration)) {
    # Extract numeric part, convert to numeric, divide by 12, and round to 2 decimal places
    return(round(as.numeric(gsub("[^0-9.]", "", duration))/12, 1))
  } else if (grepl("currently working", duration)) {
    return(0.5)  # Assuming 6 months for "currently working"
  } else if (grepl("0 to 1", duration)) {
    return(0.5)  # Assuming 6 months for "0 to 1"
  } else if (grepl("yrs", duration)) {
    # Extract numeric part and convert to numeric, rounded to 2 decimal places
    return(round(as.numeric(gsub("[^0-9.]", "", duration)), 1))
  } else if (!is.na(as.numeric(duration))) {
    # If it's a numeric value, assume it's in years, rounded to 2 decimal places
    return(round(as.numeric(duration), 1))
  } else {
    return(NA)
  }
}

# Apply the function to the "Your years of work experience" column
survey_data_cleaned$cleaned_experience <- sapply(survey_data_cleaned$"Your years of work experience", convert_to_years)
#Drop the messed up work experience column
survey_data_cleaned <- survey_data_cleaned %>%
  select(-c("Your years of work experience"))

# Move the cleaned_experience column to the second position
survey_data_cleaned <- survey_data_cleaned %>%
  select(1, cleaned_experience, everything())


# Map the original professions to simplified names
profession_mapping <- c(
  "Consultant/Consultants Engineer" = "Consultant/Engineer",
  "Implementing Agency/Developing Authority" = "Implementing Agency/Developer",
  "Contractor/Contractor's Engineer/Quality Control Engineer" = "Contractor/Quality Control Engineer",
  "Academician/Researcher" = "Academician/Researcher"
)

# Apply the mapping to the "What is your profession" column
survey_data_cleaned <- survey_data_cleaned %>%
  mutate(`What is your profession?` = case_when(
    grepl("Consultant/Consultants Engineer", `What is your profession?`) ~ "Consultant",
    grepl("Implementing Agency/Developing Authority", `What is your profession?`) ~ "Implementing Agency/Developer",
    grepl("Contractor/Contractor's Engineer|Quality Control Engineer", `What is your profession?`) ~ "Contractor/Quality Control Engineer",
    grepl("Academician/Researcher", `What is your profession?`) ~ "Academician/Researcher",
    TRUE ~ `What is your profession?`
  ))


# Export the original and cleaned data to a CSV file in the "Data" folder with the current date in the file name
write.csv(survey_data_cleaned, "Data/survey_data_cleaned.csv", row.names = FALSE)




# Get the current date in ISO 8601 format
#current_date <- format(Sys.Date(), "%Y-%m-%d")

# Export the original and cleaned data to a CSV file in the "Data" folder with the current date in the file name
write.csv(survey_data_cleaned, paste0("Data/survey_data_cleaned.csv"), row.names = FALSE)

