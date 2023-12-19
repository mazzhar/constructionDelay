#Author: Salman Mazhar
#date:  12-20-2023

library(here)       #Do not need to hardcode file paths
library(kableExtra) #For table
library(knitr)      #For table
library(ggplot2)    #For plots
library(viridis)    #Color pallete

# Create the file path using here(). No need to hardcode the full path.
csv_file_path <- here("data", "survey_data_cleaned.csv")

# Read the CSV file
survey_data <- read.csv(csv_file_path, check.names = FALSE) #check.names = FALSE removes all the dots in the column names by default.

# Summary statistics
summary_stats <- summary(survey_data$cleaned_experience)
sd <- sd(survey_data$cleaned_experience)
q1 <- quantile(survey_data$cleaned_experience, 0.25)
q3 <- quantile(survey_data$cleaned_experience, 0.75)

# Create a data frame with the summary statistics
summary_df <- data.frame(
  Statistic = c("Minimum", "1st Quartile (Q1)", "Median", "Mean", "3rd Quartile (Q3)", "Maximum", "Standard Deviation"),
  Value = c(
    format(min(survey_data$cleaned_experience), digits = 2),
    format(q1, digits = 2),
    format(median(survey_data$cleaned_experience), digits = 2),
    format(mean(survey_data$cleaned_experience), digits = 2),
    format(q3, digits = 2),
    format(max(survey_data$cleaned_experience), digits = 2),
    format(sd, digits = 2)
  )
)

# Print the beautiful table with custom formatting and a caption
kable(summary_df, format = "html", caption = "Summary statistics for work experience of the study participants") %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    position = "center"
  ) %>%
  column_spec(2, width = "2cm")  # Adjust column width as needed

#Work Experience Distribution by Profession
# Get the unique professions
unique_professions <- unique(survey_data$`What is your profession?`)
# Generate a Viridis palette with the required number of colors
viridis_palette <- viridis::viridis(length(unique_professions))
# Create a named vector for color mapping
color_mapping <- setNames(viridis_palette, unique_professions)


# Calculate the number of participants
n_participants <- nrow(survey_data)

# Create the histogram plot with default legend placement
simple_histogram_by_profession <- ggplot(survey_data, aes(x = cleaned_experience, fill = `What is your profession?`)) +
  geom_histogram(alpha = 0.7, binwidth = 1, position = "identity", color = "white") +
  labs(
    title = "Work Experience Distribution by Profession",
    subtitle = paste("Number of Participants, n =", n_participants),
    x = "Years of Work Experience",
    y = "Frequency"  # or "Density"
  ) +
  scale_fill_manual(values = color_mapping) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black")
  )

# Display the plot
print(simple_histogram_by_profession)



