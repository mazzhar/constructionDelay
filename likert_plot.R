library(tidyverse)
library(googlesheets4)
library(likert)
library(dplyr)
library(knitr)
library(ggplot2)
library(janitor) # Load the janitor package
library(cowplot) # Load the cowplot package

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
names(survey_df) <- gsub("\\.", " ", names(survey_df))

# Create a Likert plot
likert_plot <- likert(survey_df)

# Set the size of the output image
width <- 15  # in inches, adjust as needed
height <- 10  # in inches

# Set the resolution of the output image
dpi <- 600  # dots per inch

# Set the size of the text in the plot
base_size <- 10

# Create the plot
p <- plot(likert_plot, wrap = 60) +
  theme(plot.title = element_text(size = base_size * 1.5, color = "black"),
        axis.title = element_text(size = base_size, color = "black"),
        axis.text = element_text(color = "black", size = base_size),
        plot.margin = margin(5, 5, 5, 5, "mm"), # Adjust as needed
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) # Adjust linewidth as needed

# Save the plot to a file with the specified size and resolution
ggsave("likert_plot.png", plot = p, width = width, height = height, dpi = dpi)

# Display the plot
plot_grid(p, labels = "AUTO", ncol = 1)
