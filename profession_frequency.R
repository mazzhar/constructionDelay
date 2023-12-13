# Install and load the necessary packages
install.packages(c("tidyverse", "googlesheets4", "sjPlot"))
library(tidyverse)
library(googlesheets4)
library(sjPlot)
library(likert)
library(RColorBrewer) # Gnuplot Earth palette with repetition
library(knitr)
library(kableExtra)

# Authenticate your Google account
gs4_auth()

# Read the Google sheet data in R
profession_data <- read_sheet("https://docs.google.com/spreadsheets/d/12HrHClwlinGBj-zpIPYBi14shBsk1Tk08RTvRtyKuk0/edit?usp=drive_link", sheet = "data", range = "A1:AG61")


# Summary statistics
summary_stats <- summary(profession_data$`Your years of work experience`)
sd <- sd(profession_data$`Your years of work experience`)
q1 <- quantile(profession_data$`Your years of work experience`, 0.25)
q3 <- quantile(profession_data$`Your years of work experience`, 0.75)

# Create a data frame with the summary statistics
summary_df <- data.frame(
  Statistic = c("Minimum", "1st Quartile (Q1)", "Median", "Mean", "3rd Quartile (Q3)", "Maximum", "Standard Deviation"),
  Value = c(
    format(min(profession_data$`Your years of work experience`), digits = 2),
    format(q1, digits = 2),
    format(median(profession_data$`Your years of work experience`), digits = 2),
    format(mean(profession_data$`Your years of work experience`), digits = 2),
    format(q3, digits = 2),
    format(max(profession_data$`Your years of work experience`), digits = 2),
    format(sd, digits = 2)
  )
)

# Print the beautiful table with custom formatting
kable(summary_df, format = "html") %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    position = "center"
  ) %>%
  column_spec(2, width = "2cm")  # Adjust column width as needed

# Violin Plot with Subtitle and Enhancements
n_participants <- nrow(profession_data)  # Number of participants

violin_plot <- ggplot(profession_data, aes(x = `What is your profession?`, y = `Your years of work experience`)) +
  geom_violin(fill = "skyblue", color = "black", alpha = 0.7) +
  geom_jitter(color = "gray", alpha = 0.5, size = 0.7) +  # Add jitter points for better visibility
  labs(title = "Work Experience Distribution by Profession",
       subtitle = paste("Number of Participants, n =", n_participants),
       x = "Profession",
       y = "Years of Work Experience") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


#Work Experience Distribution by Profession
# Get the unique professions
unique_professions <- unique(profession_data$`What is your profession?`)
# Generate a Set3 palette with the required number of colors
set3_palette <- brewer.pal(n = length(unique_professions), name = "Set3")
# Create a named vector for color mapping
color_mapping <- setNames(set3_palette, unique_professions)

simple_histogram_by_profession <- ggplot(profession_data, aes(x = `Your years of work experience`, fill = `What is your profession?`)) +
  geom_histogram(alpha = 0.7, bins = 15, position = "identity", color = "white") +
  labs(title = "Work Experience Distribution by Profession",
       subtitle = paste("Number of Participants, n =", n_participants),
       x = "Years of Work Experience",
       y = "Count") +
  scale_fill_manual(values = color_mapping) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(1, "cm"),
        legend.title = element_text(size = 10, face = "bold"),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12))


# Display the plots
print(simple_histogram_by_profession)
print(violin_plot)















# #Kernel Density Plot
# density_plot <-ggplot(profession_data, aes(x = `Your years of work experience`)) +
#   geom_density(fill = "skyblue", alpha = 0.7) +
#   labs(title = "Kernel Density Plot of Work Experience",
#        x = "Years of Work Experience",
#        y = "Density") +
#   theme_minimal()
# print(density_plot)



# # Smooth Histogram Plot with Enhancements, Subtitle, and Median
# n_participants <- nrow(profession_data)  # Number of participants
# median_value <- median(profession_data$`Your years of work experience`)  # Median value
# mean_value <- mean(profession_data$`Your years of work experience`)  # Mean value
# 
# # Set a color palette for improved visibility
# color_palette <- c("#4C72B0", "#55A868", "#C44E52", "#8172B2", "#CCB974", "#64B5CD")
# 
# enhanced_histogram_plot <- ggplot(profession_data, aes(x = `Your years of work experience`)) +
#   geom_histogram(fill = color_palette[1], color = "white", alpha = 0.7, bins = 15) +  # Adjust color and border
#   geom_vline(xintercept = c(mean_value, median_value),
#              linetype = c("dotted", "solid"),
#              color = c(color_palette[3], color_palette[4]),  # Improved line colors
#              size = c(1, 1.5)) +  # Add mean and median lines
#   labs(title = "Smooth Histogram of Work Experience",
#        subtitle = paste("Number of Participants, n =", n_participants),
#        x = "Years of Work Experience",
#        y = "Density") +
#   annotate("text", x = mean_value, y = 0.025,
#            label = paste("Mean: ", round(mean_value, 2)),
#            color = color_palette[3], size = 3, angle = 90, vjust = 1) +  # Annotate mean value
#   annotate("text", x = median_value, y = 0.02,
#            label = paste("Median: ", round(median_value, 2)),
#            color = color_palette[4], size = 3, angle = 90, vjust = 1) +  # Annotate median value
#   theme_bw()
# 
# # Display the enhanced plot with subtitle, mean, and median
# print(enhanced_histogram_plot)

# # Distribution plot
# hist_plot <- ggplot(profession_data, aes(x = `Your years of work experience`)) +
#   geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
#   labs(title = "Distribution of Work Experience",
#        x = "Years of Work Experience",
#        y = "Count") +
#   theme_minimal() +
#   theme(panel.grid.major = element_line(colour = "gray", size = 0.5))


# # Box plot
# box_plot <- ggplot(profession_data, aes(x = `What is your profession?`, y = `Your years of work experience`)) +
#   geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7, outlier.shape = NA) +
#   geom_jitter(size = 0.5, color = "gray", alpha = 0.5) +
#   labs(title = "Work Experience by Profession",
#        x = "Profession",
#        y = "Years of Work Experience") +
#   theme_minimal()


# # Create a horizontal bar plot with data labels
# bar_plot <- ggplot(profession_data, aes(x = after_stat(count), y = fct_reorder(`What is your profession?`, -after_stat(count)))) +
#   geom_bar(stat = "count", fill = "skyblue", color = "black", alpha = 0.7) +
#   geom_text(aes(label = paste0(after_stat(count), " (", round(after_stat(prop)*100, 1), "%)")), hjust = -0.2, color = "darkred") +
#   labs(title = "Count and Percentage of Participants by Profession",
#        x = "Count",
#        y = "Profession") +
#   theme_minimal()


