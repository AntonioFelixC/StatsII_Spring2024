install.packages("haven")
library(haven)


data <- read_dta("C:/Users/Antonio Felix/Dropbox/My PC (SHAW-72)/Downloads/AllDataMerged_15May2023_weighted.dta")
summary(data)
str(data)
head(data)
hist(data$variable)
View(data)
# Display variable names in the dataset
names(data)


# Load the dplyr package
library(dplyr)
# Group data by month and calculate the mean idle index for each month
monthly_mean_idle_index <- data %>%
  group_by(month) %>%
  summarise(monthly_mean_idle_index = mean(IDLE_index, na.rm = TRUE))



#Replication Table 1
# Load required libraries
library(knitr)

# Create a data frame with the relevant values
table_data <- data.frame(
  Dataset = rep(c("SCAD", "ACLED", "UCDP-GED"), each = 6),
  Estimate = c(0.0032, 0.0032, 0.0032, 0.0035, 0.0028, 0.0029,
               0.0083, 0.0083, 0.0083, 0.0101, 0.0081, 0.0101,
               0.0035, 0.0035, 0.0035, 0.0037, 0.0032, 0.0037),
  SE = c(0.0009, 0.0008, 0.0008, 0.0008, 0.0009, 0.0008,
         0.0021, 0.0018, 0.0018, 0.0018, 0.0021, 0.0018,
         0.0014, 0.0011, 0.0011, 0.0012, 0.0013, 0.0012),
  Perc_Change = c(20.8, 20.8, 20.8, 22.9, 18.6, 18.8,
                  9.9, 9.9, 9.9, 12.1, 9.6, 12.1,
                  8.3, 8.3, 8.3, 8.7, 7.5, 8.7),
  Observations = c(242928, 242928, 242928, 242928, 241248, 242928,
                   182196, 182196, 182196, 182196, 182196, 182196,
                   242928, 242928, 242928, 242928, 241248, 242928),
  R2 = c(0.08, 0.33, 0.33, 0.33, 0.33, 0.34,
         0.22, 0.47, 0.47, 0.47, 0.47, 0.47,
         0.17, 0.45, 0.45, 0.45, 0.45, 0.46)
)

# Print the table using kable
kable(table_data, format = "markdown",
      col.names = c("Dataset", "Estimate", "SE", "Perc Change", "Observations", "R2"))


#Table 2
# Create a data frame with the relevant values for Table 2
table2_data <- data.frame(
  Estimate = c(0.0037, 0.0037, 0.0037, 0.0043, 0.0038, 0.0035),
  SE = c(0.0013, 0.0012, 0.0012, 0.0012, 0.0013, 0.0012),
  Perc_Change = c(17.8, 17.8, 17.8, 20.9, 18.3, 17.1),
  Observations = c(147492, 147492, 147492, 147492, 146472, 147492),
  R2 = c(0.12, 0.34, 0.34, 0.34, 0.34, 0.35)
)

# Print the table using kable
kable(table2_data, format = "markdown",
      col.names = c("Estimate", "SE", "Perc Change", "Observations", "R2"))

#Figure 1

install.packages("cowplot")

# Load necessary libraries
library(ggplot2)
library(cowplot)

# Create data for the distribution of idle index (assuming it's stored in a variable named 'idle_index')
# Create data for the mean of idle index for each month across Africa (assuming it's stored in a variable named 'monthly_mean_idle_index')

# Check for missing or non-numeric values in monthly_mean_idle_index
summary(monthly_mean_idle_index)

# Remove rows with missing or non-numeric values
monthly_mean_idle_index <- monthly_mean_idle_index[complete.cases(monthly_mean_idle_index), ]

# Convert monthly_mean_idle_index to numeric
monthly_mean_idle_index$monthly_mean_idle_index <- as.numeric(monthly_mean_idle_index$monthly_mean_idle_index)

# Convert month to a factor
monthly_mean_idle_index$month <- factor(monthly_mean_idle_index$month, levels = month.abb)

# Plotting Figure 1 again
# Distribution of idle index (left panel)
histogram <- ggplot(data = data, aes(x = IDLE_index)) +
  geom_histogram(binwidth = 0.005, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Distribution of Idle Index",
       x = "Idle Index",
       y = "Frequency") +
  theme_minimal()

# Mean of idle index for each month across Africa (right panel)
lineplot <- ggplot(data = monthly_mean_idle_index, aes(x = month, y = monthly_mean_idle_index)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Mean Idle Index for Each Month",
       x = "Month",
       y = "Mean Idle Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Combine both plots
combined_plot <- cowplot::plot_grid(histogram, lineplot, labels = "AUTO", nrow = 1)






# Check the structure of monthly_mean_idle_index
str(monthly_mean_idle_index)

# Print the first few rows of monthly_mean_idle_index to understand its structure
head(monthly_mean_idle_index)

# Check for any missing or non-finite values
summary(monthly_mean_idle_index)


# Convert month to a factor with ordered levels
monthly_mean_idle_index$month <- factor(monthly_mean_idle_index$month, levels = month.abb, ordered = TRUE)



# Remove rows with missing values in IDLE_index
data <- data[complete.cases(data$IDLE_index), ]

# Check for non-numeric values in IDLE_index
non_numeric <- data[!is.numeric(data$IDLE_index), "IDLE_index"]
if (length(non_numeric) > 0) {
  print("Non-numeric values found in IDLE_index:")
  print(non_numeric)
} else {
  print("No non-numeric values found in IDLE_index.")
}

# Plotting Figure 1 again
# Distribution of idle index (left panel)
histogram <- ggplot(data = data, aes(x = IDLE_index)) +
  geom_histogram(binwidth = 0.005, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Distribution of Idle Index",
       x = "Idle Index",
       y = "Frequency") +
  theme_minimal()

# Mean of idle index for each month across Africa (right panel)
lineplot <- ggplot(data = monthly_mean_idle_index, aes(x = month, y = monthly_mean_idle_index)) +
  geom_point(position = position_dodge(width = 0.5), color = "blue", size = 3) +
  labs(title = "Mean Idle Index for Each Month",
       x = "Month",
       y = "Mean Idle Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Combine both plots
combined_plot <- cowplot::plot_grid(histogram, lineplot, labels = "AUTO", nrow = 1)




# Plotting Figure 1 again
# Distribution of idle index (left panel)
histogram <- ggplot(data = data, aes(x = IDLE_index)) +
  geom_histogram(binwidth = 0.005, fill = "red", color = "red", alpha = 0.8) +  # Set both fill and color to "red"
  labs(title = "Distribution of Idle Index",
       x = "Idle Index",
       y = "Frequency") +
  theme_minimal()

# Mean of idle index for each month across Africa (right panel)
lineplot <- ggplot(data = monthly_mean_idle_index, aes(x = month, y = monthly_mean_idle_index)) +
  geom_point(position = position_dodge(width = 0.5), color = "blue", size = 3) +
  labs(title = "Mean Idle Index for Each Month",
       x = "Month",
       y = "Mean Idle Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Combine both plots
combined_plot <- cowplot::plot_grid(histogram, lineplot, labels = "AUTO", nrow = 1)

# Display the combined plot
print(combined_plot)





# Load required libraries
library(ggplot2)
library(cowplot)

# Display the first few rows of data
head(data)

# Display the first few rows of monthly_mean_idle_index
head(monthly_mean_idle_index)


# Plotting Figure 1 again
# Distribution of idle index (left panel)
histogram <- ggplot(data = data, aes(x = IDLE_index)) +
  geom_histogram(binwidth = 0.005, fill = "red", color = "red", alpha = 0.8) +  # Set both fill and color to "red"
  labs(title = "Distribution of Idle Index",
       x = "Idle Index",
       y = "Frequency") +
  theme_minimal()

# Mean of idle index for each month across Africa (right panel)
lineplot <- ggplot(data = monthly_mean_idle_index, aes(x = month, y = monthly_mean_idle_index)) +
  geom_line(color = "blue", size = 1) +  # Use geom_line() for a line plot
  labs(title = "Mean Idle Index for Each Month",
       x = "Month",
       y = "Mean Idle Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Combine both plots
combined_plot <- cowplot::plot_grid(histogram, lineplot, labels = "AUTO", nrow = 1)

# Display the combined plot
print(combined_plot)





# Define the dependent variables
Dependent_Variables <- c("violent_conflict_SCAD", "onset_conflict_ACLED", "state_nonstate_conflict_UCDP")

# Define the predictor variables
Predictor_Variables <- c("Year", "Other_Predictors")

# Specify the dataset containing the relevant variables
dataset <- data

# Loop through each dependent variable and create separate regression models
for (outcome_var in Dependent_Variables) {
  # Build the regression model
  model <- lm(as.formula(paste(outcome_var, "~", paste(Predictor_Variables, collapse = " + "))), data = dataset)
  
  # Plot coefficients with 95% confidence intervals
  plot <- ggplot(model, aes(x = Year, y = Coef., ymin = Lower_CI, ymax = Upper_CI, color = Outcome_Variable)) +
    geom_point() +
    geom_errorbar(width = 0.2) +
    geom_line() +
    labs(title = paste("Effect of Year Constraint on", outcome_var),
         x = "Year Constraint",
         y = "Coefficient") +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Save or print the plot
  print(plot)
}





#REPLICATION

#Rename variables
library(dplyr)
data <- data %>%
  mutate(idle_index = IDLE_index,
         ym = date_month)

#Set Panel Structure
install.packages("plm")
library(plm)
pdata <- pdata.frame(data, index = c("objectid", "ym"))

# Create new variable
pdata$SCADantigov <- ifelse(pdata$n_etype8 > 0 | pdata$n_etype9 > 0, 1, 0)

# Replace missing values with NA
pdata$SCADantigov[is.na(pdata$n_etype8)] <- NA

#Create new variables
#pdata$SCADantigov <- ifelse(pdata$n_etype8 > 0 | pdata$n_etype9 > 0, 1, 0)
#pdata$SCADantigov[pdata$n_etype8 == .] <- NA

pdata$py_SCADantigov <- pdata$SCADantigov
pdata$py_SCADantigov <- pdata$SCADantigov^2
pdata$py_SCADantigov <- pdata$SCADantigov^3


# Load necessary packages
library(ggplot2)
library(dplyr)

# Generate sample indicator variable
data <- data %>%
  mutate(sample = ifelse(e.sample == 1, 1, 0))

# Histogram of idle index
histogram <- ggplot(data = data[data$sample == 1, ], aes(x = idle_index)) +
  geom_histogram(binwidth = 20, fill = "green", alpha = 0.6) +
  labs(title = "Distribution of Idle Index",
       x = "Idle Index",
       y = "% of Observations") +
  theme_minimal()

# Table of mean idle index by month
mean_idle_by_month <- data %>%
  filter(sample == 1) %>%
  group_by(mon) %>%
  summarise(mean_idle_index = mean(idle_index))

# Bar plot of mean idle index by month
barplot <- ggplot(mean_idle_by_month, aes(x = factor(mon), y = mean_idle_index)) +
  geom_bar(stat = "identity", fill = "navy") +
  labs(title = "Mean Idle Index by Month",
       x = "Month",
       y = "Mean Idle Index") +
  theme_minimal()

# Scatter plot of cultivated land versus idle index
scatterplot <- ggplot(data[data$sample == 1, ], aes(x = cultivated, y = idle_index)) +
  geom_point(color = "red", alpha = 0.3) +
  labs(title = "Idle Index vs. Cultivated Land",
       x = "% of cultivated land",
       y = "Idle Index") +
  theme_minimal()

# Combine plots
combined_plot <- gridExtra::grid.arrange(histogram, barplot, scatterplot,
                                         nrow = 3, heights = c(1, 1, 1))

# Save combined plot
ggsave("FigTbl/Fig1_Idlediag.pdf", combined_plot)




# Load necessary packages
library(ggplot2)
library(dplyr)

# Generate sample indicator variable
data <- data %>%
  mutate(sample = ifelse(sample == 1, 1, 0))





library(ggplot2)
library(dplyr)

# Filter data for the sample
sample_data <- data %>% filter(sample == 1)

# Create histogram
histogram <- ggplot(sample_data, aes(x = idle_index)) +
  geom_histogram(binwidth = 20, fill = "green", color = "black") +
  labs(title = "Distribution of Idle Index",
       x = "Idle Index",
       y = "% of Observations") +
  theme_minimal()

# Create bar plot
barplot <- ggplot(sample_data, aes(x = factor(month), y = idle_index)) +
  geom_bar(stat = "summary", fun = "mean", fill = "navy") +
  labs(title = "Mean by Month",
       x = "Month",
       y = "Mean Idle Index") +
  theme_minimal()

# Combine plots
combined_plot <- plot_grid(histogram, barplot, nrow = 2)

# Export the combined plot as a PDF
ggsave("FigTbl/Fig1_Idlediag.pdf", combined_plot, width = 10, height = 10, units = "in")



str(data)
table(data$sample)

head(data)

# Load the dplyr package
library(dplyr)

#Rename a few variables 
data <- data %>%
  rename(idle_index = IDLE_index,
         ym = date_month)

# Summary statistics for idle_index and ym
summary(data$idle_index)
summary(data$ym)

# Histogram of idle_index
hist(data$idle_index, main = "Distribution of idle_index")

# Density plot of ym
plot(density(data$ym), main = "Density Plot of ym")

# Scatterplot of idle_index against ym
plot(data$ym, data$idle_index, main = "Scatterplot of ym vs. idle_index", xlab = "ym", ylab = "idle_index")

# Print the first few rows of idle_index
head(data$idle_index)

# Print the first few rows of ym
head(data$ym)
