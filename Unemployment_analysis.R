# Load necessary libraries
library(dplyr)
library(ggplot2)
library(plotly)

# Import data
data <- read.csv("/Users/elisalassarre/Desktop/unemployment analysis.csv")

# Remove missing values
data <- na.omit(data)

# Display unique country names
unique_countries <- unique(data$Country.Name)

# Descriptive analysis function
descriptive_analysis <- function(df) {
  summary_df <- summary(df)
  print(summary_df)
  return(summary_df)
}
descriptive_analysis(data)

# Filter data for France
filtered_data <- filter(data, Country.Name == "France")
print(filtered_data)

# Add a column for the average unemployment rate
data$Average_Unemployment <- apply(data[, 2:ncol(data)], 1, function(row) mean(as.numeric(row), na.rm = TRUE))

# Classify based on the average unemployment rate
data$Class <- ifelse(data$Average_Unemployment < 5, "Low", 
                     ifelse(data$Average_Unemployment < 10, "Medium", "High"))

# Calculate the overall average unemployment rate
overall_average <- mean(data$Average_Unemployment, na.rm = TRUE)

# Add a column for relative rate
data <- data %>%
  mutate(Relative_Rate = Average_Unemployment / overall_average)

# Summarize the Average Unemployment column
summary_data <- data %>%
  summarize(
    Mean_Average_Unemployment = mean(Average_Unemployment, na.rm = TRUE),
    Median_Average_Unemployment = median(Average_Unemployment, na.rm = TRUE),
    Max_Average_Unemployment = max(Average_Unemployment, na.rm = TRUE),
    Min_Average_Unemployment = min(Average_Unemployment, na.rm = TRUE)
  )
print(summary_data)

# Calculate the threshold for high unemployment rates (95th percentile)
threshold <- quantile(data$X2021, 0.95, na.rm = TRUE)

# Add a column to indicate countries exceeding the threshold
data <- data %>%
  mutate(label = ifelse(X2021 > threshold, as.character(Country.Name), ""))

# Create and customize the graph with ggplot2
ggplot(data, aes(x = reorder(Country.Name, X2021), y = X2021)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = label), vjust = -0.5, size = 3) +
  labs(x = NULL, y = "Unemployment Rate in 2021", title = "Unemployment Rate by Country in 2021") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

# Create the interactive plot with plotly
fig <- plot_ly(data, x = ~Country.Name, y = ~Average_Unemployment, type = 'bar',
               text = ~round(Average_Unemployment, 2), textposition = 'auto') %>%
  layout(title = "Average Unemployment Rates by Country (1991-2021)",
         xaxis = list(title = "Country"),
         yaxis = list(title = "Average Unemployment Rate"))

# Display the interactive plot
fig

