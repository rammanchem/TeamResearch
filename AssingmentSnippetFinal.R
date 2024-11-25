# 1. Import the data
data <- read.csv("E:TeamResearch/Food_Costs.csv")

# 2. Convert the `Cumulative.Cost` column to numeric, handling non-numeric characters (e.g., commas, dollar signs)
data$Cumulative.Cost <- as.numeric(gsub("[\\$,]", "", data$Cumulative.Cost))

# 3. Check for missing values and remove them (we can remove rows with any NA)
data_cleaned <- na.omit(data)

# 4. Ensure the `Cumulative.Cost` is numeric and there are no NAs
summary(data_cleaned$Cumulative.Cost)

# 5. Optional: If the data spans large ranges, you can apply a transformation (like scaling to millions) for readability
data_cleaned$Cumulative_Cost_Millions <- data_cleaned$Cumulative.Cost / 1e6


colnames(data_cleaned)

# 6. Load necessary libraries
library(ggplot2)
library(scales)

# Create a box plot for 'Cumulative.Cost' without adding a new column
ggplot(data_cleaned, aes(x = "", y = Cumulative.Cost / 1e6)) +  # Scale here directly in ggplot
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(title = "Boxplot of Cumulative Cost (Millions)", 
       y = "Cumulative Cost (Millions)", 
       x = "") +
  theme_minimal()

ggplot(data_cleaned, aes(x = Cumulative.Cost / 1e6)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Cumulative Costs (Millions)", x = "Cumulative Cost (Millions)", y = "Frequency")

# Select the top 5 and bottom 5 entities based on Cumulative.Cost
top_5 <- tail(data, 5)
bottom_5 <- head(data, 5)

# Perform Mann-Whitney U Test (Wilcoxon rank-sum test) directly on subsets
test_result <- wilcox.test(top_5$Cumulative.Cost, bottom_5$Cumulative.Cost)

# View the results
print(test_result)


