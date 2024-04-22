# Data Pre-processing


# 2a) Load Dataset
whr_df = read.csv("WHR2023.csv")
head(whr_df)

# 2b) Get dimensions of dataset
dim(whr_df)

# 2c) get datatype of each column
datatypes <- sapply(whr_df, class)
datatypes

# 2d) Handle outliers
# function to identify outliers
identify_outliers <- function(x, y) {
  # calculate z score for each value
  # scale() function can also be used as alternative
  z_scores <- abs((x - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE))
  outliers <- x[z_scores > 3]
  return(outliers)
}

# Excluding country names
col_check = names(whr_df) != "Country.name"
# Checking for outliers
outliers <- mapply(identify_outliers, whr_df[col_check], whr_df[col_check])
print(outliers)

# function to replace outliers with median
handle_outliers <- function(df){
  for (col in names(df)){
    outliers <- identify_outliers(df[[col]], df[[col]])
    if (length(outliers) > 0){
      # Replace outliers with median
      df[[col]][df[[col]] %in% outliers] <- median(df[[col]], na.rm = TRUE)
    }
  }
  return(df)
}

# Replacing outliers with median value
filtered_df <- handle_outliers(whr_df[col_check])
# Checking for outliers after replacement
filtered_df["Country.name"] <- whr_df["Country.name"]
new_outliers <- mapply(identify_outliers, 
                       filtered_df[names(filtered_df) != "Country.name"], whr_df[col_check])
print(new_outliers)

# 2e) Impute missing data
# Checking for missing data
colSums(is.na(filtered_df))

# 3a) Generosity vs Ladder point graph
# Import visualization package
library(ggplot2)
# Visualize the Generosity vs Ladder point graph
gen_vs_lad <- ggplot(filtered_df, aes(x=Generosity, y=Ladder.score))+
  geom_point() + ylab("Ladder Score") + 
  ggtitle("Generosity vs Ladder Score Point Plot")
gen_vs_lad

# Checking correlation between Generosity and Ladder Score
cor(filtered_df$Generosity, filtered_df$Ladder.score)

# 3b) Social Support vs Healthy Life Expectancy Bar Graph

library(tidyr) # To format data
library(dplyr) # For data manipulation
# Excluding the 2 variables to be analyzed from a list
bar_df <- select(filtered_df, "Social.support", 
                 "Healthy.life.expectancy", "Country.name")

# Split dataset for better representation
# Set the seed for reproducibility (optional)
set.seed(43)
# Split dataset
sample <- sample(c(TRUE, FALSE), nrow(bar_df), replace = TRUE, prob=c(0.5,0.5))
bar_df1 <- bar_df[sample,]
bar_df2 <- bar_df[!sample,]
bar_df1
bar_df2


# Formatting data
data_long1 <- gather(bar_df1, key = "Variable", value = "Value", -Country.name)
data_long2 <- gather(bar_df2, key = "Variable", value = "Value", -Country.name)
# Visualize Social Support vs Healthy Life Expectancy bar graph
soc_vs_life.exp1 <- ggplot(data_long1, 
                      aes(x = Country.name, y = Value, fill = Variable)) +
                      geom_bar(stat = "identity", position = "dodge") +
                      xlab("Countries") +
                      ylab("Score") +
                      ggtitle("Bar Graph 1: Social Support vs Healthy Life Expectancy") +
                          theme(axis.text.x = element_text(angle = 70, vjust = 0.4))
soc_vs_life.exp2 <- ggplot(data_long2, 
                      aes(x = Country.name, y = Value, fill = Variable)) +
                      geom_bar(stat = "identity", position = "dodge") +
                      xlab("Countries") +
                      ylab("Score") +
                      ggtitle("Bar Graph 2: Social Support vs Healthy Life Expectancy") +
                      theme(axis.text.x = element_text(angle = 70, vjust = 0.4))
soc_vs_life.exp1
soc_vs_life.exp2
# Checking correlation between Social Support and Healthy life expectancy
cor(filtered_df$Social.support, filtered_df$Healthy.life.expectancy)

# Question 4
# Extract  columns to find correlation with happiness/ladder score
factors_col = c("Logged.GDP.per.capita", "Social.support",
                "Healthy.life.expectancy", 
                "Freedom.to.make.life.choices",
                "Generosity", "Perceptions.of.corruption")
# Labels of columns
factors_label = c("Economic Production",
                  "Social Support",
                  "Healthy Life Expectancy",
                  "Freedom", "Absence of Corruption",
                  "Generosity")
# Set the iteration range
range <- 1:length(factors_col)
range
# Calculate the correlation score between happiness score and each 6 factor
for (i in range){
  cat("Happiness vs", factors_label[i], "correlation score: " 
        , cor(filtered_df$Ladder.score, filtered_df[[factors_col[i]]]), "\n")
}

# 5) analyzing Malaysia's metrics
malaysia_row <- filtered_df[filtered_df$Country.name == "Malaysia",]
cat("Highest economic production score:", max(filtered_df$Logged.GDP.per.capita))
economic_prod <- select(filtered_df, "Country.name", "Logged.GDP.per.capita", "Ladder.score")
economic_prod
factors_df <- select(filtered_df, "Logged.GDP.per.capita", "Social.support",
                     "Healthy.life.expectancy", 
                     "Freedom.to.make.life.choices",
                     "Generosity", "Perceptions.of.corruption",
                     "Ladder.score")
best_country <- filtered_df[filtered_df$Ladder.score == max(filtered_df$Ladder.score),]
best_country
