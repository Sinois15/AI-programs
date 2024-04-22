# Importing Dataset
survey_df <- read.csv("Survey_AI.csv")
head(survey_df) # Checking correct importation

# 1b) label encode dataset
label_encode <- function(col){
  data <- as.vector(col)
  encoded_data <- as.integer(factor(data))
  encoded_data <- as.data.frame(encoded_data)
  return(encoded_data)
}
encoded_df <- survey_df
encoded_df$Q2.AI_sources <- label_encode(survey_df$Q2.AI_sources)
head(encoded_df$Q2.AI_sources) # output first 6 rows
encoded_df$Q6.Domains <- label_encode(survey_df$Q6.Domains)
head(encoded_df$Q6.Domains) # output first 6 rows


# 1c) dealing with question 2 and 6 columns
names(survey_df)
# Extracting columns concerning q2 and q6 respectively
q2_df <- select(survey_df, "Q2.AI_sources", "Q2.1.Internet", "Q2.2.Books.Papers"                   
                 ,"Q2.3.Social_media", "Q2.4.Discussions", "Q2.5.NotInformed")
q6_df <- select(survey_df, "Q6.Domains", "Q6.1.Education", "Q6.2.Medicine", 
                "Q6.3.Agriculture", "Q6.4.Constructions" , "Q6.5.Marketing",
                "Q6.6.Administration", "Q6.7.Art")
q2_df[1,]
q6_df[1,]
# Dropping Q2 and Q6 columns
survey_df <- select(survey_df, -"Q2.AI_sources", -"Q6.Domains")
names(survey_df)

# 1d) Q16 Data imputation
cat("Number of missing values in Q16 column:", sum(is.na(survey_df$Q16.GPA)))
median = median(survey_df$Q16.GPA, na.rm = TRUE)
survey_df$Q16.GPA[is.na(survey_df$Q16.GPA)] <- median

# 1e) getting basic info of dataset
summary(survey_df)

# Data Pre-processing
# Impute missing values
for (col in names(survey_df)){
  if (sum(is.na(survey_df[[col]])) > 0){
    median <- median(survey_df[[col]], na.rm = TRUE)
    cat("Imputing", col, "column with value:", median, "\n")
    survey_df[[col]][is.na(survey_df[[col]])] <- median
  }
}

# Splitting the dataset
train <- sample_frac(survey_df, 0.7)
test <- anti_join(survey_df, train, by = "ID")
nrow(train)
nrow(test)

# Checking for independence between explanatory variables
x <- select(survey_df, "Q14.Major", "Q15.Passed_exams")
cor_matrix <- cor(x)
cor_matrix

# Checking for normal distribution
hist(survey_df$Q16.GPA)


# 2a) Multiple linear regression
library(broom) # for linear regression
formula = Q16.GPA ~ Q15.Passed_exams + Q14.Major
# Fitting model with training data
gpa.lm <- lm(formula = formula, data = train)
# Getting summary of model
summary(gpa.lm)
# Testing Linear regression model
# extracting exploratory variables from testing set
test_data <- data.frame(Q14.Major = test$Q14.Major, 
                        Q15.Passed_exams = test$Q15.Passed_exams)
# predicting values using testing set
linear_predictions <- predict.lm(gpa.lm, newdata = test_data)
actual_values <- test$Q16.GPA # Extracting actual GPA values from testing set
# Getting mean squared error (lower value = better accuracy)
linear_mse <- mean((actual_values - linear_predictions)^2)
linear_mse


# 2b) Random forest model
library(randomForest) # For random forest model
num_trees <- 100 # Number of trees used
# Fitting training data to train model
gpa.forest <- randomForest(formula = formula, data = train, ntree = num_trees)
# Testing random forest model
# Predicting values
forest_predictions <- predict(gpa.forest, newdata = test_data)
# Calculating mse score
forest_mse <-mean((actual_values - forest_predictions)^2)
forest_mse

# 3a)i) Visualize multiple linear regression model
# Prepare plotting data
plotting.data<-expand.grid(
  Q14.Major = seq(min(survey_df$Q14.Major), 
                  max(survey_df$Q14.Major), length.out=30),
  Q15.Passed_exams =c(min(survey_df$Q15.Passed_exams), 
                      mean(survey_df$Q15.Passed_exams), 
                      max(survey_df$Q15.Passed_exams)))

plotting.data

# Predict values
plotting.data$predicted.y <- predict.lm(gpa.lm, newdata = plotting.data)
plotting.data$predicted.yS

# Round Passed_exams values to 2dps
plotting.data$Q15.Passed_exams <- round(plotting.data$Q15.Passed_exams, digits = 2)
# Format Passed_exams values as a factor
plotting.data$Q15.Passed_exams <- as.factor(plotting.data$Q15.Passed_exams)

# Plot original data with regression line
library(ggpubr)
gpa.plot <- ggplot(survey_df, aes(x = Q14.Major, y = Q16.GPA)) + 
  geom_point() + 
  # Add regression line
  geom_line(data = plotting.data, 
                           aes(x = Q14.Major, y = predicted.y, 
                               color = Q15.Passed_exams), size = 1.25) +
  # Add more comprehensible titles
  theme_bw() + labs(title = "GPA as a function of Majors and Exams passed",
                    x = "Major", y = "Number of exams passed", 
                    color = "Exams Passed") 

gpa.plot


# 3a)ii) Visualize Random Forest as a variable importance plot
varImpPlot(gpa.forest, 
           main = "GPA Variable Importance Plot")
