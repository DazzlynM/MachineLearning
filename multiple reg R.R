# Multiple linear regression

# Importing data
dataset <- read.csv("50_Startups.csv")
# Encoding categorical data
dataset$State <- factor(dataset$State,
                        levels = c("New york", "Califorina", "Florida"),
                        labels = c(1,2,3))
# splitting the dataset into Training set and Test set
library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)
# Fitting Multiple Linear Regression to the Training
regressor <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State, training_set)
# regressor <- lm(formula = Profit ~ ., data = training_set)
y_pred <- predict(regressor, newdata = test_set)
y_pred
# Backward elimination method
regressor <- lm(Profit ~ R.D.Spend + Marketing.Spend, dataset)
summary(regressor)
