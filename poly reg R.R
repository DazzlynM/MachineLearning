# POlynomial Regression

# Importing the dataset
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[2:3]
dataset
# It is a small sample, so splitting of dataset into training and test set and
# also no feature scaling

# Fitting Linear Regression to the dataset
lin_reg <- lm(formula = Salary ~ Level, data = dataset)

# Fitting Polynomial Regression to the dataset
dataset$Level2 <- dataset$Level^2
dataset$Level3 <- dataset$Level^3
dataset$Level4 <- dataset$Level^4
poly_reg <- lm(formula = Salary ~ ., data = dataset)

# visualizing the LInear Regression results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            color = "blue") +
  ggtitle("Truth 0r bluff (Linear Regression)") +
  xlab("Level") +
  ylab("Salary")

# Visualizing the Polynomial Regression results
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            color = "blue") +
  ggtitle("Truth or Bluff (Polynomial Regression") +
  xlab("Position Level") +
  ylab("Salary")

# Visualizing with higher resolution and smother curve
x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = "red") +
  geom_line(aes(x = x_grid, y = predict(poly_reg, newdata = data.frame(Level = x_grid,
                                                                       Level2 = x_grid^2,
                                                                       Level3 = x_grid^3,
                                                                       Level4 = x_grid^4))),
            color = "blue") +
  ggtitle("Truth or Bluff (Poly Reg)") +
  xlab("Level") +
  ylab("Salary")

# Predicting a new result with Linear Regression
y_pred <- predict(lin_reg, data.frame(Level = 6.5))
y_pred

# Predicting a new result with Polynomial Regression
y_pred2 <- predict(poly_reg, data.frame(Level = 6.5,
                                        Level2= 6.5^2,
                                        Level3= 6.5^3,
                                        Level4= 6.5^4))
y_pred2
