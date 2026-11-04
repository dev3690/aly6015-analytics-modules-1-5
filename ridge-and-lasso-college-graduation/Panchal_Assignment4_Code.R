# Load required libraries
library(ISLR)      # For the College dataset
library(glmnet)    # For Ridge and Lasso regression
library(caret)     # For data splitting

# Create a plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Load the College dataset
data(College)

# Display the structure of the dataset to understand it better
str(College)

# Check for missing values
sum(is.na(College))

# Set a random seed for reproducibility
set.seed(123)

# Create an index for splitting the data (80% train, 20% test)
trainIndex <- createDataPartition(College$Grad.Rate, p = 0.8, list = FALSE)

# Split the data into training and test sets
train_data <- College[trainIndex, ]
test_data <- College[-trainIndex, ]

# Verify the split
cat("Training set dimensions:", dim(train_data), "\n")
cat("Test set dimensions:", dim(test_data), "\n")

# Check the distribution of the target variable in both sets
summary(train_data$Grad.Rate)
summary(test_data$Grad.Rate)

# Question 2: Ridge Regression - Estimating lambda values

# Prepare the data for glmnet (requires matrix format)
x_train <- model.matrix(Grad.Rate ~ ., train_data)[,-1]  # Remove intercept
y_train <- train_data$Grad.Rate

# Perform cross-validation for Ridge regression
set.seed(123)  # For reproducibility
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10)

# Extract lambda.min and lambda.1se values
lambda_min <- ridge_cv$lambda.min
lambda_1se <- ridge_cv$lambda.1se

# Print the values
cat("Ridge Regression:\n")
cat("lambda.min:", lambda_min, "\n")
cat("lambda.1se:", lambda_1se, "\n")

# Compare the values
cat("Ratio lambda.1se/lambda.min:", lambda_1se/lambda_min, "\n")

# Question 3: Plot the cross-validation results
# Plot the cross-validation results
# For the cross-validation plot (Question 3)
png("plots/ridge_cv_plot.png", width = 800, height = 600)
plot(ridge_cv, main = "Ridge Regression - Cross-Validation")
dev.off()

# Question 4: Fit Ridge regression model and analyze coefficients

# Fit Ridge regression model with lambda.min
ridge_model_min <- glmnet(x_train, y_train, alpha = 0, lambda = lambda_min)

# Extract coefficients
ridge_coef_min <- coef(ridge_model_min)

# Display the coefficients
cat("Ridge Regression Coefficients (lambda.min):\n")
print(ridge_coef_min)

# Create a data frame of coefficients for better visualization
coef_df <- data.frame(
  Variable = rownames(ridge_coef_min),
  Coefficient = as.numeric(ridge_coef_min)
)

# Sort by absolute coefficient value
coef_df <- coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE), ]

# Display the sorted coefficients
print(coef_df)

# Create a bar plot of the coefficients (excluding intercept)
coef_df_no_intercept <- coef_df[coef_df$Variable != "(Intercept)", ]
# pdf("plots/ridge_coefficients_plot.pdf", width = 12, height = 8)
png("plots/ridge_coefficients_plot.png", width = 800, height = 600)

barplot(coef_df_no_intercept$Coefficient, 
        names.arg = coef_df_no_intercept$Variable, 
        main = "Ridge Regression Coefficients", 
        las = 2,  # Rotate variable names
        cex.names = 0.7,  # Reduce text size
        col = ifelse(coef_df_no_intercept$Coefficient > 0, "blue", "red"))
abline(h = 0, lty = 2)  # Add a horizontal line at y=0
dev.off()

# Save the coefficients table to a CSV file
write.csv(coef_df, "plots/ridge_coefficients.csv", row.names = FALSE)


# Question 5: Calculate RMSE for the training set

# Make predictions on the training set
train_pred <- predict(ridge_model_min, newx = x_train)

# Convert predictions to vector if they're in matrix form
train_pred <- as.vector(train_pred)

# Calculate RMSE
train_rmse <- sqrt(mean((y_train - train_pred)^2))

# Print the RMSE
cat("Ridge Regression - Training Set RMSE:", train_rmse, "\n")

# Create a data frame for actual vs predicted values
train_results <- data.frame(
  Actual = y_train,
  Predicted = train_pred,
  Residual = y_train - train_pred
)

# Create a scatter plot of actual vs predicted values
png("plots/ridge_train_predictions.png", width = 800, height = 600)
plot(train_results$Actual, train_results$Predicted,
     xlab = "Actual Graduation Rate",
     ylab = "Predicted Graduation Rate",
     main = "Ridge Regression: Actual vs Predicted (Training Set)",
     pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # Add a 45-degree line
legend("topleft", legend = c("Predictions", "Perfect Prediction"),
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
dev.off()

# Create a histogram of residuals
png("plots/ridge_train_residuals.png", width = 800, height = 600)
hist(train_results$Residual,
     xlab = "Residuals",
     main = "Ridge Regression: Residuals Distribution (Training Set)",
     col = "skyblue",
     breaks = 30)
abline(v = 0, col = "red", lwd = 2, lty = 2)
dev.off()


# Question 6: Calculate RMSE for the test set

# Prepare the test data for prediction
x_test <- model.matrix(Grad.Rate ~ ., test_data)[,-1]  # Remove intercept
y_test <- test_data$Grad.Rate

# Make predictions on the test set
test_pred <- predict(ridge_model_min, newx = x_test)
test_pred <- as.vector(test_pred)

# Calculate RMSE
test_rmse <- sqrt(mean((y_test - test_pred)^2))

# Print the RMSE
cat("Ridge Regression - Test Set RMSE:", test_rmse, "\n")
cat("Ratio of Test RMSE to Training RMSE:", test_rmse/train_rmse, "\n")

# Create a data frame for actual vs predicted values
test_results <- data.frame(
  Actual = y_test,
  Predicted = test_pred,
  Residual = y_test - test_pred
)

# Create a scatter plot of actual vs predicted values
png("plots/ridge_test_predictions.png", width = 800, height = 600)
plot(test_results$Actual, test_results$Predicted,
     xlab = "Actual Graduation Rate",
     ylab = "Predicted Graduation Rate",
     main = "Ridge Regression: Actual vs Predicted (Test Set)",
     pch = 16, col = "green")
abline(0, 1, col = "red", lwd = 2)  # Add a 45-degree line
legend("topleft", legend = c("Predictions", "Perfect Prediction"),
       col = c("green", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
dev.off()

# Create a histogram of residuals
png("plots/ridge_test_residuals.png", width = 800, height = 600)
hist(test_results$Residual,
     xlab = "Residuals",
     main = "Ridge Regression: Residuals Distribution (Test Set)",
     col = "lightgreen",
     breaks = 30)
abline(v = 0, col = "red", lwd = 2, lty = 2)
dev.off()



# Question 7: LASSO Regression - Estimating lambda values

# Perform cross-validation for LASSO regression
set.seed(123)  # For reproducibility
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 10)

# Extract lambda.min and lambda.1se values
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_1se <- lasso_cv$lambda.1se

# Print the values
cat("\nLASSO Regression:\n")
cat("lambda.min:", lasso_lambda_min, "\n")
cat("lambda.1se:", lasso_lambda_1se, "\n")

# Compare the values
cat("Ratio lambda.1se/lambda.min:", lasso_lambda_1se/lasso_lambda_min, "\n")

# Plot the cross-validation results
png("plots/lasso_cv_plot.png", width = 800, height = 600)
plot(lasso_cv, main = "LASSO Regression - Cross-Validation")
dev.off()

# Compare Ridge and LASSO lambda values
lambda_comparison <- data.frame(
  Model = c("Ridge", "LASSO"),
  lambda.min = c(lambda_min, lasso_lambda_min),
  lambda.1se = c(lambda_1se, lasso_lambda_1se),
  Ratio = c(lambda_1se/lambda_min, lasso_lambda_1se/lasso_lambda_min)
)

# Print the comparison
print(lambda_comparison)

# Save the comparison to a CSV file
write.csv(lambda_comparison, "plots/lambda_comparison.csv", row.names = FALSE)

# Create a bar plot comparing lambda values
png("plots/lambda_comparison_plot.png", width = 800, height = 600)
barplot(
  t(as.matrix(lambda_comparison[, c("lambda.min", "lambda.1se")])),
  beside = TRUE,
  names.arg = lambda_comparison$Model,
  col = c("skyblue", "lightgreen"),
  main = "Comparison of Lambda Values: Ridge vs LASSO",
  ylab = "Lambda Value",
  legend.text = c("lambda.min", "lambda.1se")
)
dev.off()

# Question 8: Analyze the LASSO cross-validation plot
# The plot has already been created in Question 7
# We'll add more detailed annotations to highlight key features

# Create an enhanced version of the LASSO CV plot with annotations
png("plots/lasso_cv_plot_annotated.png", width = 1000, height = 700)
plot(lasso_cv, main = "LASSO Regression - Cross-Validation Analysis")
# Add text annotations to highlight key features
text(log(lasso_lambda_min), min(lasso_cv$cvm) + 5, "lambda.min", pos = 4, col = "blue")
text(log(lasso_lambda_1se), min(lasso_cv$cvm) + 5, "lambda.1se", pos = 2, col = "blue")
text(log(lasso_cv$lambda[1]), max(lasso_cv$cvm) - 5, 
     "Fewer variables\n(High regularization)", pos = 4, col = "darkgreen")
text(log(lasso_cv$lambda[length(lasso_cv$lambda)]), max(lasso_cv$cvm) - 5, 
     "More variables\n(Low regularization)", pos = 2, col = "darkgreen")
dev.off()

# Question 9: Fit LASSO regression model and analyze coefficients

# Fit LASSO regression model with lambda.min
lasso_model_min <- glmnet(x_train, y_train, alpha = 1, lambda = lasso_lambda_min)

# Extract coefficients
lasso_coef_min <- coef(lasso_model_min)

# Display the coefficients
cat("LASSO Regression Coefficients (lambda.min):\n")
print(lasso_coef_min)

# Create a data frame of coefficients for better visualization
lasso_coef_df <- data.frame(
  Variable = rownames(lasso_coef_min),
  Coefficient = as.numeric(lasso_coef_min)
)

# Count non-zero coefficients
non_zero_count <- sum(lasso_coef_df$Coefficient != 0)
zero_count <- sum(lasso_coef_df$Coefficient == 0)

cat("Number of non-zero coefficients:", non_zero_count, "\n")
cat("Number of zero coefficients:", zero_count, "\n")

# Identify variables with zero coefficients
zero_coef_vars <- lasso_coef_df$Variable[lasso_coef_df$Coefficient == 0]
cat("Variables with zero coefficients:\n")
print(zero_coef_vars)

# Sort by absolute coefficient value
lasso_coef_df <- lasso_coef_df[order(abs(lasso_coef_df$Coefficient), decreasing = TRUE), ]

# Display the sorted coefficients
print(lasso_coef_df)

# Create a bar plot of the non-zero coefficients (excluding intercept)
lasso_coef_df_non_zero <- lasso_coef_df[lasso_coef_df$Coefficient != 0 & 
                                        lasso_coef_df$Variable != "(Intercept)", ]
png("plots/lasso_coefficients_plot.png", width = 800, height = 600)
barplot(lasso_coef_df_non_zero$Coefficient, 
        names.arg = lasso_coef_df_non_zero$Variable, 
        main = "LASSO Regression Non-Zero Coefficients", 
        las = 2,  # Rotate variable names
        cex.names = 0.7,  # Reduce text size
        col = ifelse(lasso_coef_df_non_zero$Coefficient > 0, "blue", "red"))
abline(h = 0, lty = 2)  # Add a horizontal line at y=0
dev.off()

# Create a comparison plot of Ridge vs LASSO coefficients
ridge_coef_vector <- as.numeric(ridge_coef_min)
lasso_coef_vector <- as.numeric(lasso_coef_min)
coef_names <- rownames(ridge_coef_min)

# Create a data frame for comparison
comparison_df <- data.frame(
  Variable = coef_names,
  Ridge = ridge_coef_vector,
  LASSO = lasso_coef_vector
)

# Save the comparison to a CSV file
write.csv(comparison_df, "plots/ridge_lasso_comparison.csv", row.names = FALSE)

# Save the LASSO coefficients table to a CSV file
write.csv(lasso_coef_df, "plots/lasso_coefficients.csv", row.names = FALSE)

# Question 10: Calculate RMSE for the LASSO model on the training set

# Make predictions on the training set using the LASSO model
lasso_train_pred <- predict(lasso_model_min, newx = x_train)
lasso_train_pred <- as.vector(lasso_train_pred)

# Calculate RMSE
lasso_train_rmse <- sqrt(mean((y_train - lasso_train_pred)^2))

# Print the RMSE
cat("LASSO Regression - Training Set RMSE:", lasso_train_rmse, "\n")

# Create a data frame for actual vs predicted values
lasso_train_results <- data.frame(
  Actual = y_train,
  Predicted = lasso_train_pred,
  Residual = y_train - lasso_train_pred
)

# Create a scatter plot of actual vs predicted values
png("plots/lasso_train_predictions.png", width = 800, height = 600)
plot(lasso_train_results$Actual, lasso_train_results$Predicted,
     xlab = "Actual Graduation Rate",
     ylab = "Predicted Graduation Rate",
     main = "LASSO Regression: Actual vs Predicted (Training Set)",
     pch = 16, col = "darkblue")
abline(0, 1, col = "red", lwd = 2)  # Add a 45-degree line
legend("topleft", legend = c("Predictions", "Perfect Prediction"),
       col = c("darkblue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
dev.off()

# Create a histogram of residuals
png("plots/lasso_train_residuals.png", width = 800, height = 600)
hist(lasso_train_results$Residual,
     xlab = "Residuals",
     main = "LASSO Regression: Residuals Distribution (Training Set)",
     col = "lightblue",
     breaks = 30)
abline(v = 0, col = "red", lwd = 2, lty = 2)
dev.off()

# Compare Ridge and LASSO training RMSE
rmse_comparison <- data.frame(
  Model = c("Ridge", "LASSO"),
  Training_RMSE = c(train_rmse, lasso_train_rmse)
)

# Print the comparison
print(rmse_comparison)

write.csv(rmse_comparison, "plots/ridge_lasso_rmse_comparison.csv", row.names = FALSE)


# Question 11: Calculate RMSE for the LASSO model on the test set

# Make predictions on the test set using the LASSO model
lasso_test_pred <- predict(lasso_model_min, newx = x_test)
lasso_test_pred <- as.vector(lasso_test_pred)

# Calculate RMSE
lasso_test_rmse <- sqrt(mean((y_test - lasso_test_pred)^2))

# Print the RMSE
cat("LASSO Regression - Test Set RMSE:", lasso_test_rmse, "\n")
cat("Ratio of Test RMSE to Training RMSE:", lasso_test_rmse/lasso_train_rmse, "\n")

# Create a data frame for actual vs predicted values
lasso_test_results <- data.frame(
  Actual = y_test,
  Predicted = lasso_test_pred,
  Residual = y_test - lasso_test_pred
)

# Create a scatter plot of actual vs predicted values
png("plots/lasso_test_predictions.png", width = 800, height = 600)
plot(lasso_test_results$Actual, lasso_test_results$Predicted,
     xlab = "Actual Graduation Rate",
     ylab = "Predicted Graduation Rate",
     main = "LASSO Regression: Actual vs Predicted (Test Set)",
     pch = 16, col = "darkgreen")
abline(0, 1, col = "red", lwd = 2)  # Add a 45-degree line
legend("topleft", legend = c("Predictions", "Perfect Prediction"),
       col = c("darkgreen", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
dev.off()

# Create a histogram of residuals
png("plots/lasso_test_residuals.png", width = 800, height = 600)
hist(lasso_test_results$Residual,
     xlab = "Residuals",
     main = "LASSO Regression: Residuals Distribution (Test Set)",
     col = "lightgreen",
     breaks = 30)
abline(v = 0, col = "red", lwd = 2, lty = 2)
dev.off()

# Compare LASSO training and test RMSE
lasso_rmse_comparison <- data.frame(
  Dataset = c("Training", "Test"),
  RMSE = c(lasso_train_rmse, lasso_test_rmse)
)

# Print the comparison
print(lasso_rmse_comparison)

# Create a bar plot comparing training and test RMSE values
png("plots/lasso_rmse_comparison.png", width = 800, height = 600)
barplot(lasso_rmse_comparison$RMSE,
        names.arg = lasso_rmse_comparison$Dataset,
        col = c("lightblue", "lightgreen"),
        main = "LASSO Regression: Training vs Test RMSE",
        ylab = "RMSE")
dev.off()

# Compare Ridge and LASSO test performance
model_comparison <- data.frame(
  Model = c("Ridge", "LASSO"),
  Training_RMSE = c(train_rmse, lasso_train_rmse),
  Test_RMSE = c(test_rmse, lasso_test_rmse),
  Ratio = c(test_rmse/train_rmse, lasso_test_rmse/lasso_train_rmse)
)

# Print the comparison
print(model_comparison)

# Create a grouped bar plot comparing Ridge and LASSO performance
png("plots/ridge_lasso_comparison.png", width = 800, height = 600)
barplot(t(as.matrix(model_comparison[, c("Training_RMSE", "Test_RMSE")])),
        beside = TRUE,
        names.arg = model_comparison$Model,
        col = c("skyblue", "lightgreen"),
        main = "Performance Comparison: Ridge vs LASSO",
        ylab = "RMSE",
        legend.text = c("Training", "Test"))
dev.off()

write.csv(model_comparison, "plots/ridge_lasso_comparison.csv", row.names = FALSE)

# Question 12: Comprehensive comparison of Ridge and LASSO models

# Create a comprehensive comparison table
comprehensive_comparison <- data.frame(
  Metric = c("Training RMSE", "Test RMSE", "Overfitting Ratio (Test/Train)", 
             "Number of Non-Zero Coefficients", "lambda.min", "lambda.1se"),
  Ridge = c(train_rmse, test_rmse, test_rmse/train_rmse, 
            sum(ridge_coef_min != 0), lambda_min, lambda_1se),
  LASSO = c(lasso_train_rmse, lasso_test_rmse, lasso_test_rmse/lasso_train_rmse,
            sum(lasso_coef_min != 0), lasso_lambda_min, lasso_lambda_1se)
)

# Print the comprehensive comparison
print(comprehensive_comparison)

# Save the comprehensive comparison to a CSV file
write.csv(comprehensive_comparison, "plots/comprehensive_comparison.csv", row.names = FALSE)

# Create a radar chart for visual comparison
# First, normalize the metrics for better visualization
library(fmsb)

# Select numeric metrics for radar chart
radar_data <- comprehensive_comparison[1:3, 2:3]
rownames(radar_data) <- comprehensive_comparison$Metric[1:3]

# Add max and min rows required by the radar chart function
radar_data <- rbind(rep(max(radar_data), 2), rep(min(radar_data), 2), radar_data)


# Alternative comparison visualization
png("plots/ridge_lasso_comparison_alt.png", width = 800, height = 600)
# Create a new plot
plot(1:3, comprehensive_comparison$Ridge[1:3], type = "o", col = "blue", 
     pch = 16, lty = 1, lwd = 2,
     xlab = "Metric", ylab = "Value", 
     main = "Ridge vs LASSO Performance Comparison",
     xaxt = "n", # Remove x-axis labels
     ylim = c(min(comprehensive_comparison[1:3, 2:3]), 
              max(comprehensive_comparison[1:3, 2:3])))
# Add LASSO line
lines(1:3, comprehensive_comparison$LASSO[1:3], type = "o", 
      col = "red", pch = 16, lty = 1, lwd = 2)
# Add custom x-axis labels
axis(1, at = 1:3, labels = comprehensive_comparison$Metric[1:3])
# Add legend
legend("topright", 
       legend = c("Ridge", "LASSO"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 2,
       pch = 16,
       bty = "n")
dev.off()