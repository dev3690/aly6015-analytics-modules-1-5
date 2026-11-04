# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(psych)
library(corrplot)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dir.create("plots", showWarnings = FALSE)

ames_data <- read.csv("AmesHousing.csv")

#--------------------
# 1. Initial Data Exploration
#--------------------
# Display basic information
head(ames_data)
str(ames_data)
summary(ames_data)

# descriptive statistics
numeric_stats <- describe(ames_data[sapply(ames_data, is.numeric)])
print(numeric_stats)

#--------------------
# 2. Exploratory Data Analysis
#--------------------
# Sale Price Distribution
sale_price_hist <- ggplot(ames_data, aes(x = SalePrice)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.5) +
  labs(title = "Distribution of Sale Prices",
       x = "Sale Price",
       y = "Count") +
  theme_minimal()
ggsave("plots/sale_price_distribution.png", 
       sale_price_hist, width = 10, height = 6)

# Quality vs Price
quality_price_plot <- ggplot(ames_data, aes(x = as.factor(Overall_Qual), y = SalePrice)) +
  geom_boxplot() +
  labs(title = "Sale Price by Overall Quality",
       x = "Overall Quality",
       y = "Sale Price") +
  theme_minimal()
ggsave("plots/quality_vs_price.png", 
       quality_price_plot, width = 10, height = 6)

# Sale Price Density
density_plot <- ggplot(ames_data, aes(x = SalePrice)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Distribution of Sale Prices",
       x = "Sale Price",
       y = "Density") +
  theme_minimal()
ggsave("plots/sale_price_density.png", 
       density_plot, width = 10, height = 6)

#--------------------
# 3. Missing Value Analysis
#--------------------
# Identify missing values
missing_values <- colSums(is.na(ames_data))
missing_percent <- (missing_values/nrow(ames_data)) * 100
missing_summary <- data.frame(
  Missing_Count = missing_values,
  Missing_Percent = missing_percent
)
print(missing_summary[missing_summary$Missing_Count > 0, ])

# Create imputed dataset
ames_data_imputed <- ames_data
for(col in names(ames_data)) {
  if(is.numeric(ames_data[[col]])) {
    ames_data_imputed[[col]][is.na(ames_data_imputed[[col]])] <- mean(ames_data_imputed[[col]], na.rm = TRUE)
  }
}

#--------------------
# 4. Correlation Analysis
#--------------------
# Prepare numeric data
numeric_columns <- sapply(ames_data_imputed, is.numeric)
numeric_data <- ames_data_imputed[, numeric_columns]
numeric_data$Order <- NULL

correlation_matrix <- cor(numeric_data)

# Select top correlated variables with SalePrice
sale_price_cors <- sort(abs(correlation_matrix[, "SalePrice"]), decreasing = TRUE)
top_vars <- names(sale_price_cors)[1:10]  # Top 10 correlated variables

reduced_matrix <- correlation_matrix[top_vars, top_vars]

# Save correlation plot with improved readability
png("plots/correlation_matrix.png",
    width = 1000, height = 1000, res = 150)

corrplot(reduced_matrix, 
         method = "color", 
         type = "upper",
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,        # Adjust size of correlation numbers
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),  # Custom color palette
         title = "Top 10 Correlated Variables with SalePrice",
         mar = c(2,2,4,2),       # Adjust margins
         number.digits = 2)       # Round to 2 decimal places

dev.off()

print("Top correlations with SalePrice:")
print(head(sale_price_cors, 10))

#--------------------
# 5. Correlation Matrix Interpretation
#--------------------
# Get exact correlations with SalePrice
sale_price_correlations <- correlation_matrix[, "SalePrice"]
sorted_correlations <- sort(sale_price_correlations, decreasing = TRUE)

# interpretation summary
cat("\nCorrelation Analysis Summary:\n")
cat("Strongest positive correlations with SalePrice:\n")
print(head(sorted_correlations, 5))

cat("\nWeakest correlations with SalePrice:\n")
print(tail(sorted_correlations, 5))

#--------------------
# 6. Scatter Plot Analysis
#--------------------
# Find variables for scatter plots
highest_cor <- names(sale_price_cors)[2]  # First is SalePrice itself
lowest_cor <- names(sort(abs(sale_price_correlations)))[1]
mid_cor <- names(which.min(abs(abs(sale_price_correlations) - 0.5)))

# Create scatter plots
# Highest correlation
high_scatter <- ggplot(ames_data_imputed, aes_string(x = highest_cor, y = "SalePrice")) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = paste("SalePrice vs", highest_cor),
       subtitle = paste("Correlation:", round(correlation_matrix[highest_cor, "SalePrice"], 3))) +
  theme_minimal()
ggsave("plots/highest_correlation_scatter.png", 
       high_scatter, width = 10, height = 6)

# Lowest correlation
low_scatter <- ggplot(ames_data_imputed, aes_string(x = lowest_cor, y = "SalePrice")) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = paste("SalePrice vs", lowest_cor),
       subtitle = paste("Correlation:", round(correlation_matrix[lowest_cor, "SalePrice"], 3))) +
  theme_minimal()
ggsave("plots/lowest_correlation_scatter.png", 
       low_scatter, width = 10, height = 6)

# Medium correlation (closest to 0.5)
mid_scatter <- ggplot(ames_data_imputed, aes_string(x = mid_cor, y = "SalePrice")) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = paste("SalePrice vs", mid_cor),
       subtitle = paste("Correlation:", round(correlation_matrix[mid_cor, "SalePrice"], 3))) +
  theme_minimal()
ggsave("plots/medium_correlation_scatter.png", 
       mid_scatter, width = 10, height = 6)

#--------------------
# 7. Initial Regression Model
#--------------------
# top 3 continuous variables based on correlation
top_predictors <- names(sale_price_cors)[2:4]  # Excluding SalePrice itself
cat("\nSelected predictors for regression model:\n")
print(top_predictors)

# Create the regression model
initial_model <- lm(SalePrice ~ get(top_predictors[1]) + 
                      get(top_predictors[2]) + 
                      get(top_predictors[3]), 
                    data = ames_data_imputed)

# Model summary
cat("\nRegression Model Summary:\n")
print(summary(initial_model))

# Create diagnostic plots
png("plots/regression_diagnostics.png",
    width = 1200, height = 1200, res = 150)
par(mfrow=c(2,2))
plot(initial_model)
dev.off()

# VIF to check for multicollinearity
library(car)
vif_values <- vif(initial_model)
cat("\nVariance Inflation Factors:\n")
print(vif_values)

#--------------------
# 8. Model Equation and Coefficient Interpretation
#--------------------
# Get model coefficients and variable names
model_coef <- round(coef(initial_model), 2)
model_vars <- top_predictors

# Print model equation
cat("\nModel Equation:\n")
cat("SalePrice =", model_coef[1], "+", 
    paste(model_coef[-1], "×", model_vars, collapse = " + "), "\n")

# Print coefficient interpretations
cat("\nCoefficient Interpretations:\n")
cat("• Intercept:", model_coef[1], 
    "- The expected sale price when all predictors are zero\n")

for(i in 1:length(model_vars)) {
  cat("•", model_vars[i], "coefficient:", model_coef[i+1],
      "- For each one unit increase in", model_vars[i],
      ", the sale price is expected to increase by $", model_coef[i+1],
      ", holding other variables constant\n")
}

#--------------------
# 9. Diagnostic Plot Interpretation
#--------------------
# Create diagnostic plots with detailed labels
png("plots/detailed_diagnostics.png",
    width = 1200, height = 1200, res = 150)
par(mfrow=c(2,2))

# Plot 1: Residuals vs Fitted
plot(initial_model, which = 1, 
     main = "Residuals vs Fitted\n(Check for Linearity)",
     caption = "")

# Plot 2: Normal Q-Q
plot(initial_model, which = 2,
     main = "Normal Q-Q Plot\n(Check for Normality)",
     caption = "")

# Plot 3: Scale-Location
plot(initial_model, which = 3,
     main = "Scale-Location\n(Check for Homoscedasticity)",
     caption = "")

# Plot 4: Residuals vs Leverage
plot(initial_model, which = 5,
     main = "Residuals vs Leverage\n(Check for Influential Points)",
     caption = "")

dev.off()

# diagnostic summary
cat("\nDiagnostic Plot Interpretations:\n")
cat("1. Residuals vs Fitted: Check for non-linear patterns\n")
cat("2. Normal Q-Q: Check if residuals follow normal distribution\n")
cat("3. Scale-Location: Check for constant variance\n")
cat("4. Residuals vs Leverage: Identify influential cases\n")

#--------------------
# 10. Multicollinearity Analysis
#--------------------
# VIF analysis
cat("\nDetailed Multicollinearity Analysis:\n")
cat("VIF Values and Interpretation:\n")
for(i in 1:length(top_predictors)) {
  cat(sprintf("%s: VIF = %.2f - %s\n", 
              top_predictors[i], 
              vif_values[i],
              ifelse(vif_values[i] > 5, "Potential multicollinearity", "Acceptable")))
}

# Correlation between predictors
predictor_cor <- cor(ames_data_imputed[, top_predictors])
cat("\nCorrelation between predictors:\n")
print(round(predictor_cor, 3))

cat("\nSuggestions for handling multicollinearity:\n")
high_vif <- names(vif_values[vif_values > 5])
if(length(high_vif) > 0) {
  cat("Variables with high VIF:", paste(high_vif, collapse=", "), "\n")
  cat("Recommended actions:\n")
  cat("1. Consider removing one of the highly correlated predictors\n")
  cat("2. Create composite variables\n")
  cat("3. Use regularization techniques\n")
} else {
  cat("No severe multicollinearity detected (all VIF < 5)\n")
}

#--------------------
# 11. Outlier Analysis
#--------------------
# Calculate standardized residuals
std_resid <- rstandard(initial_model)
outlier_indices <- which(abs(std_resid) > 2)

# Calculate Cook's distance
cooks_d <- cooks.distance(initial_model)
influential_points <- which(cooks_d > 4/length(cooks_d))

# Calculate leverage values
leverage <- hatvalues(initial_model)
high_leverage <- which(leverage > 2 * mean(leverage))

# summary dataframe of potential problematic points
problem_points <- data.frame(
  Index = 1:length(std_resid),
  Std_Residual = std_resid,
  Cooks_Distance = cooks_d,
  Leverage = leverage,
  SalePrice = ames_data_imputed$SalePrice
)

# Identify outliers
cat("\nOutlier Analysis Results:\n")
cat("\n1. Standardized Residuals (|std.residuals| > 2):\n")
print(problem_points[outlier_indices, ])

cat("\n2. Influential Points (Cook's distance):\n")
print(problem_points[influential_points, ])

cat("\n3. High Leverage Points:\n")
print(problem_points[high_leverage, ])

# Create visualization of outliers
png("plots/outlier_analysis.png",
    width = 1200, height = 800, res = 150)
par(mfrow = c(2,2))

# Plot 1: Standardized Residuals
plot(std_resid, 
     main = "Standardized Residuals",
     ylab = "Standardized Residuals",
     xlab = "Observation Number")
abline(h = c(-2, 2), col = "red", lty = 2)

# Plot 2: Cook's Distance
plot(cooks_d,
     main = "Cook's Distance",
     ylab = "Cook's Distance",
     xlab = "Observation Number")
abline(h = 4/length(cooks_d), col = "red", lty = 2)

# Plot 3: Leverage
plot(leverage,
     main = "Leverage Values",
     ylab = "Leverage",
     xlab = "Observation Number")
abline(h = 2 * mean(leverage), col = "red", lty = 2)

# Plot 4: Residuals vs Leverage with Cook's distance
plot(initial_model, which = 5)

dev.off()

# Recommendations
cat("\nRecommendations for Outlier Treatment:\n")
cat("1. Number of potential outliers (|std.residuals| > 2):", length(outlier_indices), "\n")
cat("2. Number of influential points:", length(influential_points), "\n")
cat("3. Number of high leverage points:", length(high_leverage), "\n")

# Decision criteria
cat("\nOutlier Treatment Decision:\n")
if(length(outlier_indices) > 0 || length(influential_points) > 0) {
  cat("Consider the following actions:\n")
  cat("1. Investigate extreme cases for data entry errors\n")
  cat("2. Consider removing points with both high leverage and large residuals\n")
  cat("3. Consider robust regression techniques\n")
  cat("4. Transform variables to reduce the impact of outliers\n")
} else {
  cat("No severe outlier issues detected\n")
}

#--------------------
# 12. Model Improvements
#--------------------
# Remove outliers identified by both high leverage and large residuals
outlier_threshold <- 2
leverage_threshold <- 2 * mean(leverage)
severe_outliers <- which(abs(std_resid) > outlier_threshold & leverage > leverage_threshold)

# Create cleaned dataset
ames_data_cleaned <- ames_data_imputed[-severe_outliers, ]

# Log transform the response variable to address non-linearity
ames_data_cleaned$log_SalePrice <- log(ames_data_cleaned$SalePrice)

# Create improved model
improved_model <- lm(log_SalePrice ~ get(top_predictors[1]) + 
                       get(top_predictors[2]) + 
                       get(top_predictors[3]), 
                     data = ames_data_cleaned)

# Compare models
cat("\nOriginal Model Summary:\n")
print(summary(initial_model)$r.squared)
print(summary(initial_model)$adj.r.squared)

cat("\nImproved Model Summary:\n")
print(summary(improved_model)$r.squared)
print(summary(improved_model)$adj.r.squared)

# Check if multicollinearity improved
improved_vif <- vif(improved_model)
cat("\nImproved Model VIF Values:\n")
print(improved_vif)

# Create diagnostic plots for improved model
png("plots/improved_model_diagnostics.png",
    width = 1200, height = 1200, res = 150)
par(mfrow=c(2,2))
plot(improved_model)
dev.off()

# Model improvement summary
cat("\nModel Improvement Summary:\n")
cat("1. Outliers removed:", length(severe_outliers), "observations\n")
cat("2. Response variable transformed using log transformation\n")
cat("3. R-squared comparison:\n")
cat("   Original:", round(summary(initial_model)$r.squared, 4), "\n")
cat("   Improved:", round(summary(improved_model)$r.squared, 4), "\n")

#--------------------
# 13. All Subsets Regression
#--------------------
# Install and load required package
if (!require(leaps)) install.packages("leaps")
library(leaps)

# Prepare data for all subsets regression
# Select numeric predictors (excluding SalePrice and log_SalePrice)
predictors <- names(ames_data_cleaned)[sapply(ames_data_cleaned, is.numeric)]
predictors <- predictors[!predictors %in% c("SalePrice", "log_SalePrice", "Order")]

# Create model matrix
X <- as.matrix(ames_data_cleaned[predictors])
y <- ames_data_cleaned$log_SalePrice

# Perform all subsets regression
all_subsets <- regsubsets(X, y, nbest = 1, nvmax = 8, method = "exhaustive")
summary_subsets <- summary(all_subsets)

# Create summary data frame
models_summary <- data.frame(
  Vars = 1:length(summary_subsets$rsq),
  RSS = summary_subsets$rss,
  R2 = summary_subsets$rsq,
  Adj_R2 = summary_subsets$adjr2,
  Cp = summary_subsets$cp,
  BIC = summary_subsets$bic
)

# Find best model based on adjusted R-squared
best_adj_r2 <- which.max(models_summary$Adj_R2)

# Get variables in best model
best_vars <- names(coef(all_subsets, best_adj_r2))[-1]  # Exclude intercept

# Fit the best model
best_model <- lm(log_SalePrice ~ ., 
                 data = ames_data_cleaned[c("log_SalePrice", best_vars)])

# Print results
cat("\nBest Model Selection Results:\n")
cat("Number of variables:", length(best_vars), "\n")
cat("Variables selected:", paste(best_vars, collapse = ", "), "\n")
cat("Adjusted R-squared:", round(summary(best_model)$adj.r.squared, 4), "\n\n")

# Print model equation
coef_values <- round(coef(best_model), 4)
equation <- paste("log(SalePrice) =", round(coef_values[1], 4))
for(i in 2:length(coef_values)) {
  term <- paste(ifelse(coef_values[i] >= 0, " + ", " - "),
                abs(coef_values[i]), " × ", 
                names(coef_values)[i])
  equation <- paste(equation, term)
}

cat("Best Model Equation:\n")
cat(equation, "\n")

# Create diagnostic plots for best model
png("plots/best_model_diagnostics.png",
    width = 1200, height = 1200, res = 150)
par(mfrow=c(2,2))
plot(best_model)
dev.off()

#--------------------
# 14. Model Comparison
#--------------------
# Compare model summaries
cat("\nModel Comparison:\n")
cat("\n1. Improved Model (Step 12):\n")
print(summary(improved_model)$call)
cat("Adjusted R-squared:", round(summary(improved_model)$adj.r.squared, 4))
cat("\nAIC:", AIC(improved_model))
cat("\nBIC:", BIC(improved_model))

cat("\n\n2. Best Subset Model (Step 13):\n")
print(summary(best_model)$call)
cat("Adjusted R-squared:", round(summary(best_model)$adj.r.squared, 4))
cat("\nAIC:", AIC(best_model))
cat("\nBIC:", BIC(best_model))

# Compare predictions
improved_predictions <- exp(predict(improved_model))
best_predictions <- exp(predict(best_model))

# Calculate RMSE for both models
improved_rmse <- sqrt(mean((ames_data_cleaned$SalePrice - improved_predictions)^2))
best_rmse <- sqrt(mean((ames_data_cleaned$SalePrice - best_predictions)^2))

cat("\n\nModel Performance Metrics:")
cat("\n1. Improved Model RMSE:", round(improved_rmse, 2))
cat("\n2. Best Subset Model RMSE:", round(best_rmse, 2))

# Create comparison visualization
png("plots/model_comparison.png",
    width = 1200, height = 600, res = 150)
par(mfrow = c(1,2))

# Plot 1: Predicted vs Actual for Improved Model
plot(ames_data_cleaned$SalePrice, improved_predictions,
     main = "Improved Model\nPredicted vs Actual",
     xlab = "Actual Price",
     ylab = "Predicted Price")
abline(0, 1, col = "red")

# Plot 2: Predicted vs Actual for Best Subset Model
plot(ames_data_cleaned$SalePrice, best_predictions,
     main = "Best Subset Model\nPredicted vs Actual",
     xlab = "Actual Price",
     ylab = "Predicted Price")
abline(0, 1, col = "red")

dev.off()

# Print comprehensive comparison
cat("\n\nComprehensive Model Comparison:\n")
cat("\n1. Model Complexity:")
cat("\n   - Improved Model Variables:", length(coef(improved_model)))
cat("\n   - Best Subset Model Variables:", length(coef(best_model)))

cat("\n\n2. Model Accuracy:")
cat("\n   - Improved Model:")
cat("\n     * Adjusted R-squared:", round(summary(improved_model)$adj.r.squared, 4))
cat("\n     * RMSE:", round(improved_rmse, 2))
cat("\n   - Best Subset Model:")
cat("\n     * Adjusted R-squared:", round(summary(best_model)$adj.r.squared, 4))
cat("\n     * RMSE:", round(best_rmse, 2))

cat("\n\n3. Model Selection Criteria:")
cat("\n   - Improved Model:")
cat("\n     * AIC:", round(AIC(improved_model), 2))
cat("\n     * BIC:", round(BIC(improved_model), 2))
cat("\n   - Best Subset Model:")
cat("\n     * AIC:", round(AIC(best_model), 2))
cat("\n     * BIC:", round(BIC(best_model), 2))

# Preferred model recommendation
cat("\n\nPreferred Model Recommendation:")
if(summary(best_model)$adj.r.squared > summary(improved_model)$adj.r.squared && 
   best_rmse < improved_rmse) {
  cat("\nThe Best Subset Model is preferred because:")
  cat("\n1. Higher adjusted R-squared")
  cat("\n2. Lower RMSE")
  cat("\n3. More comprehensive variable selection")
} else if(summary(improved_model)$adj.r.squared > summary(best_model)$adj.r.squared && 
          improved_rmse < best_rmse) {
  cat("\nThe Improved Model is preferred because:")
  cat("\n1. Higher adjusted R-squared")
  cat("\n2. Lower RMSE")
  cat("\n3. More parsimonious (simpler model)")
} else {
  cat("\nBoth models have their merits:")
  cat("\n- Best Subset Model: More comprehensive but complex")
  cat("\n- Improved Model: Simpler but still effective")
  cat("\nFinal choice depends on specific needs (simplicity vs. accuracy)")
}