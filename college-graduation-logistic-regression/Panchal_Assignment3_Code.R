# Load required libraries
library(ISLR)      # For College dataset
library(ggplot2)   # For visualization
library(dplyr)     # For data manipulation
library(tidyr)     # For data cleaning
library(knitr)     # For tables
library(gridExtra) # For arranging plots
library(gridExtra) # For table plotting
library(grid)      # For table plotting

# Create plots directory if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# Load the College dataset
data(College)

# Create Table 1: Summary Statistics by University Type
table1 <- College %>%
  group_by(Private) %>%
  summarise(
    Count = n(),
    Mean_Apps = round(mean(Apps, na.rm = TRUE), 2),
    Mean_Acceptance = round(mean(Accept/Apps * 100, na.rm = TRUE), 2),
    Mean_Enrollment = round(mean(Enroll, na.rm = TRUE), 2),
    Mean_Grad_Rate = round(mean(Grad.Rate, na.rm = TRUE), 2)
  )

# Convert table1 to grob
table1_grob <- tableGrob(table1, rows = NULL, 
                         theme = ttheme_minimal(
                           core = list(fg_params=list(fontsize=10)),
                           colhead = list(fg_params=list(fontsize=12, fontface="bold"))
                         ))



# Add library for Excel export
library(writexl)      # For Excel export

# Save College dataset to Excel
write_xlsx(College, "plots/college_dataset.xlsx")

# Create and display Table 1: Overall Summary Statistics
table1 <- summary(College[c("Apps", "Accept", "Enroll", "Top10perc", "Outstate", "Room.Board", 
                            "Books", "Personal", "PhD", "Terminal", "S.F.Ratio", "Grad.Rate")])
print("Summary Statistics of the Dataset:")
print(table1)

# Calculate correlation matrix to find most prominent variables
cor_matrix <- cor(College[sapply(College, is.numeric)])
var_importance <- data.frame(
  Variable = colnames(cor_matrix),
  Mean_Correlation = rowMeans(abs(cor_matrix))
)
var_importance <- var_importance[order(-var_importance$Mean_Correlation), ]
top_5_vars <- head(var_importance, 5)

# Create and display Table 2: Summary of Top 5 Most Prominent Variables
table2 <- summary(College[top_5_vars$Variable])
print("\nSummary of Top 5 Most Prominent Variables:")
print(table2)

# Get top 5 variables for box plot
top_5_vars <- names(sort(sapply(College[sapply(College, is.numeric)], 
                                function(x) abs(cor(x, as.numeric(College$Private)))), 
                         decreasing = TRUE)[1:5])

# Plot 1: Line plot showing trend of Grad.Rate vs Apps
p1 <- College %>%
  arrange(Apps) %>%
  ggplot(aes(x = Apps, y = Grad.Rate, color = Private)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Relationship between Applications and Graduation Rate",
       x = "Number of Applications",
       y = "Graduation Rate (%)") +
  theme_minimal()
ggsave("plots/apps_vs_gradrate.png", p1, width = 8, height = 6)

# Plot 2: Box plot of top 5 variables
p2 <- College %>%
  select(all_of(c("Private", top_5_vars))) %>%
  gather(key = "Variable", value = "Value", -Private) %>%
  ggplot(aes(x = Variable, y = Value, fill = Private)) +
  geom_boxplot() +
  labs(title = "Distribution of Top 5 Most Important Variables by University Type",
       x = "Variables",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("plots/top5_boxplot.png", p2, width = 10, height = 6)

# Plot 3: Bar plot of university counts by type (similar to example)
p3 <- College %>%
  count(Private) %>%
  ggplot(aes(x = Private, y = n, fill = Private)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Universities by Type",
       x = "University Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "#db1717", "Yes" = "#4CAF50"))  # Green color like in example
ggsave("plots/university_distribution.png", p3, width = 8, height = 6)

# Plot 3: Bar plot comparing multiple variables by university type
p3 <- College %>%
  group_by(Private) %>%
  summarise(
    Avg_Acceptance = mean(Accept/Apps * 100, na.rm = TRUE),
    Avg_Top10_Students = mean(Top10perc, na.rm = TRUE),
    Avg_Grad_Rate = mean(Grad.Rate, na.rm = TRUE),
    Avg_PhD_Faculty = mean(PhD, na.rm = TRUE)
  ) %>%
  gather(key = "Metric", value = "Value", -Private) %>%
  ggplot(aes(x = Metric, y = Value, fill = Private)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Key Metrics by University Type",
       x = "Metrics",
       y = "Average Value (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("No" = "red", "Yes" = "#4CAF50")) +
  scale_x_discrete(labels = c("Acceptance Rate", 
                              "Top 10% Students",
                              "Graduation Rate",
                              "PhD Faculty"))

# Plot 3: Bar plot comparing influential variables by university type
p3 <- College %>%
  group_by(Private) %>%
  summarise(
    Avg_Outstate = mean(Outstate, na.rm = TRUE) / 1000,  # Convert to thousands
    Avg_Expend = mean(Expend, na.rm = TRUE) / 1000,      # Convert to thousands
    Avg_Top25perc = mean(Top25perc, na.rm = TRUE),
    Avg_F_Undergrad = mean(F.Undergrad, na.rm = TRUE) / 1000  # Convert to thousands
  ) %>%
  gather(key = "Metric", value = "Value", -Private) %>%
  ggplot(aes(x = Metric, y = Value, fill = Private)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Key Differences Between Private and Public Universities",
       x = "Metrics",
       y = "Average Value") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  scale_fill_manual(values = c("No" = "#db1717", "Yes" = "#4CAF50")) +
  scale_x_discrete(labels = c("Out-of-State\nTuition (K$)", 
                              "Expenditure per\nStudent (K$)",
                              "Top 25%\nStudents (%)",
                              "Full-time\nUndergrads (K)"))

ggsave("plots/university_metrics_comparison.png", p3, width = 10, height = 6)

# Set seed for reproducibility
set.seed(123)

# Create index for splitting
train_index <- sample(1:nrow(College), 0.8 * nrow(College))

# Split the data
college_train <- College[train_index, ]
college_test <- College[-train_index, ]

# Print the dimensions of training and test sets
print("Dimensions of training set:")
print(dim(college_train))
print("Dimensions of test set:")
print(dim(college_test))

# Verify the split by checking proportions
print("Proportion of private universities in training set:")
print(table(college_train$Private)/nrow(college_train))
print("Proportion of private universities in test set:")
print(table(college_test$Private)/nrow(college_test))

# Fit logistic regression model
logistic_model <- glm(Private ~ Outstate + Top10perc, 
                      data = college_train, 
                      family = binomial)

# Get predictions for training set
train_pred_prob <- predict(logistic_model, college_train, type = "response")
train_pred <- ifelse(train_pred_prob > 0.5, "Yes", "No")

# Create confusion matrix for training set
conf_matrix <- table(Predicted = train_pred, Actual = college_train$Private)
print("\nConfusion Matrix (Training Set):")
print(conf_matrix)

# Calculate metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2,2] / sum(conf_matrix[,2])  # True Positive Rate
specificity <- conf_matrix[1,1] / sum(conf_matrix[,1])  # True Negative Rate
false_positive_rate <- 1 - specificity
false_negative_rate <- 1 - sensitivity

# Print metrics
print("\nModel Performance Metrics (Training Set):")
print(paste("Accuracy:", round(accuracy, 3)))
print(paste("Sensitivity (True Positive Rate):", round(sensitivity, 3)))
print(paste("Specificity (True Negative Rate):", round(specificity, 3)))
print(paste("False Positive Rate:", round(false_positive_rate, 3)))
print(paste("False Negative Rate:", round(false_negative_rate, 3)))

# Get predictions for test set
test_pred_prob <- predict(logistic_model, college_test, type = "response")
test_pred <- ifelse(test_pred_prob > 0.5, "Yes", "No")

# Create confusion matrix for test set
test_conf_matrix <- table(Predicted = test_pred, Actual = college_test$Private)
print("\nConfusion Matrix (Test Set):")
print(test_conf_matrix)

# Calculate metrics for test set
test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_sensitivity <- test_conf_matrix[2,2] / sum(test_conf_matrix[,2])  # True Positive Rate
test_specificity <- test_conf_matrix[1,1] / sum(test_conf_matrix[,1])  # True Negative Rate
test_precision <- test_conf_matrix[2,2] / sum(test_conf_matrix[2,])    # Positive Predictive Value

# Print test set metrics
print("\nModel Performance Metrics (Test Set):")
print(paste("Accuracy:", round(test_accuracy, 3)))
print(paste("Sensitivity (Recall):", round(test_sensitivity, 3)))
print(paste("Specificity:", round(test_specificity, 3)))
print(paste("Precision:", round(test_precision, 3)))

# Load required library for ROC curve
library(pROC)

# Create ROC curves for both training and test sets
roc_train <- roc(college_train$Private, train_pred_prob)
roc_test <- roc(college_test$Private, test_pred_prob)

# Calculate AUC values
auc_train <- auc(roc_train)
auc_test <- auc(roc_test)

# Plot ROC curves
png("plots/roc_curves.png", width = 800, height = 600)
plot(roc_train, main = "ROC Curves for College Type Prediction",
     col = "#4CAF50", lwd = 2,
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")
lines(roc_test, col = "#db1717", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")  # Add diagonal reference line

# Add legend with AUC values
legend("bottomright", 
       legend = c(
         paste("Training AUC =", round(auc_train, 3)),
         paste("Test AUC =", round(auc_test, 3))
       ),
       col = c("#4CAF50", "#db1717"), 
       lwd = 2)

dev.off()

# Print AUC values
print(paste("Training Set AUC:", round(auc_train, 3)))
print(paste("Test Set AUC:", round(auc_test, 3)))

# Create Performance Metrics Visualization - Radar Chart
library(fmsb)

# Prepare data for radar chart with numeric values
train_metrics_radar <- data.frame(
  row.names = "Metrics",
  Accuracy = as.numeric(accuracy),
  Precision = as.numeric(conf_matrix[2,2]/sum(conf_matrix[2,])),
  Sensitivity = as.numeric(sensitivity),
  Specificity = as.numeric(specificity)
)
# Add max and min rows required for radar chart
train_metrics_radar <- rbind(rep(1,4), rep(0,4), train_metrics_radar)

png("plots/performance_metrics.png", width = 800, height = 600)
radarchart(train_metrics_radar,
           pcol = "#4CAF50",
           pfcol = alpha("#4CAF50", 0.5),
           plwd = 2,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey50",
           vlcex = 0.8,
           title = "Model Performance Metrics (Training Set)")
dev.off()

# Create Metrics Distribution Chart - Violin Plot with explicit factor conversion
metrics_dist <- data.frame(
  Probability = as.numeric(train_pred_prob),
  Actual = factor(college_train$Private)
)

p_dist <- ggplot(metrics_dist, aes(x = Actual, y = Probability, fill = Actual)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  labs(title = "Distribution of Prediction Probabilities by Actual Class",
       x = "Actual University Type",
       y = "Predicted Probability of Being Private") +
  theme_minimal() +
  scale_fill_manual(values = c("No" = "#db1717", "Yes" = "#4CAF50")) +
  theme(legend.position = "none")

ggsave("plots/metrics_distribution.png", p_dist, width = 8, height = 6)

# Create Test Performance Metrics Chart - Lollipop Chart with explicit numeric conversion
test_metrics <- data.frame(
  Metric = factor(c("Accuracy", "Precision", "Recall/Sensitivity", "Specificity")),
  Value = as.numeric(c(test_accuracy, test_precision, test_sensitivity, test_specificity))
)

p_test_metrics <- ggplot(test_metrics, aes(x = Metric, y = Value)) +
  geom_segment(aes(x = Metric, xend = Metric, y = 0, yend = Value),
               color = "#db1717", linewidth = 1) +
  geom_point(color = "#db1717", size = 4) +
  geom_text(aes(label = sprintf("%.3f", Value)), vjust = -1) +
  labs(title = "Model Performance Metrics (Test Set)",
       y = "Score",
       x = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1.1)

ggsave("plots/test_performance_metrics.png", p_test_metrics, width = 8, height = 6)