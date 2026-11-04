#Section 13-2

# Question 6 - Game attendance data
attendance <- c(6210, 3150, 2700, 3012, 4875, 
                3540, 6127, 2581, 2642, 2573, 
                2792, 2800, 2500, 3700, 6030, 
                5437, 2758, 3490, 2851, 2720)

# Step 1: State the hypotheses and identify the claim
# H0: The median attendance is 3000 (claim)
# H1: The median attendance is not 3000

# Step 2: Find the critical value
# For a two-tailed test with α = 0.05 and n = 20
# Using the binomial distribution
n <- length(attendance)
alpha <- 0.05
# For a two-tailed test, we need the critical value for α/2 = 0.025
critical_value <- qbinom(alpha/2, n, 0.5)
upper_critical <- n - critical_value

# Step 3: Compute the test value
# Count how many values are less than the claimed median (3000)
less_than_median <- sum(attendance < 3000)
# Count how many values are greater than the claimed median (3000)
greater_than_median <- sum(attendance > 3000)
# Count how many values are equal to the claimed median (3000)
equal_to_median <- sum(attendance == 3000)

# Adjust sample size by removing ties
n_adjusted <- n - equal_to_median
# Test statistic is the smaller of the counts
test_statistic <- min(less_than_median, greater_than_median)

# Step 4: Make the decision
# For the sign test, we reject H0 if the test statistic is less than or equal to the critical value
decision <- if(test_statistic <= critical_value) "Reject H0" else "Fail to reject H0"

# Step 5: Summarize the results
cat("Hypothesis Test for Median Game Attendance\n")
cat("------------------------------------------\n")
cat("H0: Median attendance = 3000 (claim)\n")
cat("H1: Median attendance ≠ 3000\n")
cat("Sample size (n):", n, "\n")
cat("Number less than median:", less_than_median, "\n")
cat("Number greater than median:", greater_than_median, "\n")
cat("Number equal to median:", equal_to_median, "\n")
cat("Adjusted sample size:", n_adjusted, "\n")
cat("Test statistic (smaller count):", test_statistic, "\n")
cat("Critical value at α = 0.05:", critical_value, "\n")
cat("Decision:", decision, "\n\n")

# Calculate the actual median for comparison
actual_median <- median(attendance)
cat("Actual sample median:", actual_median, "\n")

# Create visualizations
# Histogram with median lines
library(ggplot2)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Enhanced histogram with larger font
p1 <- ggplot(data.frame(attendance), aes(x = attendance)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 3000, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = actual_median, color = "blue", linetype = "solid", size = 1) +
  labs(title = "Distribution of Game Attendance",
       subtitle = paste("Red dashed line: Claimed median (3000)\nBlue solid line: Sample median (", 
                        round(actual_median, 2), ")", sep = ""),
       x = "Attendance",
       y = "Frequency") +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

# Save the histogram to the plots folder
ggsave("plots/game_attendance_histogram.png", p1, width = 10, height = 6, dpi = 300)

# Print the histogram
print(p1)

# Additional analysis for the second question
cat("\nAnalysis for Program Printing Decision:\n")
cat("------------------------------------------\n")
if(decision == "Fail to reject H0") {
  cat("Since we failed to reject the null hypothesis, there is not enough evidence to conclude that\n")
  cat("the median attendance is different from 3000. Therefore, using 3000 as a guide for printing\n")
  cat("programs would be reasonable based on this statistical test.\n")
} else {
  cat("Since we rejected the null hypothesis, there is evidence to conclude that the median\n")
  cat("attendance is different from 3000. Therefore, using 3000 as a guide for printing programs\n")
  cat("would not be recommended. Instead, consider using the sample median of", actual_median, "as a guide.\n")
}

# P-value calculation using binomial distribution
if(test_statistic < n_adjusted/2) {
  p_value <- 2 * pbinom(test_statistic, n_adjusted, 0.5)
} else {
  p_value <- 2 * (1 - pbinom(test_statistic - 1, n_adjusted, 0.5))
}
cat("\nP-value:", p_value, "\n")
cat("Significance level (α):", alpha, "\n")

#Section 13-2

# Question 10 - Lottery Ticket Sales
# A lottery outlet owner hypothesizes that she sells 200 lottery tickets a day.
# She randomly sampled 40 days and found that on 15 days she sold fewer than 200 tickets.
# At α = 0.05, is there sufficient evidence to conclude that the median is below 200 tickets?

# Step 1: State the hypotheses and identify the claim
# H0: The median number of tickets sold is 200 or more
# H1: The median number of tickets sold is less than 200 (claim to be tested)

# Step 2: Find the critical value
# For a one-tailed test with α = 0.05 and n = 40
n <- 40
alpha <- 0.05
# For a one-tailed test, we need the critical value for α = 0.05
critical_value <- qbinom(alpha, n, 0.5)

# Step 3: Compute the test value
# We know that on 15 days, fewer than 200 tickets were sold
# This is our test statistic
test_statistic <- 15

# Step 4: Make the decision
# For the sign test with a left-tailed test, we reject H0 if the test statistic is less than or equal to the critical value
decision <- if(test_statistic <= critical_value) "Reject H0" else "Fail to reject H0"

# Step 5: Summarize the results
cat("Hypothesis Test for Median Lottery Ticket Sales\n")
cat("-----------------------------------------------\n")
cat("H0: Median ticket sales ≥ 200\n")
cat("H1: Median ticket sales < 200 (claim)\n")
cat("Sample size (n):", n, "\n")
cat("Number of days with sales < 200:", test_statistic, "\n")
cat("Critical value at α = 0.05:", critical_value, "\n")
cat("Decision:", decision, "\n\n")

# P-value calculation using binomial distribution
p_value <- pbinom(test_statistic, n, 0.5)
cat("P-value:", p_value, "\n")
cat("Significance level (α):", alpha, "\n")

# Create visualization
library(ggplot2)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Create data for visualization
days <- 1:40
sales_category <- rep(c("Below 200", "200 or above"), c(15, 25))
sales_data <- data.frame(Day = days, Category = sales_category)

# Create bar plot showing the distribution of days
p <- ggplot(sales_data, aes(x = Category, fill = Category)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 6) +
  scale_fill_manual(values = c("Below 200" = "coral", "200 or above" = "skyblue")) +
  labs(
    title = "Lottery Ticket Sales Distribution",
    subtitle = paste("Days with sales below 200 tickets:", test_statistic, "out of", n, "days"),
    x = "Sales Category",
    y = "Number of Days"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none"
  )

# Save the plot to the plots folder
ggsave("plots/lottery_ticket_sales.png", p, width = 10, height = 6, dpi = 300)

# Print the plot
print(p)

# Conclusion
cat("\nConclusion:\n")
cat("-----------------------------------------------\n")
if(decision == "Reject H0") {
  cat("Since we rejected the null hypothesis, there is sufficient evidence to conclude that\n")
  cat("the median number of lottery tickets sold per day is less than 200.\n")
} else {
  cat("Since we failed to reject the null hypothesis, there is not sufficient evidence to conclude that\n")
  cat("the median number of lottery tickets sold per day is less than 200.\n")
}

#Section 13-3

# Question 8 - Winning Baseball Games
# For the years 1970–1993 the National League (NL) and the American League (AL) (major league baseball) 
# were each divided into two divisions: East and West.
# At α = 0.05, is there sufficient evidence to conclude a difference in the number of wins?

# Step 1: State the hypotheses and identify the claim
# H0: There is no difference in the number of wins between NL and AL Eastern Divisions
# H1: There is a difference in the number of wins between NL and AL Eastern Divisions

# Input the data
NL <- c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
AL <- c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

# Step 2: Find the critical value
# For a two-tailed test with α = 0.05
alpha <- 0.05
# Using the qnorm function to get the z-critical value
z_critical <- qnorm(1 - alpha/2)
cat("Z-critical value at α =", alpha, ":", z_critical, "\n\n")

# Step 3: Compute the test value
# Perform the Wilcoxon rank sum test
wilcox_test <- wilcox.test(NL, AL, alternative = "two.sided", exact = FALSE, correct = TRUE)

# Extract the test statistic and p-value
w_statistic <- wilcox_test$statistic
p_value <- wilcox_test$p.value

# Calculate the z-value (standardized test statistic)
n1 <- length(NL)
n2 <- length(AL)
mean_w <- n1 * n2 / 2
sd_w <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
z_value <- (w_statistic - mean_w) / sd_w

# Step 4: Make the decision
decision <- if(abs(z_value) > z_critical || p_value < alpha) "Reject H0" else "Fail to reject H0"

# Step 5: Summarize the results
cat("Wilcoxon Rank Sum Test for Baseball Wins\n")
cat("--------------------------------------------------\n")
cat("H0: There is no difference in the number of wins between NL and AL Eastern Divisions\n")
cat("H1: There is a difference in the number of wins between NL and AL Eastern Divisions\n\n")
cat("Sample sizes:\n")
cat("  National League (n1):", n1, "\n")
cat("  American League (n2):", n2, "\n\n")
cat("Test Results:\n")
cat("  W statistic:", w_statistic, "\n")
cat("  Z value:", z_value, "\n")
cat("  Z critical value (two-tailed):", z_critical, "\n")
cat("  P-value:", p_value, "\n")
cat("  Significance level (α):", alpha, "\n\n")
cat("Decision:", decision, "\n\n")

# Create visualization
library(ggplot2)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Combine data for visualization
combined_data <- data.frame(
  Wins = c(NL, AL),
  League = factor(c(rep("National League", length(NL)), rep("American League", length(AL))))
)

# Calculate summary statistics
NL_mean <- mean(NL)
AL_mean <- mean(AL)
NL_median <- median(NL)
AL_median <- median(AL)

# Create boxplot comparing wins by league
p <- ggplot(combined_data, aes(x = League, y = Wins, fill = League)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "red") +
  scale_fill_manual(values = c("National League" = "darkred", "American League" = "navy")) +
  labs(
    title = "Baseball Wins by League (Eastern Division, 1970-1993)",
    subtitle = paste("Wilcoxon Rank Sum Test: p-value =", round(p_value, 4)),
    x = "League",
    y = "Number of Wins"
  ) +
  annotate("text", x = 1, y = max(NL) + 2, 
           label = paste("Mean:", round(NL_mean, 1), "\nMedian:", NL_median), 
           size = 5) +
  annotate("text", x = 2, y = max(AL) + 2, 
           label = paste("Mean:", round(AL_mean, 1), "\nMedian:", AL_median), 
           size = 5) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none"
  )

# Save the plot to the plots folder
ggsave("plots/baseball_wins.png", p, width = 10, height = 6, dpi = 300)

# Print the plot
print(p)

# Conclusion
cat("Conclusion:\n")
cat("--------------------------------------------------\n")
if(decision == "Reject H0") {
  cat("Since we rejected the null hypothesis, there is sufficient evidence to conclude that\n")
  cat("there is a difference in the number of wins between the National League and American League\n")
  cat("Eastern Divisions for the years 1970-1993.\n")
} else {
  cat("Since we failed to reject the null hypothesis, there is not sufficient evidence to conclude that\n")
  cat("there is a difference in the number of wins between the National League and American League\n")
  cat("Eastern Divisions for the years 1970-1993.\n")
}
#Section 13-3

# Question 4 - Lengths of Prison Sentences
# A random sample of men and women in prison was asked to give the length of sentence each received for a certain type of crime.
# At α = 0.05, test the claim that there is no difference in the sentence received by each gender.

# Step 1: State the hypotheses and identify the claim
# H0: There is no difference in the sentence lengths between males and females (claim)
# H1: There is a difference in the sentence lengths between males and females

# Input the data
males <- c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
females <- c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

# Step 2: Find the critical value
# For a two-tailed test with α = 0.05
alpha <- 0.05
# Using the wilcox.test function to get the critical value
# We can use the qnorm function to get the z-critical value
z_critical <- qnorm(1 - alpha/2)
cat("Z-critical value at α =", alpha, ":", z_critical, "\n\n")

# Step 3: Compute the test value
# Perform the Wilcoxon rank sum test
wilcox_test <- wilcox.test(males, females, alternative = "two.sided", exact = FALSE, correct = TRUE)

# Extract the test statistic and p-value
w_statistic <- wilcox_test$statistic
p_value <- wilcox_test$p.value

# Calculate the z-value (standardized test statistic)
n1 <- length(males)
n2 <- length(females)
mean_w <- n1 * n2 / 2
sd_w <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
z_value <- (w_statistic - mean_w) / sd_w

# Step 4: Make the decision
decision <- if(abs(z_value) > z_critical || p_value < alpha) "Reject H0" else "Fail to reject H0"

# Step 5: Summarize the results
cat("Wilcoxon Rank Sum Test for Prison Sentence Lengths\n")
cat("--------------------------------------------------\n")
cat("H0: There is no difference in the sentence lengths between males and females (claim)\n")
cat("H1: There is a difference in the sentence lengths between males and females\n\n")
cat("Sample sizes:\n")
cat("  Males (n1):", n1, "\n")
cat("  Females (n2):", n2, "\n\n")
cat("Test Results:\n")
cat("  W statistic:", w_statistic, "\n")
cat("  Z value:", z_value, "\n")
cat("  Z critical value (two-tailed):", z_critical, "\n")
cat("  P-value:", p_value, "\n")
cat("  Significance level (α):", alpha, "\n\n")
cat("Decision:", decision, "\n\n")

# Create visualization
library(ggplot2)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Combine data for visualization
combined_data <- data.frame(
  Sentence = c(males, females),
  Gender = factor(c(rep("Male", length(males)), rep("Female", length(females))))
)

# Calculate summary statistics
male_mean <- mean(males)
female_mean <- mean(females)
male_median <- median(males)
female_median <- median(females)

# Create boxplot comparing sentence lengths by gender
p <- ggplot(combined_data, aes(x = Gender, y = Sentence, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "red") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "coral")) +
  labs(
    title = "Prison Sentence Lengths by Gender",
    subtitle = paste("Wilcoxon Rank Sum Test: p-value =", round(p_value, 4)),
    x = "Gender",
    y = "Sentence Length (months)"
  ) +
  annotate("text", x = 1, y = max(males) + 2, 
           label = paste("Mean:", round(male_mean, 1), "\nMedian:", male_median), 
           size = 5) +
  annotate("text", x = 2, y = max(females) + 2, 
           label = paste("Mean:", round(female_mean, 1), "\nMedian:", female_median), 
           size = 5) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none"
  )

# Save the plot to the plots folder
ggsave("plots/prison_sentence_lengths.png", p, width = 10, height = 6, dpi = 300)

# Print the plot
print(p)

# Conclusion
cat("Conclusion:\n")
cat("--------------------------------------------------\n")
if(decision == "Reject H0") {
  cat("Since we rejected the null hypothesis, there is sufficient evidence to conclude that\n")
  cat("there is a difference in the sentence lengths between males and females for this type of crime.\n")
} else {
  cat("Since we failed to reject the null hypothesis, there is not sufficient evidence to conclude that\n")
  cat("there is a difference in the sentence lengths between males and females for this type of crime.\n")
  cat("The claim that there is no difference in sentence lengths between genders is supported by the data.\n")
}



#Section 13-4
# Use Table K to determine whether the null hypothesis should be rejected.

# Create a table with critical values for the Wilcoxon signed-rank test
wilcoxon_table <- data.frame(
  n = c(5:30),
  alpha_0.01_two = c(0, 0, 0, 0, 1, 1, 2, 3, 5, 6, 7, 9, 10, 12, 14, 16, 18, 20, 23, 25, 28, 30, 33, 36, 39, 42),
  alpha_0.025_two = c(0, 0, 2, 3, 5, 6, 8, 10, 13, 15, 17, 20, 23, 26, 29, 33, 36, 40, 43, 47, 52, 56, 60, 65, 69, 74),
  alpha_0.05_two = c(0, 2, 3, 5, 8, 10, 13, 16, 20, 23, 27, 31, 36, 40, 45, 50, 55, 61, 67, 73, 79, 85, 92, 98, 105, 112),
  alpha_0.10_two = c(1, 3, 5, 8, 11, 14, 18, 22, 27, 32, 37, 43, 49, 55, 62, 69, 76, 83, 91, 99, 107, 116, 124, 133, 143, 152),
  alpha_0.01_one = c(0, 0, 0, 1, 2, 3, 5, 6, 8, 10, 12, 14, 16, 19, 21, 24, 27, 30, 33, 36, 39, 42, 46, 49, 53, 57),
  alpha_0.025_one = c(0, 1, 2, 4, 6, 8, 11, 14, 17, 20, 24, 27, 31, 35, 39, 44, 48, 53, 58, 63, 68, 73, 79, 84, 90, 96),
  alpha_0.05_one = c(0, 2, 3, 5, 8, 10, 13, 17, 21, 25, 30, 35, 40, 45, 51, 57, 63, 69, 76, 83, 90, 97, 105, 113, 120, 128),
  alpha_0.10_one = c(1, 3, 6, 9, 13, 17, 21, 26, 31, 37, 42, 48, 55, 62, 69, 77, 85, 93, 102, 110, 119, 128, 138, 147, 157, 167)
)

# Function to get critical value from the table
get_critical_value <- function(n, alpha, tailed) {
  # Check if n is within the range of our table
  if (n < 5 || n > 30) {
    return(NA)
  }
  
  # Determine which column to use
  col_name <- paste0("alpha_", alpha, "_", ifelse(tailed == "two-tailed", "two", "one"))
  
  # Check if the column exists
  if (!(col_name %in% names(wilcoxon_table))) {
    return(NA)
  }
  
  # Get the row index for the given n
  # Adjust for the offset since our table starts at n=5
  row_idx <- n - 4
  
  # Return the critical value
  if (row_idx > 0 && row_idx <= nrow(wilcoxon_table)) {
    return(as.numeric(wilcoxon_table[row_idx, col_name]))
  } else {
    return(NA)
  }
}

# Function to make decision based on test statistic and critical value
make_decision <- function(ws, critical_value) {
  if (is.na(critical_value)) {
    return("Cannot determine (n outside table range or critical value not found)")
  }
  
  if (ws <= critical_value) {
    return("Reject H0")
  } else {
    return("Fail to reject H0")
  }
}

# Create a data frame to store all scenarios
scenarios <- data.frame(
  scenario = c(1, 2, 3, 4),
  ws = c(13, 32, 65, 22),
  n = c(15, 28, 20, 14),
  alpha = c(0.01, 0.025, 0.05, 0.10),
  tailed = c("two-tailed", "one-tailed", "one-tailed", "two-tailed"),
  stringsAsFactors = FALSE
)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Create a function to analyze and visualize each scenario
analyze_scenario <- function(scenario_num) {
  # Get scenario data
  scenario <- scenarios[scenario_num, ]
  ws <- scenario$ws
  n <- scenario$n
  alpha <- scenario$alpha
  tailed <- scenario$tailed
  
  # Get critical value
  critical_value <- get_critical_value(n, alpha, tailed)
  
  # Make decision
  decision <- make_decision(ws, critical_value)
  
  # Print results
  cat(paste0("Scenario ", scenario_num, ":\n"))
  cat("----------------------------------\n")
  cat("Test statistic (W_s):", ws, "\n")
  cat("Sample size (n):", n, "\n")
  cat("Significance level (α):", alpha, "\n")
  cat("Test type:", tailed, "\n")
  
  if (is.na(critical_value)) {
    cat("Critical value: Not available in table\n")
  } else {
    cat("Critical value:", critical_value, "\n")
  }
  
  cat("Decision:", decision, "\n\n")
  
  # Create visualization if critical value is available
  if (!is.na(critical_value)) {
    library(ggplot2)
    
    # Create data for visualization
    plot_data <- data.frame(
      x = factor(c("Test Statistic", "Critical Value"), 
                 levels = c("Test Statistic", "Critical Value")),
      y = c(ws, critical_value),
      color = c("Test Statistic", "Critical Value")
    )
    
    # Create horizontal bar chart
    p <- ggplot(plot_data, aes(x = x, y = y, fill = color)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = y), hjust = -0.2, size = 5) +
      scale_fill_manual(values = c("Test Statistic" = "steelblue", "Critical Value" = "coral")) +
      coord_flip() +
      labs(
        title = paste0("Scenario ", scenario_num, ": Wilcoxon Signed-Rank Test"),
        subtitle = paste0("α = ", alpha, ", ", tailed, "\nDecision: ", decision),
        x = "",
        y = "Value"
      ) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      ylim(0, max(ws, critical_value) * 1.2)
    
    # Save the plot
    plot_filename <- paste0("plots/wilcoxon_scenario_", scenario_num, ".png")
    ggsave(plot_filename, p, width = 8, height = 5, dpi = 300)
    
    # Print the plot
    print(p)
    
    return(list(decision = decision, plot = p))
  } else {
    return(list(decision = decision, plot = NULL))
  }
}

# Analyze all scenarios
cat("Section 13-4 Wilcoxon Signed-Rank Test Analysis\n")
cat("==============================================\n\n")

results <- list()
for (i in 1:nrow(scenarios)) {
  results[[i]] <- analyze_scenario(i)
}

# Create a combined visualization for all scenarios
library(ggplot2)
library(gridExtra)

# Filter out NULL plots
valid_plots <- Filter(Negate(is.null), lapply(results, function(x) x$plot))

if (length(valid_plots) > 0) {
  # Arrange plots in a grid
  combined_plot <- do.call(grid.arrange, c(valid_plots, ncol = 1))
  
  # Save the combined plot
  ggsave("plots/wilcoxon_all_scenarios.png", combined_plot, width = 10, height = 15, dpi = 300)
}

# Summary of all scenarios
cat("\nSummary of All Scenarios\n")
cat("======================\n")
for (i in 1:nrow(scenarios)) {
  scenario <- scenarios[i, ]
  cat(paste0("Scenario ", i, " (ws = ", scenario$ws, 
             ", n = ", scenario$n, 
             ", α = ", scenario$alpha, 
             ", ", scenario$tailed, "): ", 
             results[[i]]$decision, "\n"))
}
#Section 13-5
# Question 2 - Mathematics Literacy Scores
# Through the Organization for Economic Cooperation and Development (OECD), 15-year-olds are tested in member countries
# in mathematics, reading, and science literacy. Listed are randomly selected total mathematics literacy scores
# (i.e., both genders) for selected countries in different parts of the world.

# Input the data
western_hemisphere <- c(527, 406, 474, 381, 411)
europe <- c(520, 510, 513, 548, 496)
eastern_asia <- c(523, 547, 547, 391, 549)

# Step 1: State the hypotheses
# H0: There is no difference in the mean mathematics literacy scores among the three regions
# H1: There is a difference in the mean mathematics literacy scores among at least two of the regions

# Combine all data and create a factor for the groups
all_scores <- c(western_hemisphere, europe, eastern_asia)
groups <- factor(rep(c("Western Hemisphere", "Europe", "Eastern Asia"), each = 5))
data <- data.frame(Score = all_scores, Region = groups)

# Step 2: Find the critical value
# For Kruskal-Wallis test with df = k - 1 = 3 - 1 = 2 and α = 0.05
alpha <- 0.05
df <- length(unique(groups)) - 1
critical_value <- qchisq(1 - alpha, df)

# Step 3: Compute the test value
# Perform the Kruskal-Wallis test
kruskal_test <- kruskal.test(Score ~ Region, data = data)
h_statistic <- kruskal_test$statistic
p_value <- kruskal_test$p.value

# Step 4: Make the decision
decision <- if(h_statistic > critical_value || p_value < alpha) "Reject H0" else "Fail to reject H0"

# Step 5: Summarize the results
cat("Kruskal-Wallis Test for Mathematics Literacy Scores\n")
cat("--------------------------------------------------\n")
cat("H0: There is no difference in the mean mathematics literacy scores among the three regions\n")
cat("H1: There is a difference in the mean mathematics literacy scores among at least two of the regions\n\n")
cat("Sample sizes:\n")
cat("  Western Hemisphere:", length(western_hemisphere), "\n")
cat("  Europe:", length(europe), "\n")
cat("  Eastern Asia:", length(eastern_asia), "\n\n")
cat("Test Results:\n")
cat("  H statistic:", h_statistic, "\n")
cat("  Degrees of freedom:", df, "\n")
cat("  Critical value (χ²):", critical_value, "\n")
cat("  P-value:", p_value, "\n")
cat("  Significance level (α):", alpha, "\n\n")
cat("Decision:", decision, "\n\n")

# Create visualizations
library(ggplot2)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Calculate summary statistics
region_means <- tapply(data$Score, data$Region, mean)
region_medians <- tapply(data$Score, data$Region, median)

# Create boxplot comparing scores by region
p1 <- ggplot(data, aes(x = Region, y = Score, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "red") +
  scale_fill_manual(values = c("Western Hemisphere" = "skyblue", 
                              "Europe" = "lightgreen", 
                              "Eastern Asia" = "coral")) +
  labs(
    title = "Mathematics Literacy Scores by Region",
    subtitle = paste("Kruskal-Wallis Test: p-value =", round(p_value, 4)),
    x = "Region",
    y = "Mathematics Literacy Score"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none"
  )

# Add mean and median annotations
for (i in 1:length(unique(data$Region))) {
  region <- unique(data$Region)[i]
  p1 <- p1 + annotate("text", x = i, y = max(data$Score[data$Region == region]) + 15, 
                     label = paste("Mean:", round(region_means[region], 1), 
                                  "\nMedian:", region_medians[region]), 
                     size = 4)
}

# Save the boxplot to the plots folder
ggsave("plots/math_literacy_boxplot.png", p1, width = 10, height = 7, dpi = 300)

# Create a second visualization: mean scores with error bars
region_sd <- tapply(data$Score, data$Region, sd)
summary_data <- data.frame(
  Region = names(region_means),
  Mean = as.numeric(region_means),
  SD = as.numeric(region_sd)
)

p2 <- ggplot(summary_data, aes(x = Region, y = Mean, fill = Region)) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  geom_text(aes(label = round(Mean, 1)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Western Hemisphere" = "skyblue", 
                              "Europe" = "lightgreen", 
                              "Eastern Asia" = "coral")) +
  labs(
    title = "Mean Mathematics Literacy Scores by Region",
    subtitle = paste("Kruskal-Wallis Test: H =", round(h_statistic, 2), 
                    ", Critical χ² =", round(critical_value, 2)),
    x = "Region",
    y = "Mean Score"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none"
  ) +
  ylim(0, max(summary_data$Mean + summary_data$SD) * 1.1)

# Save the bar chart to the plots folder
ggsave("plots/math_literacy_means.png", p2, width = 10, height = 7, dpi = 300)

# Print the plots
print(p1)
print(p2)

# Conclusion
cat("Conclusion:\n")
cat("--------------------------------------------------\n")
if(decision == "Reject H0") {
  cat("Since we rejected the null hypothesis, there is sufficient evidence to conclude that\n")
  cat("there is a difference in the mean mathematics literacy scores among the three regions.\n")
  cat("Further post-hoc tests would be needed to determine which specific regions differ from each other.\n")
} else {
  cat("Since we failed to reject the null hypothesis, there is not sufficient evidence to conclude that\n")
  cat("there is a difference in the mean mathematics literacy scores among the three regions.\n")
}

# If we rejected H0, perform post-hoc pairwise comparisons
if(decision == "Reject H0") {
  cat("\nPost-hoc Analysis:\n")
  cat("--------------------------------------------------\n")
  
  # Perform pairwise Wilcoxon rank sum tests with Bonferroni correction
  pairwise_tests <- pairwise.wilcox.test(data$Score, data$Region, 
                                        p.adjust.method = "bonferroni")
  
  # Print the results
  print(pairwise_tests)
  
  # Interpret the results
  cat("\nInterpretation of Post-hoc Tests:\n")
  cat("The p-values from the pairwise comparisons indicate which specific regions\n")
  cat("have significantly different mathematics literacy scores from each other.\n")
  cat("P-values less than", alpha, "indicate significant differences between regions.\n")
}
#Section 13-6
# Subway and Commuter Rail Passengers Analysis
# Six cities are randomly selected, and the number of daily passenger trips (in thousands) 
# for subways and commuter rail service is obtained.

# Input the data
city <- 1:6
subway <- c(845, 494, 425, 313, 108, 41)
rail <- c(39, 291, 142, 103, 33, 38)

# Create a data frame
transportation_data <- data.frame(
  City = city,
  Subway = subway,
  Rail = rail
)

# Step 1: Find the Spearman rank correlation coefficient
spearman_corr <- cor(subway, rail, method = "spearman")

# Step 2: State the hypotheses
# H0: There is no correlation between subway and rail passenger trips (ρs = 0)
# H1: There is a correlation between subway and rail passenger trips (ρs ≠ 0)

# Step 3: Find the critical value
# For a two-tailed test with α = 0.05 and n = 6
alpha <- 0.05
n <- length(subway)
df <- n - 2
critical_value <- qt(1 - alpha/2, df)

# Calculate the test statistic
t_statistic <- spearman_corr * sqrt(df) / sqrt(1 - spearman_corr^2)

# Calculate the p-value
p_value <- 2 * pt(-abs(t_statistic), df)

# Step 4: Make the decision
decision <- if(abs(t_statistic) > critical_value || p_value < alpha) "Reject H0" else "Fail to reject H0"

# Step 5: Summarize the results
cat("Spearman Rank Correlation Analysis for Subway and Rail Passengers\n")
cat("--------------------------------------------------------------\n")
cat("H0: There is no correlation between subway and rail passenger trips (ρs = 0)\n")
cat("H1: There is a correlation between subway and rail passenger trips (ρs ≠ 0)\n\n")
cat("Sample size (n):", n, "\n")
cat("Spearman rank correlation coefficient (ρs):", round(spearman_corr, 4), "\n")
cat("t-statistic:", round(t_statistic, 4), "\n")
cat("Degrees of freedom:", df, "\n")
cat("Critical t-value (two-tailed):", round(critical_value, 4), "\n")
cat("P-value:", round(p_value, 4), "\n")
cat("Significance level (α):", alpha, "\n\n")
cat("Decision:", decision, "\n\n")

# Create visualizations
library(ggplot2)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# 1. Scatter plot with regression line
p1 <- ggplot(transportation_data, aes(x = Subway, y = Rail)) +
  geom_point(size = 4, color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  geom_text(aes(label = paste("City", City)), vjust = -0.8, hjust = 0.5, size = 4) +
  labs(
    title = "Relationship Between Subway and Rail Passenger Trips",
    subtitle = paste("Spearman Rank Correlation: ρs =", round(spearman_corr, 3)),
    x = "Subway Passengers (thousands per day)",
    y = "Rail Passengers (thousands per day)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

# Save the scatter plot
ggsave("plots/subway_rail_scatter.png", p1, width = 10, height = 7, dpi = 300)

# 2. Rank scatter plot
# Calculate ranks
subway_rank <- rank(-subway)  # Negative to rank in descending order
rail_rank <- rank(-rail)      # Negative to rank in descending order

rank_data <- data.frame(
  City = city,
  SubwayRank = subway_rank,
  RailRank = rail_rank
)

p2 <- ggplot(rank_data, aes(x = SubwayRank, y = RailRank)) +
  geom_point(size = 4, color = "purple", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "green", linetype = "dashed") +
  geom_text(aes(label = paste("City", City)), vjust = -0.8, hjust = 0.5, size = 4) +
  labs(
    title = "Relationship Between Subway and Rail Passenger Ranks",
    subtitle = paste("Spearman Rank Correlation: ρs =", round(spearman_corr, 3)),
    x = "Subway Passenger Rank",
    y = "Rail Passenger Rank"
  ) +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(breaks = 1:6) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

# Save the rank scatter plot
ggsave("plots/subway_rail_rank_scatter.png", p2, width = 10, height = 7, dpi = 300)

# Print the plots
print(p1)
print(p2)

# Conclusion
cat("Conclusion:\n")
cat("--------------------------------------------------------------\n")
if(decision == "Reject H0") {
  cat("Since we rejected the null hypothesis, there is sufficient evidence to conclude that\n")
  cat("there is a correlation between subway and rail passenger trips in these cities.\n")
  cat("The Spearman rank correlation coefficient of", round(spearman_corr, 4), "indicates a")
  if(spearman_corr > 0) {
    cat(" positive")
  } else {
    cat(" negative")
  }
  cat(" correlation.\n\n")
} else {
  cat("Since we failed to reject the null hypothesis, there is not sufficient evidence to conclude that\n")
  cat("there is a correlation between subway and rail passenger trips in these cities.\n\n")
}

# Practical application
cat("Practical Application for Transportation Authority:\n")
cat("--------------------------------------------------------------\n")
cat("The transportation authority could use these results to:\n")
if(decision == "Reject H0" && spearman_corr > 0) {
  cat("1. Coordinate scheduling between subway and rail services since higher subway\n")
  cat("   ridership is associated with higher rail ridership, suggesting many commuters\n")
  cat("   may use both services as part of their journey.\n")
  cat("2. Allocate resources proportionally across both transportation modes in cities\n")
  cat("   where demand is high for both services.\n")
  cat("3. Develop integrated fare systems that encourage the use of both transportation\n")
  cat("   modes, potentially increasing overall public transportation usage.\n")
} else if(decision == "Reject H0" && spearman_corr < 0) {
  cat("1. Recognize that subway and rail services may be serving different populations\n")
  cat("   or meeting different transportation needs in these cities.\n")
  cat("2. Consider reallocating resources from one mode to the other in cities where\n")
  cat("   one mode is significantly more popular than the other.\n")
  cat("3. Investigate why the negative correlation exists and develop strategies to\n")
  cat("   better integrate the two transportation systems.\n")
} else {
  cat("1. Recognize that subway and rail services may be operating independently and\n")
  cat("   serving different transportation needs or populations.\n")
  cat("2. Develop separate strategies for improving each transportation mode rather\n")
  cat("   than assuming changes in one will affect the other.\n")
}
#Section 14-3: Simulation of Prize Collection Problem

# Question 16 - Prizes in Caramel Corn Boxes
# A caramel corn company gives four different prizes, one in each box. 
# They are placed in the boxes at random. 
# Find the average number of boxes a person needs to buy to get all four prizes. (40)

# Set seed for reproducibility
set.seed(123)

# Number of simulations
num_simulations <- 40

# Number of different prizes
num_prizes <- 4

# Function to simulate one collection process
simulate_collection <- function() {
  # Initialize empty collection
  collected_prizes <- c()
  # Count boxes purchased
  boxes_purchased <- 0
  
  # Continue until all prizes are collected
  while(length(unique(collected_prizes)) < num_prizes) {
    # Buy a new box (get a random prize from 1 to 4)
    new_prize <- sample(1:num_prizes, 1)
    # Add to collection
    collected_prizes <- c(collected_prizes, new_prize)
    # Increment counter
    boxes_purchased <- boxes_purchased + 1
  }
  
  return(boxes_purchased)
}

# Run the simulation multiple times
results <- replicate(num_simulations, simulate_collection())

# Calculate the average number of boxes needed
average_boxes <- mean(results)
median_boxes <- median(results)
min_boxes <- min(results)
max_boxes <- max(results)

# Print the results
cat("Simulation of Prize Collection Problem\n")
cat("-------------------------------------\n")
cat("Number of simulations:", num_simulations, "\n")
cat("Number of different prizes:", num_prizes, "\n\n")
cat("Results:\n")
cat("Average number of boxes needed:", round(average_boxes, 2), "\n")
cat("Median number of boxes needed:", median_boxes, "\n")
cat("Minimum number of boxes needed:", min_boxes, "\n")
cat("Maximum number of boxes needed:", max_boxes, "\n")

# Create visualizations
library(ggplot2)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Create a data frame for visualization
results_df <- data.frame(Simulation = 1:num_simulations, Boxes = results)

# 1. Histogram of the number of boxes needed
p1 <- ggplot(results_df, aes(x = Boxes)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = average_boxes, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_boxes, color = "blue", linetype = "solid", size = 1) +
  labs(
    title = "Distribution of Boxes Needed to Collect All Prizes",
    subtitle = paste("Average =", round(average_boxes, 2), "boxes (red dashed line)\nMedian =", median_boxes, "boxes (blue solid line)"),
    x = "Number of Boxes",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

# Save the histogram
ggsave("plots/prize_collection_histogram.png", p1, width = 10, height = 6, dpi = 300)

# 2. Bar chart showing the results of each simulation
p2 <- ggplot(results_df, aes(x = Simulation, y = Boxes)) +
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  geom_hline(yintercept = average_boxes, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Number of Boxes Needed in Each Simulation",
    subtitle = paste("Average =", round(average_boxes, 2), "boxes (red dashed line)"),
    x = "Simulation Number",
    y = "Number of Boxes"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

# Save the bar chart
ggsave("plots/prize_collection_simulations.png", p2, width = 12, height = 6, dpi = 300)

# Print the plots
print(p1)
print(p2)

# Theoretical calculation
# The expected number of boxes to collect all prizes can be calculated as:
# E[X] = n * (1/1 + 1/2 + 1/3 + ... + 1/n) where n is the number of different prizes
theoretical_average <- num_prizes * sum(1 / (1:num_prizes))
cat("\nTheoretical expected number of boxes:", round(theoretical_average, 2), "\n")

# Comparison with simulation results
cat("Difference between simulation and theoretical:", round(abs(average_boxes - theoretical_average), 2), "\n")

# Additional analysis: Probability of getting all prizes within a certain number of boxes
probability_analysis <- function(max_boxes) {
  count <- sum(results <= max_boxes)
  probability <- count / num_simulations
  return(data.frame(MaxBoxes = max_boxes, Count = count, Probability = probability))
}

# Calculate probabilities for different numbers of boxes
prob_data <- do.call(rbind, lapply(min_boxes:max_boxes, probability_analysis))

# Create a probability plot
p3 <- ggplot(prob_data, aes(x = MaxBoxes, y = Probability)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 3) +
  labs(
    title = "Probability of Collecting All Prizes Within a Given Number of Boxes",
    x = "Maximum Number of Boxes",
    y = "Probability"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# Save the probability plot
ggsave("plots/prize_collection_probability.png", p3, width = 10, height = 6, dpi = 300)

# Print the probability plot
print(p3)


#Section 14-3
# Question 18 - Lottery Winner
# To win a certain lotto, a person must spell the word big.
# Sixty percent of the tickets contain the letter b, 30% contain the letter i,
# and 10% contain the letter g.
# Find the average number of tickets a person must buy to win the prize. (30)

# Set seed for reproducibility
set.seed(456)

# Number of simulations
num_simulations <- 30

# Function to simulate one lottery win
simulate_lottery_win <- function() {
  # Initialize collection of letters
  has_b <- FALSE
  has_i <- FALSE
  has_g <- FALSE
  
  # Count tickets purchased
  tickets_purchased <- 0
  
  # Continue until all letters are collected
  while(!(has_b && has_i && has_g)) {
    # Buy a new ticket
    tickets_purchased <- tickets_purchased + 1
    
    # Generate a random number to determine which letter (if any) is on the ticket
    random_value <- runif(1)
    
    if(random_value <= 0.6) {
      # 60% chance of getting 'b'
      has_b <- TRUE
    } else if(random_value <= 0.9) {
      # 30% chance of getting 'i'
      has_i <- TRUE
    } else {
      # 10% chance of getting 'g'
      has_g <- TRUE
    }
  }
  
  return(tickets_purchased)
}

# Run the simulation multiple times
results <- replicate(num_simulations, simulate_lottery_win())

# Calculate statistics
average_tickets <- mean(results)
median_tickets <- median(results)
min_tickets <- min(results)
max_tickets <- max(results)
sd_tickets <- sd(results)

# Print the results
cat("Lottery Winner Simulation\n")
cat("------------------------\n")
cat("Number of simulations:", num_simulations, "\n")
cat("Letter probabilities: b (60%), i (30%), g (10%)\n\n")
cat("Results:\n")
cat("Average number of tickets needed:", round(average_tickets, 2), "\n")
cat("Median number of tickets needed:", median_tickets, "\n")
cat("Minimum number of tickets needed:", min_tickets, "\n")
cat("Maximum number of tickets needed:", max_tickets, "\n")
cat("Standard deviation:", round(sd_tickets, 2), "\n")

# Create visualizations
library(ggplot2)

# Create plots directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Create a data frame for visualization
results_df <- data.frame(Simulation = 1:num_simulations, Tickets = results)

# 1. Histogram of the number of tickets needed
p1 <- ggplot(results_df, aes(x = Tickets)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_vline(xintercept = average_tickets, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_tickets, color = "blue", linetype = "solid", size = 1) +
  labs(
    title = "Distribution of Tickets Needed to Win Lottery",
    subtitle = paste("Average =", round(average_tickets, 2), "tickets (red dashed line)\nMedian =", median_tickets, "tickets (blue solid line)"),
    x = "Number of Tickets",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

# Save the histogram
ggsave("plots/lottery_winner_histogram.png", p1, width = 10, height = 6, dpi = 300)

# 2. Bar chart showing the results of each simulation
p2 <- ggplot(results_df, aes(x = Simulation, y = Tickets)) +
  geom_bar(stat = "identity", fill = "lightblue", alpha = 0.7) +
  geom_hline(yintercept = average_tickets, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Number of Tickets Needed in Each Simulation",
    subtitle = paste("Average =", round(average_tickets, 2), "tickets (red dashed line)"),
    x = "Simulation Number",
    y = "Number of Tickets"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

# Save the bar chart
ggsave("plots/lottery_winner_simulations.png", p2, width = 12, height = 6, dpi = 300)

# 3. Pie chart showing the distribution of letters
letter_probs <- data.frame(
  Letter = c("b", "i", "g"),
  Probability = c(0.6, 0.3, 0.1)
)

p3 <- ggplot(letter_probs, aes(x = "", y = Probability, fill = Letter)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("b" = "skyblue", "i" = "lightgreen", "g" = "coral")) +
  labs(
    title = "Distribution of Letters in Lottery Tickets",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, face = "bold")
  ) +
  geom_text(aes(label = paste0(Letter, " (", Probability*100, "%)")), 
            position = position_stack(vjust = 0.5), size = 5)

# Save the pie chart
ggsave("plots/lottery_letter_distribution.png", p3, width = 8, height = 6, dpi = 300)

# Print the plots
print(p1)
print(p2)
print(p3)

# Theoretical calculation
# The expected number of tickets to get at least one of each letter follows
# a different distribution than the coupon collector's problem because
# each ticket can only have one letter.

# For this specific problem, we can calculate the expected value using
# the geometric distribution for each letter.
# E[X] = 1/p_b + 1/p_i + 1/p_g = 1/0.6 + 1/0.3 + 1/0.1 = 1.67 + 3.33 + 10 = 15

theoretical_average <- 1/0.6 + 1/0.3 + 1/0.1
cat("\nTheoretical expected number of tickets:", round(theoretical_average, 2), "\n")

# Comparison with simulation results
cat("Difference between simulation and theoretical:", 
    round(abs(average_tickets - theoretical_average), 2), "\n")

# Additional analysis: Probability of winning within a certain number of tickets
probability_analysis <- function(max_tickets) {
  count <- sum(results <= max_tickets)
  probability <- count / num_simulations
  return(data.frame(MaxTickets = max_tickets, Count = count, Probability = probability))
}

# Calculate probabilities for different numbers of tickets
prob_data <- do.call(rbind, lapply(min_tickets:max_tickets, probability_analysis))

# Create a probability plot
p4 <- ggplot(prob_data, aes(x = MaxTickets, y = Probability)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 3) +
  labs(
    title = "Probability of Winning Within a Given Number of Tickets",
    x = "Maximum Number of Tickets",
    y = "Probability"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

# Save the probability plot
ggsave("plots/lottery_winner_probability.png", p4, width = 10, height = 6, dpi = 300)

# Print the probability plot
print(p4)