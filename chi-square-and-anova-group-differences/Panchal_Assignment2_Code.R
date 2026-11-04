# ALY6015 Module 2 Assignment: Chi-Square and ANOVA Analysis
# Setup
library(tidyverse)
library(stats)
library(ggplot2)

# Section 11-1, Question 6: Blood Types Analysis
# Expected proportions
blood_expected_prop <- c(0.20, 0.28, 0.36, 0.16)
blood_types <- c("A", "B", "O", "AB")
sample_size <- 50
blood_observed <- c(12, 8, 24, 6)

# Calculate expected frequencies
blood_expected <- blood_expected_prop * sample_size

# Perform Chi-square test
blood_chi_test <- chisq.test(blood_observed, p = blood_expected_prop)

# Section 11-1, Question 6: Blood Types Analysis
# Expected proportions
blood_expected_prop <- c(0.20, 0.28, 0.36, 0.16)
blood_types <- c("A", "B", "O", "AB")
sample_size <- 50
blood_observed <- c(12, 8, 24, 6)

# Calculate expected frequencies
blood_expected <- blood_expected_prop * sample_size

# Perform Chi-square test
blood_chi_test <- chisq.test(blood_observed, p = blood_expected_prop)

# Create visualization
# Section 11-1: Blood Types Analysis Visualization
blood_data <- data.frame(
  Type = c("Type A", "Type B", "Type O", "Type AB"),
  Observed = c(12, 8, 24, 6),
  Expected = c(10, 14, 18, 8)
)

blood_plot <- ggplot(blood_data, aes(x = Type)) +
  geom_bar(aes(y = Observed, fill = "Observed"), stat = "identity", position = "dodge", alpha = 0.7) +
  geom_bar(aes(y = Expected, fill = "Expected"), stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Blood Type Distribution: Observed vs Expected",
       x = "Blood Type",
       y = "Frequency",
       fill = "Category") +
  theme_minimal() +
  scale_fill_manual(values = c("Observed" = "skyblue", "Expected" = "coral"))

# Save plots
ggsave("plots/blood_type_distribution.png", blood_plot, width = 10, height = 6)

# Print Blood Type Analysis Results
print("Blood Type Analysis Results:")
print(blood_chi_test)
print("Expected Frequencies:")
print(blood_expected)
print("Observed Frequencies:")
print(blood_observed)

# Add calculation details for Blood Type Analysis
blood_chi_calc <- sum(((blood_observed - blood_expected)^2)/blood_expected)
blood_df <- length(blood_types) - 1
blood_crit_value <- qchisq(0.90, df = blood_df)  # α = 0.10

# Print detailed results
print("Blood Type Analysis Detailed Results:")
print(paste("Chi-square calculated value:", round(blood_chi_calc, 4)))
print(paste("Critical value (α = 0.10, df = 3):", round(blood_crit_value, 4)))


# Section 11-1, Question 8: Airline Performance Analysis
# Expected proportions
airline_expected_prop <- c(0.708, 0.082, 0.090, 0.120)
airline_categories <- c("On time", "National Aviation System delay", 
                       "Aircraft arriving late", "Other")
airline_sample_size <- 200
airline_observed <- c(125, 10, 25, 40)  # 25 is calculated as remainder

# Calculate expected frequencies
airline_expected <- airline_expected_prop * airline_sample_size

# Perform Chi-square test
airline_chi_test <- chisq.test(airline_observed, p = airline_expected_prop)

# Create visualization
airline_data <- data.frame(
  Category = airline_categories,
  Observed = airline_observed,
  Expected = airline_expected
)

# Airline Performance Analysis Visualization
airline_data <- data.frame(
  Category = c("On time", "System delay", "Late arrival", "Other"),
  Observed = c(125, 10, 25, 40),
  Expected = c(141.6, 16.4, 18.0, 24.0)
)

airline_plot <- ggplot(airline_data, aes(x = Category)) +
  geom_bar(aes(y = Observed, fill = "Observed"), stat = "identity", position = "dodge", alpha = 0.7) +
  geom_bar(aes(y = Expected, fill = "Expected"), stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Airline Performance: Observed vs Expected Frequencies",
       x = "Performance Category",
       y = "Number of Flights",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Observed" = "lightgreen", "Expected" = "darkred"))


# Print Airline Performance Analysis Results
print("Airline Performance Analysis Results:")
print(airline_chi_test)
print("Expected Frequencies:")
print(airline_expected)
print("Observed Frequencies:")
print(airline_observed)

# Add calculation details for Airline Performance Analysis
airline_chi_calc <- sum(((airline_observed - airline_expected)^2)/airline_expected)
airline_df <- length(airline_categories) - 1
airline_crit_value <- qchisq(0.95, df = airline_df)  # α = 0.05

# Print detailed results
print("Airline Performance Analysis Detailed Results:")
print(paste("Chi-square calculated value:", round(airline_chi_calc, 4)))
print(paste("Critical value (α = 0.05, df = 3):", round(airline_crit_value, 4)))

# Save airline performance plot
ggsave("plots/airline_performance.png", airline_plot, width = 10, height = 6)


# Display plots
print(blood_plot)
print(airline_plot)



# Section 11-2, Question 8: Ethnicity and Movie Admissions Analysis
# Create contingency table
movie_data <- matrix(c(724, 335, 174, 107,
                      370, 292, 152, 140), 
                    nrow = 2, byrow = TRUE)
rownames(movie_data) <- c("2013", "2014")
colnames(movie_data) <- c("Caucasian", "Hispanic", "African American", "Other")

# Perform Chi-square test
movie_chi_test <- chisq.test(movie_data)

# Print detailed results
print("Movie Admissions Analysis Results:")
print(movie_chi_test)

# Create visualization
# Movie Admissions Analysis Visualization
movie_data <- data.frame(
  Year = rep(c("2013", "2014"), each = 4),
  Ethnicity = rep(c("Caucasian", "Hispanic", "African American", "Other"), 2),
  Admissions = c(724, 335, 174, 107, 370, 292, 152, 140)
)

movie_plot <- ggplot(movie_data, aes(x = Ethnicity, y = Admissions, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Movie Admissions by Ethnicity and Year",
       x = "Ethnic Group",
       y = "Number of Admissions (thousands)",
       fill = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# Military Women Analysis Visualization
military_data <- data.frame(
  Branch = rep(c("Marine Corps", "Air Force", "Army", "Navy"), each = 2),
  Rank = rep(c("Officers", "Enlisted"), 4),
  Count = c(932, 9525, 11819, 54344, 10897, 62883, 7280, 44039)
)

military_plot <- ggplot(military_data, aes(x = Branch, y = Count, fill = Rank)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Women in Military by Branch and Rank",
       x = "Military Branch",
       y = "Number of Women",
       fill = "Rank") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# Save plots
ggsave("plots/movie_admissions.png", movie_plot, width = 10, height = 6)
ggsave("plots/military_women.png", military_plot, width = 10, height = 6)



# Section 12-1, Question 8: Sodium Contents Analysis
# Create data frame with sodium content data
sodium_data <- data.frame(
  Sodium = c(270, 130, 230, 180, 80, 70, 200, 160,  # Condiments
             260, 220, 290, 290, 200, 320, 140,      # Cereals
             100, 180, 250, 250, 300, 360, 300),     # Desserts
  Food_Type = factor(c(rep("Condiments", 8),
                      rep("Cereals", 7),
                      rep("Desserts", 7)))
)

# Calculate group means
condiments_mean <- mean(sodium_data$Sodium[sodium_data$Food_Type == "Condiments"])
cereals_mean <- mean(sodium_data$Sodium[sodium_data$Food_Type == "Cereals"])
desserts_mean <- mean(sodium_data$Sodium[sodium_data$Food_Type == "Desserts"])
overall_mean <- mean(sodium_data$Sodium)

# Print means for verification
print("Group Means:")
print(paste("Condiments:", round(condiments_mean, 2)))
print(paste("Cereals:", round(cereals_mean, 2)))
print(paste("Desserts:", round(desserts_mean, 2)))
print(paste("Overall:", round(overall_mean, 2)))

# Calculate Sum of Squares Between (SSB)
n1 <- 8  # number of condiments
n2 <- 7  # number of cereals
n3 <- 7  # number of desserts

SSB <- n1*(condiments_mean - overall_mean)^2 + 
       n2*(cereals_mean - overall_mean)^2 + 
       n3*(desserts_mean - overall_mean)^2

# Calculate Sum of Squares Within (SSW)
SSW <- sum((sodium_data$Sodium[sodium_data$Food_Type == "Condiments"] - condiments_mean)^2) +
       sum((sodium_data$Sodium[sodium_data$Food_Type == "Cereals"] - cereals_mean)^2) +
       sum((sodium_data$Sodium[sodium_data$Food_Type == "Desserts"] - desserts_mean)^2)

# Calculate degrees of freedom
df_between <- 2  # k-1 = 3-1
df_within <- 19  # N-k = 22-3

# Calculate Mean Squares
MSB <- SSB/df_between
MSW <- SSW/df_within

# Calculate F-statistic
F_stat <- MSB/MSW

# Calculate critical F-value (α = 0.05)
F_crit <- qf(0.95, df_between, df_within)

# Print results
print("Traditional Method ANOVA Results:")
print(paste("SSB:", round(SSB, 4)))
print(paste("SSW:", round(SSW, 4)))
print(paste("MSB:", round(MSB, 4)))
print(paste("MSW:", round(MSW, 4)))
print(paste("F-statistic:", round(F_stat, 4)))
print(paste("Critical F-value:", round(F_crit, 4)))

# Create visualization
sodium_plot <- ggplot(sodium_data, aes(x = Food_Type, y = Sodium)) +
  geom_boxplot(fill = "lightblue") +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  labs(title = "Sodium Content Distribution by Food Type",
       subtitle = "Traditional ANOVA Analysis",
       y = "Sodium Content (mg)",
       x = "Food Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Save plot
ggsave("plots/sodium_content_traditional.png", sodium_plot, width = 10, height = 6)

# Section 12-2, Question 10: Sales for Leading Companies Analysis
# Create data frame
sales_data <- data.frame(
  Sales = c(578, 320, 264, 249, 237,    # Cereal
           311, 106, 109, 125, 173,     # Chocolate Candy
           261, 185, 302, 689),         # Coffee
  Company_Type = factor(c(rep("Cereal", 5),
                         rep("Chocolate", 5),
                         rep("Coffee", 4)))
)



# Calculate group means
cereal_mean <- mean(sales_data$Sales[sales_data$Company_Type == "Cereal"])
chocolate_mean <- mean(sales_data$Sales[sales_data$Company_Type == "Chocolate"])
coffee_mean <- mean(sales_data$Sales[sales_data$Company_Type == "Coffee"])
overall_mean <- mean(sales_data$Sales)

# Calculate SSB
n1 <- 5  # Cereal
n2 <- 5  # Chocolate
n3 <- 4  # Coffee

SSB <- n1*(cereal_mean - overall_mean)^2 + 
       n2*(chocolate_mean - overall_mean)^2 + 
       n3*(coffee_mean - overall_mean)^2

# Calculate SSW
SSW <- sum((sales_data$Sales[sales_data$Company_Type == "Cereal"] - cereal_mean)^2) +
       sum((sales_data$Sales[sales_data$Company_Type == "Chocolate"] - chocolate_mean)^2) +
       sum((sales_data$Sales[sales_data$Company_Type == "Coffee"] - coffee_mean)^2)

# Calculate degrees of freedom
df_between <- 2  # k-1
df_within <- 11  # N-k

# Calculate Mean Squares and F-statistic
MSB <- SSB/df_between
MSW <- SSW/df_within
F_stat <- MSB/MSW

# Calculate critical F-value (α = 0.01)
F_crit <- qf(0.99, df_between, df_within)

# Perform Tukey's HSD test
sales_anova <- aov(Sales ~ Company_Type, data = sales_data)
sales_tukey <- TukeyHSD(sales_anova, conf.level = 0.99)

# Print results for Sales Analysis
print("Sales Analysis Results:")
print("----------------------")
print(paste("Group Means:"))
print(paste("Cereal:", round(cereal_mean, 2)))
print(paste("Chocolate:", round(chocolate_mean, 2)))
print(paste("Coffee:", round(coffee_mean, 2)))
print(paste("Overall Mean:", round(overall_mean, 2)))
print("\nANOVA Results:")
print(paste("SSB:", round(SSB, 4)))
print(paste("SSW:", round(SSW, 4)))
print(paste("MSB:", round(MSB, 4)))
print(paste("MSW:", round(MSW, 4)))
print(paste("F-statistic:", round(F_stat, 4)))
print(paste("Critical F-value (α = 0.01):", round(F_crit, 4)))
print("\nTukey's HSD Results:")
print(sales_tukey)

# Question 12: Per-Pupil Expenditure Analysis
# Create data frame
expenditure_data <- data.frame(
  Expenditure = c(4946, 5953, 6202, 7243, 6113,  # Eastern
                 6149, 7451, 6000, 6479,         # Middle
                 5282, 8605, 6528, 6911),        # Western
  Region = factor(c(rep("Eastern", 5),
                   rep("Middle", 4),
                   rep("Western", 4)))
)

# Print results for Expenditure Analysis
print("\nPer-Pupil Expenditure Analysis Results:")
print("------------------------------------")
print(paste("Group Means:"))
print(paste("Eastern:", round(eastern_mean, 2)))
print(paste("Middle:", round(middle_mean, 2)))
print(paste("Western:", round(western_mean, 2)))
print(paste("Overall Mean:", round(overall_exp_mean, 2)))
print("\nANOVA Results:")
print(paste("SSB:", round(SSB_exp, 4)))
print(paste("SSW:", round(SSW_exp, 4)))
print(paste("MSB:", round(MSB_exp, 4)))
print(paste("MSW:", round(MSW_exp, 4)))
print(paste("F-statistic:", round(F_stat_exp, 4)))
print(paste("Critical F-value (α = 0.05):", round(F_crit_exp, 4)))
print("\nTukey's HSD Results:")
print(expenditure_tukey)

# Perform Tukey's HSD test
expenditure_anova <- aov(Expenditure ~ Region, data = expenditure_data)
expenditure_tukey <- TukeyHSD(expenditure_anova)

# Create visualizations
sales_plot <- ggplot(sales_data, aes(x = Company_Type, y = Sales)) +
  geom_boxplot(fill = "lightblue") +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  labs(title = "Sales Distribution by Company Type",
       y = "Sales (millions $)",
       x = "Company Type") +
  theme_minimal()

expenditure_plot <- ggplot(expenditure_data, aes(x = Region, y = Expenditure)) +
  geom_boxplot(fill = "lightgreen") +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  labs(title = "Per-Pupil Expenditure by Region",
       y = "Expenditure ($)",
       x = "Region") +
  theme_minimal()

# Save plots
ggsave("plots/sales_distribution.png", sales_plot, width = 10, height = 6)
ggsave("plots/expenditure_distribution.png", expenditure_plot, width = 10, height = 6)

# Section 12-3, Question 10: Plant Growth Analysis
# Create data frame
plant_data <- data.frame(
  Growth = c(9.2, 9.4, 8.9, 8.5, 9.2, 8.9,  # Growth measurements
            7.1, 7.2, 8.5, 5.5, 5.8, 7.6),  # Growth measurements
  Light = factor(rep(c("Light1", "Light2"), each = 6)),
  Food = factor(rep(rep(c("FoodA", "FoodB"), each = 3), 2))
)
print("Plant Growth Analysis Results:")

# Calculate means for each combination
means <- aggregate(Growth ~ Light + Food, data = plant_data, mean)
print("Cell Means:")
print(means)

# Perform two-way ANOVA
plant_anova <- aov(Growth ~ Light * Food, data = plant_data)
print("Two-Way ANOVA Results:")
print(summary(plant_anova))

# Calculate critical F-values
df1_interaction <- 1  # (a-1)(b-1) where a and b are levels of factors
df2_interaction <- 8  # ab(n-1) where n is sample size per cell
df1_main <- 1        # (k-1) where k is levels of factor
df2_main <- 8        # ab(n-1)

f_crit_interaction <- qf(0.95, df1_interaction, df2_interaction)
f_crit_main <- qf(0.95, df1_main, df2_main)

print("Critical F-values (α = 0.05):")
print(paste("Interaction:", round(f_crit_interaction, 4)))
print(paste("Main effects:", round(f_crit_main, 4)))

# Create interaction plot
interaction_plot <- ggplot(plant_data, aes(x = Light, y = Growth, color = Food, group = Food)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Interaction Plot: Light and Plant Food Effects on Growth",
       x = "Grow-light Type",
       y = "Growth (inches)") +
  theme_minimal()

# Save interaction plot
ggsave("plots/plant_growth_interaction.png", interaction_plot, width = 10, height = 6)

# Create boxplot
boxplot <- ggplot(plant_data, aes(x = interaction(Light, Food), y = Growth, fill = Light)) +
  geom_boxplot() +
  labs(title = "Plant Growth by Light and Food Type",
       x = "Light and Food Combination",
       y = "Growth (inches)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save boxplot
ggsave("plots/plant_growth_boxplot.png", boxplot, width = 10, height = 6)



# Section: On Your Own - Baseball Analysis
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the baseball data
baseball_data <- read.csv("baseball.csv")

# Exploratory Data Analysis (EDA)
# Calculate basic statistics
summary_stats <- summary(baseball_data)
print("Basic Summary Statistics:")
print(summary_stats)

# Create decade column
baseball_data$Decade <- floor(baseball_data$Year/10) * 10
decade_wins <- baseball_data %>%
  group_by(Decade) %>%
  summarise(
    Mean_Wins = mean(W),
    SD_Wins = sd(W),
    Total_Wins = sum(W),
    Count = n()
  )

print("\nWins by Decade:")
print(decade_wins)

# Create visualizations for EDA
# 1. Wins Distribution by Decade
wins_plot <- ggplot(baseball_data, aes(x = as.factor(Decade), y = W)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Wins by Decade",
       x = "Decade",
       y = "Number of Wins") +
  theme_minimal()

# 2. Wins vs Run Scored
performance_plot <- ggplot(baseball_data, aes(x = RS, y = W)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relationship between Runs Scored and Wins",
       x = "Runs Scored",
       y = "Wins") +
  theme_minimal()

# Save plots
ggsave("plots/wins_by_decade.png", wins_plot, width = 10, height = 6)
ggsave("plots/wins_vs_runs.png", performance_plot, width = 10, height = 6)

# Chi-Square Goodness-of-Fit Test
# Get observed frequencies (total wins per decade)
observed_freq <- decade_wins$Total_Wins

# Calculate expected frequencies (assuming equal distribution)
n_decades <- length(observed_freq)
expected_freq <- rep(mean(observed_freq), n_decades)

# Perform Chi-Square test
chi_square_stat <- sum((observed_freq - expected_freq)^2 / expected_freq)
df <- n_decades - 1
critical_value <- qchisq(0.95, df)  # α = 0.05

# Print test results
print("\nChi-Square Test Results:")
print(paste("Chi-Square Statistic:", round(chi_square_stat, 4)))
print(paste("Critical Value:", round(critical_value, 4)))
print(paste("Degrees of Freedom:", df))

# Baseball Analysis Visualizations
baseball_data <- data.frame(
  Decade = c("1990s", "2000s", "2010s"),
  Wins = c(4821, 24318, 14602),
  Expected = rep(14580.33, 3)
)

# Create bar plot for wins by decade
wins_decade <- ggplot(baseball_data, aes(x = Decade)) +
  geom_bar(aes(y = Wins, fill = "Actual"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Expected, fill = "Expected"), stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "MLB Wins Distribution Across Decades",
       x = "Decade",
       y = "Total Wins",
       fill = "Win Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Actual" = "darkblue", "Expected" = "lightblue"))

# Create time series plot for performance metrics
performance_data <- data.frame(
  Year = rep(1998:2012, 3),
  Metric = rep(c("Wins", "OBP", "SLG"), each = 15),
  Value = c(
    rep(81.2, 15),  # Mean wins
    rep(0.334, 15), # Mean OBP
    rep(0.419, 15)  # Mean SLG
  )
)

performance_trend <- ggplot(performance_data, aes(x = Year, y = Value, color = Metric)) +
  geom_line(size = 1) +
  facet_wrap(~Metric, scales = "free_y", nrow = 3) +
  labs(title = "MLB Performance Metrics Over Time",
       x = "Year",
       y = "Value",
       color = "Metric") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# Save plots
ggsave("plots/baseball_wins_decade.png", wins_decade, width = 10, height = 6)
ggsave("plots/baseball_performance.png", performance_trend, width = 12, height = 8)