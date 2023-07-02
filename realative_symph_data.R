#########################################################
## ANOVA and Analysis for Symphylan Treatments at Steinke  Farm
## By: Connor Eck
#########################################################



library(reshape2)
library(ggplot2)
library(plotly)
library(ggpubr)
library(gridExtra)

#####SCATTERPLOT#####

# Read the data
symph_data <- read.csv("~/Desktop/symph_data/filtered_symph_data.csv")
#View(symph_data)

# Select the columns to exclude
exclude_columns <- c("Plot", "Rep", "Germination")

# Filter out the excluded columns
filtered_data <- symph_data[, !(names(symph_data) %in% exclude_columns)]

# Reshape the filtered data using melt
melted_data <- melt(filtered_data, id.vars = "Treatment", variable.name = "Variable", value.name = "Value")

# Create a scatterplot with tooltips using ggplot and geom_point
p <- ggplot(melted_data, aes(x = Treatment, y = Value, color = Variable, text = paste("Value:", Value))) +
  geom_point(aes(label = ifelse(is.na(Value), "", Value)), show.legend = TRUE) +
  geom_smooth(method = "lm", se = FALSE) +  # Add a line of best fit
  labs(x = "Treatment", y = "Value", color = "Variable") +
  ggtitle("Treatment vs. Variables")

# Convert the ggplot to an interactive plot using plotly
plotly_plot <- ggplotly(p, tooltip = "text")
plotly_plot

#####CONFIDENCE INTERVALS#####

# Confidence Interval for all bait columns in Control Rows
included_columns <- c("Bait.1","Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1", "Bait.1.2", "Bait.2.2", "Bait.3.2")

# Subset the data for the "Control" treatment and include the specified columns
control_data <- symph_data[symph_data$Treatment == "Control", included_columns]

# Calculate the means of the "Bait" columns within the "Control" treatment group
control_means <- colMeans(control_data)

# Calculate the standard errors for each "Bait" column
control_standard_errors <- apply(control_data, 2, function(x) sd(x) / sqrt(length(x)))

# Calculate the critical value for a 95% confidence level
control_critical_value <- qt(0.975, nrow(control_data) - 1)

# Calculate the margins of error for each "Bait" column
control_margins_of_error <- control_critical_value * control_standard_errors

# Calculate the lower and upper bounds of the confidence intervals for each "Bait" column
control_lower_bounds <- control_means - control_margins_of_error
control_upper_bounds <- control_means + control_margins_of_error

# Create a data frame to store the confidence intervals
control_confidence_intervals <- data.frame(Bait = included_columns, LowerBound = lower_bounds, UpperBound = upper_bounds)

# Print the confidence intervals
print(control_confidence_intervals)
print(control_means)


# Confidence Interval for all bait columns in Bifenture Rows
included_columns <- c("Bait.1","Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1", "Bait.1.2", "Bait.2.2", "Bait.3.2")

# Subset the data for the "Control" treatment and include the specified columns
bifenture_data <- symph_data[symph_data$Treatment == "Bifenture", included_columns]

# Calculate the means of the "Bait" columns within the "Control" treatment group
bifenture_means <- colMeans(bifenture_data)

# Calculate the standard errors for each "Bait" column
bifenture_standard_errors <- apply(bifenture_data, 2, function(x) sd(x) / sqrt(length(x)))

# Calculate the critical value for a 95% confidence level
bifenture_critical_value <- qt(0.975, nrow(bifenture_data) - 1)

# Calculate the margins of error for each "Bait" column
bifenture_margins_of_error <- bifenture_critical_value * bifenture_standard_errors

# Calculate the lower and upper bounds of the confidence intervals for each "Bait" column
bifenture_lower_bounds <- bifenture_means - bifenture_margins_of_error
bifenture_upper_bounds <- bifenture_means + bifenture_margins_of_error

# Create a data frame to store the confidence intervals
bifenture_confidence_intervals <- data.frame(Bait = included_columns, LowerBound = lower_bounds, UpperBound = upper_bounds)

# Print the confidence intervals
print(bifenture_confidence_intervals)
print(bifenture_means)


# Confidence Interval for all bait columns in Torac
included_columns <- c("Bait.1","Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1", "Bait.1.2", "Bait.2.2", "Bait.3.2")

# Subset the data for the "Control" treatment and include the specified columns
torac_data <- symph_data[symph_data$Treatment == "Torac", included_columns]

# Calculate the means of the "Bait" columns within the "Control" treatment group
torac_means <- colMeans(torac_data)

# Calculate the standard errors for each "Bait" column
torac_standard_errors <- apply(torac_data, 2, function(x) sd(x) / sqrt(length(x)))

# Calculate the critical value for a 95% confidence level
torac_critical_value <- qt(0.975, nrow(brigade_data) - 1)

# Calculate the margins of error for each "Bait" column
torac_margins_of_error <- torac_critical_value * torac_standard_errors

# Calculate the lower and upper bounds of the confidence intervals for each "Bait" column
torac_lower_bounds <- torac_means - torac_margins_of_error
torac_upper_bounds <- torac_means + torac_margins_of_error

# Create a data frame to store the confidence intervals
torac_confidence_intervals <- data.frame(Bait = included_columns, LowerBound = lower_bounds, UpperBound = upper_bounds)

# Print the confidence intervals
print(torac_confidence_intervals)
print(torac_means)


# Confidence Interval for all bait columns in Brigade
included_columns <- c("Bait.1","Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1", "Bait.1.2", "Bait.2.2", "Bait.3.2")

# Subset the data for the "Control" treatment and include the specified columns
brigade_data <- symph_data[symph_data$Treatment == "Brigade", included_columns]

# Calculate the means of the "Bait" columns within the "Control" treatment group
brigade_means <- colMeans(brigade_data)

# Calculate the standard errors for each "Bait" column
brigade_standard_errors <- apply(brigade_data, 2, function(x) sd(x) / sqrt(length(x)))

# Calculate the critical value for a 95% confidence level
brigade_critical_value <- qt(0.975, nrow(brigade_data) - 1)

# Calculate the margins of error for each "Bait" column
brigade_margins_of_error <- brigade_critical_value * brigade_standard_errors

# Calculate the lower and upper bounds of the confidence intervals for each "Bait" column
brigade_lower_bounds <- brigade_means - brigade_margins_of_error
brigade_upper_bounds <- brigade_means + brigade_margins_of_error

# Create a data frame to store the confidence intervals
brigade_confidence_intervals <- data.frame(Bait = included_columns, LowerBound = lower_bounds, UpperBound = upper_bounds)

# Print the confidence intervals
print(brigade_confidence_intervals)
print(brigade_means)


# Confidence Interval for all bait columns in Capture
included_columns <- c("Bait.1","Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1", "Bait.1.2", "Bait.2.2", "Bait.3.2")

# Subset the data for the "Control" treatment and include the specified columns
capture_data <- symph_data[symph_data$Treatment == "Capture", included_columns]

# Calculate the means of the "Bait" columns within the "Control" treatment group
capture_means <- colMeans(capture_data)

# Calculate the standard errors for each "Bait" column
capture_standard_errors <- apply(capture_data, 2, function(x) sd(x) / sqrt(length(x)))

# Calculate the critical value for a 95% confidence level
capture_critical_value <- qt(0.975, nrow(capture_data) - 1)

# Calculate the margins of error for each "Bait" column
capture_margins_of_error <- capture_critical_value * capture_standard_errors

# Calculate the lower and upper bounds of the confidence intervals for each "Bait" column
capture_lower_bounds <- capture_means - capture_margins_of_error
capture_upper_bounds <- capture_means + capture_margins_of_error

# Create a data frame to store the confidence intervals
capture_confidence_intervals <- data.frame(Bait = included_columns, LowerBound = lower_bounds, UpperBound = upper_bounds)

# Print the confidence intervals
print(capture_confidence_intervals)
print(capture_means)


# Confidence Interval for all bait columns in Warrior II
included_columns <- c("Bait.1","Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1", "Bait.1.2", "Bait.2.2", "Bait.3.2")

# Subset the data for the "Control" treatment and include the specified columns
warrior_data <- symph_data[symph_data$Treatment == "WarriorII", included_columns]

# Calculate the means of the "Bait" columns within the "Control" treatment group
warrior_means <- colMeans(warrior_data)

# Calculate the standard errors for each "Bait" column
warrior_standard_errors <- apply(warrior_data, 2, function(x) sd(x) / sqrt(length(x)))

# Calculate the critical value for a 95% confidence level
warrior_critical_value <- qt(0.975, nrow(warrior_data) - 1)

# Calculate the margins of error for each "Bait" column
warrior_margins_of_error <- warrior_critical_value * warrior_standard_errors

# Calculate the lower and upper bounds of the confidence intervals for each "Bait" column
warrior_lower_bounds <- warrior_means - warrior_margins_of_error
warrior_upper_bounds <- warrior_means + warrior_margins_of_error

# Create a data frame to store the confidence intervals
warrior_confidence_intervals <- data.frame(Bait = included_columns, LowerBound = lower_bounds, UpperBound = upper_bounds)

# Print the confidence intervals
print(warrior_confidence_intervals)
print(warrior_means)



# Confidence Interval for all bait columns in Bifender FC
included_columns <- c("Bait.1","Bait.2", "Bait.3", "Bait.1.1", "Bait.2.1", "Bait.3.1", "Bait.1.2", "Bait.2.2", "Bait.3.2")

# Subset the data for the "Control" treatment and include the specified columns
fc_data <- symph_data[symph_data$Treatment == "Bifender FC", included_columns]

# Calculate the means of the "Bait" columns within the "Control" treatment group
fc_means <- colMeans(fc_data)

# Calculate the standard errors for each "Bait" column
fc_standard_errors <- apply(fc_data, 2, function(x) sd(x) / sqrt(length(x)))

# Calculate the critical value for a 95% confidence level
fc_critical_value <- qt(0.975, nrow(fc_data) - 1)

# Calculate the margins of error for each "Bait" column
fc_margins_of_error <- fc_critical_value * fc_standard_errors

# Calculate the lower and upper bounds of the confidence intervals for each "Bait" column
fc_lower_bounds <- fc_means - fc_margins_of_error
fc_upper_bounds <- fc_means + fc_margins_of_error

# Create a data frame to store the confidence intervals
fc_confidence_intervals <- data.frame(Bait = included_columns, LowerBound = lower_bounds, UpperBound = upper_bounds)

# Print the confidence intervals
print(fc_confidence_intervals)
print(fc_means)


##### ANOVA TESTING #####

# Analysis of Variance (anova) for Bait.1
aov1 <- aov(symph_data$Bait.1~symph_data$Treatment)
summary(aov1)
TukeyHSD(aov1)

# Analysis of Variance (anova) for Bait.2
aov2 <- aov(symph_data$Bait.2~symph_data$Treatment)
summary(aov2)
TukeyHSD(aov2)

# Analysis of Variance (anova) for Bait.3
aov3 <- aov(symph_data$Bait.3~symph_data$Treatment)
summary(aov3)
TukeyHSD(aov3)

# Analysis of Variance (anova) for Bait.1.1
aov4 <- aov(symph_data$Bait.1.1~symph_data$Treatment)
summary(aov4)
TukeyHSD(aov4)

# Analysis of Variance (anova) for Bait.2.1
aov5 <- aov(symph_data$Bait.2.1~symph_data$Treatment)
summary(aov5)
TukeyHSD(aov5)

# Analysis of Variance (anova) for Bait.3.1
aov6 <- aov(symph_data$Bait.3.1~symph_data$Treatment)
summary(aov6)
TukeyHSD(aov6)

# Analysis of Variance (anova) for Bait.1.2
aov7 <- aov(symph_data$Bait.1.2~symph_data$Treatment)
summary(aov7)
TukeyHSD(aov7)


# Analysis of Variance (anova) for Bait.2.2
aov8 <- aov(symph_data$Bait.2.2~symph_data$Treatment)
summary(aov8)
TukeyHSD(aov8)

# Analysis of Variance (anova) for Bait.3.2
aov9 <- aov(symph_data$Bait.3.2~symph_data$Treatment)
summary(aov9)
TukeyHSD(aov9)

#### VISUALIZATION #####

# Visualize Bait.1 by Treatment with color grouping by Site
ggplot(symph_data, aes(x = Treatment, y = Bait.1)) +
  geom_boxplot(color = "blue") +
  stat_compare_means(method = "anova") +
  ggtitle("Comparison of Bait.1 by Treatment") +
  xlab("Treatment") +
  ylab("Bait.1 Abundance")

# Visualize Bait.2 by Treatment with color grouping by Site
ggplot(symph_data, aes(x = Treatment, y = Bait.2)) +
  geom_boxplot(color = "red") +
  stat_compare_means(method = "anova") +
  ggtitle("Comparison of Bait.2 by Treatment") +
  xlab("Treatment") +
  ylab("Bait.2 Abundance")

# Visualize Bait.3 by Treatment with color grouping by Site
ggplot(symph_data, aes(x = Treatment, y = Bait.3)) +
  geom_boxplot(color = "purple") +
  stat_compare_means(method = "anova") +
  ggtitle("Comparison of Bait.3 by Treatment") +
  xlab("Treatment") +
  ylab("Bait.3 Abundance")

# Visualize Bait.1.1 by Treatment with color grouping by Site
ggplot(symph_data, aes(x = Treatment, y = Bait.1.1)) +
  geom_boxplot(color = "orange") +
  stat_compare_means(method = "anova") +
  ggtitle("Comparison of Bait.1.1 by Treatment") +
  xlab("Treatment") +
  ylab("Bait.1.1 Abundance")

# Visualize Bait.2.1 by Treatment with color grouping by Site
ggplot(symph_data, aes(x = Treatment, y = Bait.2.1)) +
  geom_boxplot(color = "yellow") +
  stat_compare_means(method = "anova") +
  ggtitle("Comparison of Bait.2.1 by Treatment") +
  xlab("Treatment") +
  ylab("Bait.2.1 Abundance")

# Visualize Bait.3.1 by Treatment with color grouping by Site
ggplot(symph_data, aes(x = Treatment, y = Bait.3.1)) +
  geom_boxplot(color = "green") +
  stat_compare_means(method = "anova") +
  ggtitle("Comparison of Bait.3.1 by Treatment") +
  xlab("Treatment") +
  ylab("Bait.3.1 Abundance")

# Visualize Bait.1.2 by Treatment with color grouping by Site
ggplot(symph_data, aes(x = Treatment, y = Bait.1.2)) +
  geom_boxplot(color = "cyan") +
  stat_compare_means(method = "anova") +
  ggtitle("Comparison of Bait.1.2 by Treatment") +
  xlab("Treatment") +
  ylab("Bait.1.2 Abundance")

# Visualize Bait.2.2 by Treatment with color grouping by Site
ggplot(symph_data, aes(x = Treatment, y = Bait.2.2)) +
  geom_boxplot(color = "black") +
  stat_compare_means(method = "anova") +
  ggtitle("Comparison of Bait.2.2 by Treatment") +
  xlab("Treatment") +
  ylab("Bait.2.2 Abundance")

# Visualize Bait.3.2 by Treatment with color grouping by Treatment
ggplot(symph_data, aes(x = Treatment, y = Bait.3.2)) +
  geom_boxplot(color = "pink") +
  stat_compare_means(method = "anova") +
  ggtitle("Comparison of Bait.3.2 by Treatment") +
  xlab("Treatment") +
  ylab("Bait.3.2 Abundance")


#### CORRELATION BETWEEN TREATMENT AND STAND COUNT###
# Boxplot
boxplot(Germination ~ Treatment, data = symph_data,
        xlab = "Treatment", ylab = "Germination Count",
        main = "Treatment vs. Germination",
        col = "red")

# Bar Chart
barplot(symph_data$Germination, names.arg = symph_data$Treatment,
        xlab = "Treatment", ylab = "Germination",
        main = "Treatment vs. Germination",
        col = "cyan")

# Scatterplot

# Assigns numbers to treatments
treatment_numeric <- as.numeric(factor(symph_data$Treatment))

# Creating a scatter plot with a line of best fit
ggplot(data = symph_data, aes(x = treatment_numeric, y = Germination)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  xlab("Treatment") +
  ylab("Germination") +
  ggtitle("Treatment vs. Germination")


#Correlation coefficient between Treatment and Germination
correlation_coefficient <- cor(as.numeric(factor(symph_data$Treatment)), symph_data$Germination, method = "pearson")
correlation_coefficient




#Germination mean for Control treatment
germ_control_mean <- mean(symph_data$Germination[symph_data$Treatment == "Control"], na.rm = TRUE)
# Print the mean
print(germ_control_mean)

#Germination mean for Control treatment
germ_bifenture_mean <- mean(symph_data$Germination[symph_data$Treatment == "Bifenture"], na.rm = TRUE)
# Print the mean
print(germ_bifenture_mean)

#Germination mean for Torac treatment
germ_torac_mean <- mean(symph_data$Germination[symph_data$Treatment == "Torac"], na.rm = TRUE)
# Print the mean
print(germ_torac_mean)

#Germination mean for Brigade treatment
germ_brigade_mean <- mean(symph_data$Germination[symph_data$Treatment == "Brigade"], na.rm = TRUE)
# Print the mean
print(germ_brigade_mean)

#Germination mean for Capture treatment
germ_capture_mean <- mean(symph_data$Germination[symph_data$Treatment == "Capture"], na.rm = TRUE)
# Print the mean
print(germ_capture_mean)


#Germination mean for WarriorII treatment
germ_warrior_mean <- mean(symph_data$Germination[symph_data$Treatment == "WarriorII"], na.rm = TRUE)
# Print the mean
print(germ_warrior_mean)




######### Mean number of Symphs per treatment compared to germination#########

# Germination column means
germ_total_means <- list(germ_control_mean, germ_bifenture_mean, germ_torac_mean, germ_brigade_mean, germ_capture_mean, germ_warrior_mean)

print(germ_total_means)

# Total means
total_means <- c(control_means, bifenture_means, torac_means, brigade_means, capture_means, warrior_means)
print(total_means)
print(control_means)

# Correlation between total germmination and averag symph count for each treatment

total_means <- c(control_mean, bifenture_mean, torac_mean, brigade_mean, capture_mean, warrior_mean)
germ_total_means <- list(germ_control_mean, germ_bifenture_mean, germ_torac_mean, germ_brigade_mean, germ_capture_mean, germ_warrior_mean)


# Mean variables
print(control_means)
print(bifenture_means)
print(torac_means)
print(brigade_means)
print(capture_means)
print(warrior_means)


print(germ_control_mean)
print(germ_bifenture_mean)
print(germ_torac_mean)
print(germ_brigade_mean)
print(germ_capture_mean)
print(germ_warrior_mean)


# Create the data frame for symph counts in each treatment
mean <- data.frame(
  Control = control_means,
  Bifenture = bifenture_means,
  Torac = torac_means,
  Brigade = brigade_means,
  Capture = capture_means,
  Warrior = warrior_means,
)
transposed_mean <- t(mean)
# Print the data frame
print(transposed_mean)
#View(transposed_mean)



# Create the data frame for mean germination counts in each treatment
germination_mean <- data.frame(
  Germination = c(germ_control_mean, germ_bifenture_mean, germ_torac_mean, germ_brigade_mean, germ_capture_mean, germ_warrior_mean)
)

# Print the data frame
print(germination_mean)
#View(germination_mean)



# Create the merged data frame for treatments and germination means
mean <- data.frame(
  Control = control_means,
  Bifenture = bifenture_means,
  Torac = torac_means,
  Brigade = brigade_means,
  Capture = capture_means,
  Warrior = warrior_means
)

# Transpose the data frame
transposed_mean <- t(mean)

# Create the data frame for Germination
germination_mean <- data.frame(
  Germination = c(germ_control_mean, germ_bifenture_mean, germ_torac_mean, germ_brigade_mean, germ_capture_mean, germ_warrior_mean)
)

# Merge the two data frames
merged_mean <- cbind(transposed_mean, germination_mean)

# Print the merged data frame
print(merged_mean)
View(merged_mean)

#Treatment vs Mean Germination Counts
library(ggplot2)

# Create the merged data frame for treatments and germination means
mean <- data.frame(
  Treatment = c("Control", "Bifenture", "Torac", "Brigade", "Capture", "Warrior"),
  Mean = c(control_means, bifenture_means, torac_means, brigade_means, capture_means, warrior_means),
  Germination = c(germ_control_mean, germ_bifenture_mean, germ_torac_mean, germ_brigade_mean, germ_capture_mean, germ_warrior_mean)
)

# Create the scatterplot
ggplot(mean, aes(x = Treatment, y = Germination)) +
  geom_point() +
  xlab("Treatment") +
  ylab("Germination") +
  ggtitle("Scatterplot of Treatment vs Germination")





