# Set seed for reproducibility
set.seed(123)

# Load necessary packages
library(tidyverse)
library(readxl)
library(writexl)
library(lme4)
library(car)
library(glmmTMB)
library(glmnet)  # for Ridge, Lasso and Elastic Net regression

# Set file paths
input_file <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute__GRAND_SUMMARY_BirdNET_EVENTS_ALL_REGIONS_GROUPED_OCCURRENCE.xlsx"
output_dir <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models/MUNIPARA_tot_occurrence ~ Acoustic_Region-Site-Season-Scientific.name/"

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read in data from specific sheet in XLSX file
data <- read_xlsx(input_file)

# Filter data for Munipara Site only
data <- data %>%
  filter(Site == "Munipara")

# Data preprocessing
data <- data %>%
  mutate(across(c(Scientific.name, Acoustic_Region, Season), as.factor)) %>%
  mutate(tot_occurrence = as.integer(tot_occurrence)) %>%
  filter(!is.na(tot_occurrence))

# Create a model matrix for the predictors
x <- model.matrix(tot_occurrence ~ Acoustic_Region:Season:Scientific.name, data = data)[,-1]  # remove intercept column

# Create a response vector
y <- data$tot_occurrence

# Fit a Ridge regression model
ridge_model <- glmnet(x, y, alpha = 0)  # alpha = 0 for Ridge regression

# Fit a Lasso regression model
lasso_model <- glmnet(x, y, alpha = 1)  # alpha = 1 for Lasso regression

# Fit an Elastic Net regression model
elastic_net_model <- glmnet(x, y, alpha = 0.5)  # alpha = 0.5 for Elastic Net regression

# Save Ridge Coefficient Paths
pdf(file.path(output_dir,"Ridge_Coefficient_Paths.pdf"))
plot(ridge_model, xvar = "lambda", label = TRUE)
dev.off()

# Save Lasso Coefficient Paths
pdf(file.path(output_dir,"Lasso_Coefficient_Paths.pdf"))
plot(lasso_model, xvar = "lambda", label = TRUE)
dev.off()

# Save Elastic Net Coefficient Paths
pdf(file.path(output_dir,"ElasticNet_Coefficient_Paths.pdf"))
plot(elastic_net_model, xvar = "lambda", label = TRUE)
dev.off()

# Define a sequence of lambda values to try
lambda_values <- 10^seq(10, -2, length = 100) # increase length here anywhere between 100 to 1000 if concerned about high coefficent values

# Cross-Validation for optimal lambda
cv_ridge <- cv.glmnet(x, y, alpha = 0, lambda = lambda_values)
cv_lasso <- cv.glmnet(x, y, alpha = 1, lambda = lambda_values)
cv_elastic_net <- cv.glmnet(x, y, alpha = 0.5, lambda = lambda_values)

# Best lambda value
best_lambda_ridge <- cv_ridge$lambda.min
best_lambda_lasso <- cv_lasso$lambda.min
best_lambda_elastic_net <- cv_elastic_net$lambda.min

print(paste("Best lambda for Ridge: ", best_lambda_ridge))
print(paste("Best lambda for Lasso: ", best_lambda_lasso))
print(paste("Best lambda for Elastic Net: ", best_lambda_elastic_net))

# Refit the models using the best lambda
best_ridge_model <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)
best_lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)
best_elastic_net_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda_elastic_net)

# Residual Plots
par(mar = c(2, 2, 1, 1))
plot(predict(best_ridge_model, newx = x) - y,
     main="Residuals vs. Fitted Values (Ridge)",
     xlab="Fitted Values",
     ylab="Residuals")
abline(h=0,col="red")

plot(predict(best_lasso_model, newx = x) - y,
     main="Residuals vs. Fitted Values (Lasso)",
     xlab="Fitted Values",
     ylab="Residuals")
abline(h=0,col="red")

plot(predict(best_elastic_net_model, newx = x) - y,
     main="Residuals vs. Fitted Values (Elastic Net)",
     xlab="Fitted Values",
     ylab="Residuals")
abline(h=0,col="red")

# Calculate residuals
residuals_ridge <- y - predict(ridge_model, newx = x)
residuals_lasso <- y - predict(lasso_model, newx = x)
residuals_elastic_net <- y - predict(elastic_net_model, newx = x)

# Plot histograms of residuals
par(mfrow=c(2,2))
hist(residuals_ridge, main="Histogram of Residuals (Ridge)", xlab="Residuals")
hist(residuals_lasso, main="Histogram of Residuals (Lasso)", xlab="Residuals")
hist(residuals_elastic_net, main="Histogram of Residuals (Elastic Net)", xlab="Residuals")

# Save histograms to files
jpeg(file="D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/histogram_ridge_residuals.jpg")
hist(residuals_ridge, main="Histogram of Residuals (Ridge)", xlab="Residuals")
dev.off()

jpeg(file="D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/histogram_lasso_residuals.jpg")
hist(residuals_lasso, main="Histogram of Residuals (Lasso)", xlab="Residuals")
dev.off()

jpeg(file="D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/histogram_elastic_net_residuals.jpg")
hist(residuals_elastic_net, main="Histogram of Residuals (Elastic Net)", xlab="Residuals")
dev.off()

# Predictive Performance using cross-validation
ridge_cv_error <- cv_ridge$cvm[cv_ridge$lambda == best_lambda_ridge] # Changed from cv.glmnet() to cv_ridge
lasso_cv_error <- cv_lasso$cvm[cv_lasso$lambda == best_lambda_lasso] # Changed from cv.glmnet() to cv_lasso
elastic_net_cv_error <- cv_elastic_net$cvm[cv_elastic_net$lambda == best_lambda_elastic_net] # Changed from cv.glmnet() to cv_elastic_net

print(paste("Predictive performance for Ridge: ", ridge_cv_error))
print(paste("Predictive performance for Lasso: ", lasso_cv_error))
print(paste("Predictive performance for Elastic Net: ", elastic_net_cv_error))

# Save the models and cross-validation results
saveRDS(ridge_model,file.path(output_dir,"Ridge_model.rds"))
saveRDS(best_ridge_model,file.path(output_dir,"best_Ridge_model.rds"))
saveRDS(cv_ridge,file.path(output_dir,"cv_Ridge.rds"))

saveRDS(lasso_model,file.path(output_dir,"Lasso_model.rds"))
saveRDS(best_lasso_model,file.path(output_dir,"best_Lasso_model.rds"))
saveRDS(cv_lasso,file.path(output_dir,"cv_Lasso.rds"))

saveRDS(elastic_net_model,file.path(output_dir,"ElasticNet_model.rds"))
saveRDS(best_elastic_net_model,file.path(output_dir,"best_ElasticNet_model.rds"))
saveRDS(cv_elastic_net,file.path(output_dir,"cv_ElasticNet.rds"))

# Extract coefficients at the best lambda value
coefs_ridge <- coef(ridge_model, s = best_lambda_ridge)
coefs_lasso <- coef(lasso_model, s = best_lambda_lasso)
coefs_elastic_net <- coef(elastic_net_model, s = best_lambda_elastic_net)

# Save the original coefficients to CSV files with combination names
write.csv(as.matrix(coefs_ridge), file = paste0(output_dir, "Original_Ridge_Coefficients__best_Lasso_model_", best_lambda_ridge, ".csv"), row.names = TRUE)
write.csv(as.matrix(coefs_lasso), file = paste0(output_dir, "Original_Lasso_Coefficients__best_Lasso_model_", best_lambda_lasso, ".csv"), row.names = TRUE)
write.csv(as.matrix(coefs_elastic_net), file = paste0(output_dir, "Original_ElasticNet_Coefficients__best_Lasso_model_", best_lambda_elastic_net, ".csv"), row.names = TRUE)

# Convert to regular matrix then to data frame for easier manipulation
coefs_df_ridge <- as.data.frame(as.matrix(coefs_ridge))
coefs_df_lasso <- as.data.frame(as.matrix(coefs_lasso))
coefs_df_elastic_net <- as.data.frame(as.matrix(coefs_elastic_net))

# Add variable names as a column
coefs_df_ridge$Variable <- rownames(coefs_df_ridge)
coefs_df_lasso$Variable <- rownames(coefs_df_lasso)
coefs_df_elastic_net$Variable <- rownames(coefs_df_elastic_net)

# Convert 'Coef' column to numeric
coefs_df_ridge$Coef <- as.numeric(coefs_df_ridge[,1]) # assuming the coefficients are in the first column
coefs_df_lasso$Coef <- as.numeric(coefs_df_lasso[,1]) # assuming the coefficients are in the first column
coefs_df_elastic_net$Coef <- as.numeric(coefs_df_elastic_net[,1]) # assuming the coefficients are in the first column

# Filter to include only coefficients above a certain threshold
threshold <- 0.01 # replace with your desired threshold (Since threshold #1 didn't yield any results,it was reduced to 0.01 to explore prominent (though weak) relationships
coefs_df_ridge <- coefs_df_ridge[abs(coefs_df_ridge$Coef) > threshold, ]
coefs_df_lasso <- coefs_df_lasso[abs(coefs_df_lasso$Coef) > threshold, ]
coefs_df_elastic_net <- coefs_df_elastic_net[abs(coefs_df_elastic_net$Coef) > threshold, ]

# Remove terms from variable names
coefs_df_ridge$Variable <- gsub("Acoustic_Region|Scientific.name|Site|Season", "", coefs_df_ridge$Variable)
coefs_df_lasso$Variable <- gsub("Acoustic_Region|Scientific.name|Site|Season", "", coefs_df_lasso$Variable)
coefs_df_elastic_net$Variable <- gsub("Acoustic_Region|Scientific.name|Site|Season", "", coefs_df_elastic_net$Variable)

# Save the data frame to a CSV file
write.csv(coefs_df_ridge, file = paste0(output_dir, "ridge_model_threshold_coefs.csv"), row.names = FALSE)
write.csv(coefs_df_lasso, file = paste0(output_dir, "lasso_model_threshold_coefs.csv"), row.names = FALSE)
write.csv(coefs_df_elastic_net, file = paste0(output_dir, "elasticnet_model_threshold_coefs.csv"), row.names = FALSE)

# Open a new graphics device
dev.new()

# Generate the plot for Ridge regression model
p1 <- ggplot(coefs_df_ridge, aes(x = reorder(Variable, Coef), y = Coef)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "Coefficient")
plot(p1)

# Save the plot
ggsave(filename = paste0(output_dir, "plot_Ridge_model_threshold_coefs.pdf"), plot = p1, width = 10, height = 10, units = "in")

# Generate the plot for Lasso regression model
p2 <- ggplot(coefs_df_lasso, aes(x = reorder(Variable, Coef), y = Coef)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "Coefficient")
plot(p2)

# Save the plot
ggsave(filename = paste0(output_dir, "plot_Lasso_model_threshold_coefs.pdf"), plot = p2, width = 10, height = 10, units = "in")

# Generate the plot for Elastic Net regression model
p3 <- ggplot(coefs_df_elastic_net, aes(x = reorder(Variable, Coef), y = Coef)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "Coefficient")
plot(p3)

# Save the plot
ggsave(filename = paste0(output_dir, "plot_ElasticNet_model_threshold_coefs.pdf"), plot = p3, width = 10, height = 10, units = "in")

# Close the graphics device
dev.off()


# Print the best lambda and alpha values for each model
print(paste("Best lambda for Ridge: ", best_lambda_ridge))
print(paste("Best lambda for Lasso: ", best_lambda_lasso))
print(paste("Best lambda for Elastic Net: ", best_lambda_elastic_net))

# Print the predictive performance of each model
print(paste("Predictive performance for Ridge: ", ridge_cv_error))
print(paste("Predictive performance for Lasso: ", lasso_cv_error))
print(paste("Predictive performance for Elastic Net: ", elastic_net_cv_error))

# Compare the models and choose the best one based on predictive performance
if (ridge_cv_error <= min(ridge_cv_error, lasso_cv_error, elastic_net_cv_error)) {
  print("The Ridge regression model has the lowest cross-validated error and is the best model.")
} else if (lasso_cv_error <= min(ridge_cv_error, lasso_cv_error, elastic_net_cv_error)) {
  print("The Lasso regression model has the lowest cross-validated error and is the best model.")
} else {
  print("The Elastic Net regression model has the lowest cross-validated error and is the best model.")
}


########################################################
#Bar plot to visualise interactions between species and indices from original coefs_model rather than coefs_df_model

# Load necessary packages
library(tidyverse)  # or library(magrittr)
library(ggplot2)
library(DALEX)

# Assuming coefs_df_ridge is already in your environment
# Coefficient Plotting
coefs <- coefs_df_ridge

# Rename the columns for clarity
names(coefs) <- c('Coefficient', 'Feature', 'abs_Coefficient')

# Reorder the columns
coefs <- coefs[, c('Feature', 'Coefficient', 'abs_Coefficient')]

# Remove terms from variable names
coefs$Feature <- gsub("Acoustic_Region|Scientific.name|Site|Season", "", coefs$Feature)

# Define a function to plot coefficients
plot_coefs <- function(coefs, title) {
  # Exclude the intercept
  coefs <- coefs[coefs$Feature != "(Intercept)", ]
  
  # Calculate the 5th and 95th percentiles
  lower_bound <- quantile(coefs$abs_Coefficient, 0.05) 
  upper_bound <- quantile(coefs$abs_Coefficient, 0.95) 
  
  # Filter the coefficients
  filtered_coefs <- coefs %>%
    filter(abs_Coefficient <= lower_bound | abs_Coefficient >= upper_bound)
  
  p1 <- ggplot(filtered_coefs, aes(x = reorder(Feature, abs_Coefficient), y = abs_Coefficient)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = title, x = "Variable", y = "Absolute Coefficient") +
    theme_minimal()
  
  print(p1)
  
  # Save the plot to a file
  ggsave(filename = paste0(output_dir, paste0(gsub(" ", "_", title), "_MUNIPARA_Plot_best_Ridge_model.pdf")), plot = p1, width = 10, height = 10, units = "in")
}

# Call the function to plot coefficients
plot_coefs(coefs, "Ridge_Coefficients")

#######################

# Save the histogram to a PNG file
png(file = paste(output_dir, "/MUNIPARA_Histogram_Coefficients_best_Ridge_model.png", sep = ""))
hist(coefs_df_ridge$Coef, main="Histogram of Coefficients", xlab="Coefficient")
dev.off()

# Save the boxplot to a PNG file
png(file = paste(output_dir, "/MUNIPARA_Boxplot_Coefficients_best_Ridge_model.png", sep = ""))
boxplot(abs(coefs_df_ridge$Coef), main="Boxplot of Absolute Coefficients", ylab="Coefficient")
dev.off()


#######################
# Calculate summary statistics for the coefficients
summary_stats <- summary(coefs_df_ridge$Coef)

# Convert the summary statistics to a character string
summary_stats_str <- capture.output(print(summary_stats))

# Write the summary statistics to a text file
write(summary_stats_str, file = paste(output_dir, "/Summary_Statistics_best_Ridge_model.txt", sep = ""))

# Print out the summary statistics
print(summary_stats)

#####################
#best_Ridge_model in descending order of their importance (most prominent coefs_df_ridge will be displayed first)

# Assuming 'coefs_df_ridge' is your Ridge model coefficients
# Get the coefficients
df <- coefs_df_ridge

# Rename the columns for clarity
names(df) <- c('Coefficient', 'Feature', 'abs_Coefficient')

# Reorder the columns
df <- df[, c('Feature', 'Coefficient', 'abs_Coefficient')]

# Sort by absolute value of Coefficient in descending order
df <- df[order(-df$abs_Coefficient), ]

# Remove terms from variable names
df$Feature <- gsub("Acoustic_Region:|Scientific.name:|Season:|Site:|Species.names:|", "", df$Feature)
# Save the sorted coefficients to a CSV file
write.csv(df, file = paste(output_dir, "/MUNIPARA_coefficients_descending_best_Ridge_model.csv", sep = ""))

# Print out the modified data frame
print(df)

######################
