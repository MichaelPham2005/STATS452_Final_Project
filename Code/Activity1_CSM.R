####################
# Data Loading and Initial Setup
####################

setwd("C:\\Users\\LENOVO\\OneDrive - VNU-HCMUS\\Desktop\\Math\\Statistic II\\CSM")

# Import library
library(ggplot2)      
library(corrplot)     
library(car)          
library(naniar)       
library(VIM)          
library(MASS)         
library(lmtest)       
library(dplyr)        
library(leaps)        
library(skimr)        
library(GGally)       
library(missForest)   
library(caret)        
library(xgboost)      
library(effects)
library(openxlsx)

data_raw <- read.xlsx("CSM.xlsx")

# Check dimensions
dim(data_raw)

# Preview first few rows
head(data_raw)

####################
# Data Structure and Summary
####################

# View data structure
str(data_raw)

# Summary statistics (basic)
summary(data_raw)

# More comprehensive summary using skimr
skim(data_raw)  # Provides stats like mean, sd, histogram sketches, missing counts, etc.

####################
# Missing Values Summary and Visualization
####################

# Summarize missing values per variable
miss_summary <- miss_var_summary(data_raw)
print(miss_summary)

# Visualize missing patterns (upset plot for combinations)
gg_miss_upset(data_raw)

# Heatmap of missing values
vis_miss(data_raw)

####################
# Missing Mechanism Analysis: Create Indicator and Logistic Regression
####################

# Create missing indicator for Aggregate.Followers (highest missing rate ~15%)
data_raw <- data_raw %>%
  mutate(missing_followers = ifelse(is.na(Aggregate.Followers), 1, 0))

# Logistic regression to predict missing_followers (test for MAR)
# Note: glm handles NA via listwise deletion; this reduces sample size (check nobs below) and could bias if predictors have correlated missings.
# To mitigate, could impute predictors first, but here we proceed for initial check.
# Fit a logistic regression model.
logit_model <- glm(
  missing_followers ~ Views + Likes + Budget + Screens + Ratings,
  data = data_mar_test,
  family = binomial(link = "logit")
)
summary(logit_model)

# Check effective sample size after listwise deletion
nobs(logit_model)  # Outputs the number of observations used; compare to nrow(data_raw) for reduction.

# Interpretation: Significant coeffs (e.g., Ratings p=0.0187*, positive coef: higher ratings increase prob of missing—possibly art-house films with high ratings but low social/follower data availability).
# Budget marginal (p=0.0664, negative coef: lower budget increases prob of missing—suggests low-budget films less likely to have follower data tracked).
# Likes negative but not sig (p=0.2777: lower likes marginally increase missing prob).
# Overall: Evidence of MAR (deviance reduction, some sig coeffs); positive Ratings coef may indicate selection bias where high-rated but obscure (low-budget) films lack follower metrics.

####################
# Additional Checks: T-Tests for Mean Differences
####################

# T-tests: Compare means between missing (1) and non-missing (0) groups for key variables
# Significant differences support MAR (e.g., lower means in missing group for popularity vars)

t.test(Views ~ missing_followers, data = data_raw)     # p=0.1918 (not sig, but lower mean in missing: 3M vs 3.8M)
t.test(Likes ~ missing_followers, data = data_raw)     # p=0.05698 (marginal, lower mean: 8k vs 13k)
t.test(Budget ~ missing_followers, data = data_raw)    # p=3.591e-05*** (sig, lower mean: 24M vs 52M)
t.test(Ratings ~ missing_followers, data = data_raw)   # p=0.1812 (not sig, slightly higher mean: 6.64 vs 6.41—aligns with positive logit coef for Ratings)

# Conclusion: Strong evidence from Budget t-test (p<0.001), marginal from Likes; combined with logit (Ratings sig, Budget marginal) confirms MAR.
# Missing in Followers likely for low-budget/low-popularity films; higher ratings in missing may indicate art-house films with high critical acclaim but low social metrics.

####################
# Enhanced Checks: Little's MCAR Test and Missing Correlations
####################

# Refined numeric subset for MCAR: Exclude categoricals, time, and indicators
numeric_data <- data_raw %>%
  select(where(is.numeric)) %>%
  select(-missing_followers, -Genre, -Sequel, -Sentiment, -Year)

# Check for issues pre-test
library(caret)
near_zero_var <- nearZeroVar(numeric_data)
if (length(near_zero_var) > 0) numeric_data <- numeric_data[, -near_zero_var]

# Scale for stability
numeric_data_scaled <- scale(numeric_data)

# Run MCAR on refined/scaled data
mcar_test_result <- mcar_test(as.data.frame(numeric_data_scaled))
print(mcar_test_result) # p-value = 0.0482 < 0.05 => Enough evidence to reject MCAR => supports MAR/MNAR.

# Visualize correlations between missingness and observed values
num_vars_with_miss <- data_raw %>%
  select(where(is.numeric), missing_followers) 

cor_matrix_miss <- cor(num_vars_with_miss, use = "pairwise.complete.obs")
print(cor_matrix_miss)  # Focus on correlations with 'missing_followers' (e.g., negative with Budget/Likes confirms association with lower values)

# Plot correlations for visualization
corrplot(cor_matrix_miss, method = "circle", tl.cex = 0.8, title = "Correlations with Missing Followers Indicator")

####################
# Data Imputation Using MissForest
####################

library(missForest)
df <- data_raw[rowSums(is.na(data_raw)) != ncol(data_raw), ]
head(df)

sapply(df, function(x) sum(is.na(x)))
aggr(df, prop = TRUE, numbers = TRUE)
df_impute <- df[, -which(names(df) == "Movie")]

# Convert categorical-like columns to factors for proper handling in MissForest
cols_to_factor <- c("Genre", "Year")
df_impute[cols_to_factor] <- lapply(df_impute[cols_to_factor], factor)

str(df_impute)

# Set seed for reproducibility
set.seed(123) #123, 421
result_imputation <- missForest(df_impute, verbose = TRUE, ntree = 100)
df_completed <- result_imputation$ximp
sapply(df_completed, function(x) sum(is.na(x)))

# Preview imputed data
df_completed$Aggregate.Followers <- round(df_completed$Aggregate.Followers)
head(df_completed)
print(result_imputation$OOBerror)

# Additional plausibility check: Ensure no negative values in non-negative vars (e.g., Budget, Gross)
neg_checks <- sapply(c("Budget", "Gross", "Aggregate.Followers"), function(col) {
  sum(df_completed[[col]] < 0, na.rm = TRUE)
})
print(neg_checks)  # Should be zero for all

####################
# Post-Imputation EDA: Distribution Checks and Visualizations
####################

complete_data <- df_completed

# Compare distributions: Original vs. Imputed for Aggregate.Followers
ggplot() +
  geom_density(data = data_raw, aes(x = Aggregate.Followers), color = "blue", na.rm = TRUE) +
  geom_density(data = complete_data, aes(x = Aggregate.Followers), color = "red") +
  labs(title = "Distribution of Aggregate Followers: Original (blue) vs. Imputed (red)",
       x = "Aggregate Followers", y = "Density") +
  theme_minimal()

# Verify categorical variables are factors
is.factor(complete_data$Genre)
is.factor(complete_data$Year)

# Confirm no missing values remain in the completed dataset
sum(is.na(complete_data))

# Visualize skewness in Gross revenue (target variable)
ggplot(complete_data, aes(y = Gross)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Boxplot of Gross Revenue") +
  ylab("Gross Revenue")
# Interpretation: Positive skew with outliers; suggests potential need for transformation (e.g., log/sqrt) later.
# May indicate heteroscedasticity in models.

# Correlation matrix for numeric variables
num_vars <- dplyr::select(complete_data, where(is.numeric))
cor_matrix <- cor(num_vars, use = "complete.obs")  # Already complete, but included for safety
corrplot(cor_matrix, method = "circle", tl.cex = 0.8, title = "Correlation Matrix of Numeric Variables")
# Note: Look for strong correlations (e.g., >0.7) that may indicate multicollinearity.

# Additional EDA: Pairwise scatterplots to detect non-linearity
library(GGally)
ggpairs(num_vars, title = "Pairwise Scatterplots and Correlations")
# Interpretation: Helps spot non-linear relationships or clusters early.

####################
# Data Splitting for Training and Validation
####################

# Set seed for reproducibility
set.seed(50)
# Split: 80% train, 20% validation
train_index <- sample(1:nrow(complete_data), size = 0.8 * nrow(complete_data))
train <- complete_data[train_index, ]
valid <- complete_data[-train_index, ]

# Print split sizes
cat("Training rows:", nrow(train), "Validation rows:", nrow(valid), "\n")

####################
# Model Building: Full Linear Regression Model
####################

# Fit the full model with all predictors
full_model <- lm(Gross ~ ., data = train)
summary(full_model)

# Added variable plots to assess individual predictor contributions
avPlots(full_model, id = FALSE)

####################
# Feature Selection: Backward Stepwise and Regsubsets
####################

# Backward selection using AIC
null_model <- lm(Gross ~ 1, data = train)
step_aic <- step(full_model, scope = list(lower = null_model, upper = full_model),
                 direction = "backward", trace = 1)

# Backward selection using BIC (penalizes more for complexity)
n <- nrow(train)
step_bic <- step(full_model, scope = list(lower = null_model, upper = full_model),
                 direction = "backward", k = log(n), trace = 1)

# Exhaustive subset selection on candidate predictors from stepwise results
# Note: Candidates selected based on stepwise: Ratings, Budget, Screens, Sequel, Dislikes, Aggregate.Followers
# Use regsubsets for best subsets; method="exhaustive" by default
csm_models <- regsubsets(Gross ~ Ratings + Budget + Screens + Sequel + Dislikes + Aggregate.Followers,
                         data = train, nvmax = 6)  # Limit to max 6 vars for efficiency
summary_csm <- summary(csm_models)

# Display which variables are included in each subset size
summary_csm$which

# Evaluate selection criteria
summary_csm$rsq     # R-squared: Increases with more vars; check for diminishing returns (after 4-5 vars)
summary_csm$adjr2   # Adjusted R-squared: Peaks at optimal complexity (6 vars)
summary_csm$cp      # Mallow's Cp: Lowest at best bias-variance trade-off (5-6 vars)
summary_csm$bic     # BIC: Lowest at most parsimonious (4 vars)

# Conclusion: 6-variable model balances criteria (high adj R², good Cp); proceed with it
selected_model <- lm(Gross ~ Ratings + Budget + Screens + Sequel + Dislikes + Aggregate.Followers, data = train)
summary(selected_model)

####################
# Model Diagnostics: Residuals, Normality, Homoscedasticity, Multicollinearity
####################

# Residuals vs. Fitted and Q-Q plots for linearity and normality
par(mfrow = c(1, 2))
plot(selected_model, which = 1)  # Check for non-linearity or heteroscedasticity
plot(selected_model, which = 2)  # Check normality of residuals

# Shapiro-Wilk test for normality
shapiro.test(residuals(selected_model))

# Breusch-Pagan test for homoscedasticity (null: constant variance)
bptest(selected_model)

# Variance Inflation Factors (VIF) for multicollinearity (VIF > 5-10 indicates issues)
vif(selected_model)

####################
# Outlier Detection and Removal
####################

# Influence plots (Cook's distance, leverage, etc.) to identify outliers
influenceIndexPlot(selected_model, id = TRUE)

# Manually specify outliers based on plots (e.g., high Cook's distance or leverage)
# Note: Justify removal rows 1 and 29 appear influential; investigate data points for errors
outlier_rows <- c("1", "29")  # Row names from train dataset

# Remove outliers and create cleaned training set
train_cleaned <- train[!rownames(train) %in% outlier_rows, ]
cat("Removed outliers:", paste(outlier_rows, collapse = ", "),
    ". Remaining rows in train_cleaned:", nrow(train_cleaned), "\n")

# Refit the selected model on cleaned data
refit_model <- lm(Gross ~ Ratings + Budget + Screens + Sequel + Dislikes + Aggregate.Followers,
                  data = train_cleaned)
summary(refit_model)

# Re-check influence plots post-removal to confirm improvement
influenceIndexPlot(refit_model, id = TRUE)

####################
# Re-Selection Post-Outlier Removal
####################

# Re-run backward AIC on cleaned data
null_model_clean <- lm(Gross ~ 1, data = train_cleaned)
step_aic_clean <- step(refit_model, scope = list(lower = null_model_clean, upper = refit_model),
                       direction = "backward", trace = 1)

# Re-run backward BIC on cleaned data
n_clean <- nrow(train_cleaned)
step_bic_clean <- step(refit_model, scope = list(lower = null_model_clean, upper = refit_model),
                       direction = "backward", k = log(n_clean), trace = 1)

# Re-run regsubsets excluding (Dislikes, Sequel).
csm_models_clean <- regsubsets(Gross ~ Ratings + Budget + Screens + Aggregate.Followers,
                               data = train_cleaned, nvmax = 4)
summary_csm_clean <- summary(csm_models_clean)
summary_csm_clean$which

# Re-evaluate criteria
summary_csm_clean$rsq     
summary_csm_clean$adjr2 
summary_csm_clean$cp      
summary_csm_clean$bic     

# Final model post-re-selection with 5 variables is the best choice.
final_model <- lm(Gross ~ Ratings + Budget + Screens + Sequel + Aggregate.Followers,
                  data = train_cleaned)
summary(final_model)

####################
# Final Model Diagnostics
####################

# Residuals vs. Fitted and Q-Q plots
par(mfrow = c(1, 2))
plot(final_model, which = 1)
plot(final_model, which = 2)

# Shapiro-Wilk test
shapiro.test(residuals(final_model))

# Breusch-Pagan test
bptest(final_model)

# VIF
vif(final_model)

####################
# Predictor Transformations: Power Transform for Non-Categorical Variables
####################

# Use powerTransform to suggest transformations for continuous predictors
trans_predictors <- powerTransform(cbind(Ratings, Budget, Screens, Sequel, Aggregate.Followers) ~ 1,
                                   data = train_cleaned, family = "bcPower")  # bcPower for Box-Cox family
summary(trans_predictors)
# Interpretation: Suggested lambdas (sqrt for Budget/Aggregate.Followers)

# Fit model with suggested transformations and check if assumptions improve
transformed_predictors_model <- lm(Gross ~ I(Ratings**(2.0)) + sqrt(Budget) + Screens + Sequel + sqrt(Aggregate.Followers),
                                   data = train_cleaned)
summary(transformed_predictors_model)

# Diagnostics: Residuals vs. Fitted (linearity/homoscedasticity) and Q-Q plot (normality)
par(mfrow = c(1, 2))
plot(transformed_predictors_model, which = 1)
abline(h = 0, lty = 2)  # Add horizontal line for reference
plot(transformed_predictors_model, which = 2)  

####################
# Response Transformation: Box-Cox for Gross As Assumptions Violated
####################

# Summarize Gross to confirm skewness
summary(train_cleaned$Gross)

# Apply Box-Cox to suggest lambda for Gross
boxcox_result <- boxCox(transformed_predictors_model, lambda = seq(-2, 2, by = 0.1))
lambda_optimal <- boxcox_result$x[which.max(boxcox_result$y)]
cat("Optimal lambda for Box-Cox transformation:", lambda_optimal, "\n")

# Compute 95% confidence interval for lambda
max_log_like <- max(boxcox_result$y)
ci_threshold <- max_log_like - 0.5 * qchisq(0.95, 1)
ci_lambdas <- boxcox_result$x[boxcox_result$y >= ci_threshold]
ci_lower <- min(ci_lambdas)
ci_upper <- max(ci_lambdas)
cat("95% Confidence Interval for lambda: [", ci_lower, ",", ci_upper, "]\n")

# Here, 0 and 0.5 are outside CI, but sqrt chosen for simplicity/interpretability—justify based on diagnostics

# Fit model with optimal lambda on response
if (lambda_optimal == 0) {
  # Handle lambda=0 as log (add small constant if Gross has zeros/min values)
  transformed_response_opt <- lm(log(Gross + 1e-6) ~ I(Ratings**(2.0)) + sqrt(Budget) + Screens + Sequel + sqrt(Aggregate.Followers),
                                 data = train_cleaned)
} else {
  transformed_response_opt <- lm(I(Gross^lambda_optimal) ~ I(Ratings**(2.0)) + sqrt(Budget) + Screens + Sequel + sqrt(Aggregate.Followers),
                                 data = train_cleaned)
}

# Now summarize the fitted model
summary(transformed_response_opt)

# Backward stepwise on optimal lambda model
step_opt <- step(transformed_response_opt, direction = "backward", trace = TRUE)
summary(step_opt)

# Fit model with chosen lambda=0.5 (sqrt(Gross)) for comparison/simplicity
transformed_response_chosen <- lm(sqrt(Gross) ~ I(Ratings**(2.0)) + sqrt(Budget) + Screens + Sequel + sqrt(Aggregate.Followers),
                                  data = train_cleaned)
summary(transformed_response_chosen)

# Backward stepwise on chosen lambda model
step_chosen <- step(transformed_response_chosen, direction = "backward", trace = TRUE)
summary(step_chosen)

# Select final transformed model (e.g., from chosen lambda stepwise)
final_transformed_model <- step_chosen

####################
# Re-Check Assumptions for Final Transformed Model
####################

# Set up plot grid
par(mfrow = c(2, 2))

# 1. Linearity & Homoscedasticity: Residuals vs. Fitted
plot(final_transformed_model, which = 1)
# Expected: Random scatter, no funnel shape (improved homoscedasticity)

# 2. Normality: Normal Q-Q Plot
plot(final_transformed_model, which = 2)
# Expected: Points closer to diagonal line

# 3. Scale-Location (alternative for heteroscedasticity)
plot(final_transformed_model, which = 3)

# 4. Residuals vs. Leverage (for outliers/influence)
plot(final_transformed_model, which = 5)

# Statistical tests
cat("\nShapiro-Wilk Test for Normality (Post-Transformation):\n")
print(shapiro.test(residuals(final_transformed_model))) # p-value = 0.1212

cat("\nBreusch-Pagan Test for Homoscedasticity (Post-Transformation):\n")
print(bptest(final_transformed_model)) # p-value = 0.1094

# Additional: Non-constant variance score test (from car)
ncvTest(final_transformed_model) # p-value = 0.0063485

cat("\nVIF for Multicollinearity (Post-Transformation):\n")
print(vif(final_transformed_model))

####################
# Interaction Terms: Test for Significant Interactions
####################

# Test interaction between sqrt(Budget) and Screens
interact_model_1 <- lm(sqrt(Gross) ~ I(Ratings**(2.0)) + sqrt(Budget) + Screens + Sequel + sqrt(Aggregate.Followers),
                       data = train_cleaned)

# ANOVA to compare with base transformed model (null: no interaction)
anova(final_transformed_model, interact_model_1) 

# Interpretation: As p-value = 0.5962 > 0.05, no significant interaction between Budget and Screens—retain base model.

# Test interaction between Ratings and sqrt(Budget)
interact_model_2 <- lm(sqrt(Gross) ~ I(Ratings**(2.0)) + sqrt(Budget) + Screens + Sequel + sqrt(Aggregate.Followers),
                       data = train_cleaned)

# ANOVA to compare with base transformed model
anova(final_transformed_model, interact_model_2)

# Interpretation: If p-value = 0.0008649 < 0.05, significant interaction exists—include it.

# Summary of the model with significant interaction
summary(interact_model_2)

# Visualize interaction ( effect plot for Ratings * sqrt(Budget))
library(effects)
effect_plot <- effect("Ratings:sqrt(Budget)", interact_model_2)
plot(effect_plot, multiline = TRUE, ci.style = "bands")

####################
# Build and Interpret Final Model
####################

# Select best model with significant interaction
best_model <- interact_model_2

# Interpretation of final model:
# - Response: sqrt(Gross) for reduced skewness/heteroscedasticity.
# - Key predictors: Ratings (interacts with sqrt(Budget)—higher ratings amplify returns on budget),
#   Screens (wider release boosts gross), Aggregate.Followers (social buzz drives revenue).
# - Assumptions improved via transformations; interactions capture non-additive effects.


####################
# Model Evaluation: Predictions on Validation Set
####################

# Predict on validation set (on transformed scale)
predictions_sqrt <- predict(best_model, newdata = valid)

# Back-transform to original scale
predictions_original <- predictions_sqrt^2

# Extract actual values
actuals <- valid$Gross

# Calculate performance metrics on original scale
mae <- mean(abs(predictions_original - actuals))
rmse <- sqrt(mean((predictions_original - actuals)^2))
r2 <- cor(predictions_original, actuals)^2

# Print metrics
cat("Validation Metrics on Original Scale:\n")
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")
cat("R²:", r2, "\n")

# Visualize Actual vs. Predicted
ggplot(data.frame(Actual = actuals, Predicted = predictions_original), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  ggtitle("Actual vs. Predicted Gross Revenue (Original Scale)") +
  xlab("Actual Gross") +
  ylab("Predicted Gross") +
  coord_fixed(ratio = 1)  # Square aspect for better comparison
# Interpretation: Points near the red line indicate good fit; deviations show errors/outliers.


####################
# Cross-Validation: Model Verification Using 10-Fold CV
####################

# Combine cleaned training and validation data for full CV dataset
# Note: Outliers were removed from train only; if needed, check and remove from valid as well (not done here)
full_data_for_cv <- rbind(train_cleaned, valid)

# Load caret for cross-validation
library(caret)

# Define control: 10-fold CV (method="cv"); repeats=5 makes it repeated CV but use "repeatedcv" for clarity
cv_control <- trainControl(method = "cv", number = 10, repeats = 5, savePredictions = "final")

# Train linear model with CV
cv_model <- train(
  sqrt(Gross) ~ I(Ratings**(2.0)) + sqrt(Budget) + Screens + Sequel + sqrt(Aggregate.Followers)
  method = "lm",
  trControl = cv_control
)

# Print CV results (metrics on transformed scale: sqrt(Gross))
print(cv_model)

# Extract predictions and compute metrics on original scale
cv_predictions_sqrt <- cv_model$pred$pred
cv_actuals_sqrt <- cv_model$pred$obs

# Back-transform to original Gross scale
cv_predictions_original <- cv_predictions_sqrt^2
cv_actuals_original <- cv_actuals_sqrt^2

# Calculate performance metrics
cv_mae <- mean(abs(cv_predictions_original - cv_actuals_original))
cv_rmse <- sqrt(mean((cv_predictions_original - cv_actuals_original)^2))
cv_r2 <- cor(cv_predictions_original, cv_actuals_original)^2

# Print metrics
cat("Cross-Validation Metrics on Original Scale:\n")
cat("MAE:", cv_mae, "\n")
cat("RMSE:", cv_rmse, "\n")
cat("R²:", cv_r2, "\n")

# Visualize Actual vs. Predicted from CV
cv_df <- data.frame(Actual = cv_actuals_original, Predicted = cv_predictions_original)
ggplot(cv_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  ggtitle("CV: Actual vs. Predicted Gross (Original Scale)") +
  xlab("Actual Gross") +
  ylab("Predicted Gross") +
  coord_fixed(ratio = 1)

####################
# Repeated Cross-Validation: Enhanced Stability with Repeats
####################

# Define control for repeated CV: 10 folds, 5 repeats
cv_control_repeated <- trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final")

# Train model with repeated CV
cv_model_repeated <- train(
  sqrt(Gross) ~ I(Ratings**(2.0)) + sqrt(Budget) + Screens + Sequel + sqrt(Aggregate.Followers),
  data = full_data_for_cv,
  method = "lm",
  trControl = cv_control_repeated
)

# Print repeated CV results
print(cv_model_repeated)

# Extract predictions and compute metrics on original scale (similar to above)
cv_predictions_sqrt_rep <- cv_model_repeated$pred$pred
cv_actuals_sqrt_rep <- cv_model_repeated$pred$obs

# Back-transform
cv_predictions_original_rep <- cv_predictions_sqrt_rep^2
cv_actuals_original_rep <- cv_actuals_sqrt_rep^2

# Metrics
cv_mae_rep <- mean(abs(cv_predictions_original_rep - cv_actuals_original_rep))
cv_rmse_rep <- sqrt(mean((cv_predictions_original_rep - cv_actuals_original_rep)^2))
cv_r2_rep <- cor(cv_predictions_original_rep, cv_actuals_original_rep)^2

# Print metrics
cat("Repeated CV Metrics on Original Scale:\n")
cat("MAE:", cv_mae_rep, "\n")
cat("RMSE:", cv_rmse_rep, "\n")
cat("R²:", cv_r2_rep, "\n")

# Visualize Actual vs. Predicted from repeated CV
cv_df_rep <- data.frame(Actual = cv_actuals_original_rep, Predicted = cv_predictions_original_rep)
ggplot(cv_df_rep, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  ggtitle("Repeated CV: Actual vs. Predicted Gross (Original Scale)") +
  xlab("Actual Gross") +
  ylab("Predicted Gross") +
  coord_fixed(ratio = 1)

####################
# Preparation for XGBoost: Install and Load Libraries
####################

# Install if needed: install.packages("xgboost")

library(xgboost)

# We use the combined, cleaned data for a robust comparison
full_data_for_cv <- rbind(train_cleaned, valid)

# Create a rich set of engineered features
engineered_data <- full_data_for_cv %>%
  mutate(
    # Key Transformations identified from the linear model analysis
    Ratings_sq = Ratings^2,
    sqrt_Budget = sqrt(Budget),
    sqrt_Screens = sqrt(Screens), # Including this as it was also considered
    sqrt_Aggregate.Followers = sqrt(Aggregate.Followers),
    
    # The crucial interaction term
    Interaction_Ratings_Budget = Ratings_sq * sqrt_Budget,
    
    # Ensure categorical variables are factors for one-hot encoding
    Sequel = as.factor(Sequel),
    Genre = as.factor(Genre),
    Year = as.factor(Year)
  )

# Define the target variable (we predict Gross directly)
target <- engineered_data$Gross

# Define the full set of predictors for XGBoost to choose from
# This includes original, transformed, and interaction features
predictors <- engineered_data %>%
  select(
    # Original core predictors
    Ratings, Budget, Screens, Aggregate.Followers, Dislikes,
    # Categorical predictors
    Sequel, Genre, Year,
    # Our new, powerful engineered features
    Ratings_sq, sqrt_Budget, sqrt_Screens, sqrt_Aggregate.Followers,
    Interaction_Ratings_Budget
  )

# Create the final design matrix using one-hot encoding
# This converts categorical variables into a numeric format suitable for XGBoost
predictors_encoded <- model.matrix(~ . - 1, data = predictors)

# Define a grid of hyperparameters to search through
tune_grid_xgb <- expand.grid(
  nrounds = c(100, 200, 300),      # Number of boosting rounds
  max_depth = c(3, 4, 6),         # Max tree depth
  eta = c(0.05, 0.1),             # Learning rate
  gamma = 0,                      # Minimum loss reduction for a split
  colsample_bytree = 0.8,         # Fraction of columns to use per tree
  min_child_weight = 1,           # Minimum sum of instance weight in a child
  subsample = 0.8                 # Fraction of observations to use per tree
)

# Set up the repeated cross-validation control
cv_control <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 5, 
                           verboseIter = FALSE)

# Train the tuned XGBoost model using our engineered features
set.seed(123)
xgb_model_engineered <- train(
  x = predictors_encoded,
  y = target,
  method = "xgbTree",
  trControl = cv_control,
  tuneGrid = tune_grid_xgb,
  verbosity = 0 # Suppress XGBoost's verbose output
)

cat("--- Best Tuned XGBoost Parameters (with Engineered Features) ---\n")
print(xgb_model_engineered$bestTune)

cat("--- FINAL PERFORMANCE COMPARISON ---\n")

# Get CV results from the final linear model
lm_cv_results <- cv_model_repeated$results
cat(sprintf("\nBest Linear Model (CV):           R-squared = %.4f\n", 
            lm_cv_results$Rsquared))

# Get CV results from the engineered XGBoost model
# We select the row corresponding to the best tune
best_xgb_tune_results <- xgb_model_engineered$results[which.min(xgb_model_engineered$results$RMSE), ]
cat(sprintf("Engineered XGBoost Model (CV):    R-squared = %.4f\n", 
            best_xgb_tune_results$Rsquared))