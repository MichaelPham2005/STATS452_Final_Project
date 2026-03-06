# Install necessary packages if not already installed
# install.packages(c("missForest", "MASS", "car", "brglm"))

# Load libraries
library(missForest)
library(MASS)
library(car)
library(brglm)  # For handling separation in logistic regression

# Set working directory to the location of the file
setwd("D:\\B\\St\\GAIN-master\\GAIN-master\\data")

# Read the data
data <- read.csv("heart_disease_uci.csv")

# Identify qualitative variables and factor them
# Based on description: sex, cp, fbs, restecg, exang, slope, ca, thal, num
qual_vars <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal", "num","dataset")
data[qual_vars] <- lapply(data[qual_vars], as.factor)

# Handle missing values using missForest
# missForest imputes both numeric and categorical variables
imputed_data <- missForest(data)$ximp

# Create binary target for heart disease presence (num == "0" vs others)
# Fix: Use levels directly since num is factor
imputed_data$diseased <- ifelse(imputed_data$num == "0", 0, 1)
imputed_data$diseased <- as.factor(imputed_data$diseased)

# List of predictor variables
predictors <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalch", "exang", "oldpeak", "slope", "ca", "thal")

# Univariate logistic regression models
univariate_models <- list()
for (pred in predictors) {
  formula <- as.formula(paste("diseased ~", pred))
  # Use brglm to handle potential separation
  model <- glm(formula, family = binomial(link = "logit"), data = imputed_data)
  univariate_models[[pred]] <- model
  cat("\nUnivariate model for", pred, ":\n")
  print(summary(model))
  confuseMatrix <- table(predict(model, type = "response") > 0.5, imputed_data$diseased)
  print(confuseMatrix)
  accuracy_binary <- sum(diag(confuseMatrix)) / sum(confuseMatrix)
  print(accuracy_binary)
}

# Identify continuous variables for potential transformation
cont_vars <- c("age", "trestbps", "chol", "thalch", "oldpeak")

# Binary logistic regression: initial full model without transformation
# Use brglm to handle separation
full_binary_model <- glm(diseased ~ age + sex + cp + trestbps + chol + fbs + restecg + thalch + exang + oldpeak + slope + ca + thal,
                           family = binomial(link = "logit"), data = imputed_data)
summary(full_binary_model)
#caculate accuracy and matrix
confuseMatrix <- table(predict(full_binary_model, type = "response") > 0.5, imputed_data$diseased)
print(confuseMatrix)
accuracy_binary <- sum(diag(confuseMatrix)) / sum(confuseMatrix)
print(accuracy_binary)

# Apply Box-Tidwell test to check for optimal power transformations on continuous variables
# Note: Box-Tidwell requires all variables to be positive; if any are zero or negative, shift them (e.g., add a constant)
# In this dataset, oldpeak can be negative (from description: -2.6 to 6.2), so shift it to be positive
imputed_data$oldpeak_shifted <- imputed_data$oldpeak + 3  # Shift to make min >0 (min is -2.6, +3 makes it 0.4)
shift_amount <- 1  # Small positive shift


#output the data to csv
write.csv(imputed_data, "imputed_data1.csv", row.names = FALSE)
#write if the is a row in age , trestbps , chol , thalch , oldpeak_shifted have data <=0 them delete this data row
imputed_data <- imputed_data[imputed_data$age > 0 & imputed_data$trestbps > 0 & imputed_data$chol > 0 & imputed_data$thalch > 0 & imputed_data$oldpeak_shifted > 0, ]

# Now run Box-Tidwell on continuous vars (use shifted oldpeak)
# Box-Tidwell is for glm, but since we have separation, it might not work directly; proceed with caution or use glm for test
# Using glm for Box-Tidwell, ignoring separation warning temporarily
bt_result <- boxTidwell(as.numeric(diseased) - 1 ~ age + trestbps + chol + thalch + oldpeak_shifted,
                        ~ sex + cp + fbs + restecg + exang + slope + ca + thal,
                        data = imputed_data)

# Print Box-Tidwell results to see suggested powers and p-values
print(bt_result)
# Now run Box-Tidwell on continuous vars (use shifted oldpeak)
# Box-Tidwell is for glm, but since we have separation, it might not work directly; proceed with caution

# Print Box-Tidwell results to see suggested powers and p-values

# Extract powers
powers <- bt_result$result[, "MLE of lambda"]
names(powers) <- rownames(bt_result$result)
for (var in names(powers)){
  print(paste("Variable:", var, "Power:", powers[var]))
}
# Apply transformations
for (var in names(powers)) {
  if (var == "oldpeak_shifted") {
    imputed_data$oldpeak_trans <- (imputed_data[[var]]) ^ powers[var]
  } else {
    imputed_data[[paste0(var, "_trans")]] <- imputed_data[[var]] ^ powers[var]
  }
}

write.csv(imputed_data, "imputed_data_transformed.csv", row.names = FALSE)

# Update full binary model with transformed variables (replace originals)
# Use brglm
full_binary_model_trans <- glm(diseased ~ age_trans + sex + cp + trestbps_trans + chol_trans + fbs + restecg + thalch_trans + exang + oldpeak_trans + slope + ca + thal,
                                 family = binomial(link = "logit"), data = imputed_data)
summary(full_binary_model_trans)
confuseMatrix <- table(predict(full_binary_model_trans, type = "response") > 0.5, imputed_data$diseased)
print(confuseMatrix)
accuracy_binary <- sum(diag(confuseMatrix)) / sum(confuseMatrix)
print(accuracy_binary)
# Stepwise AIC for variable selection on transformed model
# Note: stepAIC works with brglm? If not, use glm and handle warnings
step_binary_model <- stepAIC(full_binary_model, direction = "both", trace = FALSE)

# Summary of the selected binary model
summary(step_binary_model)

step_confuseMatrix <- table(predict(step_binary_model, type = "response") > 0.5, imputed_data$diseased)
print(step_confuseMatrix)
accuracy_binary <- sum(diag(step_confuseMatrix)) / sum(step_confuseMatrix)
print(accuracy_binary)
#run BIC
step_binary_model_bic <- stepAIC(full_binary_model, direction = "both", trace = FALSE, k = log(nrow(imputed_data)))
summary(step_binary_model_bic)
BIC_confuseMatrix <- table(predict(step_binary_model_bic, type = "response") > 0.5, imputed_data$diseased)
print(BIC_confuseMatrix)
BIC_accuracy_binary <- sum(diag(BIC_confuseMatrix)) / sum(BIC_confuseMatrix)
print(BIC_accuracy_binary)
## Choose this model


# Now, for the subset of patients with heart disease (num > 0)
diseased_subset <- imputed_data[imputed_data$num != "0", ]

# Treat num as ordered factor for ordinal logistic regression (stages 1-4)
diseased_subset$num <- factor(diseased_subset$num, levels = c("1", "2", "3", "4"), ordered = TRUE)
write.csv(diseased_subset, "diseased_subset.csv", row.names = FALSE)
diseased_subset <- droplevels(diseased_subset)


#delete diseased	oldpeak_shifted	age_trans	trestbps_trans	chol_trans	thalch_trans	oldpeak_trans
diseased_subset <- diseased_subset[, !colnames(diseased_subset) %in% c("diseased", "oldpeak_shifted", "age_trans", "trestbps_trans", "chol_trans", "thalch_trans", "oldpeak_trans")]

# Ordinal logistic regression (cumulative odds/proportional odds model): full model with transformations
# polr doesn't handle separation directly; if issues, consider alternatives
full_ordinal_model <- polr(num ~ ., data = diseased_subset, Hess = TRUE)
summary(full_ordinal_model)
# Stepwise AIC for variable selection
step_ordinal_model <- stepAIC(full_ordinal_model, direction = "both", trace = FALSE)
AIC_ordered_confuseMatrix <- table(predict(step_ordinal_model, type = "class"), diseased_subset$num)
print(AIC_ordered_confuseMatrix)
accuracy <- sum(diag(AIC_ordered_confuseMatrix)) / sum(AIC_ordered_confuseMatrix)
print(accuracy)
step_ordinal_model_bic <- stepAIC(full_ordinal_model, direction = "both", trace = FALSE, k = log(nrow(diseased_subset)))
summary(step_ordinal_model_bic)
BIC_ordered_confuseMatrix <- table(predict(step_ordinal_model_bic, type = "class"), diseased_subset$num)
print(BIC_ordered_confuseMatrix)
BIC_accuracy <- sum(diag(BIC_ordered_confuseMatrix)) / sum(BIC_ordered_confuseMatrix)
print(BIC_accuracy)
# Summary of the selected ordinal model
summary(step_ordinal_model)








