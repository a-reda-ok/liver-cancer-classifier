

# X : variables explicatives
X <- as.matrix(df_clean1[, !(names(df_clean1) %in% c("classe_name", "patient_num"))])

X <- X[, colnames(X) %in% unique_vars]
# Y : variable cible catégorielle
Y <- factor(df_clean1$classe_name)  # s'assurer que c'est un facteur

library(caret)
# Define train-test split ratio
train_indices <- createDataPartition(Y, p = 0.7, list = FALSE)

# Split into training and test sets
X_train <- X[train_indices, ]
X_test  <- X[-train_indices, ]

Y_train <- Y[train_indices]
Y_test  <- Y[-train_indices]
cat("\nTraining set class proportions:\n")
print(prop.table(table(Y_train)))

cat("\nTest set class proportions:\n")
print(prop.table(table(Y_test)))
# Convert predictors to matrix format for glmnet
X_train_mat <- as.matrix(X_train)
X_test_mat  <- as.matrix(X_test)

# Fit LASSO with cross-validation
cv_lasso <- cv.glmnet(
  x = X_train_mat,
  y = Y_train,
  family = "multinomial",
  alpha = 1,               # LASSO
  type.measure = "class",  # classification error
  nfolds = 5
)

# Optimal lambda
best_lambda <- cv_lasso$lambda.min

# Refit model on training set with best lambda
lasso_model <- glmnet(
  x = X_train_mat,
  y = Y_train,
  family = "multinomial",
  alpha = 1,
  lambda = best_lambda
)

coef(lasso_model)  # Affiche les coefficients pour chaque classe
selected_vars <- lapply(coef(lasso_model), function(mat) {
  rownames(mat)[which(mat!=0)]
})
all_vars <- unlist(selected_vars)

# Get unique values (no duplicates)
unique_vars <- unique(all_vars)

print(unique_vars)
# Predict class labels on test set
predicted_classes <- predict(lasso_model, newx = X_test_mat, type = "class")
predicted_classes

# Confusion matrix and accuracy
confusion_matrix <- table(Predicted = predicted_classes, Actual = Y_test)
print(confusion_matrix)


accuracy <- mean(predicted_classes == Y_test)
print(paste("Test Accuracy:", round(accuracy, 3)))

#2_Random forest
library(randomForest)
Y_train_factor <- as.factor(Y_train)

# Fit Random Forest
rf_model <- randomForest(
  x = X_train,
  y = Y_train_factor,
  ntree = 500,
  mtry = floor(sqrt(ncol(X_train))),  # default for classification
  importance = TRUE
)

rf_pred <- predict(rf_model, newdata = X_test)
rf_acc <- mean(rf_pred == Y_test)
cat("Random Forest Accuracy:", round(rf_acc, 4), "\n")

# Confusion Matrix
conf_matrix_rf <- table(Predicted = rf_pred, Actual = Y_test)
print(conf_matrix_rf)

# Global accuracy = proportion of correct predictions
rf_acc <- mean(rf_pred == Y_test)

cat("Global Accuracy (Random Forest):", round(rf_acc, 4), "\n")



#3_gradient boosting
library(xgboost)

# xgboost attend des matrices numériques et labels en 0/1, donc :
Y_all <- factor(c(Y_train, Y_test))  # Ensures consistent factor levels

# Create a mapping of labels to numbers (0-based)
label_map <- levels(Y_all)

# Convert to numeric labels (0-based)
label_train <- as.numeric(factor(Y_train, levels = label_map)) - 1
label_test  <- as.numeric(factor(Y_test,  levels = label_map)) - 1

X_train_num <- apply(X_train, 2, function(x) as.numeric(as.character(x)))
X_test_num  <- apply(X_test, 2, function(x) as.numeric(as.character(x)))
X_train_matrix <- as.matrix(X_train_num)
X_test_matrix  <- as.matrix(X_test_num)
nrow(X_train_matrix)        # nombre de lignes de la matrice de données
length(label_train) 
dtrain <- xgb.DMatrix(data = X_train_matrix, label = label_train)
dtest <- xgb.DMatrix(data = X_test_matrix, label = label_test)

params <- list(
  objective = "multi:softprob",     # or "multi:softmax" if you want class indices
  eval_metric = "mlogloss",         # suitable for multiclass classification
  num_class = 3                     # set this to the number of unique classes
)

xgb_model_sel <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain),
  verbose = 0
)
# Predict probabilities
xgb_pred_prob <- predict(xgb_model_sel, dtest)

# Reshape to a matrix: one row per observation, one column per class
xgb_pred_matrix <- matrix(xgb_pred_prob, ncol = 3, byrow = TRUE)

# Choose the class index with the highest probability (0-based)
xgb_pred_class_idx <- max.col(xgb_pred_matrix) - 1

# Map back to word labels (assuming label_map exists)
# You should've defined earlier: label_map <- levels(factor(Y_train))
label_map <- levels(factor(Y_train))
xgb_pred_class <- label_map[xgb_pred_class_idx + 1]

# Calculate accuracy
xgb_acc <- mean(xgb_pred_class == Y_test)

# Optional: print accuracy
print(paste("XGBoost Accuracy:", round(xgb_acc, 3)))
print(paste("Random Forest Accuracy:", round(rf_acc, 4)))
print(paste("Lasso Accuracy:", round(accuracy, 3)))
