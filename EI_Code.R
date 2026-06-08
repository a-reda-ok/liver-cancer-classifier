library(dplyr)
library(tidyr)

data_glo<- radiomiques_global
data_slic<- multislice_excel_basic


data_converted <- data_glo
# fonction de conversion sûre
convert_to_numeric <- function(x) {
  # essaie de convertir en numérique, sinon garde la colonne telle quelle
  suppressWarnings(num <- as.numeric(x))
  if (all(!is.na(num) | is.na(x))) {
    return(num)
  } else {
    return(x)
  }
}
data_converted[] <- lapply(data_converted, function(col) {
  if (is.character(col)) {
    convert_to_numeric(col)
  } else {
    col
  }
})
# maintenant extraire seulement les colonnes numériques
numeric_data <- data_converted[sapply(data_converted, is.numeric)]
summary(numeric_data)

#Transformation des données:
# Transformation : une ligne par patient, les variables séparées par phase
df_wide <- data_glo %>%
  pivot_wider(
    id_cols = patient_num,             # colonne identifiant le patient
    names_from = temps_inj,              # colonne contenant VEIN, TARD, etc.
    values_from = -c(patient_num,temps_inj )  # toutes les autres colonnes à élargir
  )

# Vérifie le résultat
View(df_wide)



#Filtrer les phases VEIN, TARD
library(dplyr)
df_clean <- df_wide %>%
  filter(!(is.na(classe_name_VEIN) & is.na(classe_name_TARD)))

#Filtrer les phases PORT, ART:
library(dplyr)
df_clean1 <- df_clean %>%
  filter(!is.na(classe_name_PORT) , !is.na(classe_name_ART))

#Manipuler les VEIN , TARD:
library(dplyr)
library(stringr)


vein_cols <- names(df_clean1)[str_detect(names(df_clean1), "_VEIN$")]
tard_cols <- names(df_clean1)[str_detect(names(df_clean1), "_TARD$")]


common_names <- intersect(
  str_remove(vein_cols, "_VEIN$"),
  str_remove(tard_cols, "_TARD$")
)


for (name in common_names) {
  vein_col <- paste0(name, "_VEIN")
  tard_col <- paste0(name, "_TARD")
  fused_col <- paste0(name, "_FUSED_VEIN_TARD")
  
  df_clean1[[fused_col]] <- mapply(function(a, b) {
    # Vérifie si les deux sont numériques
    if (is.numeric(a) && is.numeric(b)) {
      if (!is.na(a) && !is.na(b)) {
        return(mean(c(a, b)))  # moyenne des deux
      } else {
        return(coalesce(a, b))  # une seule des deux non NA
      }
    } else {
      # Si ce ne sont pas des numériques, prend la première non NA
      return(coalesce(a, b))
    }
  }, df_clean1[[vein_col]], df_clean1[[tard_col]])
  
  df_clean1[[vein_col]] <- NULL
  df_clean1[[tard_col]] <- NULL
}

#fusionner les classes


df_clean1 <- df_clean1 %>%
  mutate(classe_name = coalesce(
    classe_name_FUSED_VEIN_TARD,
    classe_name_PORT,
    classe_name_ART
  ))
df_clean1 <- df_clean1 %>%
  select(-classe_name_PORT, -classe_name_ART, -classe_name_FUSED_VEIN_TARD)

#supprimer diagnostic:
df_clean1 <- df_clean1[, !grepl("^diagnostics", names(df_clean1))]

# Load required packages
library(glmnet)


# X : variables explicatives
X <- as.matrix(df_clean1[, !(names(df_clean1) %in% c("classe_name", "patient_num"))])

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

