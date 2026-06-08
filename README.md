# Liver Cancer Radiomics — Multiclass Classification

Automatic classification of liver cancer type (3 classes) from CT radiomic features, using two data sources and three machine learning models.

---

## Clinical context

Each patient underwent a 4-phase contrast-enhanced CT scan:

| Phase | Code | Description |
|-------|------|-------------|
| Arterial | `ART` | Early arterial enhancement |
| Portal venous | `PORT` | Portal vein opacified |
| Venous | `VEIN` | Hepatic veins opacified |
| Late / delayed | `TARD` | Wash-out visible |

Radiomic features (shape, first-order statistics, texture) were extracted from the liver segmentation at each phase using **PyRadiomics**.

---

## Datasets

| Dataset | Description |
|---------|-------------|
| `radiomiques_global` | Whole-liver 3-D segmentation — one row per patient × phase |
| `multislice_excel_basic` | 2-D axial slice segmentations — one row per patient × phase × slice |

---

## Repository structure

```
.
├── 01_preprocessing_global.R     # Clean & reshape the global radiomics data
├── 02_feature_selection_lasso.R  # LASSO feature selection (multinomial)
├── 03_preprocessing_multislice.R # Clean & reshape the multislice data
├── 04_models_no_smote.R          # LASSO + RF + XGBoost — no oversampling
├── 05_models_smote.R             # LASSO + RF + XGBoost — with SMOTE
└── 06_visualization.R            # Accuracy comparison plot
```

### Execution order

```
01  →  02  →  04   (global, no SMOTE)
01  →  02  →  05   (global, with SMOTE)
03                  (multislice preprocessing, then feed into 04 or 05)
06                  (after collecting results from all runs)
```

---

## Experimental cases

| Case | Feature selection | Class balancing |
|------|-------------------|-----------------|
| `cas1` | None | None |
| `cas2_s` | None | SMOTE |
| `cas3_l` | LASSO | None |
| `cas4_l_s` | LASSO | SMOTE |

---

## Methods

### Feature selection
Multinomial LASSO (`glmnet`, α = 1) with 5-fold cross-validation. Features with non-zero coefficients for at least one class at `lambda.min` are retained.

### Class balancing
SMOTE (`smotefamily`) with K = 7 neighbours, applied in two passes to progressively balance the three cancer classes.

### Classifiers

| Model | Package | Key hyperparameters |
|-------|---------|---------------------|
| Multinomial LASSO | `glmnet` | α = 1, λ via 5-fold CV |
| Random Forest | `randomForest` | ntree = 500, mtry = √p |
| Gradient Boosting | `xgboost` | 100 rounds, `multi:softprob` |

---

## Dependencies

```r
install.packages(c(
  "dplyr", "tidyr", "stringr",
  "glmnet", "caret",
  "randomForest",
  "xgboost",
  "smotefamily",
  "ggplot2",
  "scales"
))
```

---

## Known issues fixed

- `03_preprocessing_multislice.R`: the original code used `vein_cols`, `tard_cols`, and `common_names` (pointing to the global dataset vectors) inside the multislice fusion loop. These have been corrected to `vein_cols_multislice`, `tard_cols_multislice`, and `common_names_multislice`.
- `03_preprocessing_multislice.R`: `summary(numeric_data)` corrected to `summary(numeric_data_multislice)`.
- Duplicate `library()` calls removed throughout.
- Interactive `View()` calls removed (not suitable for non-interactive / batch execution).
