data_slic <- data_slic[, !grepl("^diagnostics", names(data_slic))]
library(dplyr)

# Make sure relevant columns exist
# Example columns assumed: patient_id, timepoint, slice_id, original_firstorder_TotalEnergy

# Filter to the slice with maximum TotalEnergy per patient and timepoint
filtered_data_slic <- data_slic %>%
  group_by(patient_num, temps_inj) %>%
  slice_max(order_by = original_firstorder_TotalEnergy, n = 1, with_ties = FALSE) %>%
  ungroup()
data_converted_multislice <- filtered_data_slic
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
data_converted_multislice[] <- lapply(data_converted_multislice, function(col) {
  if (is.character(col)) {
    convert_to_numeric(col)
  } else {
    col
  }
})
# maintenant extraire seulement les colonnes numériques
numeric_data_multislice <- data_converted_multislice[sapply(data_converted_multislice, is.numeric)]
summary(numeric_data)
#Transformation des données:
# Transformation : une ligne par patient, les variables séparées par phase
df_wide_multislice <- filtered_data_slic %>%
  pivot_wider(
    id_cols = patient_num,             # colonne identifiant le patient
    names_from = temps_inj,              # colonne contenant VEIN, TARD, etc.
    values_from = -c(patient_num,temps_inj )  # toutes les autres colonnes à élargir
  )



#Filtrer les phases VEIN, TARD
library(dplyr)
df_clean_multislice <- df_wide_multislice %>%
  filter(!(is.na(classe_name_VEIN) & is.na(classe_name_TARD)))

#Filtrer les phases PORT, ART:
library(dplyr)
df_clean1_multislice <- df_clean_multislice %>%
  filter(!is.na(classe_name_PORT) , !is.na(classe_name_ART))

#Manipuler les VEIN , TARD:
library(dplyr)
library(stringr)


vein_cols_multislice <- names(df_clean1_multislice)[str_detect(names(df_clean1_multislice), "_VEIN$")]
tard_cols_multislice <- names(df_clean1_multislice)[str_detect(names(df_clean1_multislice), "_TARD$")]


common_names_multislice <- intersect(
  str_remove(vein_cols, "_VEIN$"),
  str_remove(tard_cols, "_TARD$")
)


for (name in common_names) {
  vein_col <- paste0(name, "_VEIN")
  tard_col <- paste0(name, "_TARD")
  fused_col <- paste0(name, "_FUSED_VEIN_TARD")
  
  df_clean1_multislice[[fused_col]] <- mapply(function(a, b) {
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
  }, df_clean1_multislice[[vein_col]], df_clean1_multislice[[tard_col]])
  
  df_clean1_multislice[[vein_col]] <- NULL
  df_clean1_multislice[[tard_col]] <- NULL
}

#fusionner les classes


df_clean1_multislice <- df_clean1_multislice %>%
  mutate(classe_name = coalesce(
    classe_name_FUSED_VEIN_TARD,
    classe_name_PORT,
    classe_name_ART
  ))
df_clean1_multislice <- df_clean1_multislice %>%
  select(-classe_name_PORT, -classe_name_ART, -classe_name_FUSED_VEIN_TARD)
