# Vecteurs simulés pour chaque modèle et métrique
cas <- rep(c("cas1", "cas2_s", "cas3_l", "cas4_l_s"), each = 3)

modèles <- rep(c("LASSO", "Random Forest", "Gradient Boosting"), times = 4)

accuracy <- c(0.605, 0.526, 0.579,   # cas1
              0.803, 0.697, 0.742,   # cas2
              0.632, 0.736, 0.658,   # cas3
              0.742, 0.803, 0.818)   # cas4

# Créer le dataframe
df_perf <- data.frame(cas = cas, modèle = modèles, accuracy = accuracy)

library(tidyr)
library(ggplot2)

# Reshape en format long
df_long <- pivot_longer(df_perf, cols = c("accuracy"),
                        names_to = "métrique", values_to = "valeur")

# Graphique
ggplot(df_long, aes(x = cas, y = valeur, color = modèle, group = modèle)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~métrique, scales = "free_y") +
  labs(title = "Comparaison des modèles : Accuracy",
       x = "Cas", y = "Valeur", color = "Modèle") +
  theme_minimal() +
  theme(legend.position = "bottom")