
library(dplyr)

df_list <- lapply(clean_files, function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # 🧼 Forcer en character pour nettoyage
  df$trust_government <- as.character(df$trust_government)
  df$trust_government[df$trust_government %in% c("", "..")] <- NA
  df$trust_government <- as.numeric(df$trust_government)
  
  df$happiness_score <- as.numeric(as.character(df$happiness_score))
  df$gdp_per_capita <- as.numeric(as.character(df$gdp_per_capita))
  df$life_expectancy <- as.numeric(as.character(df$life_expectancy))
  df$freedom <- as.numeric(as.character(df$freedom))
  df$generosity <- as.numeric(as.character(df$generosity))
  df$year <- as.integer(df$year)
  
  return(df)
})

# ✅ Fusionner
df_all <- bind_rows(df_list)

# 🔍 Vérification rapide
glimpse(df_all)

# 📈 Boxplots pour détecter les valeurs aberrantes
graphics::par(mfrow = c(2, 2))  # 4 graphes sur une page

boxplot(df_all$gdp_per_capita,
        main = "PIB par habitant (toutes années)",
        col = "lightblue")

boxplot(df_all$trust_government,
        main = "Confiance dans le gouvernement (toutes années)",
        col = "lightcoral")

boxplot(df_all$generosity,
        main = "Générosité (toutes années)",
        col = "lightgreen")

boxplot(df_all$life_expectancy,
        main = "Espérance de vie (toutes années)",
        col = "orange")

library(ggplot2)

ggplot(df_all, aes(x = factor(year), y = happiness_score)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution du score de bonheur par année",
    x = "Année",
    y = "Score de bonheur"
  ) +
  theme_minimal()

#. Pays le plus heureux chaque année :
library(dplyr)

pays_plus_heureux <- df_all %>%
  group_by(year) %>%
  slice_min(happiness_rank, with_ties = FALSE) %>%
  select(year, country, happiness_score, happiness_rank)

print(pays_plus_heureux)

library(ggplot2)
library(dplyr)

# Sélectionner les pays les plus heureux par année
pays_plus_heureux <- df_all %>%
  group_by(year) %>%
  slice_min(happiness_rank, with_ties = FALSE) %>%
  select(year, country, happiness_score)

# Créer un graphique en barres
ggplot(pays_plus_heureux, aes(x = factor(year), y = happiness_score, fill = country)) +
  geom_col() +
  geom_text(aes(label = country), vjust = -0.5, size = 4) +
  labs(
    title = "Pays le plus heureux par année",
    x = "Année",
    y = "Score de bonheur",
    fill = "Pays"
  ) +
  theme_minimal()

#Pays le moins heureux chaque année
pays_moins_heureux <- df_all %>%
  group_by(year) %>%
  slice_max(happiness_rank, with_ties = FALSE) %>%
  select(year, country, happiness_score, happiness_rank)

print(pays_moins_heureux)

library(ggplot2)
library(dplyr)

# Sélectionner les pays les moins heureux par année
pays_moins_heureux <- df_all %>%
  group_by(year) %>%
  slice_max(happiness_rank, with_ties = FALSE) %>%
  select(year, country, happiness_score)

# Créer un graphique en barres
ggplot(pays_moins_heureux, aes(x = factor(year), y = happiness_score, fill = country)) +
  geom_col() +
  geom_text(aes(label = country), vjust = -0.5, size = 4) +
  labs(
    title = "Pays le moins heureux par année",
    x = "Année",
    y = "Score de bonheur",
    fill = "Pays"
  ) +
  theme_minimal()

#scores de bonheur par année
library(ggplot2)

ggplot(df_all, aes(x = factor(year), y = happiness_score)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Distribution du bonheur par année",
    x = "Année",
    y = "Score de bonheur"
  ) +
  theme_minimal()

#Moyenne du bonheur par année (barplot)
library(dplyr)
library(ggplot2)

# Calcul des moyennes
moyenne_bonheur <- df_all %>%
  group_by(year) %>%
  summarise(moyenne = mean(happiness_score, na.rm = TRUE))

# Affichage du graphe
ggplot(moyenne_bonheur, aes(x = factor(year), y = moyenne)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(moyenne, 2)), vjust = -0.5, size = 4) +
  labs(
    title = "Score de bonheur moyen par année",
    x = "Année",
    y = "Score moyen"
  ) +
  theme_minimal()

# heatmap des corrélations
library(dplyr)
library(ggplot2)
library(reshape2)  # pour transformer la matrice en long format
# Sélection des colonnes numériques pour la corrélation
numeric_cols <- df_all %>%
  select(happiness_score, gdp_per_capita, family, life_expectancy,
         freedom, trust_government, generosity, dystopia_residual, standard_error)
cor_matrix <- cor(numeric_cols, use = "complete.obs")
cor_melt <- melt(cor_matrix)

ggplot(data = cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Corrélation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  coord_fixed() +
  labs(title = "🔍 Heatmap des corrélations", x = "", y = "")

df_all %>% filter(country == "Finland") %>%
  ggplot(aes(x = year, y = happiness_score)) +
  geom_line() + geom_point() +
  labs(title = "Évolution du bonheur en Finlande", y = "Score", x = "Année")

#Analyse des variables qui influencent le bonheur
modele <- lm(happiness_score ~ gdp_per_capita + life_expectancy + freedom + trust_government, data = df_all)
summary(modele)
