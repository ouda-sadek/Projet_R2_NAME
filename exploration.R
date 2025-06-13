# 📦 Charger les packages utiles
library(dplyr)

# 📂 Lire le fichier CSV (ajuste le chemin si besoin)
df2015 <- read.csv("Data/2015.csv", stringsAsFactors = TRUE)

# 👀 Aperçu du dataset
head(df2015)
str(df2015)
summary(df2015)

# 🔎 Vérifier les valeurs manquantes
colSums(is.na(df2015))

df2015 <- df2015[df2015$Country != "Israel", ]

any(df2015$Country == "Israel")



# 📦 Voir les valeurs aberrantes avec un boxplot (exemple sur une variable numérique)
boxplot(df2015$ma_colonne_num, main = "Valeurs aberrantes ?")

boxplot(df2015$Economy..GDP.per.Capita., main = "PIB par habitant", col = "lightblue")
boxplot(df2015$Trust..Government.Corruption., main = "Confiance dans le gouvernement", col = "salmon")
boxplot(df2015$Generosity, main = "Générosité", col = "lightgreen")
boxplot(df2015$Health..Life.Expectancy., main = "Espérance de vie", col = "orange")

