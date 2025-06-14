# 📦 Packages déjà chargés
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# 🌍 Charger la carte du monde
world <- ne_countries(scale = "medium", returnclass = "sf")

# 📅 Filtrer pour 2019
df_2019 <- df_all %>% filter(year == 2019)

# 🔗 Fusionner les noms de pays (attention aux correspondances !)
map_data <- left_join(world, df_2019, by = c("name" = "country"))

# 🗺️ Créer la carte
ggplot(map_data) +
  geom_sf(aes(fill = happiness_score), color = "gray90") +
  scale_fill_viridis_c(option = "plasma", na.value = "lightgray") +
  labs(
    title = "🌐 Score de bonheur par pays (2019)",
    fill = "Score de bonheur"
  ) +
  theme_minimal()
