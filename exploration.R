
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
