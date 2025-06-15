library(dplyr)

# 📁 Créer le dossier Data_Clean s'il n'existe pas
if (!dir.exists("Data_Clean")) dir.create("Data_Clean")

# 🔢 Années à traiter
years <- 2015:2019

# 📆 Nettoyage année par année
for (year in years) {
  cat("\n🔄 Traitement du fichier", year, "\n")
  
  path <- paste0("Data/", year, ".csv")
  
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
                 error = function(e) {
                   cat("\u274c Erreur lecture", year, "\n")
                   return(NULL)
                 })
  
  if (is.null(df)) next
  
  # Nettoyage des noms vides / dupliqués
  df <- df[, !(is.na(colnames(df)) | colnames(df) == "")]
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  
  if (year == 2015 && ncol(df) == 12) {
    colnames(df) <- c("country", "region", "happiness_rank", "happiness_score",
                      "standard_error", "gdp_per_capita", "family", "life_expectancy",
                      "freedom", "trust_government", "generosity", "dystopia_residual")
  }
  
  else if (year == 2016 && ncol(df) == 13) {
    df <- df %>% select(
      Country, Region, Happiness.Rank, Happiness.Score,
      Economy..GDP.per.Capita., Family, Health..Life.Expectancy.,
      Freedom, Trust..Government.Corruption., Generosity, Dystopia.Residual
    )
    colnames(df) <- c("country", "region", "happiness_rank", "happiness_score",
                      "gdp_per_capita", "family", "life_expectancy", "freedom",
                      "trust_government", "generosity", "dystopia_residual")
    df$standard_error <- NA
  }
  
  else if (year == 2017 && ncol(df) >= 9) {
    colnames(df)[1:9] <- c("country", "happiness_rank", "happiness_score",
                           "gdp_per_capita", "family", "life_expectancy",
                           "freedom", "generosity", "trust_government")
    df$standard_error <- NA
    df$dystopia_residual <- NA
    df$region <- NA
  }
  
  else if (year %in% c(2018, 2019) && ncol(df) >= 9) {
    colnames(df)[1:9] <- c("happiness_rank", "country", "happiness_score",
                           "gdp_per_capita", "family", "life_expectancy",
                           "freedom", "generosity", "trust_government")
    df$standard_error <- NA
    df$dystopia_residual <- NA
    df$region <- NA
  }
  
  else {
    cat("\u26a0️ Structure non reconnue pour", year, "\n")
    next
  }
  
  # Supprimer Israel si présent
  if ("country" %in% colnames(df)) {
    df <- df %>% filter(country != "Israel")
  }
  
  df$year <- year
  
  # Ordre cohérent des colonnes
  df <- df %>% select(
    country, region, happiness_rank, happiness_score, standard_error,
    gdp_per_capita, family, life_expectancy, freedom,
    trust_government, generosity, dystopia_residual, year
  )
  
  # Sauvegarde
  write.csv(df, paste0("Data_Clean/df", year, "_clean.csv"), row.names = FALSE)
  cat("\u2705 Fichier", year, "sauvegardé dans Data_Clean \n")
}

