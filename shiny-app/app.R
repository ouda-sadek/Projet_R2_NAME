library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(shinycssloaders)
library(reshape2)
library(stringr)

# 🔄 Charger les données avec nettoyage correct
suppressWarnings({
  df_all <- list.files("Data_Clean", full.names = TRUE, pattern = ".csv$") %>%
    lapply(function(file) {
      df <- read.csv(file, stringsAsFactors = FALSE)
      df$trust_government  <- as.numeric(na_if(trimws(df$trust_government), ".."))
      df$happiness_score   <- as.numeric(df$happiness_score)
      df$gdp_per_capita    <- as.numeric(df$gdp_per_capita)
      df$life_expectancy   <- as.numeric(df$life_expectancy)
      df$freedom           <- as.numeric(df$freedom)
      df$generosity        <- as.numeric(df$generosity)
      df$year              <- as.integer(df$year)
      df$region            <- if (!"region" %in% colnames(df)) NA else df$region
      return(df)
    }) %>%
    bind_rows()
})

# 🤖 Fonction chatbot améliorée
repondre_question <- function(question, df) {
  question <- tolower(question)
  
  # Questions sur les pays
  if (str_detect(question, "pays le plus heureux|quel pays est le plus heureux")) {
    df_filtered <- df %>% filter(!is.na(happiness_score))
    max_score <- max(df_filtered$happiness_score, na.rm = TRUE)
    pays <- df_filtered %>% 
      filter(happiness_score == max_score) %>% 
      pull(country) %>% 
      unique()
    annee <- df_filtered %>% 
      filter(happiness_score == max_score) %>% 
      pull(year) %>% 
      unique()
    return(paste("Le pays le plus heureux est", pays, "avec un score de", round(max_score, 2), "en", annee))
    
  } else if (str_detect(question, "pays le moins heureux|quel pays est le moins heureux")) {
    df_filtered <- df %>% filter(!is.na(happiness_score))
    min_score <- min(df_filtered$happiness_score, na.rm = TRUE)
    pays <- df_filtered %>% 
      filter(happiness_score == min_score) %>% 
      pull(country) %>% 
      unique()
    annee <- df_filtered %>% 
      filter(happiness_score == min_score) %>% 
      pull(year) %>% 
      unique()
    return(paste("Le pays le moins heureux est", pays, "avec un score de", round(min_score, 2), "en", annee))
    
  } else if (str_detect(question, "pays avec le pib le plus élevé")) {
    df_filtered <- df %>% filter(!is.na(gdp_per_capita))
    max_gdp <- max(df_filtered$gdp_per_capita, na.rm = TRUE)
    pays <- df_filtered %>% 
      filter(gdp_per_capita == max_gdp) %>% 
      pull(country) %>% 
      unique()
    return(paste("Le pays avec le PIB par habitant le plus élevé est", pays, "avec", round(max_gdp, 2)))
    
    # Questions sur les statistiques globales
  } else if (str_detect(question, "score moyen|moyenne de bonheur")) {
    score_moyen <- mean(df$happiness_score, na.rm = TRUE)
    return(paste("Le score moyen de bonheur est :", round(score_moyen, 2)))
    
  } else if (str_detect(question, "corrélation")) {
    numeric_data <- df %>% select(happiness_score, gdp_per_capita, life_expectancy, freedom, trust_government, generosity) %>% na.omit()
    corr <- round(cor(numeric_data), 2)
    strongest <- which(abs(corr) == max(abs(corr[corr != 1])), arr.ind = TRUE)
    vars <- rownames(corr)[strongest[1,]]
    return(paste("La corrélation la plus forte est entre", vars[1], "et", vars[2], "(", corr[strongest[1,1], strongest[1,2]], ")"))
    
    # Questions sur les facteurs influents
  } else if (str_detect(question, "facteur.*bonheur|qu'est-ce qui influence le bonheur")) {
    model <- lm(happiness_score ~ gdp_per_capita + life_expectancy + freedom + trust_government, data = df)
    coefs <- summary(model)$coefficients[-1,]
    most_influential <- rownames(coefs)[which.max(coefs[,1])]
    return(paste("D'après l'analyse, le facteur le plus influent sur le bonheur est:", most_influential,
                 "avec un coefficient de", round(coefs[which.max(coefs[,1]), 1])))
    
    # Questions temporelles
  } else if (str_detect(question, "évolution|comment a évolué")) {
    evolution <- df %>% 
      group_by(year) %>% 
      summarise(mean_happiness = mean(happiness_score, na.rm = TRUE)) %>% 
      arrange(year)
    diff <- round(last(evolution$mean_happiness) - first(evolution$mean_happiness), 2)
    if (diff > 0) {
      return(paste("Le bonheur moyen a augmenté de", diff, "points entre", first(evolution$year), "et", last(evolution$year)))
    } else {
      return(paste("Le bonheur moyen a diminué de", abs(diff), "points entre", first(evolution$year), "et", last(evolution$year)))
    }
    
    # Questions sur les régions
  } else if (str_detect(question, "région.*heureuse|quelle région est la plus heureuse")) {
    region_data <- df %>% 
      group_by(region) %>% 
      summarise(mean_happiness = mean(happiness_score, na.rm = TRUE)) %>% 
      arrange(desc(mean_happiness))
    return(paste("La région la plus heureuse est", region_data$region[1], 
                 "avec un score moyen de", round(region_data$mean_happiness[1], 2)))
    
    # Questions spécifiques sur un pays
  } else if (str_detect(question, "score.*(france|usa|canada|chine|japon|allemagne|italie|espagne|royaume-uni|brésil|inde)")) {
    country <- str_extract(question, "france|usa|canada|chine|japon|allemagne|italie|espagne|royaume-uni|brésil|inde")
    country_data <- df %>% 
      filter(str_detect(tolower(country), tolower(country))) %>% 
      arrange(desc(year))
    if (nrow(country_data) > 0) {
      latest <- country_data[1,]
      return(paste("En", latest$year, country, "avait un score de bonheur de", 
                   round(latest$happiness_score, 2), "(PIB:", round(latest$gdp_per_capita, 2),
                   "Espérance de vie:", round(latest$life_expectancy, 2), ")"))
    } else {
      return(paste("Je n'ai pas de données pour", country))
    }
    
    # Réponse par défaut
  } else {
    suggestions <- c(
      "Essayez une de ces questions :",
      "- Quel est le pays le plus heureux?",
      "- Quelle est la corrélation avec le bonheur?",
      "- Quel facteur influence le plus le bonheur?",
      "- Comment a évolué le bonheur?",
      "- Quelle région est la plus heureuse?",
      "- Quel est le score de la France?"
    )
    return(paste(suggestions, collapse = "\n"))
  }
}

# ✅ Interface utilisateur
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = span("🌟 BONHEUR UNIVERSEL", style = "font-weight: bold; font-size: 24px; color:#00ffe1; font-family: 'Orbitron', sans-serif; letter-spacing: 2px;")),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("🏠 Accueil", tabName = "home", icon = icon("home")),
                menuItem("🌍 Carte mondiale", tabName = "carte", icon = icon("globe")),
                menuItem("📊 Graphiques", tabName = "graph", icon = icon("chart-line")),
                menuItem("📊 Graphiques avancés", tabName = "advanced", icon = icon("chart-bar")),
                menuItem("🧠 Analyse statistique", tabName = "analyse", icon = icon("brain")),
                menuItem("📊 Comparateur de pays", tabName = "compare", icon = icon("balance-scale")),
                menuItem("🔍 Explorer les données", tabName = "explorer", icon = icon("search")),
                menuItem("🤖 ChatBot", tabName = "chatbot", icon = icon("robot")),
                menuItem("🔬 Modèles linéaires", tabName = "models", icon = icon("sliders-h"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(
        "@import url('https://fonts.googleapis.com/css2?family=Orbitron:wght@500&display=swap');
        .content-wrapper, .right-side {
          background-color: #0f0f0f;
          color: #d1d1d1;
          font-family: 'Orbitron', sans-serif;
        }
        .box {
          border-radius: 10px;
          border: 1px solid #555;
          background: linear-gradient(to bottom, #1a1a1a, #0f0f0f);
          box-shadow: 0 0 10px #00ffe1;
        }
        .intro-box {
          background-color: #222;
          padding: 30px;
          border-radius: 12px;
          border: 1px solid #555;
          box-shadow: 0 0 20px #ff0080;
        }
        .intro-text {
          font-size: 16px;
          line-height: 1.6;
          color: #d1d1d1;
          font-family: 'Orbitron', sans-serif;
        }
        h2, h3 {
          color: #00ffe1;
          text-align: center;
          font-weight: bold;
          text-shadow: 0 0 8px #00ffe1;
        }
        .fa {
          animation: pulse 2s infinite;
        }
        @keyframes pulse {
          0% { transform: scale(1); }
          50% { transform: scale(1.15); }
          100% { transform: scale(1); }
        }
        .skin-black .main-sidebar {
          background: linear-gradient(to bottom, #1f1c2c, #928dab);
        }
        .img-preview {
          border: 1px solid #00ffe1;
          border-radius: 8px;
          padding: 5px;
          transition: transform 0.3s;
        }
        .img-preview:hover {
          transform: scale(1.02);
          box-shadow: 0 0 15px #ff0080;
        }
        .intro-box {
          animation: fadeIn 2s ease-in-out;
        }
        @keyframes fadeIn {
          0% {opacity: 0;}
          100% {opacity: 1;}
        }"
      ))
    ),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12, class = "intro-box",
                    h2("Bienvenue sur l'application Bonheur Universel"),
                    HTML("<h4 style='text-align:center; font-style: italic; color:#00ffe1;'>\"Le bonheur n’est pas une destination, c’est une façon de voyager.\"</h4>"),
                    p(class = "intro-text", "Ce dashboard explore les données du World Happiness Report de 2015 à 2019.")
                )
              ),
              fluidRow(
                box(width = 12, title = "🎯 Objectifs", class = "intro-box",
                    HTML("<ul class='intro-text'>
              <li>🔍 Comprendre les facteurs qui influencent le bonheur mondial</li>
              <li>🌍 Visualiser les tendances sur une carte interactive</li>
              <li>🧠 Explorer les corrélations et modèles statistiques</li>
              <li>🤖 Discuter avec un chatbot IA intégré</li>
            </ul>")
                )
              ),
              fluidRow(
                box(width = 4, title = "Score du bonheur", img(src = "Rplot.png", width = "100%", class = "img-preview")),
                box(width = 4, title = "Bonheur en Finlande", img(src = "Rplot01.png", width = "100%", class = "img-preview")),
                box(width = 4, title = "Moyenne par année", img(src = "Rplot02.png", width = "100%", class = "img-preview"))
              ),
              fluidRow(
                box(width = 6, title = "Distribution du Bonheur par année", img(src = "Rplot03.png", width = "100%", class = "img-preview")),
                box(width = 6, title = "Pays le moins heureux par année", img(src = "Rplot04.png", width = "100%", class = "img-preview"))
              ),
              fluidRow(
                box(width = 4, title = "Pays le plus heureux par année", img(src = "Rplot05.png", width = "100%", class = "img-preview")),
                box(width = 4, title = "PIB par habitant", img(src = "Rplot07.png", width = "100%", class = "img-preview"))
              )
      ),
      tabItem(tabName = "carte",
              fluidRow(
                box(width = 4, selectInput("selected_year", "Choisir l'année:", choices = sort(unique(df_all$year)), selected = 2019)),
                box(width = 12, withSpinner(leafletOutput("map_plot")))
              )
      ),
      tabItem(tabName = "graph",
              fluidRow(box(width = 12, withSpinner(plotOutput("boxplot_plot"))))
      ),
      tabItem(tabName = "advanced",
              fluidRow(box(width = 12, withSpinner(plotOutput("heatmap_plot"))))
      ),
      tabItem(tabName = "analyse",
              fluidRow(box(width = 12, withSpinner(verbatimTextOutput("summary_stats"))))
      ),
      tabItem(tabName = "compare",
              fluidRow(
                box(width = 3, selectInput("country1", "Pays 1:", choices = unique(df_all$country))),
                box(width = 3, selectInput("country2", "Pays 2:", choices = unique(df_all$country))),
                box(width = 3, selectInput("year_comp", "Année:", choices = unique(df_all$year), selected = 2019)),
                box(width = 12, withSpinner(plotOutput("compare_plot")))
              )
      ),
      tabItem(tabName = "chatbot",
              fluidRow(
                box(width = 12, title = "💬 Assistant IA", textInput("user_input", "Posez votre question :", ""),
                    actionButton("ask_button", "Envoyer"),
                    verbatimTextOutput("bot_reply"))
              )
      ),
      tabItem(tabName = "models",
              fluidRow(
                box(width = 12, title = "Test de modèles : effet du PIB", class = "box",
                    verbatimTextOutput("model_tests"))
              )
      ),
      tabItem(tabName = "explorer",
              fluidRow(
                box(width = 4, selectInput("filter_country", "Pays:", choices = unique(df_all$country))),
                box(width = 4, selectInput("filter_var", "Variable:", choices = c("happiness_score", "gdp_per_capita", "life_expectancy", "freedom", "trust_government", "generosity"))),
                box(width = 12, withSpinner(plotlyOutput("filtered_plot")))
              )
      )
    )
  )
)


# ✅ Serveur
server <- function(input, output, session) {
  output$map_plot <- renderLeaflet({
    world <- ne_countries(scale = "medium", returnclass = "sf")
    df_selected <- df_all %>% filter(year == input$selected_year)
    world <- left_join(world, df_selected, by = c("name" = "country"))
    pal <- colorNumeric("plasma", domain = world$happiness_score, na.color = "#cccccc")
    
    output$filtered_plot <- renderPlotly({
      req(input$filter_country, input$filter_var)
      df_filtered <- df_all %>% 
        filter(country == input$filter_country & !is.na(.data[[input$filter_var]]))
      
      if (nrow(df_filtered) == 0) {
        return(plotly_empty(type = "scatter", mode = "lines") %>%
                 layout(title = list(text = "Aucune donnée disponible pour cette combinaison.", font = list(color = '#ff0080')),
                        plot_bgcolor = '#0f0f0f', paper_bgcolor = '#0f0f0f'))
      }
      
      plot_ly(df_filtered,
              x = ~year,
              y = ~.data[[input$filter_var]],
              type = 'scatter',
              mode = 'lines+markers',
              line = list(color = '#00ffe1'),
              marker = list(color = '#ff0080')) %>%
        layout(
          title = paste("Évolution de", input$filter_var, "pour", input$filter_country),
          xaxis = list(title = "Année"),
          yaxis = list(title = input$filter_var),
          plot_bgcolor = '#0f0f0f',
          paper_bgcolor = '#0f0f0f',
          font = list(color = '#d1d1d1')
        )
    })
    

    
    
    
    leaflet(world) %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      addPolygons(fillColor = ~pal(happiness_score), color = "white", weight = 1, fillOpacity = 0.8,
                  popup = ~paste0("<b>", name, "</b><br>Score: ", round(happiness_score, 2))) %>%
      addLegend(pal = pal, values = ~happiness_score, title = "Score de bonheur", position = "bottomright", opacity = 1)
  })
  
  output$boxplot_plot <- renderPlot({
    ggplot(df_all, aes(x = as.factor(year), y = happiness_score, fill = as.factor(year))) +
      geom_boxplot(color = "blue", alpha = 0.4) +
      labs(title = "Distribution du bonheur par année", x = "Année", y = "Score de bonheur") +
      theme_minimal()
  })
  
  output$heatmap_plot <- renderPlot({
    numeric_data <- df_all %>% select(happiness_score, gdp_per_capita, life_expectancy, freedom, trust_government, generosity) %>% na.omit()
    corr <- round(cor(numeric_data), 2)
    ggplot(melt(corr), aes(Var1, Var2, fill = value)) + geom_tile() + geom_text(aes(label = value), color = "white") +
      scale_fill_gradient2(low = "#ff0080", high = "#00ffe1", mid = "#000000", midpoint = 0) +
      theme_minimal() + labs(title = "Matrice de corrélation")
  })
  
  output$summary_stats <- renderPrint({
    model <- lm(happiness_score ~ gdp_per_capita + life_expectancy + freedom + trust_government, data = df_all)
    summary(model)
  })
  
  output$compare_plot <- renderPlot({
    df_comp <- df_all %>% filter(year == input$year_comp, country %in% c(input$country1, input$country2))
    df_melted <- melt(df_comp[, c("country", "happiness_score", "gdp_per_capita", "life_expectancy", "freedom", "trust_government", "generosity")], id.vars = "country")
    
    ggplot(df_melted, aes(x = variable, y = value, fill = country)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = paste("Comparaison entre", input$country1, "et", input$country2, "en", input$year_comp), x = "Variable", y = "Valeur")
  })
  
  observeEvent(input$ask_button, {
    req(input$user_input)
    question <- input$user_input
    reponse <- repondre_question(question, df_all)
    output$bot_reply <- renderText({ reponse })
  })
  
  output$model_tests <- renderPrint({
    model_simple <- lm(happiness_score ~ gdp_per_capita, data = df_all)
    model_duo    <- lm(happiness_score ~ gdp_per_capita + life_expectancy, data = df_all)
    model_full   <- lm(happiness_score ~ gdp_per_capita + life_expectancy + freedom + trust_government, data = df_all)
    
    cat("🔹 MODELE SIMPLE (PIB uniquement)\n")
    print(summary(model_simple)$coefficients)
    cat("\n🔹 MODELE DUO (PIB + Life Expectancy)\n")
    print(summary(model_duo)$coefficients)
    cat("\n🔹 MODELE COMPLET (PIB + Life + Freedom + Trust)\n")
    print(summary(model_full)$coefficients)
  })
}

shinyApp(ui, server)