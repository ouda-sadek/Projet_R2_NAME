# Projet_R2_NAME

# 🌟 Bonheur Universel - Dashboard interactif R Shiny

**Bonheur Universel** est une application R Shiny interactive qui explore les données du World Happiness Report de 2015 à 2019.  
Elle permet de visualiser, comparer, analyser et prédire les scores de bonheur à l’échelle mondiale à partir de plusieurs facteurs explicatifs.

---

## 📊 Fonctionnalités principales

🌍 **Carte du bonheur** : visualisation mondiale du score de bonheur par pays
📈 **Évolution des indicateurs** : visualisation par pays et par variable
⚖ **Comparateur de pays** : comparaison de deux pays sur une même année
🧠 **Analyse statistique** : modèles linéaires explicatifs du bonheur
🤖 **Chatbot IA** : assistant conversationnel pour explorer les données
📉 **Prédiction** : score de bonheur prédit à partir de 4 facteurs clés
🔥 **Design personnalisé** : style néon, police Orbitron, animations CSS

---

## 📁 Structure du projet

Projet_R2_NAME/
├── Data_Clean/            # Données nettoyées (2015-2019)
├── shiny-app/
│   ├── app.R              # Fichier principal de l'application Shiny
│   └── www/               # Dossiers d’images et assets CSS
└── README.md              

---

## 🧪 Modèle prédictif

Un modèle linéaire a été entraîné pour prédire le score de bonheur à partir de :

gdp_per_capita (PIB par habitant)
life_expectancy (espérance de vie)
freedom (liberté)
trust_government (confiance dans le gouvernement)

📌 **Résultat :** un **R² de 0.53**, ce qui signifie que le modèle explique 53 % de la variance du bonheur mondial.

---

## 🔧 Technologies

**R** & **Shiny**
ggplot2, plotly, leaflet, sf, shinydashboard
CSS custom + polices Google Fonts

---

## 🚀 Lancer l'application

Depuis RStudio ou un terminal R :

r
shiny::runApp("shiny-app")

---

## 🙋‍♀️ Réalisé par

**Dina Yahiaoui**  **Rayanne Sadek**  **Ouda Sadek**