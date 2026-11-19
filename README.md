# Dataviz 24/25 – Application Shiny de visualisation de données

Application **R Shiny** réalisée en équipe dans le cadre du **Master Actuariat 2024/2025**. Elle compare les outils de visualisation de R (graphiques de base, ggplot2, plotly, rAmCharts, leaflet, gganimate) et calcule un indicateur actuariel : le **ratio sinistres/primes (S/P)**.

🔗 **Application en ligne :** https://elixirsoro27.shinyapps.io/projetapp_dataviz/

## Fonctionnalités

- **Accueil** : carrousel d'images (slickR).
- **Analyse – Data chart** : tableau interactif des données (DT).
- **Analyse – Graphics** : 12 questions de visualisation
  - a–f : boxplots, nuages de points et histogrammes, en comparant **ggplot2** et les graphiques de base de R (jeu de données `iris`) ;
  - g : graphiques **rAmCharts** ;
  - h : carte interactive **leaflet** ;
  - i–j : graphiques interactifs **plotly** / `ggplotly` ;
  - k : boxplot animé avec **gganimate** (jeu de données `mtcars`) ;
  - l : **ratio S/P** par année ou par assuré, avec ligne de total.
- **Réponses** : code source et explication de chaque visualisation.
- **Guide utilisateur** et écran de connexion (**shinymanager**), avec un thème personnalisé.

## Lancer l'application en local

```r
install.packages(c("shiny", "shinydashboard", "shinydashboardPlus", "shinymanager",
                   "ggplot2", "plotly", "leaflet", "DT", "rAmCharts", "gganimate",
                   "gifski", "slickR", "dplyr", "readxl", "htmltools", "markdown", "rsconnect"))
shiny::runApp()
```

L'application est protégée par un écran de connexion. **Aucun identifiant n'est écrit dans le code** : avant de la lancer, copiez `.Renviron.example` en `.Renviron` et choisissez votre identifiant et votre mot de passe (`APP_USER`, `APP_PASSWORD`). Le fichier `.Renviron` est exclu du dépôt.

## Données

Le fichier `data/BASE_ETUDE.xlsx` de ce dépôt est un **jeu de données fictif**, généré pour la démonstration. Il reprend la structure du fichier utilisé dans le projet (année, intermédiaire, assuré, prime émise, charge sinistre, nombre de sinistres), mais aucun nom ni montant ne correspond à des données réelles. Les images du carrousel sont également des visuels de remplacement.

## Équipe

Angoran · Badolo · Ouedraogo · Soro Onansongui Elixir

## Outils

R · Shiny · shinydashboard · ggplot2 · plotly · leaflet · rAmCharts · gganimate · DT · dplyr
