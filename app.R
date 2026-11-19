# app.R

# ------------------------ #
#   Chargement librairie   # 
# ------------------------ #
library(markdown)
library(shiny)
library(graphics)
library(rsconnect)
library(stats)
library(gifski)
library(shinydashboard)
library(base)
library(datasets)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(rAmCharts)
library(gganimate)
library(shinymanager)
library(slickR)
library(graphics)
library(shinydashboardPlus)
library(dplyr)
library(htmltools)
library(readxl)




# ------------------- #
#   Données images    #
# ------------------- #
dt <- data.frame(
  name = c("IMG1", "IMG2", "IMG3", "IMG4","IMG5", "IMG6", "IMG7", "IMG8","IMG9", "IMG10"),
  Picture = c("IMG1.jpg", "IMG2.jpg", "IMG3.jpg", "IMG4.jpg","IMG5.jpg", "IMG6.jpg", "IMG7.jpg", "IMG8.jpg", "IMG9.jpg", "IMG10.jpg"),
  stringsAsFactors = FALSE
)

# ------------------- #
#   Données assurés   #
# ------------------- #
df <- read_excel("data/BASE_ETUDE.xlsx")

# ------------------- #
#   Credentials       #
# ------------------- #
# Les identifiants ne sont JAMAIS écrits dans le code.
# Ils sont lus dans les variables d'environnement APP_USER et APP_PASSWORD
# (fichier .Renviron, non versionné : voir .Renviron.example).
app_user <- Sys.getenv("APP_USER")
app_password <- Sys.getenv("APP_PASSWORD")
if (app_user == "" || app_password == "") {
  stop("Définissez APP_USER et APP_PASSWORD dans un fichier .Renviron (voir .Renviron.example).")
}
credentials <- data.frame(
  user = app_user,
  password = app_password,
  stringsAsFactors = FALSE
)


# --------------------------------------- #
# UI : logo, thème, authentification      #
# --------------------------------------- #

ui <- secure_app(
  # -- Logo dans l’interface d’authentification --
  tags_top = tags$div(
    tags$head(
      tags$style(HTML("
        /* Thème Lion pour l'interface d'authentification */
        .auth-content, .panel-auth {
          background-color: #1a1a1a !important;
          color: #333333!important;
        }
        .panel-auth .panel-heading {
          background-color: #000000 !important;
          border: none !important;
          color: #FFD700 !important;
        }
        .panel-auth .panel-body label {
          color: #333333 !important;
        }
        .panel-auth input.form-control {
          background-color: #333333 !important;
          color: #FFFFFF !important;
          border: 1px solid #000000 !important;
        }
      "))
    ),
    tags$img(
      src = "logo.png",  
      width = "200px",
      style = "display: block; margin: 0 auto 10px auto;"
    )
  ),
  language = "fr", 
  # -- Interface du dashboard --
  dashboardPage(
    dashboardHeader(
      title = tags$div(
        tags$img(src = "logo.png", height = "50px", style = "margin-right: 20px;"),
        "Dataviz-24/25"
      )
    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("ACCUEIL", tabName = "accueil", icon = icon("home")),
        menuItem("ANALYSE", icon = icon("chart-bar"),
                 menuSubItem("DATA_CHART", tabName = "data_chart"),
                 menuSubItem("GRAPHICS", tabName = "graphics")
        ),
        menuItem("RÉPONSES", tabName = "reponses", icon = icon("file-code")),
        menuItem("GUIDE UTILISATEUR", tabName = "guide", icon = icon("question-circle"))
      )
    ),
    
    dashboardBody(
      uiOutput("customCSS"),
      
      tabItems(
        # Accueil
        tabItem(tabName = "accueil",
                h2("Bienvenue dans notre application d'analyse des données"),
                selectInput("theme_select", "Choisissez un thème:",
                            choices = list(
                              "Par défaut" = "default", 
                              "Sombre" = "dark", 
                              "Bleu" = "blue", 
                              "Vert" = "green",
                              "Violet" = "purple",
                              "Rouge" = "magenta",
                              "Light-blue" = "cyan",
                              # Nouveau thème Lion
                              "Lion" = "lion"
                            ),
                            selected = "lion"),
                br(),
                uiOutput("carousel")
        ),
        
        # Data Chart
        tabItem(tabName = "data_chart",
                fluidRow(
                  box(title = "Tableaux de Données", width = 12,
                      dataTableOutput("tableData")
                  )
                )
        ),
        
        # Graphics
        tabItem(tabName = "graphics",
                fluidRow(
                  column(width = 4,
                         selectInput("question_select", "Choisissez une question:",
                                     choices = list(
                                       "a. Boxplots (ggplot2 vs base)" = "q1",
                                       "b. Avantages de ggplot2" = "q2",
                                       "c. Ajout d'un titre au ggplot2" = "q3",
                                       "d. Nuages de points (ggplot2 vs base)" = "q4",
                                       "e. Histogrammes (ggplot2 vs base)" = "q5",
                                       "f. Nuages de points avec gradient" = "q6",
                                       "g. Graphiques avec rAmCharts" = "q7",
                                       "h. Carte interactive avec leaflet" = "q8",
                                       "i. Scatter plot interactif (plotly/dplyr)" = "q9",
                                       "j. Boxplot interactif (ggplotly)" = "q10",
                                       "k. Boxplot animé (gganimate)" = "q11",
                                       "l. Calcul du ratio S/P" = "q12"
                                     ),
                                     selected = "q1")
                  ),
                  column(width = 8,
                         uiOutput("graphicsOutput")
                  )
                )
        ),
        
        # Réponses
        tabItem(tabName = "reponses",
                fluidRow(
                  box(title = "DÉTAILS", width = 12,
                      verbatimTextOutput("codeDetails")
                  )
                )
        ),
        
        # Guide
        tabItem(tabName = "guide",
                fluidRow(
                  box(title = "Guide Utilisateur", width = 12,
                      includeMarkdown("guide_utilisateur.md")
                  )
                )
        )
      )
    )
  )
)

# ------------------ #
# SERVER             #
# ------------------ #

server <- function(input, output, session) {
  
  # Authentification
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # CSS dynamique
  output$customCSS <- renderUI({
    theme <- ifelse(is.null(input$theme_select), "default", input$theme_select)
    css <- switch(theme,
                  "default" = "
                    /* Thème par défaut : aucun style personnalisé */
                  ",
                  "dark" = "
                    body { background-color: #333333; color: white; }
                    .skin-blue .main-header, .skin-blue .main-sidebar { background-color: #222222 !important; }
                    .box { background: #444444; color: white; }
                  ",
                  "blue" = "
                    body { background-color: #cce5ff; color: #003366; }
                    .skin-blue .main-header, .skin-blue .main-sidebar { background-color: #0056b3 !important; }
                    .box { background: #e6f0ff; color: #003366; }
                  ",
                  "green" = "
                    body { background-color: #e2f7e2; color: #2d6a2d; }
                    .skin-blue .main-header, .skin-blue .main-sidebar { background-color: #4CAF50 !important; }
                    .box { background: #f0fff0; color: #2d6a2d; }
                  ",
                  "purple" = "
                    body { background-color: #f3e5f5; color: #4a148c; }
                    .skin-blue .main-header, .skin-blue .main-sidebar { background-color: #4a148c !important; }
                    .box { background: #ce93d8; color: #4a148c; }
                  ",
                  "magenta" = "
                    body { background-color: #ffe6f2; color: #660033; }
                    .skin-blue .main-header, .skin-blue .main-sidebar { background-color: #cc3366 !important; }
                    .box { background: #ffccdd; color: #660033; }
                  ",
                  "cyan" = "
                    body { background-color: #e0ffff; color: #007777; }
                    .skin-blue .main-header, .skin-blue .main-sidebar { background-color: #00bcd4 !important; }
                    .box { background: #b2ebf2; color: #007777; }
                  ",
                  # Thème Lion
                  "lion" = "
                    body {
                      background-color: #1a1a1a;
                      color: #FFD700;
                    }
                    /* Entête et sidebar */
                    .skin-blue .main-header, .skin-blue .main-sidebar {
                      background-color: #000000 !important;
                    }
                    /* Boîtes */
                    .box {
                      background: #333333;
                      color: #FFD700;
                    }
                    /* Contenu principal */
                    .content-wrapper, .right-side {
                      background-color: #1a1a1a;
                    }
                    
                    /* -- Pour les DataTables : texte en blanc -- */
                    .dataTables_wrapper table.dataTable,
                    .dataTables_wrapper table.dataTable th,
                    .dataTables_wrapper table.dataTable td {
                      color: #FFFFFF !important;
                    }
                    
                    /* -- Interface d'authentification shinymanager -- */
                    .auth-content, .panel-auth {
                      background-color: #1a1a1a !important;
                      color: #FFD700 !important;
                    }
                    .panel-auth .panel-heading {
                      background-color: #000000 !important;
                      border: none !important;
                      color: #FFD700 !important;
                    }
                    .panel-auth .panel-body label {
                      color: #FFD700 !important;
                    }
                    .panel-auth input.form-control {
                      background-color: #333333 !important;
                      color: #FFD700 !important;
                      border: 1px solid #FFD700 !important;
                    }
                  "
    )
    tags$style(HTML(css))
  })
  
  # Carousel
  output$carousel <- renderUI({
    images <- lapply(dt$Picture, function(img) {
      tags$img(src = img, height = 900, width = 5000)
    })
    
    slickR(images) + settings(
      slidesToShow = 1,
      slidesToScroll = 1,
      autoplay = TRUE,
      autoplaySpeed = 1000,
      fade = TRUE,
      speed = 1000,
      cssEase = "linear"
    )
  })
  
  # Data Chart
  output$tableData <- renderDataTable({
    ds <- df
    ds
  })
  
  # Graphics output
  output$graphicsOutput <- renderUI({
    switch(input$question_select,
           "q1" = tagList(
             h3("Question a: Boxplots pour le jeu de données iris"),
             plotOutput("boxplot_ggplot"),
             plotOutput("boxplot_base")
           ),
           "q2" = tagList(
             h3("Question b: Avantages de ggplot2"),
             verbatimTextOutput("explanation_ggplot2")
           ),
           "q3" = tagList(
             h3("Question c: Ajout d'un titre dans ggplot2"),
             plotOutput("boxplot_ggplot_title")
           ),
           "q4" = tagList(
             h3("Question d: Nuages de points pour iris"),
             plotOutput("scatter_ggplot"),
             plotOutput("scatter_base")
           ),
           "q5" = tagList(
             h3("Question e: Histogrammes de Sepal.Length"),
             plotOutput("hist_ggplot"),
             plotOutput("hist_base")
           ),
           "q6" = tagList(
             h3("Question f: Nuages de points avec gradient selon Petal.Width"),
             plotOutput("scatter_gradient_ggplot"),
             plotOutput("scatter_gradient_base")
           ),
           "q7" = tagList(
             h3("Question g: Graphiques avec rAmCharts"),
             amChartsOutput("box_amchart"),
             amChartsOutput("hist_amchart")
           ),
           "q8" = tagList(
             h3("Question h: Carte interactive avec leaflet"),
             leafletOutput("mapLeaflet"),
             textInput("lat", "Latitude", value = "5.348"),
             textInput("lng", "Longitude", value = "-4.024"),
             actionButton("updateMap", "Mettre à jour la carte"),
             dataTableOutput("mapHistory")
           ),
           "q9" = tagList(
             h3("Question i: Scatter plot interactif avec plotly et dplyr"),
             plotlyOutput("scatter_plotly")
           ),
           "q10" = tagList(
             h3("Question j: Boxplot interactif avec ggplotly"),
             plotlyOutput("boxplot_ggplotly")
           ),
           "q11" = tagList(
             h3("Question k: Boxplot animé avec gganimate"),
             imageOutput("boxplot_animate")
           ),
           "q12" = tagList(
             h3("Question l: Calcul et affichage du ratio S/P"),
             selectInput("ratio_choice", "Choisissez le type de ratio S/P:",
                         choices = c("S/P ANNEE" = "annee", "S/P ASSURE" = "assure"),
                         selected = "annee"),
             dataTableOutput("ratioSP")
           )
    )
  })
  
  # -- Implémentations Questions --
  output$boxplot_ggplot <- renderPlot({
    ggplot(iris, aes(x = Species, y = Sepal.Length)) +
      geom_boxplot() +
      theme_minimal()
  })
  output$boxplot_base <- renderPlot({
    boxplot(Sepal.Length ~ Species, data = iris, 
            main = "Boxplot de Sepal.Length par Species (Base R)")
  })
  
  output$explanation_ggplot2 <- renderText({
    "ggplot2 offre une syntaxe cohérente basée sur la grammaire des graphiques,
     permettant une personnalisation poussée, la gestion intuitive des couches et une flexibilité élevée."
  })
  
  output$boxplot_ggplot_title <- renderPlot({
    ggplot(iris, aes(x = Species, y = Sepal.Length)) +
      geom_boxplot() +
      ggtitle("Distribution de Sepal.Length par Espèces avec ggplot2") +
      theme_minimal()
  })
  
  output$scatter_ggplot <- renderPlot({
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      theme_minimal()
  })
  output$scatter_base <- renderPlot({
    plot(iris$Sepal.Length, iris$Sepal.Width,
         col = as.numeric(iris$Species),
         main = "Scatter plot (Base R)",
         xlab = "Sepal.Length", ylab = "Sepal.Width")
    legend("topright", legend = levels(iris$Species),
           col = 1:3, pch = 1)
  })
  
  output$hist_ggplot <- renderPlot({
    ggplot(iris, aes(x = Sepal.Length)) +
      geom_histogram(fill = "red", color = "black", bins = 30) +
      ggtitle("Histogramme de Sepal.Length (ggplot2)") +
      xlab("Sepal.Length") + ylab("Fréquence") +
      theme_minimal()
  })
  output$hist_base <- renderPlot({
    hist(iris$Sepal.Length, breaks = 30,
         main = "Histogramme de Sepal.Length (Base R)",
         xlab = "Sepal.Length", col = "red", border = "black")
  })
  
  output$scatter_gradient_ggplot <- renderPlot({
    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Width)) +
      geom_point() +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal()
  })
  output$scatter_gradient_base <- renderPlot({
    colors <- colorRampPalette(c("blue", "red"))(nrow(iris))[rank(iris$Petal.Width)]
    plot(iris$Sepal.Length, iris$Sepal.Width,
         col = colors,
         main = "Scatter plot avec gradient (Base R)",
         xlab = "Sepal.Length", ylab = "Sepal.Width")
    legend("topright", legend = c("Faible Petal.Width", "Élevé Petal.Width"),
           fill = c("blue", "red"))
  })
  
  output$box_amchart <- renderAmCharts({
    amBoxplot(Sepal.Length ~ Species, data = iris,
              main = "Boxplot de Sepal.Length par Species",
              col = "pink")
  })
  output$hist_amchart <- renderAmCharts({
    amHist(iris$Sepal.Length,
           main = "Histogramme de Sepal.Length",
           col = "red")
  })
  
  # Carte interactive
  mapHistory <- reactiveVal(data.frame(Latitude = numeric(), Longitude = numeric(), stringsAsFactors = FALSE))
  
  output$mapLeaflet <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(lng = -4.024, lat = 5.348, zoom = 15) %>%
      addMarkers(lng = -4.024, lat = 5.348, popup = "École de Police d'Abidjan")
  })
  
  observeEvent(input$updateMap, {
    lat <- as.numeric(input$lat)
    lng <- as.numeric(input$lng)
    leafletProxy("mapLeaflet") %>% 
      clearMarkers() %>%
      addMarkers(lng = lng, lat = lat, popup = paste("Lat:", lat, "Lng:", lng))
    nouveau <- data.frame(Latitude = lat, Longitude = lng)
    mapHistory(rbind(mapHistory(), nouveau))
  })
  
  output$mapHistory <- renderDataTable({
    mapHistory()
  })
  
  # Scatter plot interactif
  output$scatter_plotly <- renderPlotly({
    data <- iris %>% mutate(ID = row_number())
    r <- ggplot(data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      theme_minimal()
    ggplotly(r)
  })
  
  # Boxplot interactif
  output$boxplot_ggplotly <- renderPlotly({
    p <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
      geom_boxplot() +
      theme_minimal()
    ggplotly(p)
  })
  
  # Boxplot animé
  output$boxplot_animate <- renderImage({
    outfile <- tempfile(fileext = '.gif')
    
    t <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Set2") +  
      transition_states(gear, transition_length = 0.5, state_length = 0.5) +
      labs(title = "Boxplot de mpg par cyl - Gear : {closest_state}",
           x = "Nombre de cylindres",
           y = "Mpg",
           fill = "Cylindres") +
      theme_minimal()
    
    anim <- animate(t, nframes = 100, renderer = gifski_renderer())
    anim_save(outfile, anim)
    
    list(src = outfile,
         contentType = 'image/gif',
         width = "100%",
         height = "auto",
         alt = "Animation de Boxplot coloré")
  }, deleteFile = TRUE)
  
  # Ratio S/P
  output$ratioSP <- renderDataTable({
    data <- df
    if(!all(c("CHARGE_SIN_SURVENANCE", "PRIME_EMISE") %in% names(data))){
      showNotification("Les colonnes 'CHARGE_SIN_SURVENANCE' et 'PRIME_EMISE' doivent être présentes dans le fichier.", type = "error")
      return(NULL)
    }
    
    if (input$ratio_choice == "annee") {
      if("ANNEE" %in% names(data)){
        stats_annee <- data %>% 
          group_by(ANNEE) %>% 
          summarise(
            Total_Prime = sum(PRIME_EMISE, na.rm = TRUE),
            Total_Sinistre = sum(CHARGE_SIN_SURVENANCE, na.rm = TRUE),
            Ratio_SP = ifelse(sum(PRIME_EMISE, na.rm = TRUE) == 0, NA, sum(CHARGE_SIN_SURVENANCE, na.rm = TRUE) / sum(PRIME_EMISE, na.rm = TRUE))
          ) %>% mutate(ANNEE = as.character(ANNEE))
        total_prime <- sum(data$PRIME_EMISE, na.rm = TRUE)
        total_sinistre <- sum(data$CHARGE_SIN_SURVENANCE, na.rm = TRUE)
        total_ratio <- ifelse(total_prime == 0, NA, total_sinistre / total_prime)
        total <- data.frame(
          ANNEE = "TOTAL",
          Total_Prime = total_prime,
          Total_Sinistre = total_sinistre,
          Ratio_SP = total_ratio
        )
        result <- bind_rows(stats_annee, total)
        DT::datatable(result)
      } else {
        total_prime <- sum(data$PRIME_EMISE, na.rm = TRUE)
        total_sinistre <- sum(data$CHARGE_SIN_SURVENANCE, na.rm = TRUE)
        total_ratio <- ifelse(total_prime == 0, NA, total_sinistre / total_prime)
        result <- data.frame(
          Indicator = "TOTAL",
          Total_Prime = total_prime,
          Total_Sinistre = total_sinistre,
          Ratio_SP = total_ratio
        )
        DT::datatable(result)
      }
    } else if (input$ratio_choice == "assure") {
      if("ASSURE" %in% names(data)){
        stats_assure <- data %>%
          group_by(ASSURE) %>%
          summarise(
            Total_Prime = sum(PRIME_EMISE, na.rm = TRUE),
            Total_Sinistre = sum(CHARGE_SIN_SURVENANCE, na.rm = TRUE),
            Ratio_SP = ifelse(sum(PRIME_EMISE, na.rm = TRUE) == 0, NA, sum(CHARGE_SIN_SURVENANCE, na.rm = TRUE) / sum(PRIME_EMISE, na.rm = TRUE))
          ) %>%
          ungroup()
        total_prime <- sum(data$PRIME_EMISE, na.rm = TRUE)
        total_sinistre <- sum(data$CHARGE_SIN_SURVENANCE, na.rm = TRUE)
        total_ratio <- ifelse(total_prime == 0, NA, total_sinistre / total_prime)
        total <- data.frame(
          ASSURE = "TOTAL",
          Total_Prime = total_prime,
          Total_Sinistre = total_sinistre,
          Ratio_SP = total_ratio
        )
        result <- bind_rows(stats_assure, total)
        DT::datatable(result)
      } else {
        showNotification("La colonne 'ASSURE' n'est pas présente dans le fichier.", type = "error")
        return(NULL)
      }
    }
  })
  
  # --- Partie RÉPONSES : Affichage des codes sources et explications ---
  output$codeDetails <- renderPrint({
    cat("===== DÉTAILS DES RÉPONSES =====\n\n")
    
    cat("Question a: Boxplots pour le jeu de données iris\n")
    cat("  ggplot2:\n    ggplot(iris, aes(x = Species, y = Sepal.Length)) + geom_boxplot() + theme_minimal()\n")
    cat("  Base R:\n    boxplot(Sepal.Length ~ Species, data = iris, main = \"Boxplot de Sepal.Length par Species avec la fonction boxplot de base\")\n\n")
    
    cat("Question b: Avantages de ggplot2\n")
    cat("  Explication:\n    ggplot2 offre une syntaxe cohérente basée sur la grammaire des graphiques, permettant une personnalisation poussée, une gestion intuitive des couches et une flexibilité élevée.\n\n")
    
    cat("Question c: Boxplot ggplot2 avec ajout d'un titre\n")
    cat("  Code:\n    ggplot(iris, aes(x = Species, y = Sepal.Length)) + geom_boxplot() + ggtitle(\"Distribution de Sepal.Length par Espèces avec ggplot2\") + theme_minimal()\n\n")
    
    cat("Question d: Nuages de points pour iris\n")
    cat("  ggplot2:\n    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point() + theme_minimal()\n")
    cat("  Base R:\n    plot(iris$Sepal.Length, iris$Sepal.Width, col = as.numeric(iris$Species), main = \"Scatter plot (Base R)\", xlab = \"Sepal.Length\", ylab = \"Sepal.Width\")\n")
    cat("    legend(\"topright\", legend = levels(iris$Species), col = 1:3, pch = 1)\n\n")
    
    cat("Question e: Histogrammes de Sepal.Length\n")
    cat("  ggplot2:\n    ggplot(iris, aes(x = Sepal.Length)) + geom_histogram(fill = \"red\", color = \"black\", bins = 30) + ggtitle(\"Histogramme de Sepal.Length (ggplot2)\") + xlab(\"Sepal.Length\") + ylab(\"Fréquence\") + theme_minimal()\n")
    cat("  Base R:\n    hist(iris$Sepal.Length, breaks = 30, main = \"Histogramme de Sepal.Length (Base R)\", xlab = \"Sepal.Length\", col = \"red\", border = \"black\")\n\n")
    
    cat("Question f: Nuages de points avec gradient (selon Petal.Width)\n")
    cat("  ggplot2:\n    ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Width)) + geom_point() + scale_color_gradient(low = \"blue\", high = \"red\") + theme_minimal()\n")
    cat("  Base R:\n    colors <- colorRampPalette(c(\"blue\", \"red\"))(nrow(iris))[rank(iris$Petal.Width)]\n")
    cat("    plot(iris$Sepal.Length, iris$Sepal.Width, col = colors, main = \"Scatter plot avec gradient (Base R)\", xlab = \"Sepal.Length\", ylab = \"Sepal.Width\")\n")
    cat("    legend(\"topright\", legend = c(\"Faible Petal.Width\", \"Élevé Petal.Width\"), fill = c(\"blue\", \"red\"))\n\n")
    
    cat("Question g: Graphiques avec rAmCharts\n")
    cat("  Boxplot:\n    amBoxplot(Sepal.Length ~ Species, data = iris, main = \"Boxplot de Sepal.Length par Species\", col = \"pink\")\n")
    cat("  Histogramme:\n    amHist(iris$Sepal.Length, main = \"Histogramme de Sepal.Length\", col = \"red\")\n\n")
    
    cat("Question h: Carte interactive avec leaflet\n")
    cat("  Code:\n    leaflet() %>% addTiles() %>% setView(lng = -4.024, lat = 5.348, zoom = 15) %>% addMarkers(lng = -4.024, lat = 5.348, popup = \"École de Police d'Abidjan\")\n\n")
    
    cat("Question i: Scatter plot interactif avec plotly et dplyr\n")
    cat("  Code:\n    data <- iris %>% mutate(ID = row_number())\n")
    cat("    r <- ggplot(data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point() + theme_minimal()\n")
    cat("    ggplotly(r)\n\n")
    
    cat("Question j: Boxplot interactif avec ggplotly\n")
    cat("  Code:\n    p <- ggplot(iris, aes(x = Species, y = Sepal.Length)) + geom_boxplot() + theme_minimal()\n")
    cat("    ggplotly(p)\n\n")
    
    cat("Question k: Boxplot animé avec gganimate\n")
    cat("  Code:\n    t <- ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) + geom_boxplot() +\n")
    cat("         scale_fill_brewer(palette = \"Set2\") + transition_states(gear, transition_length = 1, state_length = 1) +\n")
    cat("         labs(title = \"Boxplot de mpg par cyl - Gear : {closest_state}\", x = \"Nombre de cylindres\", y = \"Mpg\", fill = \"Cylindres\") + theme_minimal()\n")
    cat("    anim <- animate(t, nframes = 100, renderer = gifski_renderer())\n")
    cat("    anim_save(outfile, anim)\n\n")
    
    cat("Question l: Calcul et affichage du ratio S/P\n")
    cat("  Pour 'S/P ANNEE':\n")
    cat("    - Regroupement par colonne 'ANNEE' (si présente) avec calcul du total des primes, sinistres et du ratio.\n")
    cat("  Pour 'S/P ASSURE':\n")
    cat("    - Regroupement par colonne 'ASSURE' avec calcul du total des primes, sinistres et du ratio, puis ajout d'une ligne 'TOTAL'.\n\n")
    
    cat("===== CODE SOURCE COMPLET DE L'APPLICATION =====\n\n")
    
    file_path <- "app.R"
    if(file.exists(file_path)){
      code_lines <- readLines(file_path)
      cat(paste(code_lines, collapse = "\n"))
    } else {
      cat("Le fichier source app.R n'a pas été trouvé.")
    }
  })
}


# ------------------ #
#   shinyApp         #
# ------------------ #

shinyApp(ui, server)
