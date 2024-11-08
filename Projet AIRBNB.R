data <- read.csv("./listings.csv.gz")
head(data)

library(dplyr)
library(caret)
library(questionr)
library(zoo)
# enlever le "$" de chaque prix et transformer en numérique
data$price <- as.numeric(gsub("\\$", "", data$price))

data<- data %>%
  arrange(bedrooms)
data$bedrooms <- na.locf(data$bedrooms, na.rm = FALSE)
data$bathrooms <- na.locf(data$bathrooms, na.rm = FALSE)
data$beds <- na.locf(data$beds, na.rm = FALSE)
data$price <- na.locf(data$price, na.rm = FALSE)
data$review_scores_rating <- na.locf(data$review_scores_rating, na.rm = FALSE)
data <- subset(data, bedrooms <= 6)

checkNoNa <- function(data){
  for (i in 1:ncol(data)){
    if (sum(is.na(data[,i])) > 0){
      print(paste("Column", colnames(data)[i], "a", sum(is.na(data[,i])), "valeur NA"))
    }
  }
}

# Calculer le prix moyen pour chaque nombre de chambres (bedrooms)
mean_price_by_bedrooms <- data %>%
  group_by(bedrooms) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

# Afficher les prix moyens par nombre de chambres
print(mean_price_by_bedrooms)

# Remplacer les NA dans 'bedrooms' en fonction du prix moyen
data <- data %>%
  mutate(
    bedrooms = ifelse(
      is.na(bedrooms),
      # Trouver le nombre de chambres le plus proche du prix actuel
      mean_price_by_bedrooms$bedrooms[
        which.min(abs(mean_price_by_bedrooms$mean_price - price))
      ],
      bedrooms
    )
  )
summary(data)

data$price <- log(data$price + 1)

## Recodage de data$neighbourhood_cleansed
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Batignolles-Monceau"] <- "0"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Bourse"] <- "1"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Buttes-Chaumont"] <- "2"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Buttes-Montmartre"] <- "3"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Élysée"] <- "4"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Entrepôt"] <- "5"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Gobelins"] <- "6"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Hôtel-de-Ville"] <- "7"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Louvre"] <- "8"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Luxembourg"] <- "9"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Ménilmontant"] <- "10"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Observatoire"] <- "11"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Opéra"] <- "12"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Palais-Bourbon"] <- "13"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Panthéon"] <- "14"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Passy"] <- "15"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Popincourt"] <- "16"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Reuilly"] <- "17"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Temple"] <- "18"
data$neighbourhood_cleansed[data$neighbourhood_cleansed == "Vaugirard"] <- "19"
data$neighbourhood_cleansed <- as.numeric(data$neighbourhood_cleansed)
data <- subset(data, select = c("price", "bedrooms", "minimum_nights", "beds", "bedrooms", "bathrooms", "review_scores_rating", "neighbourhood_cleansed"))
checkNoNa(data)

questionr::describe(data)

table(data$bedrooms)

# data split
split <- 0.7
trainIndex<-createDataPartition(data$price, p=split, list = FALSE)

data_train<-data[trainIndex, ]
data_test<-data[-trainIndex,]

cor(data_train[, c("bedrooms", "beds", "bathrooms", "price", "review_scores_rating", "neighbourhood_cleansed")])

# On peut voir que les variables "room_type", "neighbourhood" et "number_of_reviews" ne sont pas significatives
# On va donc les retirer du modèle
modele <- lm(price ~ bedrooms + beds + bathrooms + review_scores_rating + neighbourhood_cleansed, data = data_train)

x_test<-data_test[, -2]
y_test<-data_test[,2]

# prediction
predictions<-predict(modele, data_test)

summary(modele)

library(shiny)

# Chargement du modèle linéaire que vous avez entraîné
# Si vous avez sauvegardé le modèle, vous pouvez le charger avec saveRDS() / readRDS() par exemple

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Prédiction de prix avec un modèle de Régréssion linéaire"),
  # ajouter une image en backgournd

  tags$style(
    HTML("
      body {
        background-color: #ff5a5f;
        background-size: cover;
        background-position: center;
      }
    ")
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("neighbourhood_cleansed", "quartier:", value = 0, min = 0, max = 19),
      numericInput("review_scores_rating", "évaluation cliente :", value = 4.21, min = 0, max = 5),
      numericInput("bedrooms", "chambres:", value = 1, min = 0),
      numericInput("beds", "lits:", value = 4, min = 0),
      numericInput("bathrooms", "salles de bains:", value = 1, min = 0),
      actionButton("predict", "Prédire le prix")
    ),
    mainPanel(
      h3("Résultat de la prédiction :"),
      verbatimTextOutput("prediction")

    )
  )
)

# Serveur
server <- function(input, output) {
  # Définir la réaction pour la prédiction
  observeEvent(input$predict, {
    # Créer une dataframe avec les valeurs de l'UI
    new_data <- data.frame(
      neighbourhood_cleansed = input$neighbourhood_cleansed,
      review_scores_rating = input$review_scores_rating,
      bedrooms = input$bedrooms,
      beds = input$beds,
      bathrooms = input$bathrooms
    )
    # Calcul de la prédiction
    prediction <- predict(modele, new_data)
    # Afficher le résultat
    output$prediction <- renderText({ paste("Prix prédit:", round(prediction, 2) * 100, "USD") })
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
