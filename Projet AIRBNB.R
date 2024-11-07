data <- read.csv("/home/enzo/Téléchargements/listings.csv")
head(data)

library(caret)
library(Metrics)
library(gbm)
# enlever le "$" de chaque prix et transformer en numérique
data$price <- as.numeric(gsub("\\$", "", data$price))

# supprimer les na de la colonne price
data <- data[!is.na(data$price),]
data <- data[!is.na(data$review_scores_rating),]
data <- data[!is.na(data$bathrooms),]
data <- data[!is.na(data$bedrooms),]
data$beds[is.na(data$beds)] <- 1
data$minimum_nights[data$minimum_nights > 120] <- 120

checkNoNa <- function(data){
  for (i in 1:ncol(data)){
    if (sum(is.na(data[,i])) > 0){
      print(paste("Column", colnames(data)[i], "a", sum(is.na(data[,i])), "valeur NA"))
    }
  }
}

data <- subset(data, select = c("room_type", "price", "minimum_nights", "neighbourhood", "number_of_reviews", "review_scores_rating", "calculated_host_listings_count", "availability_365", "bedrooms", "minimum_nights", "beds", "bedrooms", "bathrooms", "number_of_reviews"))
checkNoNa(data)

questionr::describe(data)
plot(data$review_scores_rating, data$price)
plot(data$minimum_nights, data$price)

# data split
split <- 0.8
trainIndex<-createDataPartition(data$price, p=split, list = FALSE)

data_train<-data[trainIndex, ]
data_test<-data[-trainIndex,]

modele <- lm(price ~ ., data = data)

summary(modele)

# On peut voir que les variables "room_type", "neighbourhood" et "number_of_reviews" ne sont pas significatives
# On va donc les retirer du modèle
modele <- lm(price ~ minimum_nights + review_scores_rating + calculated_host_listings_count + availability_365 + bedrooms + beds + bathrooms, data = data_train)

x_test<-data_test[, c(-2)]
y_test<-data_test[,2]

# prediction
predictions<-predict(modele, x_test)

# calcul de l'erreur
precision <- rmse(y_test, predictions)

summary(modele)

predict(modele, data.frame(minimum_nights = 1, review_scores_rating = 4.21, calculated_host_listings_count = 1, availability_365 = 100, bedrooms = 4, beds = 4, bathrooms = 1))