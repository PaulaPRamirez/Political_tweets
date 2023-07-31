

#PROYECTO ANÁLISIS ELECCIONES 


# author: Paula Pareja
# date: Julio, 2023
# Proyecto: Análisis de los tweets de los cuatro partidos principales y candidatos de la presidencia
# durante la última semana electoral antes del 23 de julio 
# Objetivo del proyecto: desarrollar un enfoque innovador de las elecciones a través del NLP para estudiar el 
# marketing político de los diferentes partidos


#Uso la libreria httr para las solicitudes HTTP con la API de twitter. 



# Antes de nada, limpiamos el workspace, por si hubiera alg?n dataset o informaci?n cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Instalamos y cargamos las librerías necesarias
library(rjson)
require(httr)
require(jsonlite)
require(dplyr)
library(purrr)
library(lubridate)
library(rtweet)
library(tidyr)



# Establezco las ccredenciales y conecto la API

bearer_token = "Bearer_token"


authorization <- paste0("Bearer ", bearer_token)
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))


########################
########################

# ANÁLISIS DE USUARIO
########################
########################


####################
#Primero tengo que identificar el número ID del usuario 

# Establezco el nombre de usuario
screen_name <- "nombre_usuario"

# Set the endpoint URL
users_url <- paste0("https://api.twitter.com/2/users/by/username/", screen_name)

# Set the request headers
headers <- c(
  Authorization = paste("Bearer", bearer_token),
  "Content-Type" = "application/json"
)

# Send the GET request
response <- GET(users_url, add_headers(.headers = headers))

# Extract the response content as JSON
response_content <- content(response, as = "text")
response_json <- fromJSON(response_content)

# Retrieve the user ID from the response
user_id <- response_json$data$id

# Print the user ID
print(user_id)


# Una vez tengo todos los id de usuario de las cuentas de partido y los candidatos a presidencia
# continúo con las descargas

###############################################################################################


##########################################################################
##########################################################################
#Primera descarga 

# user ID
user_id <- "aqui_USER_id"

# Endpoint URL
tweets_url <- paste0("https://api.twitter.com/2/users/", user_id, "/tweets")


# Establezco los parámetros
query_params <- list(
  tweet.fields = "created_at,author_id,public_metrics,entities,referenced_tweets",
  expansions = "author_id,referenced_tweets.id",
  user.fields = "username,description,id,location",
  media.fields = "type", 
  max_results = 100,
  start_time = "2023-07-15T00:00:00Z",  # Descargo los tweets del 15 de julio
  end_time = "2023-07-15T23:59:59Z" 
)


# Establezco el encabezado de la solicitud
headers <- c(
  Authorization = paste("Bearer", bearer_token),
  "Content-Type" = "application/json"
)

# Solicitud GET
response <- GET(tweets_url, query = query_params, add_headers(.headers = headers))

# Extraigo la respuesta como JSON
json_content <- fromJSON(content(response, as = "text"), flatten = TRUE)
print(json_content)


##################################

# LIMPIEZA DE LOS DATOS

##################################
##################################

tweets <- json_content$data
users <- json_content$includes$users
tweets2 <- json_content$includes$tweets

users_df <- as.data.frame(users)
tweets_df <- as.data.frame(tweets)
tweets2_df <- as.data.frame(tweets2)


colSums(is.na(users_df))
colSums(is.na(tweets_df))
colSums(is.na(tweets2_df))

#Desanidamos el df de usuarios
users_df2 <- users_df

# Asegurarse de que la columna "location" sea una lista o un dataframe
users_df2 <- users_df2 %>%
  mutate(location = lapply(location, as.list))

# Extraer los componentes de la columna "location" y crear nuevas columnas
users_df2 <- users_df2 %>% 
  mutate(location_description = location$description,
         location_username = location$username,
         location_id = location$id,
         location_name = location$name,
         location_location = location$location) %>%
  select(-location)  # Eliminar la columna anidada original

users_df2 <- distinct(users_df2) #elimino las columnas repetidas 

# Seleccionar las columnas deseadas
selected_columns <- c("name", "description", "username", "id")
users_df2 <- users_df2[, selected_columns]

# Desanidar columnas anidadas en tweets_df
tweets_df2 <-tweets_df

# Crear un nuevo data frame solo con las columnas a expandir

# Cambiar el nombre de la columna "id" a "id_tweet"
colnames(tweets_df2)[colnames(tweets_df2) == "id"] <- "id_tweet"
colnames(tweets_df2)

expanded_df <- tweets_df2 %>%
  select(referenced_tweets) %>%
  unnest_wider(everything())

selected_columns <- c("referenced_tweets")
tweets_df2 <- tweets_df2[, -which(names(tweets_df2) %in% selected_columns)]


colnames(expanded_df)
colnames(expanded_df)[colnames(expanded_df) == "id"] <- "id_user_response"


lista_tipo <- list(expanded_df$type)
lista_tipo <- unlist(lapply(lista_tipo[[1]], function(x) list(x)), recursive = FALSE)
expanded_df$lista_tipo <- lista_tipo
expanded_df$lista_tipo <- as.character(expanded_df$lista_tipo)
str(expanded_df$lista_tipo)

lista_id_response <- list(expanded_df$id_user_response)
lista_id_response <- unlist(lapply(lista_id_response[[1]], function(x) list(x)), recursive = FALSE)
expanded_df$lista_id_response <- lista_id_response
expanded_df$lista_id_response <- as.character(expanded_df$lista_id_response)
str(expanded_df$lista_id_response)

selected_columns <- c("type", "id_user_response")
expanded_df <- expanded_df[, -which(names(expanded_df) %in% selected_columns)]

colnames(expanded_df)
colnames(expanded_df)[colnames(expanded_df) == "lista_tipo"] <- "type"
colnames(expanded_df)[colnames(expanded_df) == "lista_id_response"] <- "id_user_response"

colnames(tweets_df2)
tweets_df2 <- tweets_df2[, -which(names(tweets_df2) == "entities.mentions")]
tweets_df2 <- tweets_df2[, -which(names(tweets_df2) == "entities.urls")]
tweets_df2 <- tweets_df2[, -which(names(tweets_df2) == "entities.annotations")]


hashtags <- list(tweets_df2$entities.hashtags)
hashtags <- unlist(lapply(hashtags[[1]], function(x) list(x)), recursive = FALSE)

tweets_df2$hashtags <- hashtags

# Creo un nuevo vector de caracteres
tag_values <- character(length(tweets_df2$hashtags))

# Ahora creo una función para extraer el valor "tag" de la lista anidada
for (i in seq_along(tweets_df2$hashtags)) {
  x <- tweets_df2$hashtags[[i]]
  if (!is.null(x) && is.data.frame(x) && "tag" %in% names(x)) {
    tags <- x$tag
    valid_tags <- tags[nchar(tags) > 2]
    tag_values[i] <- paste(valid_tags, collapse = ",")
  } else {
    tag_values[i] <- NA
  }
}

# La añado como una nueva columna
tweets_df2$tags_column <- tag_values

str(tweets_df2$tags_column)
tweets_df2 <- tweets_df2[, -which(names(tweets_df2) == "hashtags")]
tweets_df2 <- tweets_df2[, -which(names(tweets_df2) == "entities.hashtags")]

tweets_df_3 <- cbind(tweets_df2,expanded_df)

str(tweets_df_3)
colnames(tweets_df_3)
tweets_df_3$edit_history_tweet_ids <- as.character(tweets_df_3$edit_history_tweet_ids)


colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.retweet_count"] <- "retweets"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.reply_count"] <- "replies"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.like_count"] <- "likes"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.quote_count"] <- "quotes"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.bookmark_count"] <- "bookmark"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.impression_count"] <- "impressions"
colnames(tweets_df_3)[colnames(tweets_df_3) == "tags_column"] <- "hashtags"

str(users_df)

combined_df <- merge(users_df2, tweets_df_3, by.x = "id", by.y = "author_id")
colnames(combined_df)

###################################################################################################
#Ahora con el de tweets originales


tweets2_df2 <- tweets2_df
colnames(tweets2_df2)

colnames(tweets2_df2)[colnames(tweets2_df2) == "id"] <- "id_tweet"

colnames(tweets2_df2)


expanded_df <- tweets2_df2 %>%
  select(referenced_tweets) %>%
  unnest_wider(everything())

selected_columns <- c("referenced_tweets")
tweets2_df2 <- tweets2_df2[, -which(names(tweets2_df2) %in% selected_columns)]


lista_tipo <- list(expanded_df$type)
lista_tipo <- unlist(lapply(lista_tipo[[1]], function(x) list(x)), recursive = FALSE)
expanded_df$lista_tipo <- lista_tipo
expanded_df$lista_tipo <- as.character(expanded_df$lista_tipo)
str(expanded_df$lista_tipo)

lista_id_response <- list(expanded_df$id)
lista_id_response <- unlist(lapply(lista_id_response[[1]], function(x) list(x)), recursive = FALSE)
expanded_df$lista_id_response <- lista_id_response
expanded_df$lista_id_response <- as.character(expanded_df$lista_id_response)
str(expanded_df$lista_id_response)

selected_columns <- c("type", "id")
expanded_df <- expanded_df[, -which(names(expanded_df) %in% selected_columns)]

colnames(expanded_df)
colnames(expanded_df)[colnames(expanded_df) == "lista_tipo"] <- "type"
colnames(expanded_df)[colnames(expanded_df) == "lista_id_response"] <- "id_user_response"

colnames(tweets2_df2)

tweets2_df2 <- tweets2_df2[, -which(names(tweets2_df2) == "entities.mentions")]
tweets2_df2 <- tweets2_df2[, -which(names(tweets2_df2) == "entities.urls")]
tweets2_df2 <- tweets2_df2[, -which(names(tweets2_df2) == "entities.annotations")]

hashtags <- list(tweets2_df2$entities.hashtags)
hashtags <- unlist(lapply(hashtags[[1]], function(x) list(x)), recursive = FALSE)

tweets2_df2$hashtags <- hashtags

# Creo un vector de caracteres
tag_values <- character(length(tweets2_df2$hashtags))

# Funcion para extraer los valores de hashtags
for (i in seq_along(tweets2_df2$hashtags)) {
  x <- tweets2_df2$hashtags[[i]]
  if (!is.null(x) && is.data.frame(x) && "tag" %in% names(x)) {
    tags <- x$tag
    valid_tags <- tags[nchar(tags) > 2]
    tag_values[i] <- paste(valid_tags, collapse = ",")
  } else {
    tag_values[i] <- NA
  }
}


tweets2_df2$tags_column <- tag_values
str(tweets2_df2$tags_column)

tweets2_df2 <- tweets2_df2[, -which(names(tweets2_df2) == "entities.hashtags")]
tweets2_df2 <- tweets2_df2[, -which(names(tweets2_df2) == "hashtags")]

tweets_df_3 <- cbind(tweets2_df2,expanded_df)

str(tweets_df_3)
colnames(tweets_df_3)
tweets_df_3$edit_history_tweet_ids <- as.character(tweets_df_3$edit_history_tweet_ids)

colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.retweet_count"] <- "retweets"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.reply_count"] <- "replies"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.like_count"] <- "likes"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.quote_count"] <- "quotes"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.bookmark_count"] <- "bookmark"
colnames(tweets_df_3)[colnames(tweets_df_3) == "public_metrics.impression_count"] <- "impressions"
colnames(tweets_df_3)[colnames(tweets_df_3) == "tags_column"] <- "hashtags"


tweets_orignales <- tweets_df_3
tweets_respuestas <- combined_df
str(tweets_orignales)
str(tweets_respuestas)

colSums(is.na(tweets_orignales))
colSums(is.na(tweets_respuestas))

library(openxlsx)

# Create a new workbook
wb <- createWorkbook()

# Write 'tweets_respuestas' data to the first sheet
addWorksheet(wb, "tweets_respuestas")
writeData(wb, "tweets_respuestas", tweets_respuestas)

# Write 'tweets_orignales' data to a new sheet
addWorksheet(wb, "Tweets_originales")
writeData(wb, "Tweets_originales", tweets_orignales)


# Save the workbook to a file
saveWorkbook(wb, "Descargas/descarga_sabado15.xlsx")


#Hago lo mismo con todos los días de la semana para asegurarme que descargo todos los tweets y que con las limitaciones
#de la API no me deje ninguno atrás. 


#Una vez realizada la descarga, combino todos los dataframes para tener un único df de cada usuario. 
#Continúo con la limpieza y el análisis en el archivo R "Analisis_tweets_comparativo.R de mi repositorio"






