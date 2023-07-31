#Análisis comparativo entre los tweets de los partidos

# author: Paula Pareja
# date: Julio, 2023
# Proyecto: Análisis de los tweets de los cuatro partidos principales y candidatos de la presidencia
# durante la última semana electoral antes del 23 de julio 
# Objetivo del proyecto: desarrollar un enfoque innovador de las elecciones a través del NLP para estudiar el 
# marketing político de los diferentes partidos

# Antes de nada, limpiamos el workspace, por si hubiera alg?n dataset o informaci?n cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#Importamos las librerías
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)
library(cowplot)
library(ggplot2)
library(plotly)
library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(tidytext)
library(udpipe) 
library(data.table)
library(BTM) 
library(widyr) 
library(igraph)
library(ggraph)
library(tm)
library(quanteda)
library(scales)
library(syuzhet) 
library(wordcloud) 
library(stringi)
library(stringr)

#Descargamos los datasets limpios 

PSOE <- read_excel("Descargas/Listas_limpias/tweets_PSOE.xlsx")
PP <- read_excel("Descargas/Listas_limpias/tweets_PP.xlsx")
SUMAR <- read_excel("Descargas/Listas_limpias/tweets_SUMAR.xlsx")
VOX <- read_excel("Descargas/Listas_limpias/tweets_VOX.xlsx")


######################
# ANÁLISIS COMPARATIVO 

# Compruebo la limpieza de las listas 

str(PSOE)
str(PP)
str(SUMAR)
str(VOX)

#Elimino la primera columna 
PSOE <- PSOE[,-1]
PP <- PP[,-1]
SUMAR <- SUMAR[,-1]
VOX <- VOX[,-1]

#Creo una columna del nombre del partido 
PSOE$partido <- "PSOE"
PP$partido <- "PP"
SUMAR$partido <- "SUMAR"
VOX$partido <- "VOX"


# Uno todo en el mismo df

#Combino los cuatro conjuntos de datos en uno solo
tweets <- rbind(PSOE, PP, SUMAR, VOX)


#Comienzo comparando el impacto promedio de tweets con las impresiones 

impresionesporpartido <- tweets %>%
  group_by(partido) %>%
  summarise(promedioimpresiones = mean(impressions))

# Asignar colores a cada partido
colores_partidos <- c("PP" = "#184A90", "PSOE" = "#E4010B", "VOX" = "#5AC035", "SUMAR" = "#ED6F91")

# Convertir la variable "partido" a factor con los niveles correctos
impresionesporpartido$partido <- factor(impresionesporpartido$partido, levels = names(colores_partidos))

# Gráfico de barras para visualizar las medias de retweets por partido
impresiones <- ggplot(impresionesporpartido, aes(x = reorder(partido, -promedioimpresiones), y = promedioimpresiones, fill = partido)) +
  geom_bar(stat = "identity",width = 0.7) +
  labs(title = "Media de impresiones por Tweet",
       x = "Partido",
       y = "Promedio de impresiones") +
  theme_minimal() +
  scale_fill_manual(values = colores_partidos) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5),
        axis.text.y = element_text(size = 12),
        legend.position = "none")

impresiones
ggsave(filename = "Gráficas/tweets/media_impresiones.jpg", plot = impresiones, width = 6, height = 4)



#Medias de impacto 
# Calcular las medias de cada columna
medias_impacto <- tweets %>%
  group_by(partido) %>%
  summarise(
    retweets = mean(retweets),
    likes = mean(likes),
    respuestas = mean(replies)
  )

# Convertir los datos de 'medias_impacto' en formato tidy y cambiar los nombres de las métricas
medias_impacto_tidy <- medias_impacto %>%
  pivot_longer(
    cols = c(retweets, likes, respuestas),
    names_to = "Métrica",
    values_to = "Media"
  ) %>%
  mutate(Métrica = ifelse(Métrica == "retweets", "Retweets",
                          ifelse(Métrica == "likes", "Likes", "Respuestas")))

# Asignar colores a cada partido
colores_partidos <- c("PP" = "#184A90", "PSOE" = "#E4010B", "VOX" = "#5AC035", "SUMAR" = "#ED6F91")


# Graficar la media de cada columna en un geom_bar con labels en el eje y
impacto <- ggplot(medias_impacto_tidy, aes(x = reorder(Métrica, -Media), y = Media, fill = reorder(partido, -Media))) +
  geom_bar(stat = "identity", position= "dodge", width = 0.6) +
  labs(title = "Media de Impacto en Twitter",
       x = NULL,
       y = "Media",
       fill= "Partido") +
  theme_minimal() +
  scale_fill_manual(values = colores_partidos) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5),
        axis.text.y = element_text(size = 12))
impacto


ggsave(filename = "Gráficas/tweets/media_impacto.jpg", plot = impacto, width = 6, height = 4)


#Calculo el engagement de cada partido 

# Supongamos que tienes un dataframe llamado 'tweets' que contiene los datos de los tweets de cada partido.

# 1. Calcular la suma de likes y comentarios por cada partido
engagement_partidos <- tweets %>%
  group_by(partido) %>%
  summarise(
    likes_comments = sum(likes + replies, na.rm = TRUE)
  )

tweets$followers <- NA
tweets <- tweets %>%
  mutate(followers = case_when(
    partido == "PSOE" ~ 866000,
    partido == "PP" ~ 858000,
    partido == "SUMAR" ~ 130000,
    partido == "VOX" ~ 532000
  ))

# 2. Obtener el número de seguidores para cada partido (supongamos que tienes una columna llamada 'seguidores')
seguidores_partidos <- tweets %>%
  group_by(partido) %>%
  summarise(
    seguidores = sum(followers, na.rm = TRUE)
  )

# 3. Unir las dos tablas y calcular el engagement
engagement_partidos <- left_join(engagement_partidos, seguidores_partidos, by = "partido") %>%
  mutate(engagement = (likes_comments / seguidores) * 100)

# Mostrar el resultado final con el engagement de cada partido
print(engagement_partidos)
# A tibble: 4 × 4
#partido likes_comments seguidores engagement
#<chr>            <dbl>      <dbl>      <dbl>
#  1 PP               59299  163878000     0.0362
#2 PSOE            310773  200046000     0.155 
#3 SUMAR           193418   73060000     0.265 
#4 VOX             420384  361228000     0.116 




#############################################
#############################################
#Analisis de sentimiento 

###################################################################################################

#nrc: categorizes words into positive and negative categories, as well as by type of sentiment 
#      (anger, anticipation, disgust, fear, joy, sadness, surprise, and trust)


# ---------------- PSOE ---------------------


afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

tweets_PSOE <- PSOE$text

#Creo una función para eliminar las menciones, emojis y caracteres especiales 


# Función para eliminar caracteres especiales y emojis
limpiar_texto <- function(texto) {
  # Convertir el texto a minúscula
  texto <- tolower(texto)
  
  # Eliminar palabras que comienzan con "https" y caracteres no permitidos
  patron <- "https\\S*|[^[:alnum:]|[:space:]|[:punct:]]"
  texto_limpio <- stri_replace_all_regex(texto, patron, " ")
  
  # Eliminar caracteres "@" y "#"
  texto_limpio <- gsub("[@#]|\r\n\r\n|\r\n", "", texto_limpio)
  
  # Eliminar números
  texto_limpio <- gsub("\\b\\d+\\b", "", texto_limpio)
  
  # Eliminar signos de interrogación
  texto_limpio <- gsub("[?¿]", "", texto_limpio)
  
  # Devolver el texto limpio
  return(texto_limpio)
}

# Aplicar la función de limpieza al data frame de tweets
tweets_PSOE_limpios <- data.frame(texto_limpio = sapply(tweets_PSOE, limpiar_texto))
str(tweets_PSOE_limpios)
print(tweets_PSOE_limpios)

# Vamos a usar una muestra de mensajes
mensajes<-sample_n(tweets_PSOE_limpios,
                   190)

mensajes$texto_limpio <- removeWords(mensajes$texto_limpio, stopwords("spanish"))

tuits_afinn <- 
  mensajes %>%
  unnest_tokens(input = "texto_limpio", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 


str(tuits_afinn)

grafico_afinn <- ggplot(tuits_afinn, aes(x = Tipo, fill = Tipo)) +
  geom_bar(width = 0.6) +
  labs(x = "Tipo de palabra", y = "Cantidad", title = "Emociones tweets PSOE") +
  theme_minimal()+
  theme(legend.position = "none")
grafico_afinn


##################################


#Pruebo con emociones con la libreria syuzhet

palabra.df <- as.vector(tweets_PSOE_limpios$texto_limpio)


#Aplicamos la función indicando el vector y el idioma y creamos
#un nuevo data frame llamado emocion.df
emocion.df <- get_nrc_sentiment(char_v = palabra.df, language = "spanish")

#Unimos emocion.df con el vector tweets.df para ver como
#trabajó la función get_nrc_sentiment cada uno de los tweets
emocion.df2 <- cbind(tweets_PSOE_limpios, emocion.df)
head(emocion.df2)
emocionesPSOE <- emocion.df2
#Creamos un data frame en el cual las filas serán las emociones
#y las columnas los puntajes totales

#Empezamos transponiendo emocion.df
emocion.df3 <- data.frame(t(emocion.df))

#Sumamos los puntajes de cada uno de los tweets para cada emocion
emocion.df3 <- data.frame(rowSums(emocion.df3))

#Nombramos la columna de puntajes como cuenta
names(emocion.df3)[1] <- "cuenta"

#Dado que las emociones son los nombres de las filas y no una variable
#transformamos el data frame para incluirlas dentro
emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)

#Quitamos el nombre de las filas
rownames(emocion.df3) <- NULL

#Verificamos el data frame
print(emocion.df3)

# Definir la paleta de colores para las emociones
colores_emociones <- c(
  "trust" = "#FFC0CB",
  "sadness" = "#87CEFA",
  "anticipation" = "#A9CC8A",
  "fear" = "#DAC8F7",
  "joy" = "#FFEA6E",
  "anger" = "#B39EB5",
  "surprise" = "#98FB98",
  "disgust" = "#AEC6CF"
)


# Define Spanish emotion labels
spanish_emotion_labels <- c(
  "anger" = "Enojo",
  "anticipation" = "Anticipación",
  "disgust" = "Asco",
  "fear" = "Miedo",
  "joy" = "Alegría",
  "sadness" = "Tristeza",
  "surprise" = "Sorpresa",
  "trust" = "Confianza"
)

# Modify the emotions data frame to use Spanish labels
emocion.df3$sentimiento_español <- factor(emocion.df3$sentimiento, levels = names(spanish_emotion_labels), labels = spanish_emotion_labels)


#Primer gráfico: se detallaran las 8 emociones con sus puntajes respectivos
sentimientos1 <- ggplot(emocion.df3[1:8,],
                        aes(x = reorder(sentimiento_español, -cuenta),
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores_emociones) +
  labs(title = "Análisis de sentimiento \n PSOE",
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=10, angle = 45, hjust = 1),
        axis.title = element_text(size=10,face = "bold"),
        title = element_text(size=12,face = "bold"),
        legend.position = "none")
print(sentimientos1)

ggsave(filename = "Gráficas/tweets/analisis_sentimiento_PSOE.jpg", plot = sentimientos1, width = 10, height = 6)


########################
########################
# NUBE DE PALABRAS
########################

#Quiero hacer una nube de palabras para visualizar qué palabra usa cada candidato y a qué emoción está ligado 


#voy a verlo por palabras para poder hacer la nube

tweets_PSOE_limpios<-sample_n(tweets_PSOE_limpios,
                              190)
tweets_PSOE_limpios$texto_limpio <- removeWords(tweets_PSOE_limpios$texto_limpio, stopwords("spanish"))

texto_palabras <- get_tokens(tweets_PSOE_limpios$texto_limpio)
head(texto_palabras)


sentimientos_palabras <- get_nrc_sentiment(texto_palabras, lang="spanish")


#nube de palabras 
nube_emociones_vector <- c(
  paste(texto_palabras[sentimientos_palabras$sadness> 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$joy > 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$anger > 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$trust > 0], collapse = " "))

nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)

colnames(nube_tdm) <- c('tristeza', 'felicidad', 'enfado', 'confianza')
head(nube_tdm)

set.seed(757)
nube_PSOE <- comparison.cloud(nube_tdm, random.order = FALSE,
                              colors = c("green", "red", "orange", "blue"),
                              title.size = 1, max.words = 100, scale = c(0.9, 0.5), rot.per = 0.4)



# ---------------- PP ---------------------

tweets_PP <- PP$text


# Aplicar la función de limpieza al data frame de tweets
tweets_PP_limpios <- data.frame(texto_limpio = sapply(tweets_PP, limpiar_texto))
str(tweets_PP_limpios)
print(tweets_PP_limpios)

# Vamos a usar una muestra de mensajes
mensajes<-sample_n(tweets_PP_limpios,
                   190)

tuits_afinn <- 
  mensajes %>%
  unnest_tokens(input = "texto_limpio", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 


str(tuits_afinn)

grafico_afinn <- ggplot(tuits_afinn, aes(x = Tipo, fill = Tipo)) +
  geom_bar(width = 0.6) +
  labs(x = "Tipo de palabra", y = "Cantidad", title = "Emociones tweets PP") +
  theme_minimal()+
  theme(legend.position = "none")
grafico_afinn



##################################

#Pruebo con emociones con la libreria syuzhet

palabra.df <- as.vector(tweets_PP_limpios$texto_limpio)


#Aplicamos la función indicando el vector y el idioma y creamos
#un nuevo data frame llamado emocion.df
emocion.df <- get_nrc_sentiment(char_v = palabra.df, language = "spanish")

#Unimos emocion.df con el vector tweets.df para ver como
#trabajó la función get_nrc_sentiment cada uno de los tweets
emocion.df2 <- cbind(tweets_PP_limpios, emocion.df)
head(emocion.df2)
emocionesPP <- emocion.df2
#Creamos un data frame en el cual las filas serán las emociones
#y las columnas los puntajes totales

#Empezamos transponiendo emocion.df
emocion.df3 <- data.frame(t(emocion.df))

#Sumamos los puntajes de cada uno de los tweets para cada emocion
emocion.df3 <- data.frame(rowSums(emocion.df3))

#Nombramos la columna de puntajes como cuenta
names(emocion.df3)[1] <- "cuenta"

#Dado que las emociones son los nombres de las filas y no una variable
#transformamos el data frame para incluirlas dentro
emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)

#Quitamos el nombre de las filas
rownames(emocion.df3) <- NULL

#Verificamos el data frame
print(emocion.df3)

# Definir la paleta de colores para las emociones
colores_emociones <- c(
  "trust" = "#FFC0CB",
  "sadness" = "#87CEFA",
  "anticipation" = "#A9CC8A",
  "fear" = "#DAC8F7",
  "joy" = "#FFEA6E",
  "anger" = "#B39EB5",
  "surprise" = "#98FB98",
  "disgust" = "#AEC6CF"
)


# Define Spanish emotion labels
spanish_emotion_labels <- c(
  "anger" = "Enojo",
  "anticipation" = "Anticipación",
  "disgust" = "Asco",
  "fear" = "Miedo",
  "joy" = "Alegría",
  "sadness" = "Tristeza",
  "surprise" = "Sorpresa",
  "trust" = "Confianza"
)

# Modify the emotions data frame to use Spanish labels
emocion.df3$sentimiento_español <- factor(emocion.df3$sentimiento, levels = names(spanish_emotion_labels), labels = spanish_emotion_labels)


#Primer gráfico: se detallaran las 8 emociones con sus puntajes respectivos
sentimientos1 <- ggplot(emocion.df3[1:8,],
                        aes(x = reorder(sentimiento_español, -cuenta),
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores_emociones) +
  labs(title = "Análisis de sentimiento \n PP",
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=10, angle = 45, hjust = 1),
        axis.title = element_text(size=10,face = "bold"),
        title = element_text(size=12,face = "bold"),
        legend.position = "none")+
  ylim(0,250)
print(sentimientos1)

ggsave(filename = "Gráficas/tweets/analisis_sentimiento_PP.jpg", plot = sentimientos1, width = 10, height = 6)


########################
########################
# NUBE DE PALABRAS
########################

#Quiero hacer una nube de palabras para visualizar qué palabra usa cada candidato y a qué emoción está ligado 


#voy a verlo por palabras para poder hacer la nube

tweets_PP_limpios<-sample_n(tweets_PP_limpios,
                            190)

texto_palabras <- get_tokens(tweets_PP_limpios$texto_limpio)
head(texto_palabras)


sentimientos_palabras <- get_nrc_sentiment(texto_palabras, lang="spanish")


#nube de palabras 
nube_emociones_vector <- c(
  paste(texto_palabras[sentimientos_palabras$sadness> 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$joy > 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$anger > 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$trust > 0], collapse = " "))

nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)

colnames(nube_tdm) <- c('tristeza', 'felicidad', 'enfado', 'confianza')
head(nube_tdm)

set.seed(757)
nube_PP <- comparison.cloud(nube_tdm, random.order = FALSE,
                            colors = c("green", "red", "orange", "blue"),
                            title.size = 1, max.words = 100, scale = c(0.9, 0.5), rot.per = 0.4)



# ---------------- SUMAR ---------------------

tweets_SUMAR <- SUMAR$text


# Aplicar la función de limpieza al data frame de tweets

tweets_SUMAR_limpios <- data.frame(sapply(tweets_SUMAR, limpiar_texto))

str(tweets_SUMAR_limpios)
print(tweets_SUMAR_limpios)

# Vamos a usar una muestra de mensajes

tweets_SUMAR_limpios$texto_limpio <- tweets_SUMAR_limpios$sapply.tweets_SUMAR..limpiar_texto.
tweets_SUMAR_limpios<-sample_n(tweets_SUMAR_limpios,
                               190)

tuits_afinn <- 
  mensajes %>%
  unnest_tokens(input = "texto_limpio", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 


str(tuits_afinn)

grafico_afinn <- ggplot(tuits_afinn, aes(x = Tipo, fill = Tipo)) +
  geom_bar(width = 0.6) +
  labs(x = "Tipo de palabra", y = "Cantidad", title = "Emociones tweets SUMAR") +
  theme_minimal()+
  theme(legend.position = "none")
grafico_afinn


##################################


#Pruebo con emociones con la libreria syuzhet

palabra.df <- as.vector(tweets_SUMAR_limpios$texto_limpio)


#Aplicamos la función indicando el vector y el idioma y creamos
#un nuevo data frame llamado emocion.df
emocion.df <- get_nrc_sentiment(char_v = palabra.df, language = "spanish")

#Unimos emocion.df con el vector tweets.df para ver como
#trabajó la función get_nrc_sentiment cada uno de los tweets
emocion.df2 <- cbind(tweets_SUMAR_limpios, emocion.df)
head(emocion.df2)
emocionesSUMAR <- emocion.df2
emocionesSUMAR <- emocionesSUMAR[,-1]
#Creamos un data frame en el cual las filas serán las emociones
#y las columnas los puntajes totales

#Empezamos transponiendo emocion.df
emocion.df3 <- data.frame(t(emocion.df))

#Sumamos los puntajes de cada uno de los tweets para cada emocion
emocion.df3 <- data.frame(rowSums(emocion.df3))

#Nombramos la columna de puntajes como cuenta
names(emocion.df3)[1] <- "cuenta"

#Dado que las emociones son los nombres de las filas y no una variable
#transformamos el data frame para incluirlas dentro
emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)

#Quitamos el nombre de las filas
rownames(emocion.df3) <- NULL

#Verificamos el data frame
print(emocion.df3)

# Definir la paleta de colores para las emociones
colores_emociones <- c(
  "trust" = "#FFC0CB",
  "sadness" = "#87CEFA",
  "anticipation" = "#A9CC8A",
  "fear" = "#DAC8F7",
  "joy" = "#FFEA6E",
  "anger" = "#B39EB5",
  "surprise" = "#98FB98",
  "disgust" = "#AEC6CF"
)


# Define Spanish emotion labels
spanish_emotion_labels <- c(
  "anger" = "Enojo",
  "anticipation" = "Anticipación",
  "disgust" = "Asco",
  "fear" = "Miedo",
  "joy" = "Alegría",
  "sadness" = "Tristeza",
  "surprise" = "Sorpresa",
  "trust" = "Confianza"
)

# Modify the emotions data frame to use Spanish labels
emocion.df3$sentimiento_español <- factor(emocion.df3$sentimiento, levels = names(spanish_emotion_labels), labels = spanish_emotion_labels)


#Primer gráfico: se detallaran las 8 emociones con sus puntajes respectivos
sentimientos1 <- ggplot(emocion.df3[1:8,],
                        aes(x = reorder(sentimiento_español, -cuenta),
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores_emociones) +
  labs(title = "Análisis de sentimiento \n SUMAR",
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=10, angle = 45, hjust = 1),
        axis.title = element_text(size=10,face = "bold"),
        title = element_text(size=12,face = "bold"),
        legend.position = "none")+
  ylim(0,250)
print(sentimientos1)

ggsave(filename = "Gráficas/tweets/analisis_sentimiento_SUMAR.jpg", plot = sentimientos1, width = 10, height = 6)


########################
########################
# NUBE DE PALABRAS
########################

#Quiero hacer una nube de palabras para visualizar qué palabra usa cada candidato y a qué emoción está ligado 


#voy a verlo por palabras para poder hacer la nube

texto_palabras <- get_tokens(tweets_SUMAR_limpios$texto_limpio)
head(texto_palabras)


sentimientos_palabras <- get_nrc_sentiment(texto_palabras, lang="spanish")


#nube de palabras 
nube_emociones_vector <- c(
  paste(texto_palabras[sentimientos_palabras$sadness> 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$joy > 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$anger > 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$trust > 0], collapse = " "))

nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)

colnames(nube_tdm) <- c('tristeza', 'felicidad', 'enfado', 'confianza')
head(nube_tdm)

set.seed(757)
nube_SUMAR <- comparison.cloud(nube_tdm, random.order = FALSE,
                               colors = c("green", "red", "orange", "blue"),
                               title.size = 1, max.words = 100, scale = c(0.9, 0.5), rot.per = 0.4)





# ---------------- VOX ---------------------

tweets_VOX <- VOX$text

# Aplicar la función de limpieza al data frame de tweets

tweets_VOX_limpios <- data.frame(sapply(tweets_VOX, limpiar_texto))

str(tweets_VOX_limpios)
print(tweets_VOX_limpios)

# Vamos a usar una muestra de mensajes

tweets_VOX_limpios$texto_limpio <- tweets_VOX_limpios$sapply.tweets_VOX..limpiar_texto.
tweets_VOX_limpios<-sample_n(tweets_VOX_limpios,
                             190)

tuits_afinn <- 
  mensajes %>%
  unnest_tokens(input = "texto_limpio", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 


str(tuits_afinn)

grafico_afinn <- ggplot(tuits_afinn, aes(x = Tipo, fill = Tipo)) +
  geom_bar(width = 0.6) +
  labs(x = "Tipo de palabra", y = "Cantidad", title = "Emociones tweets VOX") +
  theme_minimal()+
  theme(legend.position = "none")
grafico_afinn

##################################


#Pruebo con emociones con la libreria syuzhet

palabra.df <- as.vector(tweets_VOX_limpios$texto_limpio)


#Aplicamos la función indicando el vector y el idioma y creamos
#un nuevo data frame llamado emocion.df
emocion.df <- get_nrc_sentiment(char_v = palabra.df, language = "spanish")

#Unimos emocion.df con el vector tweets.df para ver como
#trabajó la función get_nrc_sentiment cada uno de los tweets
emocion.df2 <- cbind(tweets_VOX_limpios, emocion.df)
head(emocion.df2)
emocionesVOX <- emocion.df2
emocionesVOX <- emocionesVOX[,-1]

#Creamos un data frame en el cual las filas serán las emociones
#y las columnas los puntajes totales

#Empezamos transponiendo emocion.df
emocion.df3 <- data.frame(t(emocion.df))

#Sumamos los puntajes de cada uno de los tweets para cada emocion
emocion.df3 <- data.frame(rowSums(emocion.df3))

#Nombramos la columna de puntajes como cuenta
names(emocion.df3)[1] <- "cuenta"

#Dado que las emociones son los nombres de las filas y no una variable
#transformamos el data frame para incluirlas dentro
emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)

#Quitamos el nombre de las filas
rownames(emocion.df3) <- NULL

#Verificamos el data frame
print(emocion.df3)

# Definir la paleta de colores para las emociones
colores_emociones <- c(
  "trust" = "#FFC0CB",
  "sadness" = "#87CEFA",
  "anticipation" = "#A9CC8A",
  "fear" = "#DAC8F7",
  "joy" = "#FFEA6E",
  "anger" = "#B39EB5",
  "surprise" = "#98FB98",
  "disgust" = "#AEC6CF"
)


# Define Spanish emotion labels
spanish_emotion_labels <- c(
  "anger" = "Enojo",
  "anticipation" = "Anticipación",
  "disgust" = "Asco",
  "fear" = "Miedo",
  "joy" = "Alegría",
  "sadness" = "Tristeza",
  "surprise" = "Sorpresa",
  "trust" = "Confianza"
)

# Modify the emotions data frame to use Spanish labels
emocion.df3$sentimiento_español <- factor(emocion.df3$sentimiento, levels = names(spanish_emotion_labels), labels = spanish_emotion_labels)


#Primer gráfico: se detallaran las 8 emociones con sus puntajes respectivos
sentimientos1 <- ggplot(emocion.df3[1:8,],
                        aes(x = reorder(sentimiento_español, -cuenta),
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores_emociones) +
  labs(title = "Análisis de sentimiento \n VOX",
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=10, angle = 45, hjust = 1),
        axis.title = element_text(size=10,face = "bold"),
        title = element_text(size=12,face = "bold"),
        legend.position = "none")+
  ylim(0,250)
print(sentimientos1)

ggsave(filename = "Gráficas/tweets/analisis_sentimiento_VOX.jpg", plot = sentimientos1, width = 10, height = 6)


########################
########################
# NUBE DE PALABRAS
########################

#Quiero hacer una nube de palabras para visualizar qué palabra usa cada candidato y a qué emoción está ligado 


#voy a verlo por palabras para poder hacer la nube

texto_palabras <- get_tokens(tweets_VOX_limpios$texto_limpio)
head(texto_palabras)


sentimientos_palabras <- get_nrc_sentiment(texto_palabras, lang="spanish")


#nube de palabras 
nube_emociones_vector <- c(
  paste(texto_palabras[sentimientos_palabras$sadness> 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$joy > 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$anger > 0], collapse = " "),
  paste(texto_palabras[sentimientos_palabras$trust > 0], collapse = " "))

nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

nube_corpus <- Corpus(VectorSource(nube_emociones_vector))
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)

colnames(nube_tdm) <- c('tristeza', 'felicidad', 'enfado', 'confianza')
head(nube_tdm)

set.seed(757)
nube_VOX <- comparison.cloud(nube_tdm, random.order = FALSE,
                             colors = c("green", "red", "orange", "blue"),
                             title.size = 1, max.words = 100, scale = c(0.9, 0.5), rot.per = 0.4)






