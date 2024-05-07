library(tm)
library(quanteda)
library(knitr)
library(grid)
library(gridExtra)

data <- read.csv('./Sociologia/Tesis/Datos/comentarios.csv')


#Son comentarios casi pero no exactamente iguales. No los toma el duplicated
#data <- subset(data, !(X %in% c(282, 239, 276. 244)))
#data <- subset(data, !(X %in% c( 330, 415)))
#data <- subset(data, !(X %in% c(205, 222, 244)))
#data <- subset(data, !(X %in% c(87, 95)))


duplicados <- data[duplicated(data$post), ]

#Remover duplicados
data <- data[!duplicated(data$post), ]

corpus <- Corpus(VectorSource(data$post))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub('["“”]', '', x)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, stripWhitespace)

stopwords_custom <- c("ahi", "ver", "mar", "plata", "hacer", "zona", "gente", "solo", "van", "hace", "ser", "solo", "mas","año","años","dia","vez","ahi", "ahí", "ahíla", "ahila", "así","acá","etc","tan", "paso", "hacen", "quieren", "gratisseria")
corpus <- tm_map(corpus, removeWords, stopwords_custom)

corpus2 <- corpus(corpus)

tokens <- tokens(corpus2, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_separators = TRUE)

bigramas <- tokens_ngrams(tokens, n = 2L, skip = 0L, concatenator = " ")
trigramas <- tokens_ngrams(tokens, n = 3L, skip = 0L, concatenator = " ")

frecuencia_bigramas <- table(unlist(bigramas))
frecuencia_trigramas <- table(unlist(trigramas))

frecuencia_ordenada <- sort(frecuencia_bigramas, decreasing = TRUE)
frecuencia_ordenada_tri <- sort(frecuencia_trigramas, decreasing = TRUE)

print(head(frecuencia_ordenada, 10))
print(head(frecuencia_ordenada_tri, 10))

resultados_df <- as.data.frame(head(frecuencia_ordenada, 10))
resultados_df_tri <- as.data.frame(head(frecuencia_ordenada_tri, 10))

colnames(resultados_df) <- c("Bigramas", "Frecuencia")
colnames(resultados_df_tri) <- c("Trigramas", "Frecuencia")

tabla_html <- kable(resultados_df, "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = "striped")

tabla_html_tri <- kable(resultados_df_tri, "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = "striped")

print(tabla_html)
View(tabla_html)

print(tabla_html_tri)
View(tabla_html_tri)
writeLines(tabla_html, "tabla_bigramas.html")



#Otra manera de graficar Tablas
png("output.png", width=480,height=480,bg = "white")
grid.table(resultados_df)
dev.off()
getwd()
