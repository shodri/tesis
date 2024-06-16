library(topicmodels)
library(tm)
library(ggplot2)
library(knitr)
require(kableExtra)
library(webshot)

comentarios <- read.csv('./Sociologia/Tesis/Datos/comentarios.csv')
# Crear un Corpus
corpus <- Corpus(VectorSource(comentarios$post))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub('["“”]', '', x)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, stripWhitespace)

stopwords_custom <- c("vos", "estan", "ahora", "nunca", "ahi", "ver", "mar", "plata", "hacer", "zona", "gente", "solo", "van", "hace", "ser", "solo", "mas","año","años","dia","vez","ahi","así","acá","etc","tan", "paso", "hacen", "quieren")
corpus <- tm_map(corpus, removeWords, stopwords_custom)

dtm <- DocumentTermMatrix(corpus)

# Chequear Filas Nulas
matriz <- as.matrix(dtm)
suma_filas <- rowSums(matriz)
filas_nulas <- which(suma_filas == 0)
print(filas_nulas)
dtm_filtrada <- dtm[-filas_nulas, ]

lda_model <- LDA(dtm_filtrada, k = 5) 
terms <- terms(lda_model, 5)

df <- as.data.frame(terms)

tabla_html <- kable(df, format = "html", escape = TRUE) %>%
  kable_styling(full_width = FALSE)

print(tabla_html)
