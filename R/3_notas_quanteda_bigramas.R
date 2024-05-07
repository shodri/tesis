library(tm)
library(quanteda)
library(knitr)
library(grid)
library(gridExtra)
library(kableExtra)
notas <- read.csv('./Sociologia/Tesis/Datos/notas.csv')

corpus <- Corpus(VectorSource(notas$cuerpo))

corpus <- tm_map(corpus, content_transformer(tolower))
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

