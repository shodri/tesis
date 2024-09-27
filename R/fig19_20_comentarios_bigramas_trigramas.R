library(tm)
library(quanteda)
library(knitr)
library(kableExtra)

# Cargar datos
data <- read.csv('./Sociologia/Tesis/Datos/comentarios.csv')

# Eliminar duplicados manuales
data <- subset(data, !(X %in% c(330, 205, 87, 282, 276)))

# Detectar y eliminar duplicados en los comentarios
data <- data[!duplicated(data$post), ]

# Crear Corpus
corpus <- Corpus(VectorSource(data$post))

# Preprocesamiento del texto
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub('["“”]', '', x)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, stripWhitespace)

# Stopwords personalizadas
stopwords_custom <- c("ahi", "ver", "mar", "plata", "hacer", "zona", "gente", "solo", "van", "hace", "ser", "mas",
                      "año", "años", "dia", "vez", "ahí", "así", "acá", "etc", "tan", "paso", "hacen", "quieren", 
                      "gratisseria")
corpus <- tm_map(corpus, removeWords, stopwords_custom)

# Convertir el corpus para uso en quanteda
corpus_quanteda <- corpus(corpus)

# Tokenización y creación de bigramas y trigramas
tokens <- tokens(corpus_quanteda, remove_punct = TRUE, remove_numbers = TRUE)
bigramas <- tokens_ngrams(tokens, n = 2L, concatenator = " ")  
trigramas <- tokens_ngrams(tokens, n = 3L, concatenator = " ") 

# Frecuencia de bigramas y trigramas
frecuencia_bigramas <- sort(table(unlist(bigramas)), decreasing = TRUE)
frecuencia_trigramas <- sort(table(unlist(trigramas)), decreasing = TRUE)

# Convertir los resultados en dataframes
resultados_bigramas <- as.data.frame(head(frecuencia_bigramas, 10))
resultados_trigramas <- as.data.frame(head(frecuencia_trigramas, 10))

# Renombrar columnas
colnames(resultados_bigramas) <- c("Bigramas", "Frecuencia")
colnames(resultados_trigramas) <- c("Trigramas", "Frecuencia")

# Crear tablas HTML
tabla_bigramas <- kable(resultados_bigramas, "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = "striped")

tabla_trigramas <- kable(resultados_trigramas, "html") %>%
  kable_styling(full_width = FALSE, bootstrap_options = "striped")

# Imprimir tablas
print(tabla_bigramas)
print(tabla_trigramas)

# Guardar tablas en archivos HTML
writeLines(as.character(tabla_bigramas), "tabla_bigramas.html")
writeLines(as.character(tabla_trigramas), "tabla_trigramas.html")

