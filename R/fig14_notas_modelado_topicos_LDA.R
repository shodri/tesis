library(topicmodels)
library(tm)
library(knitr)
require(kableExtra)

notas <- read.csv('./Sociologia/Tesis/Datos/notas.csv')
# Crear un Corpus
corpus <- Corpus(VectorSource(notas$cuerpo))

removeURL <- function(x) gsub("http[^[:space:]]+", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))
corpus <- tm_map(corpus, content_transformer(function(x) gsub('["“”]', '', x)))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, stripWhitespace)

stopwords_custom <- c("ahi", "ver", "mar", "plata", "hacer", "zona", "gente", "solo", "van", "hace", "ser", "solo", "mas","año","años","dia","vez","ahi","así","acá","etc","tan", "paso", "hacen", "quieren")
corpus <- tm_map(corpus, removeWords, stopwords_custom)

dtm <- DocumentTermMatrix(corpus)

lda_model <- LDA(dtm, k = 5) 
terms <- terms(lda_model, 5)

print(terms)

df <- as.data.frame(terms)

tabla_html <- kable(df, format = "html", escape = TRUE) %>%
  kable_styling(full_width = FALSE)

print(tabla_html)

