library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)

notas <- read.csv('./Sociologia/Tesis/Datos/notas.csv')
# Crear un Corpus
corpus <- Corpus(VectorSource(notas$cuerpo))
# Preprocesamiento del texto
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub('["“”]', '', x)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))

# Si tienes stopwords adicionales o palabras personalizadas a remover, puedes agregarlas así:
stopwords_custom <- c('"'," mar", "plata", "hacer", "zona", "gente", "solo", "van", "hace", "ser", "solo", "mas")
corpus <- tm_map(corpus, removeWords, stopwords_custom)

corpus <- tm_map(corpus, stripWhitespace)

# Crear la matriz de términos
dtm <- DocumentTermMatrix(corpus)

# Obtener frecuencia de palabras
freq <- colSums(as.matrix(dtm))

# Crear el dataframe con las palabras y su frecuencia
word_freq <- data.frame(word = names(freq), freq = freq)

# Ordenar por frecuencia descendente
word_freq <- word_freq[order(-word_freq$freq), ]

# Visualizar las 10 palabras más frecuentes
head(word_freq, 10)

wordcloud(words = word_freq$word, freq = word_freq$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

