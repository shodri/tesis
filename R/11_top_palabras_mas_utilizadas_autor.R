library(dplyr)
library(tm)
library(stringr)
library(kableExtra)
library(knitr)
library(webshot2)

comentarios <- read.csv('./Sociologia/Tesis/Datos/comentarios.csv')

comentarios <- comentarios %>% select(autor, post)

# Contar el número de comentarios por autor
comentarios_por_autor <- comentarios %>% group_by(autor) %>% summarise(num_comentarios = n())

# Ordenar los autores por el número de comentarios en orden descendente
top_autores <- comentarios_por_autor %>% arrange(desc(num_comentarios)) %>% head(20)

# Crear un vector con los nombres de los autores más comentadores
autores_seleccionados <- top_autores$autor

# Crear una función para obtener las palabras y su frecuencia por autor
obtener_frecuencia_palabras <- function(autor) {
  comentarios_autor <- comentarios %>% filter(autor == !!autor)
  
  # Preprocesar los comentarios para el análisis de frecuencia de palabras
  corpus <- Corpus(VectorSource(comentarios_autor$post))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
  
  # Crear una matriz de términos
  dtm <- DocumentTermMatrix(corpus)
  
  # Convertir la matriz de términos a un data frame
  matriz_palabras <- as.data.frame(as.matrix(dtm))
  
  # Calcular la frecuencia de palabras por autor
  frecuencia_palabras <- colSums(matriz_palabras)
  
  # Crear un data frame con la frecuencia de palabras
  resultado <- data.frame(autor = autor, palabra = names(frecuencia_palabras), frecuencia = frecuencia_palabras)
  
  return(resultado)
}

# Aplicar la función a cada autor seleccionado
resultados_frecuencia <- lapply(autores_seleccionados, obtener_frecuencia_palabras)

# Convertir la lista de resultados a un data frame
resultados_frecuencia <- do.call(rbind, resultados_frecuencia)

# Definir una función para obtener las palabras más frecuentes por autor
top_palabras_por_autor <- function(df) {
  df %>%
    arrange(desc(frecuencia)) %>%
    slice_head(n = 5)
}

# Aplicar la función por autor
top_palabras <- resultados_frecuencia %>%
  group_by(autor) %>%
  group_modify(~ top_palabras_por_autor(.))
# Crear la tabla con knitr

top_palabras <- top_palabras %>%
  group_by(autor) %>%
  top_n(5, frecuencia) %>%
  summarize(palabras_concatenadas = paste(palabra, collapse = ", ")) %>%
  ungroup()

tabla <- kable(top_palabras[, c("autor", "palabras_concatenadas")], format = "html", caption = "Palabras más frecuentes por autor")

# Mostrar la tabla
print(tabla)

writeLines(as.character(tabla), "tabla_palabras_mas_frecuentes-autor.html")

