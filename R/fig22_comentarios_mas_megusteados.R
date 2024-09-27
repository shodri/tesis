library(ggplot2)
library(stringr)  # Para el uso de str_extract y str_detect

# Cargar el archivo CSV de comentarios
comentarios <- read.csv('./Sociologia/Tesis/Datos/comentarios.csv')

# Función para extraer el número de "me gusta"
extraer_megustas <- function(megustas) {
  numero_megustas <- str_extract(megustas, "\\d+")
  if (!is.na(numero_megustas) && str_detect(megustas, "\\d+ ·")) {
    return(as.integer(numero_megustas))
  } else {
    return(0)
  }
}

# Aplicar la función para extraer los "me gusta"
comentarios$num_megustas <- sapply(comentarios$megustas, extraer_megustas)

# Ordenar los comentarios por número de "me gusta" y seleccionar los top 10
comentarios_ordenados <- comentarios[order(-comentarios$num_megustas), ]
top_comentarios <- head(comentarios_ordenados, 10)

# Seleccionar las columnas relevantes para la tabla
df_seleccionado <- top_comentarios[, c("autor", "post", "num_megustas")]

# Guardar la tabla como HTML
write.csv(df_seleccionado, "top_10_comentarios_mas_megusteados.csv", row.names = FALSE)

