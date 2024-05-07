library(knitr)
library(ggplot2)
library(kableExtra)
library(gridExtra)
library(png)
library(webshot)

comentarios <- read.csv('./Sociologia/Tesis/Datos/comentarios.csv')

extraer_megustas <- function(megustas) {
  numero_megustas <- str_extract(megustas, "\\d+")
  if (!is.na(numero_megustas) && str_detect(megustas, "\\d+ ·")) {
    return(as.integer(numero_megustas))
  } else {
    return(0)
  }
}

comentarios$num_megustas <- sapply(comentarios$megustas, extraer_megustas)

comentarios_ordenados <- comentarios[order(-comentarios$num_megustas), ]

top_comentarios <- head(comentarios_ordenados, 20)

df_seleccionado <- top_comentarios %>% select(autor, post, num_megustas)

tabla_html <- kable(df_seleccionado, format = "html", escape = TRUE) %>%
  kable_styling(full_width = FALSE)

ggplot() +
  annotation_custom(tableGrob(tabla_html), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()  # Remove unnecessary elements for a cleaner image

writeLines(as.character(tabla_html), "top_20_comentarios_mas_megusteados.html")



