library(tidytext)
library(dplyr)
library(tm)
library(SnowballC)
library(tidyverse)
library(lubridate)
library(zoo)
library(scales)

comentarios <- read.csv('./Sociologia/Tesis/Datos/comentarios.csv')

comentarios <- comentarios %>%
  select(-X)
comentarios <- comentarios %>%
  select(-Column1)

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

comentarios <- comentarios %>%
  mutate(comentario_id = row_number())

comentarios_afinn <- 
  comentarios %>%
  unnest_tokens(input = "post", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>%
mutate(comentario_id = comentario_id)

#Obtenemos puntuación por comentario
puntuacion_total_por_comentario <- comentarios_afinn %>%
  group_by(comentario_id) %>%
  summarise(Puntuacion_total = sum(Puntuacion))

# Unir los comentarios originales con la puntuación total por comentario
comentarios_con_puntuacion <- comentarios %>%
  left_join(puntuacion_total_por_comentario, by = "comentario_id")

comentarios_fecha <-comentarios_con_puntuacion %>%
  mutate(fecha = as_datetime(utime, origin = "1970-01-01"))
comentarios_fecha <- comentarios_fecha %>%
  mutate(fecha = as_date(fecha))

# Asegurarse de que la columna 'Puntuacion_total' es numérica
comentarios_fecha$Puntuacion_total <- as.numeric(comentarios_fecha$Puntuacion_total)

# Crear la nueva columna 'etiqueta' manejando los NA
comentarios_fecha$polaridad <- ifelse(is.na(comentarios_fecha$Puntuacion_total) | comentarios_fecha$Puntuacion_total == 0, NA, 
                      ifelse(comentarios_fecha$Puntuacion_total >= 0, "positivo", "negativo"))

comentarios_fecha$valor_polaridad <- ifelse(comentarios_fecha$polaridad == "positivo", 1, 
                                            ifelse(comentarios_fecha$polaridad == "negativo", -1, NA))

# Agrupar por fecha y sumar los valores de polaridad
resultado <- comentarios_fecha %>%
  group_by(fecha) %>%
  summarise(puntuacion_total = sum(valor_polaridad, na.rm = TRUE),  # Sumar los valores de polaridad
            cantidad_comentarios = n())

# Filtrar por año y mes
resultado <- resultado %>%
  filter(year(fecha) >= 2023) %>%
  filter(month(fecha) <= 5)

# Graficar la suma de polaridades por fecha
ggplot(resultado, aes(x = fecha, y = puntuacion_total)) +
  geom_line() + 
  geom_point() +
  labs(x = "Fecha", y = "Polaridad (suma de +1 y -1)", title = "Suma de polaridad por día") +
  theme_minimal()

# Agregar línea LOESS para suavizar los datos
ggplot(resultado, aes(x = fecha, y = puntuacion_total)) +
  geom_line() + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Línea de suavizado LOESS
  labs(x = "Fecha", y = "Suma de sentimientos", title = "Suma de polaridad por día") +
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b") +  # Etiquetas cada dos días
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
