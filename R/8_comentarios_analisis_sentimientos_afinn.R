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

resultado <- comentarios_fecha %>%
  group_by(fecha) %>%
  summarise(puntuacion_total = sum(Puntuacion_total, na.rm = TRUE),
            cantidad_comentarios = n())
resultado <- resultado %>%
  mutate(puntuacion_promedio = puntuacion_total / cantidad_comentarios)

resultado <- resultado %>%
  filter(year(fecha) >= 2023)

resultado <- resultado %>%
  filter(month(fecha) <= 5)

ggplot(resultado, aes(x = fecha, y = puntuacion_promedio)) +
  geom_line() + 
  geom_point() +
  labs(x = "Fecha", y = "Puntuación total", title = "Puntuación total por día") +
  theme_minimal()

ggplot(resultado, aes(x = fecha, y = puntuacion_promedio)) +
  geom_line() + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Agregar la línea de regresión LOESS
  labs(x = "Fecha", y = "Puntuación total", title = "Puntuación total por día") +
  theme_minimal()