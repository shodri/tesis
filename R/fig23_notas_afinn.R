library(tidytext)
library(dplyr)
library(tm)
library(SnowballC)
library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(ggplot2)
#Definimos un tema para facilitar la visualización de nuestros resultados.

notas <- read.csv('./Sociologia/Tesis/Datos/notas.csv')

tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

#Necesitamos separar cada tuit en palabras, para así asignarle a cada palabra relevante una puntuación de sentimiento usando el léxico Afinn
notas_afinn <- 
  notas %>%
  unnest_tokens(input = "cuerpo", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Autor" = autor)

#Obtenemos también una puntuación por tuit. Tambien asignamos a los tuits sin puntuación positiva o negativa un valor de 0, que indica neutralidad

notas <-
  notas_afinn %>%
  group_by(X) %>%
  summarise(Puntuacion_comment = mean(Puntuacion)) %>%
  left_join(notas, ., by = "X") %>% 
  mutate(Puntuacion_nota = ifelse(is.na(Puntuacion_comment), 0, Puntuacion_comment))

# Únicas
notas_afinn %>% 
  group_by(link) %>% 
  distinct(Palabra) %>% 
  count()

df_agrupado <- notas_afinn %>%
  group_by(link, fecha) %>%
  summarise(puntuacion_total = sum(Puntuacion)) %>%
  mutate(sentimiento = ifelse(puntuacion_total > 0, "Positivo",
                              ifelse(puntuacion_total == 0, "Neutro", "Negativo")))

df_agrupado$fecha <- as.POSIXct(df_agrupado$fecha, format = "%d de %B de %Y %H:%M")

df_agrupado <- df_agrupado %>%
  filter(year(fecha) >= 2023)

df_agrupado <- df_agrupado %>%
  filter(month(fecha) <= 5)


ggplot(data = df_agrupado, aes(x = fecha, y = puntuacion_total)) +
  geom_line() +
  geom_point(color = "black") +
  labs(x = "Fecha", y = "Puntuación Total") +
  scale_x_datetime(date_breaks = "5 days", date_labels = "%d %b", expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(data = df_agrupado, aes(x = fecha, y = puntuacion_total)) +
  geom_line() +
  geom_point(color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") + # Agregar la línea de regresión suavizada
  labs(x = "Fecha", y = "Puntuación Total") +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b", expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

