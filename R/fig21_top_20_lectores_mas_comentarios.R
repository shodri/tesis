library(dplyr)
library(tidytext)
library(ggplot2)
library(kableExtra)


comentarios <- read.csv('./Sociologia/Tesis/Datos/comentarios.csv')

top_users <- comentarios %>%
  group_by(autor) %>%
  summarise(total_comentarios = n()) %>%
  arrange(desc(total_comentarios)) %>%
  head(20)

ggplot(top_users, aes(x = reorder(autor, -total_comentarios), y = total_comentarios)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Top 20 Usuarios con Más Comentarios",
       x = "Usuarios",
       y = "Cantidad de Comentarios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Guardar la imagen en un archivo (por ejemplo, PNG)
ggsave("top_usuarios_comentarios.jpg", my_plot, width = 10, height = 6, units = "in")
