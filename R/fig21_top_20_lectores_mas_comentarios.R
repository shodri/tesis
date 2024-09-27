library(dplyr)
library(ggplot2)

# Cargar datos
comentarios <- read.csv('./Sociologia/Tesis/Datos/comentarios.csv')

# Top 20 usuarios por cantidad de comentarios
top_users <- comentarios %>%
  count(autor, name = "total_comentarios") %>%
  arrange(desc(total_comentarios)) %>%
  head(20)

# Crear gráfico
ggplot(top_users, aes(x = reorder(autor, -total_comentarios), y = total_comentarios)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Top 20 Usuarios con Más Comentarios",
       x = "Usuarios",
       y = "Cantidad de Comentarios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Guardar el gráfico
ggsave("top_20_usuarios_comentarios.jpg", width = 10, height = 6, units = "in")

