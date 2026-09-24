library(tidyverse)
# ============================================================
# Crear variable "grupo" a partir de la edad
# ============================================================

# 1. Cargar o crear el dataframe (usamos el simulado anterior)
set.seed(123)

n <- 1000
edad <- round(rnorm(n, mean = 50, sd = 15))
edad <- edad[edad >= 20 & edad <= 80]
n <- length(edad)

puntaje <- round(70 - 0.55 * (edad - 20) + rnorm(n, 0, 6))
puntaje <- pmax(pmin(puntaje, 75), 20)

datos <- data.frame(Edad = edad, VLT_Total_Recall = puntaje)

# ------------------------------------------------------------
# 2. Crear la variable "grupo"
# ------------------------------------------------------------

datos <- datos %>%
  mutate(grupo = cut(Edad,
                     breaks = c(20, 30, 40, 50, 60, 70, 81),
                     labels = c("20-29", "30-39", "40-49",
                                "50-59", "60-69", "70-80"),
                     right = FALSE,
                     include.lowest = TRUE))

# ------------------------------------------------------------
# 3. Verificar resultados
# ------------------------------------------------------------

# Primeras filas
head(datos, 10)

# Frecuencia por grupo
table(datos$grupo)

# Resumen de la variable grupo
summary(datos$grupo)

# ------------------------------------------------------------
# 4. Guardar el dataframe actualizado
# ------------------------------------------------------------

write.csv(datos, "datos_con_grupo.csv", row.names = FALSE)

### GRAFICO
ggplot(datos, aes(x = Edad, y = VLT_Total_Recall)) +
  geom_point(shape = 1, size = 1.8,) +
  scale_x_continuous(
    breaks = seq(20, 80, by = 10),
    limits = c(15, 85)
  ) +
  scale_y_continuous(
    breaks = seq(20, 70, by = 10),
    limits = c(15, 80)
  ) +
  labs(
    x = "Edad (en años)",
    y = "RAVLT Total evocación"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),        # sin grilla (como el original)
    panel.border = element_rect(linewidth = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    plot.margin = margin(10, 10, 10, 10)
  )



