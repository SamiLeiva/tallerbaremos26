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

## medias
datos %>% 
  group_by(grupo) %>% 
  summarise(mean(VLT_Total_Recall),
            sd(VLT_Total_Recall),
            n())->tabla

library(writexl)
write_xlsx(
  tabla,
  "tabla_VLT_por_grupo.xlsx"
)



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



### Z -------------
# Crear valores de Z
z <- seq(-4, 4, length.out = 1000)

# Densidad de la distribución normal estándar
densidad <- dnorm(z)

# Crear dataframe
datos_z <- data.frame(
  Z = z,
  Densidad = densidad
)

# Gráfico
ggplot(datos_z, aes(x = Z, y = Densidad)) +
  
  # Curva normal
  geom_line(linewidth = 1) +
  
  # Media: Z = 0
  geom_vline(
    xintercept = 0,
    linetype = "solid",
    linewidth = 0.8
  ) +
  
  # ±1 desvío estándar
  geom_vline(
    xintercept = c(-1, 1),
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  # Z = -1.9 en rojo
  geom_vline(
    xintercept = -1.9,
    color = "red",
    linetype = "solid",
    linewidth = 0.9
  ) +
  
  # Z = -0.9 en verde
  geom_vline(
    xintercept = -0.9,
    color = "green",
    linetype = "solid",
    linewidth = 0.9
  ) +
  
  # Ejes
  scale_x_continuous(
    breaks = seq(-4, 4, by = 1),
    limits = c(-4, 4)
  ) +
  
  labs(
    x = "Puntuación Z",
    y = "Densidad",
    title = "Distribución normal estándar"
  ) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(linewidth = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    plot.title = element_text(size = 14)
  )



## Otro ---------
# Valores de Z
z <- seq(-4, 4, length.out = 1000)

# Densidad normal estándar
densidad <- dnorm(z)

# Dataframe
datos_z <- data.frame(
  Z = z,
  Densidad = densidad
)

# Valores de referencia
z_media <- 0
z_menos1 <- -1
z_mas1 <- 1
z_rojo <- -1.9
z_verde <- -0.9

# Altura de la curva en cada punto
y_media <- dnorm(z_media)
y_menos1 <- dnorm(z_menos1)
y_mas1 <- dnorm(z_mas1)
y_rojo <- dnorm(z_rojo)
y_verde <- dnorm(z_verde)


# ============================================================
# GRÁFICO
# ============================================================

ggplot(datos_z, aes(x = Z, y = Densidad)) +
  
  # Curva normal
  geom_line(
    linewidth = 1.1
  ) +
  
  # Media: Z = 0
  geom_segment(
    aes(
      x = z_media,
      xend = z_media,
      y = 0,
      yend = y_media
    ),
    linewidth = 0.9
  ) +
  
  # -1 DE y +1 DE
  geom_segment(
    aes(
      x = z_menos1,
      xend = z_menos1,
      y = 0,
      yend = y_menos1
    ),
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  geom_segment(
    aes(
      x = z_mas1,
      xend = z_mas1,
      y = 0,
      yend = y_mas1
    ),
    linetype = "dashed",
    linewidth = 0.7
  ) +
  
  # Z = -1.9 (rojo)
  geom_segment(
    aes(
      x = z_rojo,
      xend = z_rojo,
      y = 0,
      yend = y_rojo
    ),
    color = "red",
    linewidth = 1
  ) +
  
  # Z = -0.9 (verde)
  geom_segment(
    aes(
      x = z_verde,
      xend = z_verde,
      y = 0,
      yend = y_verde
    ),
    color = "green",
    linewidth = 1
  ) +
  
  # Eje X
  scale_x_continuous(
    breaks = seq(-4, 4, by = 1),
    limits = c(-4, 4)
  ) +
  
  # Eje Y
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = "Puntuación Z",
    y = "Densidad"
  ) +
  
  theme_classic() +
  
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    axis.line = element_line(linewidth = 0.8),
    plot.margin = margin(10, 15, 10, 15)
  )
