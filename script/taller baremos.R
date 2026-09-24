library(readxl)
library(tidyverse)
datos <- read_xlsx("base2.xlsx")

datos %>% 
  group_by(grupo) %>% 
  summarise(mean(puntaje),
            sd(puntaje))

ggplot(datos, aes(x = edad, y = puntaje)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Edad",
    y = "Puntaje",
    title = "Relación entre edad y puntaje"
  ) +
  theme_minimal()


set.seed(123)

n <- 1000

# Edad: distribución con más densidad entre 30 y 70
edad <- round(rnorm(n, mean = 50, sd = 15))
edad <- edad[edad >= 20 & edad <= 80]
n <- length(edad)

# Puntaje VLT: relación negativa con la edad + ruido
# A los 20 años ~ 62 puntos; a los 80 ~ 30 puntos
puntaje <- round(70 - 0.55 * (edad - 20) + rnorm(n, 0, 6))

# Ajustar al rango observado (20-75)
puntaje <- pmax(pmin(puntaje, 75), 20)

# Base de datos
datos <- data.frame(Edad = edad, VLT_Total_Recall = puntaje)

# Ver primeras filas
head(datos, 20)

# Guardar CSV
write.csv(datos, "datos_simulados_VLT.csv", row.names = FALSE)

# Gráfico para comparar
plot(datos$Edad, datos$VLT_Total_Recall,
     xlab = "Age (in years)",
     ylab = "VLT Total Recall score",
     pch = 1, col = "gray40",
     main = "Datos simulados")

