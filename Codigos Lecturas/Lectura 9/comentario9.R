# Carga de librerías
library(MASS)
library(dplyr)
library(ggplot2)

# Fijamos la semilla que pide el enunciado
set.seed(11)

# Obtención de 25 observaciones de cada raza
datos <- birthwt |> select(race, age) |> group_by(race) |> slice_sample(n = 25)
datos$race <- as.factor(datos$race)

# Calculamos el ANOVA original y extraemos su valor F
anova_obs <- aov(age ~ race, data = datos)
f_observado <- summary(anova_obs)[[1]][["F value"]][1]

# Definimos el número de permutaciones
B <- 2999
f_permutados <- numeric(B) # Vector vacío para guardar los resultados

# Bucle de permutación
for(i in 1:B){
  # Permutamos aleatoriamente la columna 'age'
  age_simulado <- sample(datos$age)
  
  # Calculamos el ANOVA con la variable permutada
  anova_sim <- aov(age_simulado ~ race, data = datos)
  
  # Guardamos el valor F simulado
  f_permutados[i] <- summary(anova_sim)[[1]][["F value"]][1]
}

# Se calcula la proporción de veces que el F simulado superó o igualó al F real.
# Sumamos el caso observado tanto al numerador como al denominador (buenas prácticas).
p_valor <- sum(c(f_permutados, f_observado) >= f_observado) / (B + 1)


# 1. Preparar los datos para ggplot
# Convertimos el vector de permutaciones a un data.frame y asignamos los valores
dist_perm <- data.frame(F = f_permutados)
F_obs <- f_observado 
R <- B # Usamos la variable B (10000) de nuestro bucle anterior

# 2. Generar el título y la etiqueta del valor p
titulo <- sprintf("Contraste de hipótesis usando %d permutaciones", R)

# Simplificamos la creación de la nota sin usar el paquete 'glue'
nota <- if_else(p_valor < 0.05, 
               "italic(p) < '0.001'", 
               sprintf("italic(p) == '%.4f'", p_valor))

# 3. Construir el gráfico (usando la lógica exacta de tu imagen)
dist_perm_pl <- dist_perm |>
  mutate(extremo = factor(if_else(F >= F_obs, 1, 0))) |> 
  ggplot(aes(x = F)) +
  geom_histogram(aes(alpha = extremo), 
                 boundary = F_obs,
                 bins = 50, # Usamos 50 barras en lugar de la función nclass.scott
                 color = "steelblue4", fill = "steelblue") +
  labs(title = titulo, x = "Estadístico F (Permutado)", y = "Frecuencia") +
  geom_vline(xintercept = F_obs, colour = "steelblue4", linetype = "dashed") +
  # Ajusté el hjust para que el texto del valor p no pise la línea
  annotate("text", x = F_obs, y = Inf, hjust = -0.1, vjust = 2, 
           label = nota, parse = TRUE) +
  scale_alpha_manual(values = c("0" = 0.2, "1" = 1), guide = "none") +
  theme_minimal() + # Usamos un tema nativo de ggplot2 para no depender de ggpubr
  theme(plot.title = element_text(hjust = 0.5))

# 4. Imprimir el gráfico
print(dist_perm_pl)