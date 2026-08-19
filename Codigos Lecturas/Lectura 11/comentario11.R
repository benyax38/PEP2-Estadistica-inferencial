library(dplyr)
library(scatterplot3d)
library(MASS)
library(car)

View(Boston)

# Uso de semilla y obtención de 250 observaciones
set.seed(299)
muestra <- Boston[sample(1:nrow(Boston), 250, replace = TRUE),]

# Cálculo de correlación
cor(muestra[, c("rm", "medv", "lstat")])

# Creación de modelo
modelo <- lm(rm ~ medv + lstat, data = muestra)
summary(modelo)

# Graficar el modelo ajustado , diferenciando valores sobre y bajo el plano
i_color <- 1 + (resid(modelo) > 0)
g <- scatterplot3d(
 muestra[["medv"]], muestra[["lstat"]], muestra[["rm"]], type = "p", angle = 50,
 pch = 16, color = c("steelblue1", "steelblue4")[i_color],
 xlab = "Valor mediano de las viviendas ocupadas en $1000",
 ylab = "Porcentaje de la población de\n clase baja o estatus inferior [%]\n\n\n",
 zlab = "Habitaciones por vivienda",
 mar = c(3, 3, 1, 0) + 0.1
)
g$plane3d(modelo, draw_lines = TRUE , lty = "dotted")

# Ajuste de modelos simples
modelo_0 <- lm(rm ~ 1, data = muestra)
modelo_1 <- lm(rm ~ medv, data = muestra)
modelo_2 <- lm(rm ~ lstat, data = muestra)

# Mostrando AIC y BIC de los modelos
AIC(modelo_0, modelo_1, modelo_2, modelo)
BIC(modelo_0, modelo_1, modelo_2, modelo)

# Comparando modelos
anova(modelo_0, modelo_1, modelo_2, modelo)

residualPlots(modelo, type = "rstandard",
              id = list (method = "r", n = 3, cex = 0.7, location = "lr"),
              col = "steelblue2", pch = 20 , col.quad = "red")

durbinWatsonTest(modelo)

marginalModelPlots(modelo, sd = TRUE ,
                   id = list(method = "r", n = 3, cex = 0.7 , location = "lr"),
                   col = "steelblue", pch = 20, col.line = c("steelblue", "red"))
ncvTest(modelo)

vif(modelo)

round(influencePlot(modelo), 3)

library(caret)

# Ajustar y mostrar el modelo usando validación cruzada de 5 pliegues
set.seed(266)
entrenamiento <- train (rm ~ medv + lstat, data = muestra, method = "lm",
                        trControl = trainControl(method = "cv", number = 10))
modelo <- entrenamiento[["finalModel"]]
print(summary(modelo))

# Mostrar los resultados de cada pliegue
cat("Errores en cada pliegue:\n")
print(entrenamiento [["resample"]])

# Mostrar el resultado estimado para el modelo
cat("\nError estimado para el modelo:\n")
print(entrenamiento[["results"]])