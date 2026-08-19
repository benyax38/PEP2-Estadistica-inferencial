library(dplyr)
library(ggpubr)
library(car)

datos <- mtcars |> filter(wt > 2 & wt < 5)

correlacion <- cor(datos$qsec, datos$mpg)

modelo <- lm(mpg ~ qsec, data = datos)
summary(modelo)

g1 <- ggscatter(datos, x = "qsec", y = "mpg",
                color = "steelblue", fill = "steelblue1", alpha = 0.5 , size = 3,
                ylab = "Millas por galón [millas/galón]")
g1 <- g1 + geom_abline(intercept = coef(modelo)[1], slope = coef(modelo)[2],
                       color = "steelblue4")
g1 <- g1 + xlab(bquote("Tiempo en 1/4 de milla" ~ group("[", "s", "]")) )
print(g1)

# Definir valores del predictador para vehículos no incluidos
qsec <- c(17.98, 17.82, 17.42, 18.52, 19.90, 18.90, 16.90)

# Usar el modelo para predecir el rendimiento de otros modelos
potencia_est <- predict(modelo, data.frame(qsec))

# Graficar los valores predichos
nuevos <- data.frame(qsec, mpg = potencia_est)
g2 <- ggscatter(nuevos, x = "qsec", y = "mpg",
                color = "steelblue", fill = "steelblue1", alpha = 0.5 , size = 3,
                ylab = "Millas por galón [millas/galón]")
g2 <- g2 + xlab(bquote("Tiempo en 1/4 de milla" ~ group("[", "s", "]")) )
print(g2)

# Unir los gráficos en uno solo
g1 <- ggpar(g1, xlim = c(14, 23), ylim = c(13, 33))
g2 <- ggpar(g2, xlim = c(14, 23), ylim = c(13, 33))
g <- ggarrange(g1, g2, labels = c("Modelo", "Predicciones"), hjust = c(-1.2, -0.7))
print(g)

residualPlots(modelo , type = "rstandard",
              id = list (method = "r", n = 3, cex = 0.7, location = "lr"),
              col = "steelblue2", pch = 20 , col.quad = "red")


set.seed (19)
durbinWatsonTest(modelo)

marginalModelPlots(modelo, sd = TRUE ,
                   id = list(method = "r", n = 3, cex = 0.7 , location = "lr") ,
                   col = "steelblue", pch = 20, col.line = c("steelblue", "red"))
ncvTest(modelo)


round(influencePlot(modelo), 3)

# Creamos un nuevo dataset excluyendo al "Fiat 128"
df_sin_fiat <- subset(datos, rownames(datos) != "Fiat 128")

# Ajustamos el nuevo modelo sin este caso
modelo_sin_fiat <- lm(mpg ~ qsec, data = df_sin_fiat)

# Comparamos los coeficientes (el intercepto y la pendiente)
coef(modelo)
coef(modelo_sin_fiat)

# Comparamos el poder predictivo (R-cuadrado)
cat("\nR-cuadrado Original:", summary(modelo)$r.squared, "\n")
cat("R-cuadrado Nuevo:", summary(modelo_sin_fiat)$r.squared, "\n")

################################################################################
n <- nrow(datos)

# Crear conjuntos de entrenamiento y prueba.
set.seed (101)
n_entrenamiento <- floor(0.8 * n)
i_entrenamiento <- sample.int(n = n, size = n_entrenamiento, replace = FALSE )
entrenamiento <- datos[i_entrenamiento,]
prueba <- datos[-i_entrenamiento,]

# Ajustar y mostrar el modelo con el conjunto de entrenamiento.
modelo <- lm(mpg ~ qsec, data = entrenamiento)
print(summary(modelo))

# Calcular error cuadrado promedio para el conjunto de entrenamiento.
rmse_entrenamiento <- sqrt(mean(resid(modelo) ** 2))
cat("MSE para el conjunto de entrenamiento:", rmse_entrenamiento, "\n")

# Hacer predicciones para el conjunto de prueba.
predicciones <- predict(modelo, prueba)

# Calcular error cuadrado promedio para el conjunto de prueba.
error <- prueba[["mpg"]] - predicciones
rmse_prueba <- sqrt(mean(error** 2))
cat("MSE para el conjunto de prueba:", rmse_prueba)
################################################################################
library(caret)

# Ajustar y mostrar el modelo usando validación cruzada de 5 pliegues
set.seed(111)
entrenamiento <- train (mpg ~ qsec, data = datos, method = "lm",
                        trControl = trainControl(method = "cv", number = 5))
modelo <- entrenamiento[["finalModel"]]
print(summary(modelo))

# Mostrar los resultados de cada pliegue
cat("Errores en cada pliegue:\n")
print(entrenamiento [["resample"]])

# Mostrar el resultado estimado para el modelo
cat("\nError estimado para el modelo:\n")
print(entrenamiento[["results"]])
################################################################################
library(caret)

set.seed(111)
entrenamiento <- train(qsec ~ mpg, data = datos, method = "lm",
                       trControl = trainControl(method = "LOOCV"))
modelo <- entrenamiento[["finalModel"]]
print(summary(modelo))

cat("Predicciones en cada pliegue:\n")
print(entrenamiento[["pred"]])

cat("\nError estimado para el modelo:\n")
print(entrenamiento[["results"]])