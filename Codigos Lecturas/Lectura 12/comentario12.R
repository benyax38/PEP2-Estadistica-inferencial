library(dplyr)
library(purrr)
library(caret)

# Cargar y filtrar los datos, teniendo cuidado de dejar "automático" como el
# 2do nivel de la variable "am" para que sea considerada como la clase positiva.
datos <- mtcars |> filter (wt > 2 & wt < 5) |>
    mutate(am = factor(am, levels = c(1, 0), labels = c("manual", "automático")))

# Ajustar modelo usando validación cruzada de 4 pliegues, asegurando que
# se guardan las predicciones de cada pliegue.
set.seed (113)
modelo_ent <- train(am ~ wt, data = datos, method = "glm", metric = "ROC",
                    family = binomial(link = "logit") ,
                    trControl = trainControl(method = "cv", number = 4,
                                             savePredictions = TRUE,
                                             classProbs = TRUE,
                                             summaryFunction = twoClassSummary))

# Mostrar los coeficientes del modelo obtenido
modelo_final <- modelo_ent[["finalModel"]]
modelo_final_str <- capture.output(print(summary(modelo_final), signif.stars = FALSE))
cat("Coeficientes del modelo final :\n")
cat(modelo_final_str[6:9], sep = "\n")

# caret ya calculó ROC (AUC), Sensibilidad y Especificidad
metricas_tab <- modelo_ent$resample |> 
  select(ROC, Sens, Spec, Resample) |> 
  rename(AUC = ROC, 
         Sensibilidad = Sens, 
         Especificidad = Spec, 
         Pliegue = Resample)

# Agregar los promedios y desviaciones estándar (Adaptación de L38 a L42)
medias_tab <- data.frame(t(apply(metricas_tab[, -4], 2, mean)), Pliegue = "Media")
desv_tab <- data.frame(t(apply(metricas_tab[, -4], 2, sd)), Pliegue = "D.E. ")
metricas_tab_final <- rbind(metricas_tab, medias_tab, desv_tab)

# Mostrar las métricas obtenidas (Exactamente tu código L44 a L50)
metricas_str_tab <- capture.output(print(metricas_tab_final, digits = 3, row.names = FALSE))
cat("Detalle por pliegue:\n", metricas_str_tab[1], "\n", sep = "")
cat(" ", strrep("-", nchar(metricas_str_tab[1])), "\n", sep = "")
cat(metricas_str_tab[2:5], sep = "\n")
cat(" ", strrep("-", nchar(metricas_str_tab[1])), "\n", sep = "")
cat(metricas_str_tab[6:7], sep = "\n")