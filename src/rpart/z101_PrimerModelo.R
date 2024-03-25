# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require('tidyverse')
# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("X:\\gdrive\\labo2024v1\\") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dataset = dataset[,-(nulos$columnas_con_nulos), with = F]

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dtrain$clase_ternaria = as.factor(dtrain$clase_ternaria)
dtrain = dtrain[,-c('numero_de_cliente','foto_mes'), with = F]
dtrain %>% group_by(clase_ternaria) %>% count()
#dtrain = as.data.frame(dtrain)

#dtrain = upSample(x = dtrain[, -ncol(dtrain)],
#                    y = dtrain$clase_ternaria)
#dtrain = dtrain %>% rename(clase_ternaria = Class )

dtrain %>% group_by(clase_ternaria) %>% count()

dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo


######################

library(rpart)

# Supongamos que 'dtrain' es tu conjunto de entrenamiento y 'clase_ternaria' tu variable objetivo

# 1. Definir el espacio de hiperparámetros
cp_values <- seq(0.01, 0.1, by = 0.01)
minsplit_values <- c(200, 400, 600, 800)
minbucket_values <- c(50, 100, 200, 300)
maxdepth_values <- c(4, 6, 8, 10)

# Estructura para guardar los resultados
results <- expand.grid(cp = cp_values, minsplit = minsplit_values, minbucket = minbucket_values, maxdepth = maxdepth_values, Accuracy = NA_real_)

# 2. Bucle sobre la grilla de hiperparámetros y entrenar modelos
counter <- 1
for(cp in cp_values) {
  for(minsplit in minsplit_values) {
    for(minbucket in minbucket_values) {
      for(maxdepth in maxdepth_values) {
        set.seed(123) # Para reproducibilidad
        model <- rpart(
          formula = "clase_ternaria ~ .",
          data = dtrain,
          xval = 0,
          control = rpart.control(cp = cp, minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth)
        )
        
        # 3. Evaluar el modelo (por ejemplo, usando la precisión como métrica)
        # Aquí necesitarías aplicar tu modelo a un conjunto de validación o usar una técnica de validación cruzada
        # Este es un ejemplo placeholder para el proceso de evaluación
        pred <- predict(model, dtrain, type = "class")
        accuracy <- sum(pred == dtrain$clase_ternaria) / nrow(dtrain)
        
        # Guardar el resultado
        results$Accuracy[counter] <- accuracy
        counter <- counter + 1
      }
    }
  }
}

# 4. Encontrar la mejor combinación de hiperparámetros
best_model_index <- which.max(results$Accuracy)
best_parameters <- results[best_model_index, ]
print(best_parameters)







###################

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -1, # esto significa no limitar la complejidad de los splits
        minsplit = 600, # minima cantidad de registros para que se haga el split
        minbucket = 200, # tamaño minimo de una hoja
        maxdepth = 6
) # profundidad maxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

## CONTAR LOS 1 Predichos ###
dapply %>% group_by(Predicted) %>% count()

which(dapply$Predicted == 1) %>% length()

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_001.csv",
        sep = ","
)


# Identificar los índices de las filas donde la columna es igual a 1
indices_unos <- which(dapply$Predicted == 0)

# Calcular el 80% de estos índices para reducir
num_a_reducir <- floor(length(indices_unos) * 0.3)

# Seleccionar al azar el 80% de estos índices
set.seed(123) # Para reproducibilidad
indices_para_reducir <- sample(indices_unos, size = num_a_reducir)

# Asignar 0 a estos índices seleccionados
# Aquí, usamos un enfoque ligeramente diferente para asegurarnos de que la asignación funcione correctamente
dapply[indices_para_reducir, Predicted := 1]


