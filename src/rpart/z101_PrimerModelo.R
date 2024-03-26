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

#############

modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = -1, # esto significa no limitar la complejidad de los splits
  minsplit = 600, # minima cantidad de registros para que se haga el split
  minbucket = 200, # tamaño minimo de una hoja
  maxdepth = 10
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


#### PRUEBA #####

# Cargar bibliotecas necesarias
library(rpart)
library(caret) # Para la función createDataPartition

# Dividir el dataset en entrenamiento y prueba
set.seed(123) # Para reproducibilidad
indices_train <- createDataPartition(dtrain$clase_ternaria, p = 0.7, list = FALSE)
datos_entrenamiento <- dtrain[indices_train, ]
datos_prueba <- dtrain[-indices_train, ]

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = datos_entrenamiento, # los datos donde voy a entrenar
  xval = 0,
  cp = -1, # esto significa no limitar la complejidad de los splits
  minsplit = 600, # minima cantidad de registros para que se haga el split
  minbucket = 200, # tamaño minimo de una hoja
  maxdepth = 10
) # profundidad maxima del arbol



# aplico el modelo a los datos nuevos
prediccion_p <- predict(
  object = modelo,
  newdata = datos_prueba,
  type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
datos_prueba[, prob_baja2 := prediccion_p[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
datos_prueba[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

estimulos = datos_prueba$Predicted == 1
estimulos = estimulos %>% sum()

baja2_real = datos_prueba$clase_ternaria == "BAJA+2"

aciertos = (estimulos & baja2_real) %>% sum()
ganancia = (aciertos * 117000) - (estimulos - aciertos) * 3000


36783000

#### FIN PRUEBA #### 



# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_001.csv",
        sep = ","
)





### TRAMPA #####

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


