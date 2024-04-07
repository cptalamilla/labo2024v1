# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require('tidyverse')
# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("X:\\gdrive\\labo2024v1\\") # Establezco el Working Directory
setwd("~/Desktop/01_Austral_MDS/06_Labo1/labo1")
# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")



##### Eliinar columnas al azar
# Calcular el 80% de las columnas
#set.seed(45543)  # Para reproducibilidad
# Calcular los índices de todas las columnas, excluyendo la segunda y la última
all_cols <- setdiff(seq_along(dataset), c(1, 2, ncol(dataset)))
num_cols <- length(all_cols)
cols_to_select <- round(num_cols * 0.8)

# Seleccionar al azar el 80% de los índices de las columnas, excluyendo la segunda y la última columna
selected_cols_indices <- sample(all_cols, cols_to_select)

# Agregar manualmente la segunda y la última columna al conjunto seleccionado
final_cols_indices <- c(1,2, selected_cols_indices, ncol(dataset))

# Ordenar los índices para mantener el orden original de las columnas donde sea relevante
final_cols_indices <- sort(final_cols_indices)

# Subconjuntar el data.table con las columnas seleccionadas, incluyendo la segunda y la última columna
dataset <- dataset[, ..final_cols_indices]
#####




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


positiveWeight = 1.0 / (nrow(subset(dtrain, clase_ternaria == 'CONTINUA')) / nrow(dtrain))
negativeWeight = 1.0 / (nrow(subset(dtrain, clase_ternaria != 'CONTINUA')) / nrow(dtrain))


pesos_vector <- ifelse(dtrain$clase_ternaria != "CONTINUA", 2, 1)




modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = -0.8, # esto significa no limitar la complejidad de los splits
  minsplit = 200, # minima cantidad de registros para que se haga el split
  minbucket = 100, # tamaño minimo de una hoja
  maxdepth = 4, 
  #weights = pesos_vector
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
dapply[, Predicted := as.numeric(prob_baja2 > 0.025)]

## CONTAR LOS 1 Predichos ###
dapply %>% group_by(Predicted) %>% count()

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
setwd("~/Desktop/01_Austral_MDS/06_Labo1/labo1")
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K101_001.csv",
       sep = ","
)



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
  maxdepth = 10,
  weights = pesos_clases,
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




##### Asumiendo que tienes un dataframe llamado datos_entrenamiento y una columna clase_ternaria
total_observaciones <- nrow(dtrain)
frecuencias <- table(dtrain$clase_ternaria)

# Calcular los pesos inversamente proporcionales a las frecuencias
pesos_clases <- total_observaciones / frecuencias

# Asegurándose de que cada observación tenga el peso correspondiente
dtrain$peso <- with(dtrain, ifelse(clase_ternaria == "BAJA+1", pesos_clases["BAJA+1"],
                                                             ifelse(clase_ternaria == "BAJA+2", pesos_clases["BAJA+2"], pesos_clases["CONTINUA"])))

dapply$peso <- with(dapply, ifelse(clase_ternaria == "BAJA+1", pesos_clases["BAJA+1"],
                                     ifelse(clase_ternaria == "BAJA+2", pesos_clases["BAJA+2"], pesos_clases["CONTINUA"])))

# Verificar que la longitud del vector de pesos es igual al número de filas en datos_entrenamiento
length(dtrain$peso) == nrow(dtrain) # Esto debe ser TRUE





### Prueba Generacion de archivos kagles masivos#### 
library(readr)

gridsearch <- fread("exp/HT2020/gridsearch.txt")


hyperparams = unique(gridsearch[order(-ganancia_promedio)], by = "ganancia_promedio")[c(1, 2, 5, 10, 30, 46)]
hyperparams


# Asumiendo que tienes 'dtrain' y 'dapply' ya definidos como tus conjuntos de datos
for (i in 1:nrow(hyperparams)) {
  # Extraer los hiperparámetros de la fila actual
  params <- hyperparams[i]
  print(params$max_depth)
  # Entrenar el modelo con los hiperparámetros de la fila actual
  modelo <- rpart(
    formula = "clase_ternaria ~ .",
    data = dtrain,
    xval = 0,
    cp = params$max_depth,
    minsplit = params$minsplit,
    minbucket = params$minbucket,
    maxdepth = params$cp
    # weights = pesos_vector (si lo necesitas)
  )
  

  # Aplicar el modelo a 'dapply'
  prediccion <- predict(
    object = modelo,
    newdata = dapply,
    type = "prob"
  )
  
  # Agregar la columna de probabilidad de BAJA+2 a dapply
  dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
  print(dapply$prob_baja2)
  # Calcular la columna 'Predicted'
  dapply[, Predicted := as.numeric(prob_baja2 > 0.025)]
  print(table(dapply$Predicted))
  # Generar el nombre del archivo CSV basado en el índice del bucle
  filename <- sprintf("./exp/KA2001/K101_%03d.csv", i)
  
  # Solo guardar los campos necesarios para Kaggle
  fwrite(dapply[, .(numero_de_cliente, Predicted)], file = filename, sep = ",")
}







modelo <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = -0.5308669, # esto significa no limitar la complejidad de los splits
  minsplit = 1568, # minima cantidad de registros para que se haga el split
  minbucket = 5, # tamaño minimo de una hoja
  maxdepth = 3, 
  #weights = pesos_vector
) # profundidad maxima del arbol



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
dapply[, Predicted := as.numeric(prob_baja2 > 0.025)]

## CONTAR LOS 1 Predichos ###
dapply %>% group_by(Predicted) %>% count()

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
setwd("~/Desktop/01_Austral_MDS/06_Labo1/labo1")
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K101_001.csv",
       sep = ","
)



fread("exp/KA2001/K101_005.csv") %>% group_by(Predicted) %>% count()


