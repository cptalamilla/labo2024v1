# esqueleto de grid search
# se espera que los alumnos completen lo que falta
#   para recorrer TODOS cuatro los hiperparametros

rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")
require("parallel")

PARAM <- list()
# reemplazar por las propias semillas
PARAM$semillas <- c(100019, 110023, 120041, 130043, 140053)

#------------------------------------------------------------------------------
# particionar agrega una columna llamada fold a un dataset
#  que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)
#   crea una particion 70, 30

particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)

  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
    by = agrupa
  ]
}
#------------------------------------------------------------------------------

ArbolEstimarGanancia <- function(semilla, param_basicos) {
  # particiono estratificadamente el dataset
  particionar(dataset, division = c(7, 3), agrupa = "clase_ternaria", seed = semilla)

  # genero el modelo
  # quiero predecir clase_ternaria a partir del resto
  modelo <- rpart("clase_ternaria ~ .",
    data = dataset[fold == 1], # fold==1  es training,  el 70% de los datos
    xval = 0,
    control = param_basicos
  ) # aqui van los parametros del arbol

  # aplico el modelo a los datos de testing
  prediccion <- predict(modelo, # el modelo que genere recien
    dataset[fold == 2], # fold==2  es testing, el 30% de los datos
    type = "prob"
  ) # type= "prob"  es que devuelva la probabilidad

  # prediccion es una matriz con TRES columnas,
  #  llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades


  # calculo la ganancia en testing  ques fold==2
  ganancia_test <- dataset[
    fold == 2,
    sum(ifelse(prediccion[, "BAJA+2"] > 0.025,
      ifelse(clase_ternaria == "BAJA+2", 117000, -3000),
      0
    ))
  ]

  # escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada <- ganancia_test / 0.3

  return(ganancia_test_normalizada)
}
#------------------------------------------------------------------------------

ArbolesMontecarlo <- function(semillas, param_basicos) {
  # la funcion mcmapply  llama a la funcion ArbolEstimarGanancia
  #  tantas veces como valores tenga el vector  PARAM$semillas
  ganancias <- mcmapply(ArbolEstimarGanancia,
    semillas, # paso el vector de semillas
    MoreArgs = list(param_basicos), # aqui paso el segundo parametro
    SIMPLIFY = FALSE,
    mc.cores = 5 # en Windows este valor debe ser 1
  )

  ganancia_promedio <- mean(unlist(ganancias))
  

  return(ganancia_promedio)
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Aqui se debe poner la carpeta de la computadora local
setwd("~/Desktop/01_Austral_MDS/06_Labo1/labo1") # Establezco el Working Directory
# cargo los datos

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# trabajo solo con los datos con clase, es decir 202107
dataset <- dataset[clase_ternaria != ""]

# genero el archivo para Kaggle
# creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create("./exp/HT2020/", showWarnings = FALSE)
archivo_salida <- "./exp/HT2020/gridsearch.txt"
archivo_salida2 <- "./exp/HT2020/gridsearch2.txt"
# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table( min_bucket = integer(),
                              min_split = integer(),
                              max_depth = integer(),
                              cp = numeric(),
                              ganancia_promedio = numeric() )





# itero por los loops anidados para cada hiperparametro

# Definiendo los rangos para los nuevos parámetros
vmin_bucket = seq(100, 300, 100)
vmin_split = seq(200, 1000, 200)
cps = seq(-0.8, -0.1, 0.2)  # Por ejemplo, valores de complejidad de poda
max_depths = seq(4, 8, 1)  # Ejemplo de profundidades máximas

hyperparams = expand.grid(vmin_bucket=vmin_bucket,vmin_split=vmin_split,cps=cps , max_depths=max_depths)
hyperparams
setDT(hyperparams)
# Inicializando la tabla para recopilar los resultados
#tb_grid_search <- list()  # Asegúrate de que esto esté correctamente inicializado

for (i in 1:nrow(hyperparams)) {
  #print(hyperparams[i])
  print(hyperparams[i]$vmin_split)
}


for (i in 1:nrow(hyperparams)) {
  # Extraer los hiperparámetros de la fila actual
  params <- hyperparams[i]
  
  # Entrenar el modelo con los hiperparámetros de la fila actual
  modelo <- rpart(
    formula = "clase_ternaria ~ .",
    data = dataset,
    xval = 0,
    cp = params$cps,
    minsplit = params$vmin_split,
    minbucket = params$vmin_bucket,
    maxdepth = params$max_depths
    # weights = pesos_vector (si lo necesitas)
  )
  ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)

  
  # Agregando los resultados a la tabla
  tb_grid_search <- rbindlist( 
    list(tb_grid_search, 
         list(params$vmin_bucket, params$vmin_split,params$max_depths, params$cps, ganancia_promedio))
  )
  
  Sys.sleep(2)  # Pequeña pausa
  
  fwrite(tb_grid_search,
         file = archivo_salida2,
         sep = "\t" )
  print(tb_grid_search)
}



library(readr)
library(tidyverse)
gridsearch <- read_delim("exp/HT2020/gridsearch.txt", 
    delim = "\t", escape_double = FALSE, 
    trim_ws = TRUE)

setDT(gridsearch)

gan = unique(gridsearch[order(-ganancia_promedio)], by = "ganancia_promedio")[c(1, 2, 5, 10, 30, 46)]
gan





positiveWeight = 1.0 / (nrow(subset(dataset, clase_ternaria == 'CONTINUA')) / nrow(dataset))
negativeWeight = 1.0 / (nrow(subset(dataset, clase_ternaria != 'CONTINUA')) / nrow(dataset))


