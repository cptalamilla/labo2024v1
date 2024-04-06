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
  sd_ganancia <- sd(unlist(ganancias))

  return(c(ganancia_promedio, sd_ganancia))
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

# genero la data.table donde van los resultados del Grid Search
tb_grid_search <- data.table( min_bucket = integer(),
                              min_split = integer(),
                              max_depth = integer(),
                              cp_ = numeric(),
                              ganancia_promedio = numeric() )


# Inicializar tb_grid_search si no se ha hecho previamente
tb_grid_search <- data.table(bucket=integer(), 
                             split=integer(), 
                             cp=numeric(), 
                             max_depth=integer(), 
                             ganancia_promedio_1=numeric(), 
                             ganancia_sd=numeric(), 
                             peso=integer())


# itero por los loops anidados para cada hiperparametro

# Definiendo los rangos para los nuevos parámetros
vmin_bucket = 240#seq(110, 1000, 200)
vmin_split = 800#seq(200, 1000, 200)
cps = -0.9#seq(-0.5, -0.4, 0.2)  # Por ejemplo, valores de complejidad de poda
max_depths = 13#seq(4, 8, 1)  # Ejemplo de profundidades máximas
pesos <- 1#c(2, 4, 8, 16, 32, 64) # Vector de pesos a probar
expand.grid(vmin_bucket,vmin_split,cps , max_depths , pesos)

# Inicializando la tabla para recopilar los resultados
#tb_grid_search <- list()  # Asegúrate de que esto esté correctamente inicializado

for (cp in cps) {
  for (max_depth in max_depths) {
    for (bucket in vmin_bucket) {
      for (split in vmin_split) {
        # Actualización de los parámetros con los valores actuales del bucle
        param_basicos <- list(
          "cp" = cp,
          "minsplit" = split,
          "minbucket" = bucket,
          "maxdepth" = max_depth
        )

        # Simulación o llamada a la función con los parámetros
      
        ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
      
        print(ganancia_promedio[1])
        print(ganancia_promedio[2])
        print(bucket)
        print(split)
        print(cp)
        print(max_depth)
      

        # Agregando los resultados a la tabla
        tb_grid_search <- rbindlist( 
          list(tb_grid_search, 
               list(bucket, split, cp, max_depth, ganancia_promedio[1], ganancia_promedio[2]))
        )
      }
    }
    # Puede ser útil escribir la tabla a disco dentro de algunos de los bucles
    # para asegurarse de no perder datos si la ejecución se interrumpe
    Sys.sleep(2)  # Pequeña pausa

    fwrite(tb_grid_search,
           file = archivo_salida,
           sep = "\t" )
  }
}




#### PESOS #####

library(data.table) # Para rbindlist y fwrite
library(rpart) # Para el modelo de árbol de decisión

# Suponiendo que tb_grid_search y archivo_salida están inicializados adecuadamente

pesos <- c(2, 4, 8, 16, 32, 64) # Vector de pesos a probar
for (peso in pesos) {
  for (cp in cps) {
    for (max_depth in max_depths) {
      for (bucket in vmin_bucket) {
        for (split in vmin_split) {
          # Asignación de pesos específicos a las clases distintas de "CONTINUA"
          # Nota: Asumiendo que 'datos_entrenamiento' ya tiene una columna 'clase_ternaria'
          # Aquí generamos un vector de pesos para pasar a la lista de parámetros
          pesos_vector <- ifelse(dataset$clase_ternaria == "CONTINUA", 1, peso)
          
          # Actualización de los parámetros con los valores actuales del bucle
          # Incluimos el vector de pesos en la lista de parámetros
          param_basicos <- list(
            "cp" = cp,
            "minsplit" = split,
            "minbucket" = bucket,
            "maxdepth" = max_depth,
            "weights" = pesos_vector  # Agregamos los pesos aquí
          )
          
          # Simulación o llamada a la función con los parámetros
          ganancia_promedio <- ArbolesMontecarlo(PARAM$semillas, param_basicos)
          
          # Salidas de depuración para verificar los valores actuales
          print(ganancia_promedio[1])
          print(ganancia_promedio[2])
          print(bucket)
          print(split)
          print(cp)
          print(max_depth)
          print(peso)
          
          # Agregando los resultados a la tabla
          tb_grid_search <- rbindlist( 
            list(tb_grid_search, 
                 list(bucket=bucket, split=split, cp=cp, max_depth=max_depth, 
                      ganancia_promedio_1=ganancia_promedio[1], ganancia_sd=ganancia_promedio[2], peso=peso))
          )
        }
      }
      # Pausa y escritura de seguridad en disco
      Sys.sleep(1)  # Pequeña pausa
      
      fwrite(tb_grid_search,
             file = archivo_salida,
             sep = "\t")
    }
  }
}


######

library(readr)
library(tidyverse)
gridsearch <- read_delim("exp/HT2020/gridsearch.txt", 
    delim = "\t", escape_double = FALSE, 
    trim_ws = TRUE)
gridsearch %>% arrange(desc(ganancia_promedio_1))



positiveWeight = 1.0 / (nrow(subset(dataset, clase_ternaria == 'CONTINUA')) / nrow(dataset))
negativeWeight = 1.0 / (nrow(subset(dataset, clase_ternaria != 'CONTINUA')) / nrow(dataset))


