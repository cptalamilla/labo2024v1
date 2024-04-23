# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# Librerias _______________________________________________________
library(data.table)
library(dplyr) # to perform some data wrangling tasks
library(rpart) # to fit decision trees without tuning
library(rpart.plot) # to plot our decision trees
library(tidyverse)
#__________________________________________________________________

#setwd("~/buckets/b1")# Establezco el Working Directory
setwd("~/Desktop/01_Austral_MDS/06_Labo1/labo1")
#__________________________________________________________________
# Dataset__________________________________________________________

dataset <- fread('datasets/dataset_pequeno.csv' )
dataset_original = fread('datasets/dataset_pequeno.csv' )


# Función para realizar PCA y unir resultados con columnas seleccionadas
realizar_PCA_y_fusionar <- function(dt, id_cols = c("numero_de_cliente", "foto_mes", "clase_ternaria")) {
  # Asegurarse de que el input es un data.table
  if (!is.data.table(dt)) {
    stop("El input debe ser un data.table")
  }
  
  # Separar columnas de identificación y target
  datos_id <- dt[, ..id_cols]
  datos_resto <- dt[, !id_cols, with = FALSE]
  
  # Eliminar columnas con valores nulos del dataset de análisis
  datos_resto <- datos_resto[, colSums(is.na(datos_resto)) == 0, with = FALSE]
  
  # Realizar PCA sobre el resto de las columnas
  pca_result <- prcomp(datos_resto, scale. = TRUE)
  
  # Calcular la varianza explicada acumulada
  varianza_acumulada <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
  num_componentes <- which(varianza_acumulada >= 0.95)[1]
  
  # Seleccionar las componentes principales necesarias
  componentes_principales <- pca_result$x[, 1:num_componentes, drop = FALSE]
  
  # Convertir la matriz de componentes principales en un data.table
  componentes_principales <- as.data.table(componentes_principales)

  # Fusionar las columnas aisladas con las componentes principales
  resultado_final <- cbind(datos_id, componentes_principales)
  
  return(resultado_final)
}


# Aplicar la función al dataset
resultado <- realizar_PCA_y_fusionar(dataset)
resultado$var

# Imprimir los resultados
print(resultado)

# Opcionalmente, guardar los resultados
write.csv(resultado, 'datasets/dataset_pca_fusionado.csv', row.names = FALSE)
