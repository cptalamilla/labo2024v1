# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# Librerias _______________________________________________________
require("data.table")
require("rpart")
require("rpart.plot")

library(data.table)
library(dplyr) # to perform some data wrangling tasks
library(rpart) # to fit decision trees without tuning
library(rpart.plot) # to plot our decision trees
library(Metrics) #to assess the performance of our models
library(mlr) # to train our model’s hyperparameters
library(ggplot2) # for general plots we will do
library(plotly) # for 3-D plots
library(sampling)
library(survey)
#__________________________________________________________________
# Funciones _______________________________________________________

# Función para identificar columnas con valores nulos y contarlos
analizar_nulos <- function(dt) {
  # Asegurarse de que el input es un data.table
  if (!is.data.table(dt)) {
    stop("El input debe ser un data.table")
  }
  
  # Paso 1: Encontrar columnas con valores nulos y contarlos
  nulos_df <- sapply(dt, function(x) sum(is.na(x))) # Cuenta los NAs por columna
  nulos_df <- data.frame(columna = names(nulos_df), n_nulos = nulos_df) # Convertir a data.frame
  nulos_df <- nulos_df[nulos_df$n_nulos > 0, ] # Filtrar columnas con al menos un NA
  
  # Paso 2: Obtener los nombres de las columnas con valores nulos
  columnas_con_nulos <- names(dt)[colSums(is.na(dt)) > 0]
  
  # Devolver una lista con ambos componentes
  list(
    columnas_nulos_df = nulos_df,
    columnas_con_nulos = columnas_con_nulos
  )
}

#__________________________________________________________________
# Working Directory________________________________________________


setwd("~/buckets/b1")# Establezco el Working Directory
#__________________________________________________________________
# Dataset__________________________________________________________

dataset <- fread('datasets/dataset_pequeno.csv' )
dataset_original = fread('datasets/dataset_pequeno.csv' )
ncol(dataset)

#__________________________________________________________________
# Limpieza Datos __________________________________________________

# Excluir las variables especificadas
id_cols = dataset_original[, c("numero_de_cliente", "foto_mes", "clase_ternaria") ]
dataset[, c("numero_de_cliente", "foto_mes", "clase_ternaria") := NULL]

# Identificar las variables binarias
variables_binarias <- sapply(dataset, function(x) all(unique(x) %in% c(0, 1)))

# Mostrar las variables binarias
print(names(variables_binarias)[variables_binarias])

# Obtener los nombres de las columnas que son numéricas y no binarias
variables_numericas <- names(dataset)[!variables_binarias & sapply(dataset, is.numeric)]
print(variables_numericas)

# Normalizar las variables numéricas
dataset[, (variables_numericas) := lapply(.SD, scale), .SDcols = variables_numericas]
summary(dataset$mcuenta_debitos_automaticos)#chequeo el cambio

# Eliminar columnas  con valores faltantes
#dataset <- dataset[complete.cases(dataset), ]

nulos = analizar_nulos(dataset)


dataset = dataset[,-(nulos$columnas_con_nulos), with = F]



# Realizar el PCA
pca_result <- prcomp(dataset, scale. = FALSE)

componentes = pca_result$x

dataset_componentes = cbind(id_cols, componentes) %>% as.data.frame() 

dataset_componentes


# Obtener los loadings de las componentes principales
loadings <- pca_result$rotation

# Calcular la varianza explicada por cada variable
varianza_explicada_por_variable <- apply(loadings^2, 2, function(x) sum(x * pca_result$sdev^2))

# Ordenar las variables por la cantidad de varianza explicada
orden <- order(varianza_explicada_por_variable, decreasing = TRUE)

# Obtener los nombres de las variables ordenadas
nombres_variables_ordenados <- colnames(dataset)[orden]
print(nombres_variables_ordenados)

# Obtener la varianza explicada acumulada por cada componente
varianza_acumulada <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)

# Encontrar el número mínimo de componentes principales necesarias
num_componentes_necesarias <- which.max(varianza_acumulada >= 0.95)
print(num_componentes_necesarias)


dataset_componentes <- dataset_componentes[,c(1:78)]
dataset_componentes

setDT(dataset_componentes)
write.csv(dataset_componentes, 'datasets/dataset_pca_dt.csv')


