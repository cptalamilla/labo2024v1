dataset[,-(nulos$columnas_con_nulos), with = F] %>% dim()
dataset[,c(nulos$columnas_con_nulos), with = F] %>% dim()


c(nulos$columnas_con_nulos , 'numero_de_cliente')

dataset$numero_de_cliente
library(data.table)

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

nulos = analizar_nulos(dataset)
nulos$columnas_nulos_df %>% arrange(n_nulos)
nulos$columnas_con_nulos


dataset[,-(nulos$columnas_con_nulos), with = F] %>% dim()
dataset[,nulos$columnas_con_nulos, with = F] %>% dim()

dtrain %>% group_by(clase_ternaria) %>% count()



generar_predicciones <- function(dtrain, dapply, profundidades) {
  resultados <- data.table(prof_arbol = integer(), cantidad_1 = integer(), cantidad_0 = integer())
  
  for (profundidad in profundidades) {
    modelo <- rpart(
      formula = "clase_ternaria ~ .",
      data = dtrain,
      xval = 0,
      cp = -0.3, # Sin limitar la complejidad
      minsplit = 0,
      minbucket = 1,
      maxdepth = profundidad
    )
    
    # Aplicar el modelo a dapply
    prediccion <- predict(
      object = modelo,
      newdata = dapply,
      type = "prob"
    )
    
    # Asignar probabilidades y crear columna Predicted
    dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
    dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
    
    # Calcular la cantidad de 1s y 0s
    cant_1 <- sum(dapply$Predicted == 1)
    cant_0 <- sum(dapply$Predicted == 0)
    
    # Añadir los resultados al dataframe
    resultados <- rbind(resultados, data.table(prof_arbol = profundidad, cantidad_1 = cant_1, cantidad_0 = cant_0))
  }
  
  return(resultados)
}

generar_predicciones(dtrain, dapply, c(2,4,6,8,10,12,14,16,18,20))





