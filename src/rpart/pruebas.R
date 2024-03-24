dataset[,-(nulos$columnas_con_nulos), with = F] %>% dim()
dataset[,c(nulos$columnas_con_nulos), with = F] %>% dim()
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

nulos = analizar_nulos(dtrain)
nulos$columnas_nulos_df %>% arrange(n_nulos)
nulos$columnas_con_nulos


dataset[,-(nulos$columnas_con_nulos), with = F] %>% dim()
dataset[,nulos$columnas_con_nulos, with = F] %>% dim()


