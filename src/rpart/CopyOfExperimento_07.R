library(ggplot2)
dataset %>% 
  filter(foto_mes==202107) %>% 
  select(clase_ternaria, cliente_antiguedad) %>% 
  ggplot(aes(x=clase_ternaria, y=cliente_antiguedad))+
  geom_boxplot()
  
dataset %>% 
  filter(foto_mes==202107) %>% 
  select(clase_ternaria, cliente_edad) %>% 
  ggplot(aes(x=clase_ternaria, y=cliente_edad))+
  geom_boxplot()

dataset %>% 
  filter(foto_mes==202107) %>% 
  select(clase_ternaria, mrentabilidad) %>% 
  ggplot(aes(x=clase_ternaria, y=mrentabilidad))+
  geom_boxplot()

dataset %>% 
  filter(foto_mes==202107) %>% 
  select(clase_ternaria, mpasivos_margen) %>% 
  ggplot(aes(x=clase_ternaria, y=mpasivos_margen))+
  geom_boxplot()+
  scale_y_log10()

dataset %>% 
  filter(foto_mes==202107) %>% 
  select(clase_ternaria, cproductos) %>% 
  ggplot(aes(x=clase_ternaria, y=cproductos))+
  geom_boxplot()

dataset %>% 
  filter(foto_mes==202107) %>% 
  select(clase_ternaria, mcaja_ahorro) %>% 
  ggplot(aes(x=clase_ternaria, y=mcaja_ahorro))+
  geom_boxplot()+
  scale_y_log10()

dataset %>% summary()
