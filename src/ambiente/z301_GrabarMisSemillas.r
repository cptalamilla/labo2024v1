# Este script almacena definitivamente sus cinco semillas
# en el bucket, de forma que NO deba cargarlas en cada script

require( "data.table" )

# reemplazar aqui por SUS semillas 
mis_semillas <- c(100019, 110023, 120041, 130043, 140053)

tabla_semillas <- as.data.table(list( semilla = mis_semillas ))

fwrite( tabla_semillas,
    file = "~/Desktop/01_Austral_MDS/06_Labo1/labo1/datasets/mis_semillas.txt",
    sep = "\t"
)
