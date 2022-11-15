# Paso 1: instalar paqueteria tidyverse con el comando install.packages("tidyverse") para poder hacer uso de la funcion "readr"
# Paso 2: Una vez instalado, carga el paquete haciendo uso de la función library(tidyverse) 

# Paso 3: Importamos los datos
sp1 <- read.csv("SP1.csv")

# Para ver el contenido de la tabla puedes usar los siguintes comandos 
View(sp1) #
head(sp1)  # Muestra las seis primeras filas de la tabla y el nombre de las columnas
str(sp1)   # Muestra la estructura de la tabla
glimpse(sp1)  # nos muestra un resumen de los datos

colnames(sp1) # muestra los nombres de las columnas

"4. Del dataframe que resulta de importar los datos a `R`, extrae las columnas que contienen los números de goles anotados 
por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG);
guárdalos en vectores separados"

FTHG <- sp1[6]
FTAG <- sp1[7]

#  a) ¿Cuántos goles tuvo el partido con mayor empate?

max_goles_empate <- sp1 %>%
  select( FTHG, FTAG) %>% # Seleccionamos las columnas que nos interesan 
  filter(FTHG == FTAG) %>% # Filtramos los partidos donde hubo empate
  arrange(-FTHG) %>%  # arreglamos de forma descendente, tambien se puede emplear arrange(desc(FTHG))
  summarize(total_goles = sum(max(FTHG)+max(FTAG))) # sumamos el total de goles en el partido con mayor empate
max_goles_empate 


#  b) ¿En cuántos partidos ambos equipos empataron 0 a 0?

total_empate_0 <- sp1 %>% 
  filter( FTHG ==  0 & FTAG == 0) %>% 
  count() 
total_empate_0

#  c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?

max_goles <- sp1 %>% 
  filter(FTAG == 0) %>% 
  summarise(max_gol=max(FTHG))
max_goles

