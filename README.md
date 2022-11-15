# Postwork
Postwork_equipo_19

## Postwork sesión 1
1. Instalar paqueteria tidyverse con el comando `install.packages("tidyverse")` para poder hacer uso de varias funciones que se emplean más abajo (`select, filter...`).
2. Una vez instalado, carga el paquete haciendo uso de la función `library(tidyverse)`. 

3. Importamos los datos
 ```
sp1 <- read.csv("SP1.csv")
```
4. Para ver el contenido de la tabla puedes usar los siguientes comandos 
```
View(sp1) 
head(sp1)  
str(sp1)   
glimpse(sp1)  
colnames(sp1) 
```
"5. Del dataframe que resulta de importar los datos a `R`, extrae las columnas que contienen los números de goles anotados 
por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG);
guárdalos en vectores separados"

```
FTHG <- sp1[6]
FTAG <- sp1[7]
```

a) ¿Cuántos goles tuvo el partido con mayor empate?
```
max_goles_empate <- sp1 %>%
  select( FTHG, FTAG) %>%  
  filter(FTHG == FTAG) %>% 
  arrange(-FTHG) %>%  
  summarize(total_goles = sum(max(FTHG)+max(FTAG))) 
max_goles_empate 
```

 b) ¿En cuántos partidos ambos equipos empataron 0 a 0?
```
total_empate_0 <- sp1 %>% 
  filter( FTHG ==  0 & FTAG == 0) %>% 
  count() 
total_empate_0
```

c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?
```
max_goles <- sp1 %>% 
  filter(FTAG == 0) %>% 
  summarise(max_gol=max(FTHG))
max_goles
```

