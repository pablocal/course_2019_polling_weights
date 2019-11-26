# Metadata ----------------------------------------------------------------
# Title: Análisis de representatividad de la muestra
# Purpose: Comparar la muestra con totales poblacionales
# Author(s): @pablocal
# Date Created: 2019-11-19
#
# Comments ----------------------------------------------------------------
# R-indicator source code from:
# https://www.cmi.manchester.ac.uk/research/projects/representative-indicators-for-survey-quality/tools/
# 
#
#
# Options and packages ----------------------------------------------------
library(tidyverse) # tidyverse para gestión de datos
library(expss) # crear tablas personalizadas

# 1. Data -----------------------------------------------------------------
encuesta <- read_rds("datos/encuesta_gesop.RDS")
datos_poblacionales <- read_rds("datos/datos_poblacionales.RDS")

# 2. Crear una tabla de resultados de la encuesta --------------------------
# crear una tabla con las variables del análisis y guardar como un data frame
tabla_muestra <- encuesta %>% # añadir datos
  tab_cells(caut, tamuni, sexo, edad, estud, ocupa) %>% # añadir variables
  tab_stat_cpct() %>% # añadir estadístico: porcentajes de columna
  tab_pivot()

colnames(tabla_muestra) <- c("variable_valor", "muestra") # cambiar los nombres de las columnas


# 3. Unir la tabla de la muestra con los resultados poblacionales ---------
tabla_muestra_pobla <- left_join(tabla_muestra, datos_poblacionales, by = "variable_valor") 

tabla_muestra_pobla <- tabla_muestra_pobla %>% # unir tabla con totales poblacionales
  mutate(dif = round(muestra - pobla, 1)) %>% # calcular la diferencia entre la muestra y el porcentaje poblacional
  filter(!is.na(dif)) # eliminar los casos que son perdidos (NA)

# 3. Análisis de representatividad ----------------------------------------
tabla_muestra_pobla %>% # calcular el error medio absoluto para cada variable
  mutate(dif_abs = abs(dif)) %>% 
  group_by(variable) %>%
  summarise(mae = mean(dif_abs)) %>% 
  ggplot(aes(x = reorder(variable, mae), y = mae)) +
  geom_col()

tabla_muestra_pobla %>% 
  filter(variable %in% c("ocupa", "estud", "recuerdo")) %>% 
  select(variable, valor, muestra, pobla, dif)
  


