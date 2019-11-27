# Metadata ----------------------------------------------------------------
# Title: Calibrar la muestra
# Purpose:
# Author(s): 
# Date Created:
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------
# install.packages("tidyverse")
# install.packages("survey")
# install.packages("expss")

library(tidyverse)
library(survey)
library(expss)

# 1. Data -----------------------------------------------------------------
encuesta <- read_rds("datos/encuesta_gesop.RDS")
datos_poblacionales <- read_rds("datos/datos_poblacionales.RDS")

# 2. Calibración sociodemográfica -----------------------------------------

## 1) Preparar totales poblacionales
# Primera variable incluir todas las categorías; sucesivas variables todas menos una
# Calibración sociodemográfica con caut, tamuni, sexo, edad, estud, ocupa
# Total poblacional de españoles mayores de 18 años residentes en España es de 34581472 (INE, 1/2019)








## 2) Preparar los datos de la encuesta
# Elevar el peso al total poblacional







## 3) Diseño de encuesta y calibración
# Establecer diseño de la encuesta
# Proceder con la calibración











## 4) Evaluación de la calibración
# Extraer ponderación y combinar con encuesta
# Comprobar mínimos y máximos del peso
# Comprobar coincidencia con totales poblacionales
# Comprobar efecto en IDV













## 5) Finalizar peso
# Escalar para que tenga media 1



