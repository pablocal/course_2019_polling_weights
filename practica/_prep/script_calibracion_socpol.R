# Metadata ----------------------------------------------------------------
# Title: Calibrar la muestra y estimar voto
# Purpose:
# Author(s): @pablocal
# Date Created: 2019-11-21
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
# Total poblacional de españoles mayores de 18 años residentes en España es de 34581472

vars_socdem <- c("caut", "tamuni", "sexo", "edad", "estud", "ocupa")

pobla_socdem <- datos_poblacionales %>% 
  mutate(total_pobla = round(pobla/100*34581472, 0)) %>% 
  filter(variable %in% vars_socdem) %>%
  filter(valor_orden != 1)

totales_pobla <- pobla_socdem$total_pobla
names(totales_pobla) <- paste0(pobla_socdem$variable, pobla_socdem$valor)
totales_pobla <- c("(Intercept)" =  34581472, totales_pobla)
totales_pobla

## 2) Preparar los datos de la encuesta
# Elevar el peso al total poblacional

encuesta <- encuesta %>% 
  mutate(peso_pobla = 34581472/nrow(.))

## 3) Diseño de encuesta y calibración
# Establecer diseño de la encuesta
# Proceder con la calibración

svy_des <- svydesign(id = ~ 0, weights = ~ peso_pobla, data = encuesta)

calib_socdem <- calibrate(design = svy_des, 
                          formula = ~ caut + tamuni + sexo + edad + estud + ocupa, 
                          population = totales_pobla, 
                          calfun = "logit", 
                          bounds = c(0, 999999))

## 4) Evaluación de la calibración
# Extraer ponderación y combinar con encuesta
# Comprobar mínimos y máximos del peso
# Comprobar coincidencia con totales poblacionales
# Comprobar efecto en IDV

peso_calib_socdem <- weights(calib_socdem)
encuesta$peso_calib_socdem <- peso_calib_socdem

sjmisc::descr(encuesta$peso_calib_socdem)
hist(encuesta$peso_calib_socdem)

tabla <- encuesta %>%
  tab_weight(peso_calib_socdem) %>% 
  tab_cells(caut, tamuni, sexo, edad, estud, ocupa) %>%
  tab_stat_cpct(total_statistic = "w_cpct") %>%
  tab_pivot()

colnames(tabla) <- c("variable_valor", "calibrado")

tabla_checks <- left_join(tabla, datos_poblacionales, by = "variable_valor") %>% 
  mutate(dif = round(calibrado - pobla, 1)) 

encuesta_idv <- encuesta %>% 
  filter(!(idv %in% c("NS", "NC", "No votaría", "Nulo", "En blanco")))

sjmisc::frq(encuesta_idv$idv) 
sjmisc::frq(encuesta_idv$idv, weights =  encuesta_idv$peso_calib_socdem) 


## 5) Finalizar peso
# Escalar para que tenga media 1

encuesta <- encuesta %>% 
  mutate(peso_calib_socdem = peso_calib_socdem/mean(peso_calib_socdem))
sjmisc::descr(encuesta$peso_calib_socdem)


# 2. Calibración sociodemográfica y política ------------------------------

## 1) Preparar totales poblacionales
# Primera variable incluir todas las categorías; sucesivas variables todas menos una
# Calibración sociodemográfica con caut, tamuni, sexo, edad, estud, ocupa
# Total poblacional de españoles mayores de 18 años residentes en España es de 34581472

pobla_politica <- datos_poblacionales %>% 
  mutate(total_pobla = round(pobla/100*34581472, 0)) %>% 
  filter(variable == "recuerdo") %>%
  filter(valor_orden != 1)

totales_polit <- pobla_politica$total_pobla
names(totales_polit) <- paste0(pobla_politica$variable, pobla_politica$valor)

totales_pobla <- c(totales_pobla, totales_polit)

## 3) Diseño de encuesta y calibración
# Establecer diseño de la encuesta
# Proceder con la calibración

calib_socpol <- calibrate(design = svy_des, 
                          formula = ~ caut + tamuni + sexo + edad + estud + ocupa + recuerdo, 
                          population = totales_pobla, 
                          calfun = "logit", 
                          bounds = c(0, 999999))

## 4) Evaluación de la calibración
# Extraer ponderación y combinar con encuesta
# Comprobar mínimos y máximos del peso
# Comprobar coincidencia con totales poblacionales
# Comprobar efecto en IDV

peso_calib_socpol <- weights(calib_socpol)
encuesta$peso_calib_socpol <- peso_calib_socpol

sjmisc::descr(encuesta$peso_calib_socpol)

tabla <- encuesta %>%
  tab_weight(peso_calib_socpol) %>% 
  tab_cells(caut, tamuni, sexo, edad, estud, ocupa, recuerdo) %>%
  tab_stat_cpct(total_statistic = "w_cpct") %>%
  tab_pivot()

colnames(tabla) <- c("variable_valor", "calibrado")

tabla_checks <- left_join(tabla, datos_poblacionales, by = "variable_valor") %>% 
  mutate(dif = round(calibrado - pobla, 1)) 

encuesta_idv <- encuesta %>% 
  filter(!(idv %in% c("NS", "NC", "No votaría", "Nulo", "En blanco")))

sjmisc::frq(encuesta_idv$idv)
sjmisc::frq(encuesta_idv$idv, weights =  encuesta_idv$peso_calib_socdem)
sjmisc::frq(encuesta_idv$idv, weights =  encuesta_idv$peso_calib_socpol)

## 5) Finalizar peso
# Escalar para que tenga media 1
encuesta <- encuesta %>% 
  mutate(peso_calib_socpol = peso_calib_socpol/mean(peso_calib_socpol))
sjmisc::descr(encuesta$peso_calib_socpol)
