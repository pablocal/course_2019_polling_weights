# Metadata ----------------------------------------------------------------
# Title:
# Purpose:
# Author(s):
# Date Created: 2019-11-20
#
# Comments ----------------------------------------------------------------
# 
# 
# 
#
#
# Options and packages ----------------------------------------------------
library(tidyverse)

# 1. tamuni -----------------------------------------------------------------
mun <- readxl::read_xls("datos/_prep/datos poblacionales/muni_esp_edad.xls")

mun %>% 
  mutate(tamuni = case_when(
    pop_total < 10001 ~ "Menos de 10k",
    dplyr::between(pop_total, 10001, 100000) ~ "10-100",
    dplyr::between(pop_total, 100001, 500000) ~ "100-500",
    pop_total > 500000 ~ "+500"),
    esp_18mas = esp_total - esp_menos16 - esp_menos16/16*2) %>%
  group_by(factor(1)) %>% 
  mutate(total_esp18 = sum(esp_18mas, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(tamuni) %>% 
  summarise(esp_18mas = sum(esp_18mas),
            total_esp18 = first(total_esp18)) %>% 
  mutate(per = esp_18mas/total_esp18*100)


# 2. pop data -------------------------------------------------------------
pop <- openxlsx::read.xlsx("datos/_prep/pop_estim.xlsx") 

pop %>% 
  mutate(pobla = round(pobla, 1)) %>% 
  write_rds("datos/datos_poblacionales.RDS")

pop <- read_rds("datos/datos_poblacionales.RDS")
pop <- filter(pop, variable != "recuerdo")
write_rds(pop, "datos/datos_poblacionales.RDS")

# 3. idv eval -------------------------------------------------------------
idv_eval <- openxlsx::read.xlsx("datos/_prep/idv_eval.xlsx") 

write_rds(idv_eval, "datos/idv_eval.RDS")

