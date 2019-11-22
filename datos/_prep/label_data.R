# Metadata ----------------------------------------------------------------
# Title: Format data and prepare for exercises
# Purpose:
# Author(s): @pablocal
# Date Created: 2019-11-19
#
# Comments ----------------------------------------------------------------
# 
# 
#   
#
#
# Options and packages ----------------------------------------------------
library(tidyverse)

value_labels <- function(var_name, labels_val, df){

  require(sjlabelled)  
  return_df <- set_labels(df[,var_name], labels = labels_val)
  return(return_df)
  
}


# 1. Data -----------------------------------------------------------------
df <- read_csv2("datos/_prep/EP_EncPreel_Feb19_Microdatos.csv")


# 2. Set labels for values  -----------------------------------------------
label_P1 <- c("Muy importantes" = 1, 
              "Bastantes importantes" = 2, 
              "Poco importantes" = 3, 
              "Nada importantes" = 4, 
              "NS" = 98, 
              "NC" = 99)

label_P2 <- c("Votaría seguro" = 1,
              "Probablemente votaría" = 2,
              "Probablemente no votaría" = 3,
              "Seguro que no votaría" = 4,
              "NS" = 98,
              "NC" = 99)

label_P3 <- c("PP" = 1,
              "PSOE" = 2,
              "C's" = 4,
              "UP" = 5,
              "JxC" = 6,
              "ERC" = 7,
              "ECP" = 8,
              "PNV" = 9,
              "Bildu" = 10,
              "En Marea" = 11,
              "CC" = 12,
              "Compromís" = 13,
              "Vox" = 14,
              "CUP" = 30,
              "Nulo" = 94,
              "Otro partido" = 95,
              "En blanco" = 96,
              "No votaría" = 97,
              "NS" = 98,
              "NC" = 99)

label_P3a <- c("PP" = 1,
              "PSOE" = 2,
              "C's" = 4,
              "UP" = 5,
              "JxC" = 6,
              "ERC" = 7,
              "ECP" = 8,
              "PNV" = 9,
              "Bildu" = 10,
              "En Marea" = 11,
              "CC" = 12,
              "Compromís" = 13,
              "Vox" = 14,
              "CUP" = 30,
              "Otro partido" = 95,
              "Ninguno" = 97,
              "NS" = 98,
              "NC" = 99)

label_P4 <- c("PP" = 1,
              "PSOE" = 2,
              "C's" = 4,
              "UP" = 5,
              "CDC" = 6,
              "ERC" = 7,
              "ECP" = 8,
              "PNV" = 9,
              "Bildu" = 10,
              "En Marea" = 11,
              "CC" = 12,
              "Compromís" = 13,
              "Vox" = 14,
              "CUP" = 30,
              "Nulo" = 94,
              "Otro partido" = 95,
              "En blanco" = 96,
              "No votó" = 97,
              "No recuerda" = 98,
              "NC" = 99)

label_sino <- c("Sí" = 1,
                "No" = 2,
                "NS" = 98,
                "NC" = 99)

label_P6 <- c("Pablo Casado" = 1,
              "Pedro Sánchez" = 2,
              "Albert Rivera" = 4, 
              "Pablo Iglesias" = 5,
              "Santiago Abascal" = 14,
              "Otro" = 95,
              "Indiferente" = 96,
              "Ninguno" = 97,
              "NS" = 98,
              "NC" = 99)

label_P7 <- c("Mediante el diálogo con la fuerzas independentistas" = 1,
              "Mediante la aplicación del artículo 155 de la CE" = 2,
              "NS" = 98,
              "NC" = 99
              )

label_P8 <- c("El Gobierno español" = 1,
              "La Generaitat" = 2,
              "Los dos por igual" = 3,
              "NS" = 98,
              "NC" = 99)

label_D1 <- c("Izquierda" = 1,
              "Centro izquierda" = 2,
              "Centro" = 3,
              "Centro derecha" = 4,
              "Derecha" = 5,
              "NS" = 98,
              "NC" = 99)

label_D2 <- c("Únicamente español" = 1,
              "Más español que de su comunidad" = 2,
              "Tan español como de su comunidad" = 3,
              "Más de su comunidad que español" = 4,
              "Únicamente de su comunidad" = 5,
              "NS" = 98,
              "NC" = 99)

label_D3 <- c("Hombre" = 1,
              "Mujer" = 2)

label_D4 <- c("18-29" = 1,
              "30-44" = 2,
              "45-59" = 3,
              ">60" = 4)

label_D5 <- c("Sin estudios obligatorios acabados" = 1,
              "Obligatorios" = 2,
              "Posobligatorios" = 3,
              "Posobligatorios profesionales" = 4,
              "Universitarios" = 5,
              "NC" = 99)

label_D6 <- c("Trabajador" = 1,
              "Parado" = 5,
              "Jubilado" = 6,
              "Tareas del hogar" = 7,
              "Estudiante" = 8,
              "NC" = 99)

label_AUT <- c("Andalucía" = 1,
               "Aragón" = 2,
               "Asturias" = 3,
               "Balears" = 4,
               "Canarias" = 5,
               "Cantabria" = 6,
               "Castilla y León" = 7,
               "Castilla - la Mancha" = 8,
               "Catalunya" = 9,
               "Comunitat Valenciana" = 10,
               "Extremadura" = 11,
               "Galicia" = 12,
               "Madrid" = 13,
               "Murcia" = 14,
               "Navarra" = 15,
               "País Vasco" = 16,
               "La Rioja" = 17,
               "Ceuta" = 18,
               "Melilla" = 19)

label_HABI <- c("< 10.000 hab." = 1,
                "10.001 a 100.000 hab." = 2,
                "100.001 a 500.000 hab." = 3,
                "Más de 500.000 hab." = 4)



vars_to_lab <- c("P1", "P2", "P3", "P3A", "P4", 
                 "P5A_1", "P5A_2", "P5A_3", "P5A_4", "P5A_5",
                 "P6", "P7", "P8", "P9", "P10", "D1", "D2",
                 "D3", "D4", "D5", "D6", "AUT", "HABI")

val_labs <- list(label_P1, label_P2, label_P3, label_P3a,
                 label_P4, label_sino, label_sino, label_sino, 
                 label_sino, label_sino, label_P6, label_P7,
                 label_P8, label_sino, label_sino, label_D1,
                 label_D2, label_D3, label_D4, label_D5,
                 label_D6, label_AUT, label_HABI)


lab_df <- map2_dfc(.x = vars_to_lab, .y = val_labs, 
                 ~value_labels(var_name = .x, labels_val = .y, df = df))


# 3. Final file rename ----------------------------------------------------

df <- df %>% 
  select(-one_of(vars_to_lab)) %>% 
  bind_cols(lab_df) %>% 
  select(-P0, -CA) %>% 
  rename(id = REGISTRO,
         import_elec = P1,
         urnas = P2,
         idv = P3,
         simpa = P3A,
         recuerdo = P4,
         conoce_pc = P5A_1,
         valora_pc = P5B_1,
         conoce_ar = P5A_2,
         valora_ar = P5B_2,
         conoce_ps = P5A_3,
         valora_ps = P5B_3,
         conoce_pi = P5A_4,
         valora_pi = P5B_4,
         conoce_sa = P5A_5,
         valora_sa = P5B_5,
         pref_pres = P6,
         conf_cat = P7,
         responsab_cat = P8,
         cesion_gob_cat = P9,
         juicio_indep_cat = P10,
         ideo = D1,
         nacional = D2,
         sexo = D3,
         edad = D4,
         estud = D5,
         ocupa = D6,
         caut = AUT,
         tamuni = HABI
         ) %>% 
  select(id, caut, tamuni, import_elec, urnas, idv, simpa, recuerdo,
       conoce_pc, valora_pc, conoce_ar, valora_ar, conoce_ps, valora_ps, 
       conoce_pi, valora_pi, conoce_sa, valora_sa, pref_pres, conf_cat,
       responsab_cat, cesion_gob_cat, juicio_indep_cat, ideo, nacional,
       sexo, edad, estud, ocupa)


# 4. Variable labels ------------------------------------------------------

df <- sjlabelled::set_label(df, c("Identificación (REGISTRO)",
                      "Comunidad autónoma de residencia (AUT)",
                      "Tamaño del municipio (HABI)",
                      "Importancia de la convocatoria electoral (P1)",
                      "Intención de votar en las elecciones (P2)",
                      "Intención directa de voto 28-A (P3)",
                      "Partido que produce más simpatía si P3 = 94,96,97,98,99 (P3A)",
                      "Recuerdo de voto 2016 (P4)",
                      "Conoce a Pablo Casado (P5A_1)",
                      "Valoración de Pablo Casado (P5B_1)",
                      "Conoce a Albert Rivera (P5A_2)",
                      "Valoración de Albert Rivera (P5B_2)",
                      "Conoce a Pedro Sánchez (P5A_3)",
                      "Valoración de Pedro Sánchez (P5B_3)",
                      "Conoce a Pablo Igelias (P5A_4)",
                      "Valoración de Pablo Iglesias (P5B_4)",
                      "Conoce a Santiago Abascal (P5A_5)",
                      "Valoración de Santiago Abascal (P5B_5)",
                      "Candidato preferido para ser presidente (P6)",
                      "Mejor solución para el conflicto en Cataluña (P7)",
                      "Responsabilidad ruptura del diálogo (P8)",
                      "¿El Gobierno ha cedido ante las exigencias independentistas? (P9)",
                      "¿Los líderes del procés tendrán un juicio justo? (P10)",
                      "Escala ideológica (D1)",
                      "Escala sentimiento nacional (D2)",
                      "Sexo (D3)",
                      "Edad (D4)",
                      "Estudios acabados (D5)",
                      "Ocupación (D6)"
                      )
                      )

# 5. Convert to factors and save ------------------------------------------

df <- as_label(df)

df <- df %>% 
  mutate(ocupa = dplyr::recode(ocupa,
                        "Trabajador" = "Trabajador",
                        "Parado" = "Parado",
                        .default = "Inactivo"),
         estud = dplyr::recode(estud,
                               "NC" = "Obligatorios"),
         recuerdo = dplyr::recode(recuerdo,
                                  "CUP" = "Otro partido",
                                  "ECP" = "UP",
                                  "En Marea" = "UP",
                                  "Compromís" = "UP")
         )

sjmisc::frq(df$ideo)
sjmisc::frq(df$simpa)
sjmisc::frq(df$recuerdo)

write_rds(df, "datos/encuesta_gesop.RDS")
