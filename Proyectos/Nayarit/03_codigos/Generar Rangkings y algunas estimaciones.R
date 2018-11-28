# en este archivo genero rankings y datos para la región centro

library(readxl)
library(tidyverse)
library(haven)

#Generar rankings

ind <- read_excel("Indicadores/02_datos_generados/indicadores_general.xlsx")
ind %>% 
  count(ent) %>% 
#ind_durango <- read_dta("Datos_insumo/ind_durango.dta") 
ind <- ind %>% 
  mutate(ent = fct_recode(ent,
                          "Ciudad de México" = "Ciudad De Mexico",
                          "México" = "Mexico",
                          "Michoacán" = "Michoacan",
                          "Nuevo León" = "Nuevo Leon",
                          "Querétaro" = "Queretaro", 
                          "San Luis Potosí" = "San Luis Potosi",
                          "Yucatán" = "Yucatan"))


nacional <- ind_durango %>% 
  filter(ent == "Nacional") %>% 
  select(no, year, indicador, valor, ent, u_med, sentido) %>% 
  mutate(dif_ab = NA,
         ranking = NA)

ind_nayarit <- ind_durango %>% 
  filter(!(is.na(valor)) & 
           ent != "Nacional") %>% 
  mutate(porcent_text = str_detect(u_med, "orcentaje"),
         dif_ab = ifelse(sentido == 1, abs(valor -1), 
                         ifelse(sentido == 3 & !(porcent_text), abs(valor - 0.5), 
                                ifelse(sentido == 3 & porcent_text, abs(valor - 50), 
                                       NA)))) %>% 
  select(no, year, indicador, valor, ent, u_med, sentido, dif_ab) %>%  
  arrange(no, year) %>% 
  group_by(no, year) %>% 
  mutate(ranking = ifelse(sentido == "2" | sentido == "NA",
                          rank(-valor),
                          ifelse(sentido == "0", 
                                 rank(valor), 
                                 ifelse(sentido == "1" | sentido == "3",
                                        rank(dif_ab), 
                                        NA)))) %>% 
  ungroup()

ind_nayarit <- rbind(ind_nayarit, nacional)

openxlsx::write.xlsx(ind_nayarit, "Datos_generados/ind_nayarit.xlsx", sheetName = "Sheet1",
                     col.names = TRUE, row.names = F, append = FALSE)

#Generar acumulados para Región Centro


pob_proyec <- read_excel("Datos_insumo/Pob_proyec.xlsx")
i176 <- read_excel("Datos_insumo/176.xlsx") %>% 
  mutate(cve_ent = as.character(cve_ent))


region_centro <- ind_durango %>% filter(no != 176) %>% 
  bind_rows(i176) %>% 
  select(no, year, indicador, valor, ent, u_med, sentido) %>% 
  filter(ent == "Aguascalientes"|
           ent == "Guanajuato"|
           ent == "Querétaro"|
           ent == "San Luis Potosí") %>% 
  arrange(no, year, ent) %>% 
  filter(no == 173 |
           no == 176 |
           no == 177) %>% 
  left_join(pob_proyec, by = c("year", "ent")) %>% 
  mutate(valor = (valor*pob)/100000) %>% 
  group_by(no, year, indicador) %>% 
    summarise(valor = sum(valor),
              pob = sum(pob)) %>% 
  mutate(ent = "Región Centro",
         valor = (valor*100000)/pob)
  
rc_demas <- ind_durango %>% 
  filter(no != 176) %>% 
  bind_rows(i176) %>% 
  filter(no == 173 |
           no == 176 |
           no == 177) 
rc <- bind_rows(region_centro, rc_demas) %>% 
  select(no, year, indicador, valor, ent) %>% 
  arrange(no, ent)

  
openxlsx::write.xlsx(rc, "Datos_generados/region_centro_173_176_177.xlsx", sheetName = "Sheet1",
                     col.names = TRUE, row.names = F, append = FALSE)
  
