# en este script genero la tasa de crecimiento promedio anual (2003-2016) del PIB por entidad federativa a precios constantes de 2013.

### Cargar paquetes ----
library(pacman)
p_load(tidyverse, readxl)

### Definir lenguaje del locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 

### Cargar base de datos y reordenar ----
PIBE_base2013 <- read_excel("Proyectos/Limpieza y actualizaciones/01_datos_brutos/PIBE_2003-2016_base2013.xlsx") 

### Limpieza ----
PIBE <- PIBE_base2013 %>% 
  gather(key = "year", value = "pib", -ent) %>% 
  arrange(ent, year) %>% 
  group_by(ent) %>% 
  mutate(pib_lag = dplyr::lag(pib)) %>% 
  ungroup() %>% 
  mutate(cres = pib-pib_lag,
         t_cres = (cres/pib_lag)*100)
### Guardar la base de datos ----
openxlsx::write.xlsx(PIBE, "Proyectos/Limpieza y actualizaciones/02_datos_generados/PIBE_base2013.xlsx", sheetName = "Sheet1", 
                     col.names = TRUE, row.names = F, append = FALSE)

### Ahora genero una base de datos con el pib por entidad ----

### Cargar base de datos y reordenar ----
PIBE_base2013_act <- read_excel("Proyectos/Limpieza y actualizaciones/01_datos_brutos/PIB_por_actividad_2003-2016_base2013.xlsx") 

### Limpieza ----
PIBE <- PIBE_base2013_act %>% 
  gather(key = "year", value = "pib", -ent, -actividad) %>% 
  arrange(ent, actividad, year) %>% 
  group_by(ent, actividad) %>% 
  mutate(pib_lag = dplyr::lag(pib)) %>% 
  ungroup() %>% 
  mutate(cres = pib-pib_lag,
         t_cres = (cres/pib_lag)*100)
### Guardar la base de datos ----
openxlsx::write.xlsx(PIBE, "Proyectos/Limpieza y actualizaciones/02_datos_generados/PIBE_base2013.xlsx", sheetName = "Sheet1", 
                     col.names = TRUE, row.names = F, append = FALSE)
