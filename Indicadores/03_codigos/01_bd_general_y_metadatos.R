### Creación base de dato general y base de datos completa.
### En este script se toman los datos individuales de los indicadores para generar una base de datos con todos los indicadores y una base de datos de

### Cargar paquetes ----
library(pacman)
p_load(tidyverse, readxl)

### Definir lenguaje del locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 

### Extraer y combinar bases individuales de los indicadores ----
## Cargar la lista de archivos de los indicadores
file.list <- list.files(path = "Indicadores/01_datos_brutos/01_01_indicadores/", pattern='*.xlsx')
file.list


## Crear función para extraer excels con títulos
extract <- function(file){
  read_excel(file, col_names = T)
}

## Extraer lista de excels
df.list <- lapply(paste0("Indicadores/01_datos_brutos/01_01_indicadores/", file.list),
                  extract)


## Función para convertir la lista de data frames a objetos
sacar_base <- function(lista){
  indicador <- as.data.frame(c(df.list[[lista]]))
}

## Crear una base de datos con los data frames individuales
can <- 1:length(df.list)


datos <- data_frame()
 
for (num in can) {
  datos <- bind_rows(datos, sacar_base(num))
}

datos <- datos %>% 
  arrange(no, year, cve_ent)
## Guardar base de datos 
openxlsx::write.xlsx(datos, "Indicadores/02_datos_generados/indicadores_libre.xlsx")

### generar base de datos con los indicadores y datos generales ----

## Cargar los datos por indicador
# Toda modificación de las características de los datos debe ir en este archivo
datos_indicadores <- read_excel("Indicadores/01_datos_brutos/datos_indicadores.xlsx")

## Combinar a base de datos libre con los datos de indicadores

bd_general <- datos %>% 
  left_join(datos_indicadores, by = "no") %>%  #Combinar bases de datos
  select(year, ent, cve_ent, no, indicador, valor, u_med, ods, obj_ods) %>%  #Seleccionar variables a usar 
  arrange(no, year, cve_ent)

## Guardar datos de base de datos general
openxlsx::write.xlsx(bd_general, "Indicadores/02_datos_generados/indicadores_general.xlsx")

### Metadatos ----

## Revisar los años cubiertos por la base de dato por indicador
periodo_ac <- datos %>% 
  group_by(no) %>% 
  summarize(inicio_year = min(year),
            fin_year = max(year)) %>% 
  mutate(periodo = paste(as.character(inicio_year), 
                         as.character(fin_year), 
                         sep = "-")) %>% 
  select(no, periodo)

## Revisar si hay datos nacionales
nac_dat <- datos %>%
  select(no, ent, cve_ent) %>% 
  mutate(nac = ifelse(cve_ent == "00", 1, 0)) %>% 
  group_by(no) %>% 
  summarise(num_nac = sum(nac)) %>% 
  mutate(cobertura_geografica = ifelse(num_nac > 0, "Nacional y estatal.", "Estatal.")) %>% 
  select(-num_nac)

## Combinar y seleccionar datos de los indicadores con el periodo

metadatos <- datos_indicadores %>% 
  select(-sentido, -ods, -obj_ods, -cobertura_geografica) %>% 
  left_join(periodo_ac, by = "no") %>% 
  left_join(nac_dat, by = "no")

## Guardar fichas de metadatos
openxlsx::write.xlsx(metadatos, "Indicadores/02_datos_generados/metadatos.xlsx")







