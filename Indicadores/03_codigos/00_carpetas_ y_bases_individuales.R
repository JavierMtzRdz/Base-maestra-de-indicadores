### Creación de las carpetas y bases individuales de proyectos. 
### En este script se toma la base de datos de indicadores de Durango para organizar en un mismo proyecto todos los indicadores, cambios, actualizaciones, metadatos y bases de dato de proyectos regionales. 

### Cargar paquetes----
library(pacman)
p_load(tidyverse, readxl, stringr)

### Definir lenguaje del locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 


### Creación de carpetas del proyecto ----
folder_names <- c("Indicadores", 
                  "Proyectos", 
                  "Indicadores/01_datos_brutos", 
                  "Indicadores/01_datos_brutos/01_01_indicadores", 
                  "Indicadores/02_datos_generados",
                  "Indicadores/03_codigos",
                  "Indicadores/04_documentos",
                  "Proyectos/Durango",
                  "Proyectos/Nayarit", 
                  "Proyectos/Región Centro")

sapply(folder_names, dir.create)


# Cargar y componer base de datos de Durango ----

ind_durango <- read_excel("Indicadores/01_datos_brutos/ind_durango.xlsx") %>% 
  mutate(ent = fct_recode(ent,
                          "Ciudad de México" = "Ciudad De Mexico",
                          "México" = "Mexico",
                          "Michoacán" = "Michoacan",
                          "Nuevo León" = "Nuevo Leon",
                          "Querétaro" = "Queretaro", 
                          "San Luis Potosí" = "San Luis Potosi",
                          "Yucatán" = "Yucatan"),
         cve_ent = fct_recode(ent,
                          "01" = "Aguascalientes"      ,
                          "02" = "Baja California"     ,
                          "03" = "Baja California Sur" ,
                          "04" = "Campeche"            ,
                          "05" = "Coahuila"            ,
                          "06" = "Colima"              ,
                          "07" = "Chiapas"             ,
                          "08" = "Chihuahua"           ,
                          "09" = "Ciudad de México"    ,
                          "10" = "Durango"             ,
                          "11" = "Guanajuato"          ,
                          "12" = "Guerrero"            ,
                          "13" = "Hidalgo"             ,
                          "14" = "Jalisco"             ,
                          "15" = "México"              , 
                          "16" = "Michoacán"           , 
                          "17" = "Morelos"             ,
                          "18" = "Nayarit"             ,
                          "19" = "Nuevo León"          , 
                          "20" = "Oaxaca"              ,
                          "21" = "Puebla"              ,
                          "22" = "Querétaro"           ,
                          "23" = "Quintana Roo"        ,
                          "24" = "San Luis Potosí"     , 
                          "25" = "Sinaloa"             ,
                          "26" = "Sonora"              ,
                          "27" = "Tabasco"             ,
                          "28" = "Tamaulipas"          ,       
                          "29" = "Tlaxcala"            ,           
                          "30" = "Veracruz"            ,
                          "31" = "Yucatán"             , 
                          "32" = "Zacatecas"           ,  
                          "00" = "Nacional"            )) 

levels(factor(ind_durango$ent))


### Generar y guardar bases de datos individuales por indicador ----
no <- as.numeric(levels(as.factor(ind_durango$no)))

for (indicador in no) {
  ind_individual <- ind_durango %>% 
    select(year, ent, cve_ent, no, valor) 
  ind_individual <- ind_individual[ind_individual$no == indicador, ]
  nom <- paste0("Indicadores/01_datos_brutos/01_01_indicadores/", as.character(indicador), ".xlsx")
  openxlsx::write.xlsx(as.data.frame(ind_individual), nom)
 
}


 



