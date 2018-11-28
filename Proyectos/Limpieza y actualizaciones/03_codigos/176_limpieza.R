library(pacman)
p_load(janitor, readxl, readr, foreign, tidyverse, stringr)


per_inseg <- read_excel("Proyectos/Limpieza y actualizaciones/01_datos_brutos/176_n.xlsx") %>% 
  filter(Indicador == "Total") %>% 
  select(-Indicador) %>% 
  gather(key = "year", value = "valor", -ent) %>% 
  mutate(cve_ent = recode(ent,
                          "Aguascalientes"      = 01,
                          "Baja California"     = 02,
                          "Baja California Sur" = 03,
                          "Campeche"            = 04,
                          "Coahuila"            = 05,
                          "Colima"              = 06,
                          "Chiapas"             = 07,
                          "Chihuahua"           = 08,
                          "Ciudad de México"    = 09,
                          "Durango"             = 10,
                          "Guanajuato"          = 11,
                          "Guerrero"            = 12,
                          "Hidalgo"             = 13,
                          "Jalisco"             = 14,
                          "México"              = 15, #Componer acento
                          "Michoacán"           = 16, #Componer acento
                          "Morelos"             = 17,
                          "Nayarit"             = 18,
                          "Nuevo León"          = 19, #Componer acento
                          "Oaxaca"              = 20,
                          "Puebla"              = 21,
                          "Querétaro"           = 22, #Componer acento
                          "Quintana Roo"        = 23,
                          "San Luis Potosí"     = 24, #Componer acento
                          "Sinaloa"             = 25,
                          "Sonora"              = 26,
                          "Tabasco"             = 27,
                          "Tamaulipas"          = 28,       
                          "Tlaxcala"            = 29,           
                          "Veracruz"            = 30,
                          "Yucatán"             = 31, #componer acento
                          "Zacatecas"           = 32,  
                          "Nacional"            = 00),
         cve_ent = as.numeric(cve_ent),
         no = 176, 
         indicador = "Tasa de personas de 18 años y más que considera insegura su entidad federativa por cada 100 mil habitantes",
         ods = 16,
         obj_ods = "Paz, justicia e instituciones sólidas", 
         u_med = "Tasa por cada cien mil habitantes",
         year = as.numeric(year),
         valor = as.numeric(valor))

pob <- read_excel("Proyectos/Limpieza y actualizaciones/02_datos_generados/Pob_proyec.xlsx") %>% 
  select(year, pob, ent)

#write_dta(per_inseg, "Proyectos/Limpieza y actualizaciones/02_datos_generados/176.dta")
openxlsx::write.xlsx(per_inseg, "Proyectos/Limpieza y actualizaciones/02_datos_generados/176.xlsx", sheetName = "Sheet1", 
                     col.names = TRUE, row.names = F, append = FALSE)

per_inseg <- per_inseg %>% 
  left_join(pob, by = c("year", "ent")) %>% 
  mutate(valor = (valor*100000)/pob) %>% 
  select(-pob)

