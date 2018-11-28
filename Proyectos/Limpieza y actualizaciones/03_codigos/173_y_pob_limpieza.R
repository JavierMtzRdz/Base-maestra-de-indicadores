#install.packages("openxlsx")
library(pacman)
p_load(janitor, readxl, readr, foreign, tidyverse, stringr)

pob_proyec <- read_csv("Proyectos/Limpieza y actualizaciones/01_datos_brutos/proyec_pob_conapo_2010.csv", locale = locale(encoding = 'ISO-8859-1'))
pob_proyecto <- glimpse(pob_proyec) %>% 
  transmute(year = AÑO,
            ent = ENTIDAD,
            pob = POB_MIT_AÑO,
            edad_med = EDAD_MED) %>% 
  mutate(ent = recode(ent, "República Mexicana" = "Nacional", 
                      "Michoacán de Ocampo" = "Michoacán",
                      "Coahuila de Zaragoza"= "Coahuila",
                      "Veracruz de Ignacio de la Llave" = "Veracruz")) %>%  

  group_by(ent) %>% 
  mutate(pob_lag = lag(pob)) %>% 
  ungroup() %>% 
  mutate(cres = pob -pob_lag,
         t_cres = ((cres/pob_lag))*100)
openxlsx::write.xlsx(pob_proyecto, "Proyectos/Limpieza y actualizaciones/02_datos_generados/Pob_proyec.xlsx", sheetName = "Sheet1", 
                     col.names = TRUE, row.names = F, append = FALSE)


  

h_ocurrencia <- read_excel("Proyectos/Limpieza y actualizaciones/01_datos_brutos/173_entidad de ocurrencia.xlsx") %>% 
  rename(Nacional = Total) %>% 
  gather(key = "ent", 
         value = "hom_ocu",
         -year) %>% 
  mutate(ent = recode(ent, "Michoacán de Ocampo" = "Michoacán",
                      "Coahuila de Zaragoza"= "Coahuila",
                      "Veracruz de Ignacio de la Llave" = "Veracruz")) %>% 
  filter(ent != "Extranjero" &
           ent != "No especificado")
h_registro <- read_excel("Proyectos/Limpieza y actualizaciones/01_datos_brutos/173_entidad de registro.xlsx") %>% 
  rename(Nacional = Total) %>% 
  gather(key = "ent", 
         value = "hom_reg",
         -year) %>% 
  mutate(ent = recode(ent, "Michoacán de Ocampo" = "Michoacán",
                      "Coahuila de Zaragoza"= "Coahuila",
                      "Veracruz de Ignacio de la Llave" = "Veracruz")) %>% 
  filter(ent != "Extranjero" &
           ent != "No especificado")

todo <- pob_proyecto %>% 
  right_join(h_ocurrencia, by = c("year", "ent")) %>% 
  left_join(h_registro, by = c("year", "ent")) %>% 
  mutate(pob = as.numeric(pob),
         hom_ocu = as.numeric(hom_ocu),
         hom_reg = as.numeric(hom_reg),
         t_ocu = (hom_ocu*100000)/pob,
         t_reg = (hom_reg*100000)/pob,
         cve_ent = recode(ent,
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
         no = 173, 
         indicador = "Tasa de homicidios por cada 100 mil habitantes",
         ods = 16,
         obj_ods = "Paz, justicia e instituciones sólidas", 
         u_med = "Tasa por cada cien mil habitantes") %>% 
  arrange(-year, ent)

datos <- todo %>% 
  transmute(year = year,
           ent = ent,
           cve_ent = cve_ent,
           valor = t_ocu, 
           no = no,
           indicador = indicador,
           ods = ods,
           obj_ods = obj_ods,
           u_med = u_med)

#write.dta(datos, "Proyectos/Limpieza y actualizaciones/02_datos_generados/173.dta")
openxlsx::write.xlsx(datos, "Proyectos/Limpieza y actualizaciones/02_datos_generados/173.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = F, append = FALSE)

