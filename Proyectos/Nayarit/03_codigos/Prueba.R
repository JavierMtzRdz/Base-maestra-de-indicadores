#Aquí grafico el indicador 176 de Región Centro. 
library(pacman)
p_load(readxl, tidyverse, magrittr)
rc <- read_excel("Datos_generados/region_centro_173_176_177.xlsx") 
ind_durango <- read_excel("Datos_insumo/ind_durango.xlsx")

ind_durango %>% 
  filter(no == 176,
         year == 2018) %>%
  ggplot(aes(x = reorder(ent, valor), y = valor)) +
  geom_col() + 
  coord_flip()

rc %>% 
  filter(no == 176,
         year == 2018) %>%
  ggplot(aes(x = reorder(ent, valor), y = valor)) +
  geom_col() + 
  coord_flip()
  
