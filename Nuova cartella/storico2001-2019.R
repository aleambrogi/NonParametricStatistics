library(sf)
library(dplyr)

# 1. Leggi dataset popolazione
df_pop <- read.csv("C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop2002-2021.csv", sep =';')  # oppure readRDS se necessario

comuni_shp <- st_read(
  "C:/Users/samsung/Documents/MAGISTRALE/PRIMO ANNO/APPLIED STATISTICS/GRINS Indice SSN/Datasets/ISTAT shape file/Limiti2021/Com2021/Com2021.shp"
) %>%
  st_transform(4326) %>%
  st_make_valid()

df_pop$COD_COM <- as.character(df_pop$COD_COM)
comuni_shp$COD_COM <- as.character(comuni_shp$PRO_COM)

df_pop_reg <- df_pop %>%
  left_join(
    comuni_shp %>% st_drop_geometry() %>% select(COD_COM, COD_REG),
    by = "COD_COM"
  )

province_shp <- st_read(
  "C:/Users/samsung/Documents/MAGISTRALE/PRIMO ANNO/APPLIED STATISTICS/GRINS Indice SSN/Datasets/ISTAT shape file/Limiti2021/ProvCM2021/ProvCM2021.shp"
) %>%
  st_transform(4326) %>%
  st_make_valid()

# prendi solo geometria dei comuni necessari
df_pop_sf <- df_pop_reg %>%
  left_join(
    comuni_shp %>% select(COD_COM, geometry),
    by = "COD_COM"
  ) %>%
  st_as_sf()

df_pop_final <- st_join(
  df_pop_sf,
  province_shp %>% select(COD_PROV),
  join = st_within
)

df_clean <- df_pop_final %>% st_drop_geometry()
df_clean <- df_clean %>%
  select(COD_COM, COD_PROV, COD_REG, ANNO, POPOLAZIONE)

write.csv(
  st_drop_geometry(median_geo),
  "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop2002-2021_adj.csv",
  row.names = FALSE
)



