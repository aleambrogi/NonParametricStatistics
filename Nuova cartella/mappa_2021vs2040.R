library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)


comuni <- st_read(
  "C:/Users/samsung/Documents/MAGISTRALE/PRIMO ANNO/APPLIED STATISTICS/GRINS Indice SSN/Datasets/ISTAT shape file/Limiti2021/Com2021/Com2021.shp",
  quiet = TRUE
) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  mutate(
    COD_COM = as.character(PRO_COM_T)
  )

stress_2021 <- read_csv(
  "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/dataset_finale_stress_2021_2024.csv"
) %>%
  mutate(
    COMUNE = as.character(COMUNE)
  ) %>%
  select(COMUNE, stress_2021)



stress_2040 <- read_csv(
  "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/dataset_stress_2040.csv"
) %>%
  mutate(
    COD_COM = as.character(COD_COM)
  ) %>%
  select(
    COD_COM,
    COMUNE,
    stress_final_lo,
    stress_final_mid,
    stress_final_hi
  )

comuni <- comuni %>%
  mutate(
    COD_COM = stringr::str_pad(as.character(COD_COM), 6, "left", "0")
  )


df_stress <- stress_2021 %>%
  left_join(stress_2040, by = "COMUNE")

df_stress <- df_stress %>%
  mutate(
    inside_interval = case_when(
      is.na(stress_2021) | is.na(stress_final_lo) | is.na(stress_final_hi) ~ NA,
      stress_2021 >= stress_final_lo & stress_2021 <= stress_final_hi ~ "Dentro intervallo",
      TRUE ~ "Fuori intervallo"
    )
  )


df_stress <- df_stress %>%
  mutate(
    COD_COM = stringr::str_pad(as.character(COD_COM), 6, "left", "0")
  )


comuni_map <- comuni %>%
  left_join(df_stress, by = "COD_COM")

st_write(
  comuni_map,
  "comuni_stress.gpkg",
  layer = "stress_2021_2040",
  delete_layer = TRUE
)

ggplot(comuni_map) +
  geom_sf(aes(fill = inside_interval), color = NA) +
  scale_fill_manual(
    values = c(
      "Dentro intervallo" = "#2ECC71",  # verde
      "Fuori intervallo"  = "#E74C3C"   # rosso
    ),
    na.value = "grey90",
    name = "Stress 2021 vs 2040"
  ) +
  theme_minimal() +
  labs(
    title = "Confronto indice di stress: 2021 vs 2040",
    subtitle = "Verde: stress 2021 contenuto nell'intervallo predetto 2040",
    caption = "Intervallo = [Lower, Upper]"
  ) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )


table(df_stress$inside_interval, useNA = "ifany")

prop.table(table(df_stress$inside_interval))

df_stress <- df_stress %>%
  mutate(
    diff_mid = stress_2021 - stress_final_mid
  )
comuni_map2 <- comuni %>%
  left_join(df_stress, by = "COD_COM")

ggplot(comuni_map2) +
  geom_sf(aes(fill = diff_mid), color = NA) +
  scale_fill_viridis_c(option = "viridis", direction = -1, na.value = 'white') +
  theme_minimal()




df_stress <- df_stress %>%
  mutate(
    width_2040 = stress_final_hi - stress_final_lo
  )
comuni_map3 <- comuni %>%
  left_join(df_stress, by = "COD_COM")

ggplot(comuni_map3) +
  geom_sf(aes(fill = width_2040), color = NA) +
  scale_fill_viridis_c(na.value = "grey90") +
  theme_minimal()


ggplot(df_stress, aes(x = stress_2021, y = stress_final_mid)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal()

