# Librerie
library(dplyr)
library(sf)
library(readr)
library(ggplot2)
library(classInt)
library(viridis)

#-----------------------------------------------------------
# 1. CARICO DATASET
#-----------------------------------------------------------
df <- read.csv("C:\\Users\\samsung\\Documents\\MAGISTRALE\\SECONDO ANNO\\NONPARAMETRIC STATISTICS\\PROJECT\\dataset_finale.csv")

# ===========================================================
# 2. CARICO DATASET OSPEDALI (stress + posti letto)
# ===========================================================
ospedali <- read.csv("C:\\Users\\samsung\\Documents\\MAGISTRALE\\PRIMO ANNO\\APPLIED STATISTICS\\GRINS Indice SSN\\Scripts\\DATASET_DIST\\ps_stress.csv")

# Rinomino colonne per evitare confusioni
ospedali <- ospedali %>%
  rename(
    stress_DO  = degenza_ordinaria_indice,
    stress_DS  = day_surgery_indice,
    stress_DH  = day_hospital_indice,
    PL_previsti = PL.Totali.Previsti
  )

# ===========================================================
# 3. MERGE PER OTTENERE STRESS E POSTI LETTO (3 MATCH)
# ===========================================================
# --- Match ospedale 1 ---
df <- df %>%
  left_join(
    ospedali,
    by = c("ps_code_1" = "ps_code", "ps_subcode_1" = "ps_subcode")
  ) %>%
  rename(
    stress_DO_1 = stress_DO,
    stress_DS_1 = stress_DS,
    stress_DH_1 = stress_DH,
    PL_1        = PL_previsti
  )

# --- Match ospedale 2 ---
df <- df %>%
  left_join(
    ospedali,
    by = c("ps_code_2" = "ps_code", "ps_subcode_2" = "ps_subcode")
  ) %>%
  rename(
    stress_DO_2 = stress_DO,
    stress_DS_2 = stress_DS,
    stress_DH_2 = stress_DH,
    PL_2        = PL_previsti
  )

# --- Match ospedale PEDIATRICO ---
df <- df %>%
  left_join(
    ospedali,
    by = c("ps_ped_code" = "ps_code", "ps_ped_subcode" = "ps_subcode")
  ) %>%
  rename(
    stress_DO_ped = stress_DO,
    stress_DS_ped = stress_DS,
    stress_DH_ped = stress_DH,
    PL_ped        = PL_previsti
  )

# ===========================================================
# 4. CARICO SHAPEFILE COMUNI
# ===========================================================
comuni <- st_read("C:\\Users\\samsung\\Documents\\MAGISTRALE\\PRIMO ANNO\\APPLIED STATISTICS\\GRINS Indice SSN\\Scripts\\DATASET_DIST\\Com2021\\Com2021.shp")
comuni <- st_transform(comuni, 4326)
comuni <- st_make_valid(comuni)

# ===========================================================
# 5. CONVERTO IL DATASET ORIGINALE IN sf
# ===========================================================
df_sf <- df %>%
  st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, remove = FALSE)

# ===========================================================
# 6. JOIN SPAZIALE PER ASSEGNARE IL COMUNE
# ===========================================================
df_joined <- st_join(df_sf, comuni, join = st_within)

# Controllo se alcuni punti sono rimasti senza comune
missing <- sum(is.na(df_joined$COMUNE))
cat("Punti senza comune:", missing, "\n")

# ===========================================================
# 7. SELEZIONO SOLO LE COLONNE UTILI
# ===========================================================
df_clean <- df_joined %>%
  select(
    COMUNE, PRO_COM,
    
    # Tempi percorrenza
    time_travel_ps_1,
    time_travel_ps_2,
    travel_time_ps_ped,
    
    # Stress ospedalieri
    stress_DO_1, stress_DS_1, stress_DH_1, PL_1,
    stress_DO_2, stress_DS_2, stress_DH_2, PL_2,
    stress_DO_ped, stress_DS_ped, stress_DH_ped, PL_ped
  )

# ===========================================================
# 8. CALCOLO LA MEDIANA PER COMUNE
# ===========================================================
df_median <- df_clean %>%
  st_drop_geometry() %>%
  group_by(COMUNE) %>%
  summarise(
    mediana_ps1      = median(time_travel_ps_1, na.rm = TRUE),
    mediana_ps2      = median(time_travel_ps_2, na.rm = TRUE),
    mediana_ps_ped   = median(travel_time_ps_ped, na.rm = TRUE),
    
    mediana_stress_DO1   = median(stress_DO_1, na.rm = TRUE),
    mediana_stress_DS1   = median(stress_DS_1, na.rm = TRUE),
    mediana_stress_DH1   = median(stress_DH_1, na.rm = TRUE),
    mediana_stress_DO2   = median(stress_DO_2, na.rm = TRUE),
    mediana_stress_DS2   = median(stress_DS_2, na.rm = TRUE),
    mediana_stress_DH2   = median(stress_DH_2, na.rm = TRUE),
    mediana_stress_DOped   = median(stress_DO_ped, na.rm = TRUE),
    mediana_stress_DSped   = median(stress_DS_ped, na.rm = TRUE),
    mediana_stress_DHped   = median(stress_DH_ped, na.rm = TRUE),
    
    mediana_PL1          = median(PL_1, na.rm = TRUE),
    mediana_PL2          = median(PL_2, na.rm = TRUE),
    mediana_PLped          = median(PL_ped, na.rm = TRUE)
  )

# ===========================================================
# 10. AGGIUNGO LA POPOLAZIONE COMUNALE
# ===========================================================

pop <- read_delim(
  "C:\\Users\\samsung\\Documents\\MAGISTRALE\\SECONDO ANNO\\NONPARAMETRIC STATISTICS\\PROJECT\\popolazione2021comuni.csv",
  delim = ";",
  locale = locale(encoding = "UTF-8"), 
  guess_max = 5000
)

df_median$COMUNE <- trimws(toupper(df_median$COMUNE))
pop$COMUNE       <- trimws(toupper(pop$Comune))

# Merge
df_finale <- df_median %>%
  left_join(pop %>% select(COMUNE, 'Popolazione Totale'), by = "COMUNE")


# ===========================================================
# 11. CALCOLO STRESS_RAW PER OGNI OSPEDALE
# ===========================================================

df_finale <- df_finale %>%
  mutate(
    # stress_raw per PS1, PS2, PSpediatrico
    stress_raw_1   = 0.72*mediana_stress_DO1 + 0.10*mediana_stress_DS1 + 0.18*mediana_stress_DH1,
    stress_raw_2   = 0.72*mediana_stress_DO2 + 0.10*mediana_stress_DS2 + 0.18*mediana_stress_DH2,
    stress_raw_ped = 0.72*mediana_stress_DOped + 0.10*mediana_stress_DSped + 0.18*mediana_stress_DHped
  )

df_finale <- df_finale %>%
  mutate(
    # stress_raw per PS1, PS2, PSpediatrico
    stress_weighted1   = `Popolazione Totale`/mediana_PL1*stress_raw_1,
    stress_weighted2   = `Popolazione Totale`/mediana_PL2*stress_raw_2,
    stress_weightedped   = `Popolazione Totale`/mediana_PLped*stress_raw_ped
  )
minmax <- function(x) {
  if(all(is.na(x))) return(x)
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if(rng == 0) return(rep(0, length(x)))   # evita divisione per zero
  (x - min(x, na.rm = TRUE)) / rng
}

df_finale <- df_finale %>%
  mutate(
    stress_weighted1   = minmax(stress_weighted1),
    stress_weighted2   = minmax(stress_weighted2),
    stress_weightedped = minmax(stress_weightedped)
  )


# ===========================================================
# 12. CALCOLO STRESS_CENTROIDE
# ===========================================================

df_finale <- df_finale %>%
  mutate(
    stress_final = 1/3 * (mediana_ps2 * stress_weighted1) / (mediana_ps1 + mediana_ps2) +
        1/3 * (mediana_ps1 * stress_weighted2) / (mediana_ps1 + mediana_ps2) +
        1/3 * mediana_ps_ped * stress_weightedped,
    stress_final = minmax(stress_final)
  )

write.csv(df_finale, 
          "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/dataset_finale_completo.csv", 
          row.names = FALSE)

comuni$COMUNE <- trimws(toupper(comuni$COMUNE))

comuni_plot <- comuni %>%
  left_join(df_finale %>% select(COMUNE, stress_final),
            by = "COMUNE")

# -----------------------------------------------------------
# PLOT DELLA MAPPA
# -----------------------------------------------------------

n_classi <- 10

brks <- classIntervals(
  comuni_plot$stress_final,
  n = n_classi,
  style = "quantile"
)$brks

# Crea un fattore per la mappa
comuni_plot$stress_class <- cut(
  comuni_plot$stress_final,
  breaks = brks,
  include.lowest = TRUE,
  dig.lab = 4
)

# --- 5. Plot della mappa quantile ---
ggplot(comuni_plot) +
  geom_sf(aes(fill = stress_class), color = NA) +
  scale_fill_viridis_d(
    option = "C",
    direction = 1,
    name = "Stress Index (quantile)"
  ) +
  theme_minimal() +
  labs(
    title = "Mappa dell'indice di Stress (Quantile Map)",
    subtitle = "Classificazione in quantili per una visualizzazione equilibrata"
  ) +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold")
  )

