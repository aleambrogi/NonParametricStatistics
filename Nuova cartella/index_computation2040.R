# ===========================================================
# LIBRERIE
# ===========================================================
library(dplyr)
library(sf)
library(readr)
library(ggplot2)
library(classInt)
library(viridis)
library(stringr)

# ===========================================================
# 1. CARICO DATASET PRINCIPALE
# ===========================================================
df <- read.csv(
  "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/dataset_finale.csv",
  stringsAsFactors = FALSE
)

# ===========================================================
# 2. CARICO DATASET OSPEDALI
# ===========================================================
ospedali <- read.csv(
  "C:/Users/samsung/Documents/MAGISTRALE/PRIMO ANNO/APPLIED STATISTICS/GRINS Indice SSN/Scripts/DATASET_DIST/ps_stress.csv",
  stringsAsFactors = FALSE
)

ospedali <- ospedali %>%
  rename(
    stress_DO   = degenza_ordinaria_indice,
    stress_DS   = day_surgery_indice,
    stress_DH   = day_hospital_indice,
    PL_previsti = PL.Totali.Previsti
  )

# ===========================================================
# 3. MERGE OSPEDALI (3 MATCH)
# ===========================================================
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
# 4. SHAPEFILE COMUNI
# ===========================================================
comuni <- st_read(
  "C:/Users/samsung/Documents/MAGISTRALE/PRIMO ANNO/APPLIED STATISTICS/GRINS Indice SSN/Datasets/ISTAT shape file/Limiti2021/Com2021/Com2021.shp",
  quiet = TRUE
) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  mutate(
    COMUNE  = str_trim(toupper(COMUNE))
  ) 

comuni <- comuni %>%
  rename(COD_COM = PRO_COM)

# ===========================================================
# 5. DATASET IN sf + JOIN SPAZIALE
# ===========================================================
df_sf <- df %>%
  st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, remove = FALSE)

df_joined <- st_join(df_sf, comuni, join = st_within)

# ===========================================================
# 6. SELEZIONE VARIABILI
# ===========================================================
df_clean <- df_joined %>%
  select(
    COMUNE,
    
    time_travel_ps_1,
    time_travel_ps_2,
    travel_time_ps_ped,
    
    stress_DO_1, stress_DS_1, stress_DH_1, PL_1,
    stress_DO_2, stress_DS_2, stress_DH_2, PL_2,
    stress_DO_ped, stress_DS_ped, stress_DH_ped, PL_ped
  )

# ===========================================================
# 7. MEDIANE PER COMUNE
# ===========================================================
df_median <- df_clean %>%
  st_drop_geometry() %>%
  group_by(COMUNE) %>%
  summarise(
    mediana_ps1    = median(time_travel_ps_1, na.rm = TRUE),
    mediana_ps2    = median(time_travel_ps_2, na.rm = TRUE),
    mediana_ps_ped = median(travel_time_ps_ped, na.rm = TRUE),
    
    mediana_stress_DO1 = median(stress_DO_1, na.rm = TRUE),
    mediana_stress_DS1 = median(stress_DS_1, na.rm = TRUE),
    mediana_stress_DH1 = median(stress_DH_1, na.rm = TRUE),
    
    mediana_stress_DO2 = median(stress_DO_2, na.rm = TRUE),
    mediana_stress_DS2 = median(stress_DS_2, na.rm = TRUE),
    mediana_stress_DH2 = median(stress_DH_2, na.rm = TRUE),
    
    mediana_stress_DOped = median(stress_DO_ped, na.rm = TRUE),
    mediana_stress_DSped = median(stress_DS_ped, na.rm = TRUE),
    mediana_stress_DHped = median(stress_DH_ped, na.rm = TRUE),
    
    mediana_PL1   = median(PL_1, na.rm = TRUE),
    mediana_PL2   = median(PL_2, na.rm = TRUE),
    mediana_PLped = median(PL_ped, na.rm = TRUE),
    .groups = "drop"
  )

df_median <- df_median %>%
  mutate(
    COMUNE = str_trim(toupper(COMUNE))
  ) %>%
  left_join(
    comuni %>% select(COMUNE, COD_COM),
    by = "COMUNE"
  )

sum(is.na(df_median$COD_COM))

# ===========================================================
# 8. CARICO POPOLAZIONE PREDDETTA 2040
# ===========================================================
pop_2040 <- read.csv(
  "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/df_predicted_agaci_reconciled.csv",
  stringsAsFactors = FALSE
) %>%
  filter(ANNO == 2040) %>%
  select(COD_COM, POP_PRED_REC, POP_LO_REC, POP_HI_REC)

df_median$COD_COM <- as.character(df_median$COD_COM)
pop_2040$COD_COM  <- as.character(pop_2040$COD_COM)

df_finale <- df_median %>%
  mutate(COMUNE = trimws(toupper(COMUNE))) %>%
  right_join(pop_2040, by = "COD_COM")

# ===========================================================
# 9. STRESS RAW
# ===========================================================
df_finale <- df_finale %>%
  mutate(
    stress_raw_1   = 0.72*mediana_stress_DO1 + 0.10*mediana_stress_DS1 + 0.18*mediana_stress_DH1,
    stress_raw_2   = 0.72*mediana_stress_DO2 + 0.10*mediana_stress_DS2 + 0.18*mediana_stress_DH2,
    stress_raw_ped = 0.72*mediana_stress_DOped + 0.10*mediana_stress_DSped + 0.18*mediana_stress_DHped
  )

# ===========================================================
# 10. STRESS WEIGHTED (POPOLAZIONE 2040)
# ===========================================================
df_finale <- df_finale %>%
  mutate(
    
    # lower bound
    stress_w1_lo   = POP_LO_REC  / mediana_PL1   * stress_raw_1,
    stress_w2_lo   = POP_LO_REC  / mediana_PL2   * stress_raw_2,
    stress_wped_lo = POP_LO_REC  / mediana_PLped * stress_raw_ped,
    
    # point estimate
    stress_w1_mid   = POP_PRED_REC / mediana_PL1   * stress_raw_1,
    stress_w2_mid   = POP_PRED_REC / mediana_PL2   * stress_raw_2,
    stress_wped_mid = POP_PRED_REC / mediana_PLped * stress_raw_ped,
    
    # upper bound
    stress_w1_hi   = POP_HI_REC  / mediana_PL1   * stress_raw_1,
    stress_w2_hi   = POP_HI_REC  / mediana_PL2   * stress_raw_2,
    stress_wped_hi = POP_HI_REC  / mediana_PLped * stress_raw_ped
  )


minmax <- function(x) {
  if (all(is.na(x))) return(x)
  r <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (r == 0) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / r
}

df_finale <- df_finale %>%
  mutate(
    across(
      c(stress_w1_lo, stress_w2_lo, stress_wped_lo,
        stress_w1_mid, stress_w2_mid, stress_wped_mid,
        stress_w1_hi, stress_w2_hi, stress_wped_hi),
      minmax
    )
  )
# ===========================================================
# 11. STRESS FINALE
# ===========================================================
df_finale <- df_finale %>%
  mutate(
    stress_final_lo =
      1/3 * (mediana_ps2 * stress_w1_lo) / (mediana_ps1 + mediana_ps2) +
      1/3 * (mediana_ps1 * stress_w2_lo) / (mediana_ps1 + mediana_ps2) +
      1/3 * mediana_ps_ped * stress_wped_lo,
    
    stress_final_mid =
      1/3 * (mediana_ps2 * stress_w1_mid) / (mediana_ps1 + mediana_ps2) +
      1/3 * (mediana_ps1 * stress_w2_mid) / (mediana_ps1 + mediana_ps2) +
      1/3 * mediana_ps_ped * stress_wped_mid,
    
    stress_final_hi =
      1/3 * (mediana_ps2 * stress_w1_hi) / (mediana_ps1 + mediana_ps2) +
      1/3 * (mediana_ps1 * stress_w2_hi) / (mediana_ps1 + mediana_ps2) +
      1/3 * mediana_ps_ped * stress_wped_hi
  ) %>%
  mutate(
    stress_final_lo  = minmax(stress_final_lo),
    stress_final_mid = minmax(stress_final_mid),
    stress_final_hi  = minmax(stress_final_hi)
  )

df_finale <- df_finale %>%
  select(COMUNE,COD_COM,stress_final_lo,stress_final_mid,stress_final_hi)

df_finale <- df_finale %>%
  filter(!is.na(stress_final_lo))

# ===========================================================
# 12. EXPORT CSV
# ===========================================================
write.csv(
  df_finale,
  "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/dataset_stress_2040.csv",
  row.names = FALSE
)
