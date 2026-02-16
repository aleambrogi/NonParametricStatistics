# Librerie
library(dplyr)
library(sf)
library(readr)
library(ggplot2)
library(classInt)

# -----------------------------------------------------------
# 1. CARICO DATASET COMPLETO (tempi e stress ospedaliero)
# -----------------------------------------------------------
df <- read.csv("C:\\Users\\samsung\\Documents\\MAGISTRALE\\SECONDO ANNO\\NONPARAMETRIC STATISTICS\\PROJECT\\dataset_finale.csv")

# ===========================================================
# 2. CARICO DATASET OSPEDALI (stress + posti letto)
# ===========================================================
ospedali <- read.csv("C:\\Users\\samsung\\Documents\\MAGISTRALE\\PRIMO ANNO\\APPLIED STATISTICS\\GRINS Indice SSN\\Scripts\\DATASET_DIST\\ps_stress.csv")

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
# PS1
df <- df %>%
  left_join(ospedali, by = c("ps_code_1" = "ps_code", "ps_subcode_1" = "ps_subcode")) %>%
  rename(stress_DO_1 = stress_DO,
         stress_DS_1 = stress_DS,
         stress_DH_1 = stress_DH,
         PL_1        = PL_previsti)

# PS2
df <- df %>%
  left_join(ospedali, by = c("ps_code_2" = "ps_code", "ps_subcode_2" = "ps_subcode")) %>%
  rename(stress_DO_2 = stress_DO,
         stress_DS_2 = stress_DS,
         stress_DH_2 = stress_DH,
         PL_2        = PL_previsti)

# PED
df <- df %>%
  left_join(ospedali, by = c("ps_ped_code" = "ps_code", "ps_ped_subcode" = "ps_subcode")) %>%
  rename(stress_DO_ped = stress_DO,
         stress_DS_ped = stress_DS,
         stress_DH_ped = stress_DH,
         PL_ped        = PL_previsti)

# ===========================================================
# 4. CARICO POPOLAZIONE AGGREGATA PER ANNI
# ===========================================================
pop <- read.csv("C:\\Users\\samsung\\Documents\\MAGISTRALE\\SECONDO ANNO\\NONPARAMETRIC STATISTICS\\PROJECT\\popolazione_aggregata_2021_2024.csv")

# Mantieni solo colonne necessarie
pop <- pop %>% select(Comune, starts_with("Pop_"))

comuni <- st_read("C:\\Users\\samsung\\Documents\\MAGISTRALE\\PRIMO ANNO\\APPLIED STATISTICS\\GRINS Indice SSN\\Scripts\\DATASET_DIST\\Com2021\\Com2021.shp")
comuni <- st_transform(comuni, 4326)
comuni <- st_make_valid(comuni)

df_sf <- df %>%
  st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, remove = FALSE)
df_joined <- st_join(df_sf, comuni, join = st_within)

df <- df_joined

# Allinea nomi comuni
df$COMUNE <- toupper(trimws(df$COMUNE))
pop$Comune <- toupper(trimws(pop$Comune))

# Merge con popolazione
df <- df %>% left_join(pop, by = c("COMUNE" = "Comune"))

# ===========================================================
# 5. CALCOLO DELLE MEDIANE PER COMUNE
# ===========================================================
df_median <- df %>%
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
    mediana_stress_DOped = median(stress_DO_ped, na.rm = TRUE),
    mediana_stress_DSped = median(stress_DS_ped, na.rm = TRUE),
    mediana_stress_DHped = median(stress_DH_ped, na.rm = TRUE),
    
    mediana_PL1   = median(PL_1, na.rm = TRUE),
    mediana_PL2   = median(PL_2, na.rm = TRUE),
    mediana_PLped = median(PL_ped, na.rm = TRUE),
    
    Pop_2021 = median(Pop_2021, na.rm = TRUE),
    Pop_2022 = median(Pop_2022, na.rm = TRUE),
    Pop_2023 = median(Pop_2023, na.rm = TRUE),
    Pop_2024 = median(Pop_2024, na.rm = TRUE),
  )

# ===========================================================
# 6. FUNZIONE MIN-MAX
# ===========================================================
minmax <- function(x) {
  if(all(is.na(x))) return(x)
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if(rng == 0) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / rng
}

# ===========================================================
# 7. CALCOLO STRESS_FINAL PER TUTTI GLI ANNI
# ===========================================================
# Prima calcolo stress_raw (costante)
df_median <- df_median %>%
  mutate(
    stress_raw_1   = 0.72*mediana_stress_DO1 + 0.10*mediana_stress_DS1 + 0.18*mediana_stress_DH1,
    stress_raw_2   = 0.72*mediana_stress_DO2 + 0.10*mediana_stress_DS2 + 0.18*mediana_stress_DH2,
    stress_raw_ped = 0.72*mediana_stress_DOped + 0.10*mediana_stress_DSped + 0.18*mediana_stress_DHped
  )

# Funzione per calcolare stress_final per un anno
calc_stress_final <- function(Pop, med_PL1, med_PL2, med_PLped,
                              med_ps1, med_ps2, med_ps_ped,
                              stress_raw1, stress_raw2, stress_rawped) {
  weighted1 <- minmax(Pop / med_PL1 * stress_raw1)
  weighted2 <- minmax(Pop / med_PL2 * stress_raw2)
  weightedped <- minmax(Pop / med_PLped * stress_rawped)
  
  stress <- 1/3 * (med_ps2 * weighted1) / (med_ps1 + med_ps2) +
    1/3 * (med_ps1 * weighted2) / (med_ps1 + med_ps2) +
    1/3 * med_ps_ped * weightedped
  minmax(stress)
}

# Calcolo per ogni anno
for (yr in 2021:2024) {
  pop_col <- paste0("Pop_", yr)
  stress_col <- paste0("stress_", yr)
  
  df_median[[stress_col]] <- calc_stress_final(
    Pop = df_median[[pop_col]],
    med_PL1 = df_median$mediana_PL1,
    med_PL2 = df_median$mediana_PL2,
    med_PLped = df_median$mediana_PLped,
    med_ps1 = df_median$mediana_ps1,
    med_ps2 = df_median$mediana_ps2,
    med_ps_ped = df_median$mediana_ps_ped,
    stress_raw1 = df_median$stress_raw_1,
    stress_raw2 = df_median$stress_raw_2,
    stress_rawped = df_median$stress_raw_ped
  )
}

# ===========================================================
# 8. SALVA IL DATASET FINALE
# ===========================================================
df_output <- df_median %>%
  select(COMUNE, starts_with("stress_"))
df_output <- df_output %>% 
  select(COMUNE,stress_2021,stress_2022,stress_2023,stress_2024)
df_output <- st_drop_geometry(df_output)

write.csv(df_output,
          "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/dataset_finale_stress_2021_2024.csv",
          row.names = FALSE)

comuni$COMUNE <- trimws(toupper(comuni$COMUNE))
df_output$COMUNE <- trimws(toupper(df_output$COMUNE))

comuni_plot <- comuni %>%
  left_join(df_output %>% st_drop_geometry(), by = "COMUNE")

years <- 2021:2024

# ---- 2. Funzione robusta per plottare la mappa ----

plot_map <- function(data, year) {
  
  var <- paste0("stress_", year)
  
  vals <- data[[var]]
  vals_no_na <- vals[!is.na(vals)]
  
  if (length(vals_no_na) == 0) stop(paste("Tutti NA per", var))
  if (length(unique(vals_no_na)) == 1) stop(paste("Tutti uguali per", var))
  
  brks <- classIntervals(vals_no_na, n = 10, style = "quantile")$brks
  
  ggplot(data) +
    geom_sf(aes(fill = cut(vals, breaks = brks, include.lowest = TRUE)), 
            color = NA) +
    scale_fill_viridis_d(name = paste("Stress", year)) +
    labs(
      title = paste("Indice di Stress -", year),
      subtitle = "Classificazione in quantili"
    ) +
    theme_minimal()
}

# ---- 3. Salva tutte le mappe in PNG ----
output_dir <- "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/maps/"
dir.create(output_dir, showWarnings = FALSE)

for (yr in years) {
  p <- plot_map(comuni_plot, yr)
  ggsave(
    filename = paste0(output_dir, "stress_map_", yr, ".png"),
    plot = p,
    width = 10,
    height = 8,
    dpi = 300
  )
}
