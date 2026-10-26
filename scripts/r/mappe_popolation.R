# Run from the repository root. See scripts/r/README.md for external inputs.
source(file.path("scripts", "r", "project_paths.R"))

# ================================
# 0. LIBRERIE
# ================================
library(sf)
library(dplyr)
library(ggplot2)
library(gganimate)
library(stringr)
library(scales)

# ================================
# 1. SHAPEFILE COMUNI ITALIA
# ================================
comuni_sf <- st_read(
  spatial_file("Datasets/ISTAT shape file/Limiti2021/Com2021/Com2021.shp"),
  quiet = TRUE
) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  mutate(
    COD_COM = str_pad(as.character(PRO_COM), 6, "left", "0")
  ) %>%
  select(COD_COM, geometry)

# ================================
# 2. DATASET PREVISIONI
# ================================
pred <- read.csv(
  project_file("df_predicted_agaci_reconciled.csv"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    COD_COM = str_pad(as.character(COD_COM), 6, "left", "0"),
    ANNO = as.integer(ANNO)
  ) %>%
  select(COD_COM, ANNO, POP_PRED_REC)

# ================================
# 3. VARIAZIONE % ANNO SU ANNO
# ================================
pred_var <- pred %>%
  arrange(COD_COM, ANNO) %>%
  group_by(COD_COM) %>%
  mutate(
    VAR_PERC = (POP_PRED_REC - lag(POP_PRED_REC)) / lag(POP_PRED_REC) * 100
  ) %>%
  ungroup() %>%
  filter(ANNO >= 2026 & ANNO <= 2040)

# ================================
# 4. JOIN SPAZIALE
# ================================
map_var <- comuni_sf %>%
  left_join(
    pred_var,
    by = "COD_COM"
  )

# ================================
# 5. PLOT BASE (ULTRA LEGGERO)
# ================================
dir.create(project_file("maps_var"), showWarnings = FALSE)

years <- sort(unique(map_var$ANNO))

for (y in years) {

  p <- ggplot(filter(map_var, ANNO == y)) +
    geom_sf(aes(fill = VAR_PERC), color = NA) +
    scale_fill_gradient2(
      low = "#b2182b",
      mid = "white",
      high = "#1a9850",
      midpoint = 0,
      limits = c(-10, 10),
      oob = scales::squish,
      name = "Variazione %"
    ) +
    coord_sf(datum = NA) +
    theme_void() +
    labs(
      title = paste("Variazione % popolazione –", y),
      caption = "Previsioni demografiche"
    )

  ggsave(
    filename = paste0(project_file("maps_var/map_"), y, ".png"),
    plot = p,
    width = 8,
    height = 10,
    dpi = 200
  )

  cat("Salvato anno", y, "\n")
}
