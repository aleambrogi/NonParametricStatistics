library(dplyr)
library(sf)
library(ggplot2)
library(mgcv)
library(viridis)

# -----------------------------------------------------------
# 1. Dataset dei comuni con stress 2024
# -----------------------------------------------------------
df_prov <- readRDS("df_prov_2024.rds")

df_plot <- df_prov %>%
  st_centroid() %>%                  # prendo i centroidi
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(COMUNE, lon, lat, stress_2024)

# -----------------------------------------------------------
# 2. Fit GAM spaziale
# -----------------------------------------------------------
gam_fit <- gam(stress_2024 ~ s(lon, lat), data = df_plot)

# -----------------------------------------------------------
# 3. Griglia regolare su coordinate italiane
# -----------------------------------------------------------
lon_seq <- seq(min(df_plot$lon), max(df_plot$lon), length.out = 200)
lat_seq <- seq(min(df_plot$lat), max(df_plot$lat), length.out = 200)
grid <- expand.grid(lon = lon_seq, lat = lat_seq)

# -----------------------------------------------------------
# 4. Shapefile dei comuni italiani
# -----------------------------------------------------------
comuni_sf <- st_read("C:\\Users\\samsung\\Documents\\MAGISTRALE\\PRIMO ANNO\\APPLIED STATISTICS\\GRINS Indice SSN\\Scripts\\DATASET_DIST\\Com2021\\Com2021.shp") %>%
  st_transform(4326) %>%
  st_make_valid()

# -----------------------------------------------------------
# 5. Predizione GAM sulla griglia
# -----------------------------------------------------------
grid$stress_pred <- predict(gam_fit, newdata = grid)

# -----------------------------------------------------------
# 6. Trasforma griglia in sf e clip sui comuni
# -----------------------------------------------------------
grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = st_crs(comuni_sf))

# Mantieni solo punti dentro i comuni
within_idx <- st_within(grid_sf, comuni_sf, sparse = FALSE)
grid_sf <- grid_sf[apply(within_idx, 1, any), ]

# Riporta a dataframe per ggplot
grid_clipped <- cbind(st_coordinates(grid_sf), stress_pred = grid_sf$stress_pred)

# -----------------------------------------------------------
# 7. Plot finale
# -----------------------------------------------------------
png("stress_2024_map.png", width = 2000, height = 2000, res = 300)
print(
  ggplot() +
    geom_raster(data = grid_clipped, aes(x = X, y = Y, fill = stress_pred), interpolate = TRUE) +
    geom_sf(data = comuni_sf, fill = NA, color = "black", size = 0.2) +
    geom_point(data = df_plot, aes(x = lon, y = lat), color = "black", size = 0.5) +
    scale_fill_viridis(option = "plasma", name = "Stress 2024") +
    theme_minimal()
)
dev.off()














library(plotly)

# Assumiamo di avere la griglia con predizione:
# grid$lon, grid$lat, grid$stress_pred

# Convertiamo in matrice per plot_ly
lon_seq <- unique(grid$lon)
lat_seq <- unique(grid$lat)

z_matrix <- matrix(grid$stress_pred, 
                   nrow = length(lon_seq), 
                   ncol = length(lat_seq))

library(rgl)

# Superficie
plot_ly(x = lon_seq, y = lat_seq, z = z_matrix) %>%
  add_surface() %>%
  layout(title = "Stress 2024 - Superficie 3D",
         scene = list(
           xaxis = list(title = "Longitudine"),
           yaxis = list(title = "Latitudine"),
           zaxis = list(title = "Stress 2024")
         ))
