# ===============================================================
# COSTRUZIONE CURVE FUNZIONALI DI ACCESSIBILITÀ SANITARIA
# Versione ottimizzata per dataset grandi (~1.2 milioni di righe)
# ===============================================================

library(data.table)   # lettura e aggregazione veloce
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# ---- 1. Lettura efficiente del dataset ----
# Specifica solo le colonne necessarie
cols_needed <- c(
  "grid_SIGLA_PROVINCIA",   # provincia
  "COD_MACRO_x",            # comune
  "TOT_P_2021",             # popolazione cella
  "time_travel_ps_1"        # tempo verso PS più vicino
)

# fread legge solo le colonne specificate
data <- fread("dataset_finale.csv", select = cols_needed)

# Controlla dimensioni e memoria
print(dim(data))
print(object.size(data), units = "Mb")

# ---- 2. Pulisci eventuali missing o anomali ----
data <- data[!is.na(time_travel_ps_1) & !is.na(TOT_P_2021) & TOT_P_2021 > 0]

# ---- 3. Definisci la griglia dei tempi (in minuti) ----
time_grid <- seq(0, 180, by = 1)  # puoi estendere a 90 o 120

# ---- 4. Funzione di calcolo della curva ----
compute_curve <- function(df, time_grid) {
  tot_pop <- sum(df$TOT_P_2021)
  if (tot_pop == 0) return(rep(NA, length(time_grid)))
  sapply(time_grid, function(t) {
    sum(df$TOT_P_2021[df$time_travel_ps_1 <= t]) / tot_pop
  })
}

# ---- 6. Calcolo per PROVINCIA (grid_SIGLA_PROVINCIA) ----
message("Calcolo curve per PROVINCE...")

curves_province <- data[, .(
  f_curve = list(compute_curve(.SD, time_grid))
), by = grid_SIGLA_PROVINCIA, .SDcols = c("TOT_P_2021", "time_travel_ps_1")]

curves_province_long <- curves_province[, {
  data.table(
    minute = time_grid,
    f_value = unlist(f_curve)
  )
}, by = grid_SIGLA_PROVINCIA]

# ---- 7. SALVA i risultati (leggeri e compressi) ----
fwrite(curves_province_long, "curves_province.csv.gz")

# ---- 8. (Opzionale) Grafico rapido di controllo ----
sample_prov <- sample(unique(curves_province_long$grid_SIGLA_PROVINCIA), 107)
ggplot(curves_province_long[grid_SIGLA_PROVINCIA %in% sample_prov],
       aes(x = minute, y = f_value, group = grid_SIGLA_PROVINCIA)) +
  geom_line(alpha = 0.2) +
  labs(title = "Curve di accessibilità verso PS (107 province)",
       x = "Tempo (minuti)", y = "Popolazione cumulata raggiungibile") +
  theme_minimal()

