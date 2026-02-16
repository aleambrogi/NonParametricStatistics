# ===============================================================
# CURVE FUNZIONALI DI ACCESSIBILITÀ SANITARIA – FUNZIONE QUANTILE
# ===============================================================

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1. Lettura efficiente del dataset ----
cols_needed <- c(
  "grid_SIGLA_PROVINCIA",   # provincia
  "TOT_P_2021",             # popolazione cella
  "time_travel_ps_1"        # tempo verso PS più vicino
)

data <- fread("dataset_finale.csv", select = cols_needed)
data <- data[!is.na(time_travel_ps_1) & !is.na(TOT_P_2021) & TOT_P_2021 > 0]

# ---- 2. Definizione della funzione per calcolo della funzione quantile ----
# p_seq: sequenza di quantili desiderati (es. 0.1..0.9)
compute_quantile_curve <- function(df, p_seq = seq(0.1, 0.9, by = 0.1)) {
  # ordina i tempi e cumulativa della popolazione
  df_ord <- df[order(time_travel_ps_1)]
  cum_pop <- cumsum(df_ord$TOT_P_2021) / sum(df_ord$TOT_P_2021)
  
  # funzione quantile: per ogni p, tempo minimo t con F(t) >= p
  sapply(p_seq, function(p) {
    idx <- which(cum_pop >= p)[1]   # primo indice che supera p
    df_ord$time_travel_ps_1[idx]
  })
}

# ---- 3. Calcolo per PROVINCIA ----
message("Calcolo funzioni quantile per province...")

p_seq <- seq(0.1, 0.9, by = 0.1)

quantiles_province <- data[, .(
  Q_curve = list(compute_quantile_curve(.SD, p_seq = p_seq))
), by = grid_SIGLA_PROVINCIA, .SDcols = c("TOT_P_2021", "time_travel_ps_1")]

# ---- 4. Conversione in formato long per ggplot o analisi funzionale ----
quantiles_province_long <- quantiles_province[, {
  data.table(
    p = p_seq,
    Q_value = unlist(Q_curve)
  )
}, by = grid_SIGLA_PROVINCIA]

# ---- 5. Salvataggio ----
fwrite(quantiles_province_long, "quantile_curves_province.csv.gz")

# ---- 6. Controllo grafico rapido ----
sample_prov <- sample(unique(quantiles_province_long$grid_SIGLA_PROVINCIA), 107)
ggplot(quantiles_province_long[grid_SIGLA_PROVINCIA %in% sample_prov],
       aes(x = p, y = Q_value, group = grid_SIGLA_PROVINCIA)) +
  geom_line(alpha = 0.3, color = "blue") +
  labs(title = "Funzioni quantile di accessibilità verso PS (107 province)",
       x = "Quantile della popolazione p", y = "Tempo per raggiungere p della popolazione (min)") +
  theme_minimal()

