library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# --- 1. Leggi il dataset recente ---
df_recent <- read.csv("C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/popolazione_aggregata_2021_2024.csv", stringsAsFactors = FALSE)

# --- 2. Trasforma in formato long ---
df_recent_long <- df_recent %>%
  select(Codice_comune.x,Pop_2021,Pop_2022,Pop_2023,Pop_2024) %>% # tolgo colonne duplicate dei codici
  pivot_longer(
    cols = starts_with("Pop_"),
    names_to = "ANNO",
    values_to = "POPOLAZIONE"
  ) %>%
  mutate(
    ANNO = as.integer(str_replace(ANNO, "Pop_", "")),
    COD_COM = str_pad(as.character(Codice_comune.x), width = 6, side = "left", pad = "0"),
    POPOLAZIONE = as.numeric(POPOLAZIONE)
  ) %>%
  select(COD_COM, ANNO, POPOLAZIONE)

# --- 3. Riempimento COD_PROV e COD_REG usando il dataset storico (ultimo anno disponibile) ---
# Prendo i codici dal dataset storico (ultimo anno per ogni comune)

df_final <- read.csv("C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop1982-2019_adj.csv", stringsAsFactors = FALSE)

df_final$COD_COM <- as.character(df_final$COD_COM)

df_final <- df_final %>%
  mutate(
    COD_COM  = str_pad(as.character(COD_COM), width = 6, side = "left", pad = "0"),
    COD_PROV = str_pad(as.character(COD_PROV), width = 3, side = "left", pad = "0"),
    COD_REG  = str_pad(as.character(COD_REG), width = 2, side = "left", pad = "0")
  )

codici_ref <- df_final %>%
  group_by(COD_COM) %>%
  filter(ANNO == max(ANNO)) %>%
  ungroup() %>%
  select(COD_COM, COD_PROV, COD_REG)

df_recent_long <- df_recent_long %>%
  left_join(codici_ref, by = "COD_COM") %>%
  select(COD_COM, COD_PROV, COD_REG, ANNO, POPOLAZIONE)

# --- 4. Stacking con df_final ---
df_full <- bind_rows(df_final, df_recent_long) %>%
  arrange(COD_COM, ANNO)

# --- 5. Controllo finale ---
summary(df_full)
any(is.na(df_full$COD_PROV))  # dovrebbe dare FALSE
any(is.na(df_full$COD_REG))   # dovrebbe dare FALSE

# --- 6. Salvataggio ---
write.csv(df_full, "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop1982-2024-2020_adj.csv",
          row.names = FALSE
)

