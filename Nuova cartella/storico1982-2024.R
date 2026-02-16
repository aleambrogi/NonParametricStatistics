library(dplyr)
library(readr)
library(stringr)

# --- 1. Leggi il dataset 2020 ---

df_2020 <- read.csv("C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop2020.csv", stringsAsFactors = FALSE, sep =';')

# --- 2. Filtra solo totale popolazione ---
df_2020_total <- df_2020 %>%
  filter(Età == 999) %>%
  mutate(
    ANNO = 2020L,
    COD_COM = str_pad(as.character(COD_COM), width = 6, side = "left", pad = "0")
  ) %>%
  select(COD_COM, POPOLAZIONE, ANNO)

# --- 3. Recupera COD_PROV e COD_REG da df_full storico ---

df_full <- read.csv("C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop1982-2024-2020_adj.csv", stringsAsFactors = FALSE) %>%
  mutate(COD_COM = str_pad(as.character(COD_COM), width = 6, side = "left", pad = "0"))

codici_ref <- df_full %>%
  group_by(COD_COM) %>%
  summarise(
    COD_PROV = first(COD_PROV[!is.na(COD_PROV)]),
    COD_REG  = first(COD_REG[!is.na(COD_REG)]),
    .groups = "drop"
  )

df_full$COD_COM <- as.character(df_full$COD_COM)
df_2020_total$COD_COM <- as.character(df_2020_total$COD_COM)
codici_ref$COD_COM <- as.character(codici_ref$COD_COM)

# --- 4. Riempi codici provincia e regione ---
df_2020_total <- df_2020_total %>%
  left_join(codici_ref, by = "COD_COM") %>%
  select(COD_COM, COD_PROV, COD_REG, ANNO, POPOLAZIONE)

# --- 5. Stacking con df_full esistente ---
df_full <- bind_rows(df_full, df_2020_total) %>%
  mutate(
    COD_PROV = as.character(COD_PROV),
    COD_REG  = as.character(COD_REG)
  ) %>%
  arrange(COD_COM, ANNO)

# --- 6. Salva il dataset completo ---
write.csv(df_full, "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop1982-2024_adj.csv", row.names = FALSE)


