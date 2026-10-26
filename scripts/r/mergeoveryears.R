# Run from the repository root. See scripts/r/README.md for external inputs.
source(file.path("scripts", "r", "project_paths.R"))

library(dplyr)
library(readr)

# Funzione per leggere la popolazione totale per comune
read_total_population <- function(file_path, year) {

  # Legge il CSV
  df <- read_delim(file_path, delim = ";", locale = locale(encoding = "UTF-8"))

  print(paste("Colonne lette per anno", year, ":"))
  print(colnames(df))

  # Seleziono colonne utili (se esistono)
  df <- df %>%
    select(Codice_comune, Comune, Eta, Totale)

  # 🔍 Controllo se esiste la riga Eta == 999
  has_999 <- any(df$Eta == 999, na.rm = TRUE)

  if (has_999) {
    # Caso normale → prendi la riga totale
    df_total <- df %>%
      filter(Eta == 999) %>%
      select(Codice_comune, Comune, Popolazione = Totale)

  } else {
    # Caso eccezionale → file contiene già i totali una sola volta
    # (quindi una riga per comune)
    message(paste("⚠️ Attenzione: file", year, "non contiene Eta = 999."))
    message("  → Uso direttamente Totale come popolazione.")

    df_total <- df %>%
      distinct(Codice_comune, Comune, Totale) %>%
      rename(Popolazione = Totale)
  }

  # Rinomina colonna popolazione in funzione dell’anno
  colnames(df_total)[colnames(df_total) == "Popolazione"] <- paste0("Pop_", year)

  return(df_total)
}

# --- 1. Lista file / anni ---
files <- list(
  project_file("popolazione2021comuni.csv"),
  project_file("popolazione2022comuni.csv"),
  project_file("popolazione2023comuni.csv"),
  project_file("popolazione2024comuni.csv")
)

years <- 2021:2024

# --- 2. Leggi file ---
pop_list <- mapply(read_total_population, files, years, SIMPLIFY = FALSE)

# --- 3. Merge di tutti gli anni usando il nome del comune ---
pop_final <- Reduce(function(x, y) full_join(x, y, by = "Comune"), pop_list)

# --- 4. Visualizza ---
head(pop_final)

# --- 5. Salva ---
write.csv(pop_final, project_file("popolazione_aggregata_2021_2024.csv"), row.names = FALSE)
