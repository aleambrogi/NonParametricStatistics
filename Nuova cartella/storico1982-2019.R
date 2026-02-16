library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)

path <- "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/1982_1991_TUTTO/1982_1991_TUTTO"

files <- list.files(
  path = path,
  pattern = "\\.csv$",
  full.names = TRUE
)
length(files)  # dovrebbe dare 95

read_prov_82_90 <- function(file) {
  
  df <- read_delim(
    file,
    delim = ";",
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
  
  df %>%
    # 1. solo totale popolazione
    filter(`Età` == 99, Sesso == "T") %>%
    
    # 2. selezione colonne (robusta)
    select(
      COD_PROV = `Codice provincia`,
      COD_COM  = `Codice comune`,
      Comune,
      starts_with("Anno")
    ) %>%
    
    # 3. wide → long
    pivot_longer(
      cols = starts_with("Anno"),
      names_to = "ANNO",
      values_to = "POPOLAZIONE"
    ) %>%
    
    # 4. pulizia anno
    mutate(
      ANNO = as.integer(gsub("Anno ", "", ANNO)),
      COD_COM  = as.character(COD_COM),
      COD_PROV = as.character(COD_PROV)
    )
}

df_82_90 <- purrr::map_dfr(files, read_prov_82_90)

range(df_82_90$ANNO)
# 1982 1990

any(duplicated(df_82_90[c("COD_COM", "ANNO")]))
# deve essere FALSE

df_82_90 <- df_82_90 %>% filter(ANNO <= 1990)


df_01_19 <- read.csv(
  "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop1991-2019_adj.csv"
) %>%
  select(COD_COM, COD_PROV, COD_REG, ANNO, POPOLAZIONE)

df_01_19$COD_PROV <- as.character(df_01_19$COD_PROV)
df_01_19$COD_COM <- as.character(df_01_19$COD_COM)

df_final <- bind_rows(
  df_82_90,
  df_01_19
)

df_final <- df_final %>%
  mutate(
    COD_REG  = str_trim(as.character(COD_REG)),
    COD_PROV = str_trim(as.character(COD_PROV)),
    COD_COM  = str_trim(as.character(COD_COM))
  ) %>%
  mutate(
    COD_REG  = str_pad(COD_REG,  width = 2, side = "left", pad = "0"),
    COD_PROV = str_pad(COD_PROV, width = 3, side = "left", pad = "0"),
    COD_COM  = str_pad(COD_COM,  width = 6, side = "left", pad = "0")
  )

summary(nchar(df_final$COD_COM))
summary(nchar(df_final$COD_PROV))
summary(nchar(df_final$COD_REG))

# Assicuriamoci che i codici siano coerenti (padding e trim)
df_final <- df_final %>%
  mutate(
    COD_COM  = str_pad(str_trim(COD_COM), 6, "left", "0"),
    COD_PROV = str_pad(str_trim(COD_PROV), 3, "left", "0"),
    COD_REG  = str_pad(str_trim(COD_REG), 2, "left", "0")
  )

comuni_ref <- st_read("C:/Users/samsung/Documents/MAGISTRALE/PRIMO ANNO/APPLIED STATISTICS/GRINS Indice SSN/Datasets/ISTAT shape file/Limiti2021/Com2021/Com2021.shp") %>% 
  st_drop_geometry() %>%
  transmute(
    COD_COM = str_pad(as.character(PRO_COM), 6, "left", "0"),
    COD_REG = str_pad(as.character(COD_REG), 2, "left", "0")
  ) %>%
  distinct(COD_COM, .keep_all = TRUE)

df_final <- df_final %>%
  left_join(comuni_ref, by = "COD_COM")


df_final <- df_final %>% select(COD_COM, COD_PROV, COD_REG.y, ANNO, POPOLAZIONE) %>%
   rename(COD_REG=COD_REG.y)

colSums(is.na(df_final[c("COD_PROV", "COD_REG")]))

write.csv(df_final, "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop1982-2019_adj.csv",
          row.names = FALSE
)





