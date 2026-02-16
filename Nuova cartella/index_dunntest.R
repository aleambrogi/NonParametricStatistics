library(dplyr)
library(sf)
library(ggplot2)
library(FSA)  # per dunnTest se serve post-hoc
library(ggsignif)
# -----------------------------------------------------------
# 1. Carico dataset stress
# -----------------------------------------------------------
df <- read.csv("C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/dataset_finale_stress_2021_2024.csv")

# Prendo solo colonne di interesse: COMUNE e ultima colonna (stress)
stress_col <- names(df)[ncol(df)]

# -----------------------------------------------------------
# 2. Carico shapefile province
# -----------------------------------------------------------
province <- st_read("C:/Users/samsung/Documents/MAGISTRALE/PRIMO ANNO/APPLIED STATISTICS/GRINS Indice SSN/Datasets/ISTAT shape file/Limiti2021/ProvCM2021/ProvCM2021.shp") %>%
  st_transform(4326) %>%
  st_make_valid()

# Assumo che nello shapefile ci siano colonne tipo: "PROV_NAME" e "REGIONE"
# Se i nomi sono diversi, cambiarli

# -----------------------------------------------------------
# 3. Join spaziale per aggiungere provincia e regione
# -----------------------------------------------------------
comuni <- st_read("C:\\Users\\samsung\\Documents\\MAGISTRALE\\PRIMO ANNO\\APPLIED STATISTICS\\GRINS Indice SSN\\Scripts\\DATASET_DIST\\Com2021\\Com2021.shp")
comuni <- st_transform(comuni, 4326)
comuni <- st_make_valid(comuni)

comuni$COMUNE <- toupper(trimws(comuni$COMUNE))
# 3. Aggiungo stress al dataset dei comuni tramite merge su nome comune
df_sf <- comuni %>%
  left_join(df %>% select(COMUNE, all_of(stress_col)), by = "COMUNE")

# 5. Join spaziale per aggiungere provincia e regione
df_prov <- st_join(df_sf, province, join = st_within)

# Controllo
sum(is.na(df_prov$DEN_PROV))  # punti senza provincia
sum(is.na(df_prov$DEN_REG))   # punti senza regione
# -----------------------------------------------------------
# 4. Kruskal-Wallis test tra regioni
# -----------------------------------------------------------
kruskal_test <- kruskal.test(df_prov[[stress_col]] ~ df_prov$COD_REG.x)
print(kruskal_test)

# -----------------------------------------------------------
# 5. Boxplot per storytelling
# -----------------------------------------------------------
ggplot(df_prov, aes(x = COD_REG.x, y = stress_col)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribuzione Stress Index per Regione",
       x = "Regione",
       y = "Stress Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_prov$COD_REG.x <- as.factor(df_prov$COD_REG.x)


# Dunn test tra regioni
dunn_res <- dunnTest(stress_2024 ~ COD_REG.x, data = df_prov, method = "bonferroni")
res_table <- dunn_res$res

# Seleziono solo confronti significativi
sig_pairs <- res_table[res_table$P.adj < 0.05, c("Comparison", "P.adj")]

# Boxplot con annotazioni
ggplot(df_prov, aes(x = COD_REG.x, y = stress_2024)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribuzione Stress Index per Regione",
       y = "Stress Index",
       x = "Regione") +
  # Aggiungi segmenti per confronti significativi
  geom_signif(
    comparisons = strsplit(sig_pairs$Comparison, " - "),
    annotations = "*",
    tip_length = 0.02
  )


# Nel file originale dove hai df_prov
saveRDS(df_prov, "df_prov_2024.rds")  # salva l’oggetto così com’è
