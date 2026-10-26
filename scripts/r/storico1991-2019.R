# Run from the repository root. See scripts/r/README.md for external inputs.
source(file.path("scripts", "r", "project_paths.R"))

library(dplyr)
library(sf)
library(tidyr)

df_old <- read.csv(project_file("STORICO/pop1992-2000.csv"), sep = ';')

df_old <- df_old %>%
  select(COD_COM, ANNO, POPOLAZIONE)

df_clean <- read.csv(project_file("STORICO/pop2001-2019_adj.csv"))

df_old <- df_old %>%
  filter(ANNO < 2001)

df_final <- bind_rows(df_old, df_clean)

df_final <- df_final %>%
  group_by(COD_COM) %>%
  fill(COD_PROV, COD_REG, .direction = "downup") %>%
  ungroup()

df_final <- df_final %>%
  arrange(COD_COM, ANNO)


write.csv(df_final, project_file("STORICO/pop1991-2019_adj.csv"),
          row.names = FALSE
)
