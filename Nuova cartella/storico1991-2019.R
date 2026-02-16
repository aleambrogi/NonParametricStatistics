library(dplyr)
library(sf)
library(tidyr)

df_old <- read.csv("C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop1992-2000.csv", sep = ';')

df_old <- df_old %>%
  select(COD_COM, ANNO, POPOLAZIONE)

df_clean <- read.csv("C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop2001-2019_adj.csv")

df_old <- df_old %>%
  filter(ANNO < 2001)

df_final <- bind_rows(df_old, df_clean)

df_final <- df_final %>%
  group_by(COD_COM) %>%
  fill(COD_PROV, COD_REG, .direction = "downup") %>%
  ungroup()

df_final <- df_final %>%
  arrange(COD_COM, ANNO)


write.csv(df_final, "C:/Users/samsung/Documents/MAGISTRALE/SECONDO ANNO/NONPARAMETRIC STATISTICS/PROJECT/STORICO/pop1991-2019_adj.csv",
          row.names = FALSE
)
