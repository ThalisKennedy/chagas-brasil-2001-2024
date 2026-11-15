# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SIM/DOBR - CHAGAS/DOBR - CHAGAS - PARTE 4.4 - PÓS SATSCAN.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SIM/DOBR - CHAGAS"))

library(sf)
library(dplyr)
library(writexl)

satscan_muni <- st_read(file.path(DIR_DADOS, "Análises/Parte 4 - Análise Espacial/SIM/Satscan_Chagas/satscan_mort_resultados_chagas.gis.dbf"), quiet = TRUE)

satscan_stats <- st_read(file.path(DIR_DADOS, "Análises/Parte 4 - Análise Espacial/SIM/Satscan_Chagas/satscan_mort_resultados_chagas.col.dbf"), quiet = TRUE)

resultados_clusters <- satscan_muni %>%
  st_drop_geometry() %>% 
  select(LOC_ID = 1, CLUSTER) %>% 
  left_join(
    st_drop_geometry(satscan_stats) %>% select(CLUSTER, P_VALUE, REL_RISK), 
    by = "CLUSTER"
  )

mapa_final_satscan <- mapa_valido %>%
  left_join(resultados_clusters, by = c("COD_MUN" = "LOC_ID")) %>%
  mutate(
    CLASSIFICACAO_SCAN = case_when(
      is.na(CLUSTER) ~ "Ausência de Clusters",
      CLUSTER == 1 & P_VALUE <= 0.001 ~ "Cluster Primário (p < 0,001)",
      CLUSTER > 1 & P_VALUE <= 0.001 ~ "Clusters Secundários (p < 0,001)",
      CLUSTER > 1 & P_VALUE <= 0.05 ~ "Clusters Secundários (p < 0,05)",
      P_VALUE > 0.05 ~ "Clusters Não Significativos",
      TRUE ~ "Ausência de Clusters"
    ),

    CLASSIFICACAO_SCAN = factor(CLASSIFICACAO_SCAN, 
                                levels = c("Cluster Primário (p < 0,001)",
                                           "Clusters Secundários (p < 0,001)",
                                           "Clusters Secundários (p < 0,05)",
                                           "Clusters Não Significativos",
                                           "Ausência de Clusters"))
  )

st_write(
  obj = mapa_final_satscan, 
  dsn = file.path(DIR_DADOS, "Análises/Parte 4 - Análise Espacial/SIM/Satscan_Chagas/mapa_chagas_satscan_final.gpkg"), 
  layer = "clusters_satscan",
  delete_dsn = TRUE
)

contagem_municipios <- satscan_muni %>%
  st_drop_geometry() %>%
  group_by(CLUSTER) %>%
  summarise(NUM_MUNICIPIOS = n(), .groups = "drop")

tabela_satscan_formatada <- satscan_stats %>%
  st_drop_geometry() %>%
  left_join(contagem_municipios, by = "CLUSTER") %>%
  select(
    Cluster = CLUSTER,
    `Nº de Municípios` = NUM_MUNICIPIOS,
    `Raio (km)` = RADIUS,
    `Nº de Casos` = OBSERVED,
    `Nº Esperado de Casos` = EXPECTED,
    `RR*` = REL_RISK,
    `LLR**` = LLR,
    `Valor de p` = P_VALUE
  ) %>%
  mutate(
    `Valor de p` = if_else(`Valor de p` < 0.001, "<0,001", as.character(round(`Valor de p`, 3)))
  ) %>%
  arrange(Cluster)

write_xlsx(tabela_satscan_formatada, file.path(DIR_DADOS, "Análises/Parte 4 - Análise Espacial/SIM/Satscan_Chagas/Tabela_Clusters_SaTScan.xlsx"))