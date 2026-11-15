# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/MapBiomas/MapBiomas v8.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/MapBiomas"))

# ==============================================================================
# 1. SESSÃO 1: Degradação Habitat Natural (MapBiomas - Média Histórica 2001-2022)
# ==============================================================================

library(tidyverse)
library(readxl)

pasta_mapbiomas      <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Bancos/MapBiomas/Dados")
pasta_saida          <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/MapBiomas")
arquivo_mestre       <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Censo 2022/Censo2022_Agregado_Municipios.csv")
arquivo_saida_degrad <- file.path(pasta_saida, "MapBiomas_Degradacao_Agregado_Municipios.csv")

arquivo_xlsx_degrad  <- list.files(pasta_mapbiomas, pattern = "brazil-city-degradation-frequency.*\\.xlsx$", full.names = TRUE)[1]

cat("\nA processar série histórica de Degradação (2001-2022)...\n")

anos_estudo <- as.character(2001:2022)

df_degrad_bruto <- read_excel(arquivo_xlsx_degrad) %>%
  filter(level_0 == "Natural") %>%
  mutate(codigo_municipio = as.character(GEOCODE), value_num = as.numeric(value)) %>%
  select(codigo_municipio, value_num, any_of(anos_estudo))

df_mapbiomas_indicador <- df_degrad_bruto %>%
  pivot_longer(cols = any_of(anos_estudo), names_to = "ano", values_to = "area_ha") %>%
  mutate(area_ha = as.numeric(area_ha)) %>%
  group_by(codigo_municipio, ano) %>%
  summarise(
    area_natural_total     = sum(area_ha[value_num == 0], na.rm = TRUE),
    area_natural_degradada = sum(area_ha[value_num >= 1 & value_num <= 5], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(perc_degradado_ano = if_else(area_natural_total > 0, (area_natural_degradada / area_natural_total) * 100, 0)) %>%
  group_by(codigo_municipio) %>%
  summarise(mapbiomas_degradacao_media_historica_perc = mean(perc_degradado_ano, na.rm = TRUE), .groups = "drop") %>%
  mutate(mapbiomas_degradacao_media_historica_perc = if_else(mapbiomas_degradacao_media_historica_perc > 100, 100, mapbiomas_degradacao_media_historica_perc))

df_gabarito <- read_csv(arquivo_mestre, col_select = c("codigo_municipio"), col_types = cols(.default = "c"))
if(!dir.exists(pasta_saida)) dir.create(pasta_saida, recursive = TRUE)

df_degrad_final <- df_gabarito %>%
  left_join(df_mapbiomas_indicador, by = "codigo_municipio") %>%
  mutate(mapbiomas_degradacao_media_historica_perc = replace_na(mapbiomas_degradacao_media_historica_perc, 0))

write_csv(df_degrad_final, arquivo_saida_degrad)

# ==============================================================================
# 2. SESSÃO 2: Efeito de Borda / Fragmentação (MapBiomas - Média 2001-2022)
# ==============================================================================

arquivo_saida_borda <- file.path(pasta_saida, "MapBiomas_Bordas_Agregado_Municipios.csv")
arquivo_xlsx_borda  <- list.files(pasta_mapbiomas, pattern = "brazil-city-edges.*\\.xlsx$", full.names = TRUE)[1]

cat("A processar Efeito de Borda (2001-2022)...\n")

df_borda_bruto <- read_excel(arquivo_xlsx_borda) %>%
  filter(level_0 == "Natural" & edge_size == 300) %>%
  mutate(codigo_municipio = as.character(GEOCODE)) %>%
  select(codigo_municipio, key, any_of(anos_estudo))

df_mapbiomas_indicador <- df_borda_bruto %>%
  pivot_longer(cols = any_of(anos_estudo), names_to = "ano", values_to = "area_ha") %>%
  mutate(area_ha = as.numeric(area_ha)) %>%
  group_by(codigo_municipio, ano) %>%
  summarise(
    area_total = sum(area_ha[key == "reference"], na.rm = TRUE),
    area_borda = sum(area_ha[key == "value"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(perc_borda_ano = if_else(area_total > 0, (area_borda / area_total) * 100, 0)) %>%
  group_by(codigo_municipio) %>%
  summarise(mapbiomas_borda_media_historica_perc = mean(perc_borda_ano, na.rm = TRUE), .groups = "drop") %>%
  mutate(mapbiomas_borda_media_historica_perc = if_else(mapbiomas_borda_media_historica_perc > 100, 100, mapbiomas_borda_media_historica_perc))

df_borda_final <- df_gabarito %>%
  left_join(df_mapbiomas_indicador, by = "codigo_municipio") %>%
  mutate(mapbiomas_borda_media_historica_perc = replace_na(mapbiomas_borda_media_historica_perc, 0))

write_csv(df_borda_final, arquivo_saida_borda)

# ==============================================================================
# 3. SESSÃO 3: Frequência de Fogo/Queimadas (MapBiomas - Média 2001-2022)
# ==============================================================================

arquivo_saida_fogo <- file.path(pasta_saida, "MapBiomas_Fogo_Agregado_Municipios.csv")
arquivo_xlsx_fogo  <- list.files(pasta_mapbiomas, pattern = "brazil-city-fire.*\\.xlsx$", full.names = TRUE)[1]

cat("A processar Fogo e Queimadas (2001-2022)...\n")

df_fogo_bruto <- read_excel(arquivo_xlsx_fogo) %>%
  filter(level_0 == "Natural" & theme == "frequency") %>%
  mutate(codigo_municipio = as.character(GEOCODE), value_num = as.numeric(value)) %>%
  select(codigo_municipio, key, value_num, any_of(anos_estudo))

df_mapbiomas_indicador <- df_fogo_bruto %>%
  pivot_longer(cols = any_of(anos_estudo), names_to = "ano", values_to = "area_ha") %>%
  mutate(area_ha = as.numeric(area_ha)) %>%
  group_by(codigo_municipio, ano) %>%
  summarise(
    area_total    = sum(area_ha[key == "reference" & value_num == 0], na.rm = TRUE),
    area_queimada = sum(area_ha[key == "value" & value_num > 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(perc_fogo_ano = if_else(area_total > 0, (area_queimada / area_total) * 100, 0)) %>%
  group_by(codigo_municipio) %>%
  summarise(mapbiomas_fogo_media_historica_perc = mean(perc_fogo_ano, na.rm = TRUE), .groups = "drop")

df_fogo_final <- df_gabarito %>%
  left_join(df_mapbiomas_indicador, by = "codigo_municipio") %>%
  mutate(mapbiomas_fogo_media_historica_perc = replace_na(mapbiomas_fogo_media_historica_perc, 0))

write_csv(df_fogo_final, arquivo_saida_fogo)

# ==============================================================================
# 4. SESSÃO 4: Vegetação Secundária / Capoeira (MapBiomas Col. 10 - 2001-2024)
# ==============================================================================

arquivo_saida_vegsec <- file.path(pasta_saida, "MapBiomas_VegSecundaria_v10_Agregado_Municipios.csv")
arquivo_xlsx_vegsec  <- list.files(pasta_mapbiomas, pattern = "MAPBIOMAS_BRAZIL-COL.10-SECONDARY_VEGETATION.*\\.xlsx$", full.names = TRUE)[1]

cat("A processar Vegetação Secundária (Coleção 10)...\n")
anos_estudo_10 <- as.character(2001:2024)

df_vegsec_bruto <- read_excel(arquivo_xlsx_vegsec, sheet = "SECONDARY_VEGETATION") %>%
  mutate(codigo_municipio = as.character(geocode)) %>%
  filter(class_level_0 == "Natural") %>%
  select(codigo_municipio, transition_name, any_of(anos_estudo_10))

df_mapbiomas_indicador <- df_vegsec_bruto %>%
  pivot_longer(cols = any_of(anos_estudo_10), names_to = "ano", values_to = "area_ha") %>%
  mutate(area_ha = as.numeric(area_ha)) %>%
  group_by(codigo_municipio, ano) %>%
  summarise(
    area_secundaria = sum(area_ha[transition_name %in% c("Veg. Secundária", "Recuperação para Veg. Secundária")], na.rm = TRUE),
    area_primaria   = sum(area_ha[transition_name == "Veg. Primária"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    area_natural_total = area_secundaria + area_primaria,
    perc_vegsec_ano    = if_else(area_natural_total > 0, (area_secundaria / area_natural_total) * 100, 0)
  ) %>%
  group_by(codigo_municipio) %>%
  summarise(mapbiomas_vegsec_media_historica_perc = mean(perc_vegsec_ano, na.rm = TRUE), .groups = "drop")

df_vegsec_final <- df_gabarito %>%
  left_join(df_mapbiomas_indicador, by = "codigo_municipio") %>%
  mutate(mapbiomas_vegsec_media_historica_perc = replace_na(mapbiomas_vegsec_media_historica_perc, 0))

write_csv(df_vegsec_final, arquivo_saida_vegsec)

# ==============================================================================
# 5. SESSÃO 5: Supressão/Desmatamento (MapBiomas Col. 10 - Hectares Brutos)
# ==============================================================================

arquivo_saida_desmat <- file.path(pasta_saida, "MapBiomas_Desmatamento_Agregado_Municipios.csv")
arquivo_xlsx_desmat  <- list.files(pasta_mapbiomas, pattern = "MAPBIOMAS_BRAZIL_DEFORESTATION.*\\.xlsx$", full.names = TRUE)[1]

cat("A processar Desmatamento (Coleção 10)...\n")

df_desmat_bruto <- read_excel(arquivo_xlsx_desmat, sheet = "DEFORESTATION") %>%
  mutate(codigo_municipio = as.character(geocode_municipality)) %>%
  select(codigo_municipio, transition_name, any_of(anos_estudo_10))

df_mapbiomas_indicador <- df_desmat_bruto %>%
  pivot_longer(cols = any_of(anos_estudo_10), names_to = "ano", values_to = "area_desmatada_ha") %>%
  mutate(area_desmatada_ha = as.numeric(area_desmatada_ha)) %>%
  group_by(codigo_municipio, ano) %>%
  summarise(desmatamento_total_ano_ha = sum(area_desmatada_ha, na.rm = TRUE), .groups = "drop") %>%
  group_by(codigo_municipio) %>%
  summarise(mapbiomas_desmatamento_media_anual_ha = mean(desmatamento_total_ano_ha, na.rm = TRUE), .groups = "drop")

df_desmat_final <- df_gabarito %>%
  left_join(df_mapbiomas_indicador, by = "codigo_municipio") %>%
  mutate(mapbiomas_desmatamento_media_anual_ha = replace_na(mapbiomas_desmatamento_media_anual_ha, 0))

write_csv(df_desmat_final, arquivo_saida_desmat)

# ==============================================================================
# 6. SESSÃO 6: Índice de Antropização do Solo (MapBiomas - Média 2001-2024)
# ==============================================================================

arquivo_saida_cob <- file.path(pasta_saida, "MapBiomas_Cobertura_Agregado_Municipios.csv")
arquivo_xlsx_cob  <- list.files(pasta_mapbiomas, pattern = "MAPBIOMAS_BRAZIL-COVERAGE_STATISTICS.*\\.xlsx$", full.names = TRUE)[1]
arquivo_xlsx_desmat <- list.files(pasta_mapbiomas, pattern = "MAPBIOMAS_BRAZIL_DEFORESTATION.*\\.xlsx$", full.names = TRUE)[1]

cat("A processar Antropização (Coleção 10.1)...\n")

dicionario_ibge <- read_excel(arquivo_xlsx_desmat, sheet = "DEFORESTATION") %>%
  select(municipality, state, geocode_municipality) %>%
  distinct() %>%
  mutate(codigo_municipio = as.character(geocode_municipality)) %>%
  select(municipality, state, codigo_municipio)

df_cobertura_bruto <- read_excel(arquivo_xlsx_cob, sheet = "COVERAGE_10.1") %>%
  left_join(dicionario_ibge, by = c("municipality", "state")) %>%
  filter(!is.na(codigo_municipio)) %>% # Filtra falhas nominais de join
  select(codigo_municipio, class_level_1, class_level_2, any_of(anos_estudo_10))

df_mapbiomas_indicador <- df_cobertura_bruto %>%
  pivot_longer(cols = any_of(anos_estudo_10), names_to = "ano", values_to = "area_ha") %>%
  mutate(area_ha = as.numeric(area_ha)) %>%
  group_by(codigo_municipio, ano) %>%
  summarise(
    area_total_municipio = sum(area_ha, na.rm = TRUE),
    area_farming = sum(area_ha[class_level_1 == "3. Farming"], na.rm = TRUE),
    area_urbana  = sum(area_ha[class_level_2 == "4.2. Urban Area"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    area_antropizada_total = area_farming + area_urbana,
    perc_antropizacao_ano  = if_else(area_total_municipio > 0, (area_antropizada_total / area_total_municipio) * 100, 0)
  ) %>%
  group_by(codigo_municipio) %>%
  summarise(mapbiomas_antropizacao_media_historica_perc = mean(perc_antropizacao_ano, na.rm = TRUE), .groups = "drop")

df_cobertura_final <- df_gabarito %>%
  left_join(df_mapbiomas_indicador, by = "codigo_municipio") %>%
  mutate(mapbiomas_antropizacao_media_historica_perc = replace_na(mapbiomas_antropizacao_media_historica_perc, 0))

write_csv(df_cobertura_final, arquivo_saida_cob)

# ==============================================================================
# SESSÃO FINAL MAPBIOMAS: Fusão e Transição para Taxas Relativas
# ==============================================================================

library(sf)

arquivo_saida_final <- file.path(pasta_saida, "MapBiomas_Agregado_Municipios_FINAL.csv")
arq_mapa <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Desfechos/TAXAS_MORTALIDADE_CHAGAS_PRONTAS.gpkg")

cat("\nA fundir as matrizes e converter Hectares em Percentagem Territorial...\n")

df_area_mun <- st_read(arq_mapa, quiet = TRUE) %>%
  mutate(
    codigo_municipio = as.character(as.integer(code_muni)),
    area_ha_mun = as.numeric(st_area(geom)) / 10000
  ) %>%
  st_drop_geometry() %>%
  select(codigo_municipio, area_ha_mun)

df_master_mapbiomas <- df_gabarito %>%
  left_join(df_degrad_final, by = "codigo_municipio") %>%
  left_join(df_borda_final, by = "codigo_municipio") %>%
  left_join(df_fogo_final, by = "codigo_municipio") %>%
  left_join(df_desmat_final, by = "codigo_municipio") %>%
  left_join(df_vegsec_final, by = "codigo_municipio") %>%
  left_join(df_cobertura_final, by = "codigo_municipio") %>%
  left_join(df_area_mun, by = "codigo_municipio") %>%
  mutate(
    mapbiomas_desmatamento_media_anual_perc = if_else(
      area_ha_mun > 0, 
      (mapbiomas_desmatamento_media_anual_ha / area_ha_mun) * 100, 
      0
    )
  ) %>%
  select(-mapbiomas_desmatamento_media_anual_ha, -area_ha_mun) %>%
  mutate(mapbiomas_desmatamento_media_anual_perc = replace_na(mapbiomas_desmatamento_media_anual_perc, 0))

write_csv(df_master_mapbiomas, arquivo_saida_final)

cat("\n--- DIAGNÓSTICO DO DATASET MAPBIOMAS ---\n")
cat("Total de Municípios:", nrow(df_master_mapbiomas), "\n")
cat("Total de Variáveis Ecológicas:", ncol(df_master_mapbiomas), "\n")
glimpse(df_master_mapbiomas)