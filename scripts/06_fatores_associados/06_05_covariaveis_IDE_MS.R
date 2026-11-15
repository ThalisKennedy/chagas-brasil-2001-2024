# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/IDE-MS/IDE-MS v15.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/IDE-MS"))

library(tidyverse)
library(sf)
library(lwgeom)

sf_use_s2(FALSE)

dir_idems  <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Bancos/IDE-MS/Dados")
dir_shapes <- file.path(DIR_DADOS, "Análises/Parte 4 - Análise Espacial/Shapes")
dir_censo  <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Censo 2022")
dir_desf   <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Desfechos")
dir_saida  <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/IDE-MS")

arq_malha_mun <- file.path(dir_shapes, "BR_Municipios_2024.shp")
arq_mestre    <- file.path(dir_censo, "Censo2022_Agregado_Municipios.csv")

# ==============================================================================
# 1. SESSÃO 1: BIOMAS
# ==============================================================================
cat("\n[1/14] Processando Biomas...\n")
malha_mun <- st_read(arq_malha_mun, quiet = TRUE) %>% 
  rename(codigo_municipio = CD_MUN) %>% st_make_valid() 

mapa_biomas <- st_read(file.path(dir_idems, "Biomas/vw_geo_biomasPolygon.shp"), quiet = TRUE) %>%
  st_transform(st_crs(malha_mun)) %>% st_make_valid()

df_bioma_bruto <- st_intersection(malha_mun, mapa_biomas) %>%
  mutate(area_interseccao = st_area(.)) %>%
  group_by(codigo_municipio) %>%
  slice_max(order_by = area_interseccao, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  select(codigo_municipio, cod_bioma = cd_bioma, nome_bioma = bioma) %>%
  mutate(codigo_municipio = str_extract(as.character(codigo_municipio), "^\\d{7}"))

df_gabarito <- read_csv(arq_mestre, col_select = c("codigo_municipio"), col_types = cols(.default = "c"))

df_bioma_final <- df_gabarito %>%
  left_join(df_bioma_bruto, by = "codigo_municipio") %>%
  mutate(
    cod_bioma = replace_na(cod_bioma, "0"),
    nome_bioma = replace_na(nome_bioma, "Indefinido")
  )
write_csv(df_bioma_final, file.path(dir_saida, "Bioma_Predominante_Municipios.csv"))

# ==============================================================================
# 2. SESSÃO 2: Leitos de UTI Habilitados
# ==============================================================================
cat("[2/14] Processando Leitos de UTI...\n")
arq_uti <- list.files(file.path(dir_idems, "Leitos de UTI Habilitados pelo Ministério da Saúde"), pattern = "\\.shp$", full.names = TRUE)[1]

df_uti_agregado <- st_read(arq_uti, quiet = TRUE) %>%
  st_drop_geometry() %>%
  group_by(codigo_mun_6 = as.character(co_ibge)) %>%
  summarise(total_leitos_uti = sum(qt_sus, na.rm = TRUE), .groups = "drop")

df_uti_final <- df_gabarito %>%
  mutate(codigo_mun_6 = str_sub(codigo_municipio, 1, 6)) %>%
  left_join(df_uti_agregado, by = "codigo_mun_6") %>%
  mutate(total_leitos_uti = replace_na(total_leitos_uti, 0)) %>%
  select(codigo_municipio, total_leitos_uti)
write_csv(df_uti_final, file.path(dir_saida, "UTI_Agregado_Municipios.csv"))

# ==============================================================================
# 3. SESSÃO 3: UPAs 24h Habilitadas
# ==============================================================================
cat("[3/14] Processando UPAs...\n")
arq_upa <- list.files(file.path(dir_idems, "UPA 24hs habilitadas e cadastradas no CNES"), pattern = "\\.shp$", full.names = TRUE)[1]

df_upa_pontos <- st_read(arq_upa, quiet = TRUE) %>% st_transform(st_crs(malha_mun)) %>% st_make_valid()

df_upa_agregado <- st_join(df_upa_pontos, malha_mun) %>%
  st_drop_geometry() %>%
  group_by(codigo_municipio) %>%
  summarise(total_upas = n(), .groups = "drop") %>%
  mutate(codigo_municipio = str_extract(as.character(codigo_municipio), "^\\d{7}")) %>%
  filter(!is.na(codigo_municipio))

df_upa_final <- df_gabarito %>%
  left_join(df_upa_agregado, by = "codigo_municipio") %>%
  mutate(total_upas = replace_na(total_upas, 0))
write_csv(df_upa_final, file.path(dir_saida, "UPA_Agregado_Municipios.csv"))

# ==============================================================================
# 4. SESSÃO 4: Hospitais que Atendem ao SUS
# ==============================================================================
cat("[4/14] Processando Hospitais SUS...\n")
arq_hosp <- list.files(file.path(dir_idems, "Hospitais que atendem ao SUS"), pattern = "\\.shp$", full.names = TRUE)[1]

df_hosp_pontos <- st_read(arq_hosp, quiet = TRUE) %>% st_transform(st_crs(malha_mun)) %>% st_make_valid()

df_hosp_agregado <- st_join(df_hosp_pontos, malha_mun) %>%
  st_drop_geometry() %>%
  group_by(codigo_municipio) %>%
  summarise(total_hospitais_sus = n(), .groups = "drop") %>%
  mutate(codigo_municipio = str_extract(as.character(codigo_municipio), "^\\d{7}")) %>%
  filter(!is.na(codigo_municipio))

df_hosp_final <- df_gabarito %>%
  left_join(df_hosp_agregado, by = "codigo_municipio") %>%
  mutate(total_hospitais_sus = replace_na(total_hospitais_sus, 0))
write_csv(df_hosp_final, file.path(dir_saida, "Hospitais_SUS_Agregado_Municipios.csv"))

# ==============================================================================
# 5. SESSÃO 5: Ambulâncias SAMU 192 (Capacidade Regional)
# ==============================================================================
cat("[5/14] Processando Ambulâncias SAMU...\n")
arq_samu <- list.files(file.path(dir_idems, "Ambulâncias SAMU 192 por Município"), pattern = "\\.shp$", full.names = TRUE)[1]

mapa_samu <- st_read(arq_samu, quiet = TRUE) %>% st_transform(st_crs(malha_mun)) %>% st_make_valid()
malha_pontos <- st_point_on_surface(malha_mun)

df_samu_cruzado <- st_join(malha_pontos, mapa_samu) %>%
  st_drop_geometry() %>%
  mutate(codigo_municipio = str_extract(as.character(codigo_municipio), "^\\d{7}")) %>%
  filter(!is.na(codigo_municipio)) %>%
  group_by(codigo_municipio) %>%
  summarise(capacidade_regional_samu = max(valor, na.rm = TRUE), .groups = "drop") %>%
  mutate(capacidade_regional_samu = if_else(is.infinite(capacidade_regional_samu), NA_real_, capacidade_regional_samu))

df_samu_final <- df_gabarito %>%
  left_join(df_samu_cruzado, by = "codigo_municipio") %>%
  mutate(capacidade_regional_samu = replace_na(capacidade_regional_samu, 0))
write_csv(df_samu_final, file.path(dir_saida, "SAMU_Agregado_Municipios.csv"))

# ==============================================================================
# 6. SESSÃO 6: Adesão ao Programa Brasil Saudável
# ==============================================================================
cat("[6/14] Processando Adesão Brasil Saudável...\n")
arq_pbs <- list.files(file.path(dir_idems, "Adesão ao Programa Brasil Saudável"), pattern = "\\.shp$", full.names = TRUE)[1]

df_pbs_bruto <- st_read(arq_pbs, quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(codigo_mun_6 = as.character(co_ibge)) %>%
  select(codigo_mun_6, adesao_brasil_saudavel = pbs)

df_pbs_final <- df_gabarito %>%
  mutate(codigo_mun_6 = str_sub(codigo_municipio, 1, 6)) %>%
  left_join(df_pbs_bruto, by = "codigo_mun_6") %>%
  mutate(
    adesao_brasil_saudavel = replace_na(adesao_brasil_saudavel, "Não"),
    adesao_brasil_saudavel = if_else(adesao_brasil_saudavel == "Sim", 1, 0)
  ) %>%
  select(codigo_municipio, adesao_brasil_saudavel)
write_csv(df_pbs_final, file.path(dir_saida, "Brasil_Saudavel_Agregado_Municipios.csv"))

# ==============================================================================
# 7. SESSÃO 7: Assentamentos da Reforma Agrária
# ==============================================================================
cat("[7/14] Processando Assentamentos (Famílias)...\n")
arq_reforma <- list.files(file.path(dir_idems, "Assentamento da Reforma Agrária"), pattern = "\\.shp$", full.names = TRUE)[1]

mapa_reforma <- st_read(arq_reforma, quiet = TRUE) %>% st_transform(st_crs(malha_mun)) %>% st_make_valid()
pontos_reforma <- st_point_on_surface(mapa_reforma)

df_reforma_agregado <- st_join(pontos_reforma, malha_mun) %>%
  st_drop_geometry() %>%
  distinct() %>% 
  group_by(codigo_municipio) %>%
  summarise(total_familias_assentadas = sum(num_famili, na.rm = TRUE), .groups = "drop") %>%
  mutate(codigo_municipio = str_extract(as.character(codigo_municipio), "^\\d{7}")) %>%
  filter(!is.na(codigo_municipio))

df_reforma_final <- df_gabarito %>%
  left_join(df_reforma_agregado, by = "codigo_municipio") %>%
  mutate(total_familias_assentadas = replace_na(total_familias_assentadas, 0))
write_csv(df_reforma_final, file.path(dir_saida, "Reforma_Agraria_Agregado_Municipios.csv"))

# ==============================================================================
# 8. SESSÃO 8: Atendimentos Ambulatoriais Cardiovasculares
# ==============================================================================
cat("[8/14] Processando Atend. Cardiovasculares...\n")
arq_cardio <- list.files(file.path(dir_idems, "Atendimentos ambulatoriais por doenças cardiovasculares"), pattern = "\\.shp$", full.names = TRUE)[1]

df_cardio_bruto <- st_read(arq_cardio, quiet = TRUE) %>%
  st_drop_geometry() %>%
  group_by(codigo_mun_6 = as.character(co_ibge)) %>%
  summarise(atendimentos_cardio = sum(nu_ocorren, na.rm = TRUE), .groups = "drop")

df_cardio_final <- df_gabarito %>%
  mutate(codigo_mun_6 = str_sub(codigo_municipio, 1, 6)) %>%
  left_join(df_cardio_bruto, by = "codigo_mun_6") %>%
  mutate(atendimentos_cardio = replace_na(atendimentos_cardio, 0)) %>%
  select(codigo_municipio, atendimentos_cardio)
write_csv(df_cardio_final, file.path(dir_saida, "Ambulatorio_Cardio_Agregado_Municipios.csv"))

# ==============================================================================
# 9. SESSÃO 9: Internações Hospitalares por Doenças Cardiovasculares
# ==============================================================================
cat("[9/14] Processando Internações Cardiovasculares...\n")
arq_intern <- list.files(file.path(dir_idems, "Internações hospitalares por doenças cardiovasculares"), pattern = "\\.shp$", full.names = TRUE)[1]

df_intern_bruto <- st_read(arq_intern, quiet = TRUE) %>%
  st_drop_geometry() %>%
  group_by(codigo_mun_6 = as.character(co_ibge)) %>%
  summarise(internacoes_cardio = sum(nu_ocorren, na.rm = TRUE), .groups = "drop")

df_intern_final <- df_gabarito %>%
  mutate(codigo_mun_6 = str_sub(codigo_municipio, 1, 6)) %>%
  left_join(df_intern_bruto, by = "codigo_mun_6") %>%
  mutate(internacoes_cardio = replace_na(internacoes_cardio, 0)) %>%
  select(codigo_municipio, internacoes_cardio)
write_csv(df_intern_final, file.path(dir_saida, "Internacoes_Cardio_Agregado_Municipios.csv"))

# ==============================================================================
# 10. SESSÃO 10: Cobertura de Agentes Comunitários de Saúde
# ==============================================================================
cat("[10/14] Processando Cobertura ACS...\n")
arq_acs <- list.files(file.path(dir_idems, "Cobertura Agentes Comunitários de Saúde"), pattern = "\\.shp$", full.names = TRUE)[1]

mapa_acs <- st_read(arq_acs, quiet = TRUE) %>% st_transform(st_crs(malha_mun)) %>% st_make_valid()

df_acs_cruzado <- st_join(malha_pontos, mapa_acs) %>%
  st_drop_geometry() %>%
  group_by(codigo_municipio) %>%
  summarise(cobertura_acs = mean(Cobertura, na.rm = TRUE), .groups = "drop") %>%
  mutate(codigo_municipio = str_extract(as.character(codigo_municipio), "^\\d{7}")) %>%
  filter(!is.na(codigo_municipio))

df_acs_final <- df_gabarito %>%
  left_join(df_acs_cruzado, by = "codigo_municipio") %>%
  mutate(cobertura_acs = replace_na(cobertura_acs, 0))
write_csv(df_acs_final, file.path(dir_saida, "Cobertura_ACS_Agregado_Municipios.csv"))

# ==============================================================================
# 11. SESSÃO 11: Cobertura Potencial da APS
# ==============================================================================
cat("[11/14] Processando Cobertura APS...\n")
arq_aps <- list.files(file.path(dir_idems, "Cobertura Potencial da Atenção Primária à Saúde"), pattern = "\\.shp$", full.names = TRUE)[1]

mapa_aps <- st_read(arq_aps, quiet = TRUE) %>% st_transform(st_crs(malha_mun)) %>% st_make_valid()

df_aps_cruzado <- st_join(malha_pontos, mapa_aps) %>%
  st_drop_geometry() %>%
  group_by(codigo_municipio) %>%
  summarise(cobertura_aps = mean(Cobertura, na.rm = TRUE), .groups = "drop") %>%
  mutate(codigo_municipio = str_extract(as.character(codigo_municipio), "^\\d{7}")) %>%
  filter(!is.na(codigo_municipio))

df_aps_final <- df_gabarito %>%
  left_join(df_aps_cruzado, by = "codigo_municipio") %>%
  mutate(cobertura_aps = replace_na(cobertura_aps, 0))
write_csv(df_aps_final, file.path(dir_saida, "Cobertura_APS_Agregado_Municipios.csv"))

# ==============================================================================
# 12. SESSÃO 12: Municípios Prioritários AdaptaCidades
# ==============================================================================
cat("[12/14] Processando AdaptaCidades...\n")
arq_adapta <- list.files(file.path(dir_idems, "Municípios Prioritários da AdaptaCidades - Ciclo Atual"), pattern = "\\.shp$", full.names = TRUE)[1]

df_adapta_bruto <- st_read(arq_adapta, quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(codigo_mun_6 = as.character(co_ibge), adapta_cidades_flag = 1) %>%
  distinct(codigo_mun_6, .keep_all = TRUE) %>%
  select(codigo_mun_6, adapta_cidades_flag)

df_adapta_final <- df_gabarito %>%
  mutate(codigo_mun_6 = str_sub(codigo_municipio, 1, 6)) %>%
  left_join(df_adapta_bruto, by = "codigo_mun_6") %>%
  mutate(adapta_cidades_flag = replace_na(adapta_cidades_flag, 0)) %>%
  select(codigo_municipio, adapta_cidades_flag)
write_csv(df_adapta_final, file.path(dir_saida, "AdaptaCidades_Agregado_Municipios.csv"))

# ==============================================================================
# 13. SESSÃO 13: Municípios da Amazônia Legal
# ==============================================================================
cat("[13/14] Processando Amazônia Legal...\n")
arq_amazonia <- list.files(file.path(dir_idems, "Municípios da Amazônia Legal"), pattern = "\\.shp$", full.names = TRUE)[1]

df_amazonia_bruto <- st_read(arq_amazonia, quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(codigo_mun_6 = as.character(co_ibge), amazonia_legal_flag = 1) %>%
  distinct(codigo_mun_6, .keep_all = TRUE) %>%
  select(codigo_mun_6, amazonia_legal_flag)

df_amazonia_final <- df_gabarito %>%
  mutate(codigo_mun_6 = str_sub(codigo_municipio, 1, 6)) %>%
  left_join(df_amazonia_bruto, by = "codigo_mun_6") %>%
  mutate(amazonia_legal_flag = replace_na(amazonia_legal_flag, 0)) %>%
  select(codigo_municipio, amazonia_legal_flag)
write_csv(df_amazonia_final, file.path(dir_saida, "Amazonia_Legal_Agregado_Municipios.csv"))

# ==============================================================================
# 14. SESSÃO 14: Consolidação e Conversão de Variáveis Absolutas em Taxas
# ==============================================================================
cat("\n[14/14] Consolidando Matriz IDE-MS e Calculando Taxas...\n")

arq_pop <- file.path(dir_desf, "POP_IBGE_MUNICIPIO_TRATADA.csv")
df_pop_2022 <- read_csv(arq_pop, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  filter(ANO_OBITO == "2022") %>%
  group_by(codigo_municipio = COD_MUN) %>%
  summarise(populacao_2022 = sum(as.numeric(populacao_residente), na.rm = TRUE), .groups = "drop")

arq_mapa <- file.path(dir_desf, "TAXAS_MORTALIDADE_CHAGAS_PRONTAS.gpkg")
df_area <- st_read(arq_mapa, quiet = TRUE) %>%
  mutate(
    codigo_municipio = as.character(as.integer(code_muni)),
    area_km2 = as.numeric(st_area(geom)) / 1000000 
  ) %>%
  st_drop_geometry() %>%
  select(codigo_municipio, area_km2)

arquivos_csv <- list.files(
  path = dir_saida, 
  pattern = "(.*_Agregado_Municipios|Bioma_Predominante_Municipios)\\.csv$", 
  full.names = TRUE
)

lista_tabelas <- map(arquivos_csv, ~ read_csv(.x, col_types = cols(codigo_municipio = "c"), show_col_types = FALSE))

df_master <- reduce(append(list(df_gabarito), lista_tabelas), left_join, by = "codigo_municipio") %>%
  left_join(df_pop_2022, by = "codigo_municipio") %>%
  left_join(df_area, by = "codigo_municipio")

df_master_taxas <- df_master %>%
  mutate(
    tx_hospitais_sus_100k = if_else(populacao_2022 > 0, (total_hospitais_sus / populacao_2022) * 100000, 0),
    tx_leitos_uti_100k    = if_else(populacao_2022 > 0, (total_leitos_uti / populacao_2022) * 100000, 0),
    tx_upas_100k          = if_else(populacao_2022 > 0, (total_upas / populacao_2022) * 100000, 0),
    
    tx_atend_cardio_10k   = if_else(populacao_2022 > 0, (atendimentos_cardio / populacao_2022) * 10000, 0),
    tx_intern_cardio_10k  = if_else(populacao_2022 > 0, (internacoes_cardio / populacao_2022) * 10000, 0),
    
    densidade_familias_assentadas_km2 = if_else(area_km2 > 0, total_familias_assentadas / area_km2, 0)
  ) %>%
  select(
    -total_hospitais_sus, -total_leitos_uti, -total_upas, 
    -atendimentos_cardio, -internacoes_cardio, -total_familias_assentadas,
    -populacao_2022, -area_km2
  )

arquivo_saida_final <- paste0(dir_saida, "/IDEMS_Agregado_Municipios_FINAL_CORRIGIDO.csv")
write_csv(df_master_taxas, arquivo_saida_final)

cat("\nDataset concluído com sucesso!\n")
cat("Linhas:", nrow(df_master_taxas), "\nColunas:", ncol(df_master_taxas), "\n")
glimpse(df_master_taxas)