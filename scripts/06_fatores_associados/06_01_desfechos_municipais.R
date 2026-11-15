# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/Desfechos/Variaveis Dependentes v1.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Desfechos"))

# ==============================================================================
# SESSÃO DESFECHOS: Variáveis Dependentes (Chagas)
# ==============================================================================

library(tidyverse)
library(sf)

pasta_desfechos <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Desfechos")

arq_pop         <- file.path(pasta_desfechos, "POP_IBGE_MUNICIPIO_TRATADA.csv")
arq_mort        <- file.path(pasta_desfechos, "TAXAS_MORTALIDADE_CHAGAS_PRONTAS.gpkg")
arq_aguda       <- file.path(pasta_desfechos, "TAXAS_CHAGAS_AGUDA_SUAVIZADAS.gpkg")
arq_cronica     <- file.path(pasta_desfechos, "MAPA_CHAGAS_CRONICA_SUAVIZADO_2023.gpkg")

arq_saida       <- file.path(pasta_desfechos, "Desfechos_Chagas_Agregado_Municipios.csv")

cat("\nA processar a matriz de população do IBGE...\n")

df_pop_bruta <- read_csv(arq_pop, col_types = cols(.default = "c")) %>%
  mutate(populacao_residente = as.numeric(populacao_residente))

df_pop_wide <- df_pop_bruta %>%
  group_by(codigo_municipio = COD_MUN, ano = ANO_OBITO) %>%
  summarise(pop_ano = sum(populacao_residente, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = ano, values_from = pop_ano, names_prefix = "pop_total_") %>%
  mutate(pop_media_periodo_ibge = rowMeans(select(., starts_with("pop_total_")), na.rm = TRUE))

cat("\nA descompactar os GeoPackages e a gerar os prefixos...\n")

extrair_desfecho <- function(caminho_gpkg, prefixo) {
  st_read(caminho_gpkg, quiet = TRUE) %>%
    st_drop_geometry() %>%
    {if("code_muni" %in% names(.)) rename(., codigo_municipio = code_muni) else rename(., codigo_municipio = matches("cd_mun|cod_mun|codigo"))} %>%
    mutate(codigo_municipio = as.character(as.integer(codigo_municipio))) %>%
    select(-any_of(c("fid", "name_muni", "code_state", "abbrev_state", "name_state", "code_region", "name_region", "COD_MUN", "municipio", "uf"))) %>%
    rename_with(~ paste0(prefixo, .x), -codigo_municipio)
}

df_mortalidade <- extrair_desfecho(arq_mort, "mort_")
df_aguda       <- extrair_desfecho(arq_aguda, "aguda_")
df_cronica     <- extrair_desfecho(arq_cronica, "cronica_")

cat("\nA unir População, Mortalidade, Aguda e Crónica...\n")

df_desfechos_final <- df_pop_wide %>%
  left_join(df_mortalidade, by = "codigo_municipio") %>%
  left_join(df_aguda, by = "codigo_municipio") %>%
  left_join(df_cronica, by = "codigo_municipio")

colunas_para_zerar <- setdiff(names(df_desfechos_final), c("codigo_municipio", names(df_pop_wide)))
df_desfechos_final <- df_desfechos_final %>%
  mutate(across(all_of(colunas_para_zerar), ~ replace_na(.x, 0)))

write_csv(df_desfechos_final, arq_saida)

cat("\n--- DIAGNÓSTICO DO DATASET DE DESFECHOS ---\n")
cat("Municípios:", nrow(df_desfechos_final), "\n")
cat("Variáveis Geradas:", ncol(df_desfechos_final), "\n")
glimpse(df_desfechos_final)