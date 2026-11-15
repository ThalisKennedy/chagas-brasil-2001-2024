# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/SIDRA/SIDRA v7.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/SIDRA"))

library(tidyverse)
library(sf)

diretorio_sidra  <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Bancos/SIDRA/Dados")
diretorio_mestre <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Censo 2022")
diretorio_desf   <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Desfechos")

arquivo_mestre   <- file.path(diretorio_mestre, "Censo2022_Agregado_Municipios.csv")
arquivo_saida    <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/SIDRA/SIDRA_Agregado_Municipios_FINAL_CORRIGIDO_2.csv")

tratar_valor_sidra <- function(valor_chr) {
  case_when(
    valor_chr == "-" ~ 0,
    valor_chr %in% c("X", "..", "...") ~ NA_real_,
    TRUE ~ as.numeric(valor_chr)
  )
}

processar_cultura_area <- function(arquivo, nome_var, sufixo_janela) {
  read_csv(arquivo, skip = 3, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8")) %>%
    rename(coluna_base = 1) %>%
    mutate(codigo_municipio = str_extract(coluna_base, "^\\d{7}")) %>%
    mutate(bloco = case_when(
      str_detect(coluna_base, "(?i)rea colhida|rea plantada") ~ "area",
      str_detect(coluna_base, "(?i)uantidade produzida|endimento|alor da produ") ~ "ignorar",
      row_number() == 1 ~ "area", 
      TRUE ~ NA_character_
    )) %>%
    fill(bloco, .direction = "down") %>%
    filter(!is.na(codigo_municipio), bloco == "area") %>%
    select(codigo_municipio, starts_with("20")) %>%
    pivot_longer(cols = starts_with("20"), names_to = "ano", values_to = "valor") %>%
    mutate(valor_limpo = tratar_valor_sidra(valor)) %>%
    group_by(codigo_municipio) %>%
    summarise(!!paste0(nome_var, "_area_media_", sufixo_janela) := mean(valor_limpo, na.rm = TRUE), .groups = "drop")
}

cat("\nProcessando Culturas (PAM - Área Colhida)...\n")
df_acai <- processar_cultura_area(file.path(diretorio_sidra, "5457 - Açai.csv"), "acai", "2015_2023") 
df_cana <- processar_cultura_area(file.path(diretorio_sidra, "5457 - Cana-de-açucar.csv"), "cana", "2001_2023")

cat("Processando PIB e Estrutura Econômica...\n")

ler_sidra_vab <- function(caminho, anos) {
  read_csv(caminho, skip = 3, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8")) %>%
    rename(coluna_base = 1) %>%
    mutate(codigo_municipio = str_extract(coluna_base, "^\\d{7}")) %>%
    filter(!is.na(codigo_municipio)) %>%
    select(codigo_municipio, any_of(anos)) %>%
    pivot_longer(cols = any_of(anos), names_to = "ano", values_to = "valor") %>%
    mutate(valor_limpo = tratar_valor_sidra(valor))
}

anos_vab <- c("2019", "2020", "2021")
ano_pib  <- "2022"

df_pub_long   <- ler_sidra_vab(file.path(diretorio_sidra, "5938 - PIB Público.csv"), anos_vab) %>% rename(vab_pub = valor_limpo)
df_total_long <- ler_sidra_vab(file.path(diretorio_sidra, "5938 - VAB Total.csv"), anos_vab) %>% rename(vab_total = valor_limpo)

df_pib_agro_raw <- read_csv(file.path(diretorio_sidra, "5938 - PIB.csv"), skip = 3, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8")) %>%
  rename(coluna_base = 1) %>%
  mutate(
    codigo_municipio = str_extract(coluna_base, "^\\d{7}"),
    bloco = case_when(
      str_detect(coluna_base, "(?i)agropecu") ~ "vab_agro",
      str_detect(coluna_base, "(?i)servi.os|ind.stria|impostos|valor adicionado") ~ "ignorar",
      row_number() == 1 ~ "pib_total",
      TRUE ~ NA_character_
    )
  ) %>%
  fill(bloco, .direction = "down") %>%
  filter(!is.na(codigo_municipio))

df_pib_total <- df_pib_agro_raw %>%
  filter(bloco == "pib_total") %>%
  select(codigo_municipio, valor = any_of(ano_pib)) %>%
  mutate(pib_total_2022 = tratar_valor_sidra(valor)) %>%
  select(codigo_municipio, pib_total_2022)

df_agro_long <- df_pib_agro_raw %>%
  filter(bloco == "vab_agro") %>%
  select(codigo_municipio, any_of(anos_vab)) %>%
  pivot_longer(cols = any_of(anos_vab), names_to = "ano", values_to = "valor") %>%
  mutate(vab_agro = tratar_valor_sidra(valor)) %>%
  select(codigo_municipio, ano, vab_agro)

df_economia <- df_total_long %>%
  left_join(df_agro_long, by = c("codigo_municipio", "ano")) %>%
  left_join(df_pub_long, by = c("codigo_municipio", "ano")) %>%
  mutate(
    perc_agro = if_else(vab_total > 0, (vab_agro / vab_total) * 100, NA_real_),
    perc_pub  = if_else(vab_total > 0, (vab_pub / vab_total) * 100, NA_real_)
  ) %>%
  group_by(codigo_municipio) %>%
  summarise(
    perc_media_agro_19_21 = mean(perc_agro, na.rm = TRUE),
    perc_media_pub_19_21  = mean(perc_pub, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(df_pib_total, by = "codigo_municipio")

cat("Processando Populações Tradicionais...\n")

processar_tradicional <- function(arquivo, nome_var) {
  read_csv(arquivo, skip = 2, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8")) %>%
    rename(valor_raw = last_col(), coluna_base = 1) %>%
    mutate(codigo_municipio = str_extract(coluna_base, "^\\d{7}")) %>%
    filter(!is.na(codigo_municipio)) %>%
    mutate(!!nome_var := tratar_valor_sidra(valor_raw)) %>%
    select(codigo_municipio, !!nome_var)
}

df_indigena   <- processar_tradicional(file.path(diretorio_sidra, "9718 – População Indígena.csv"), "pop_indigena_2022")
df_quilombola <- processar_tradicional(file.path(diretorio_sidra, "9578 – População Quilombola.csv"), "pop_quilombola_2022")

cat("Consolidando o Dataset do SIDRA...\n")
df_gabarito <- read_csv(arquivo_mestre, col_select = c("codigo_municipio"), col_types = cols(codigo_municipio = "c"))

df_sidra_bruto <- df_gabarito %>%
  left_join(df_acai, by = "codigo_municipio") %>%
  left_join(df_cana, by = "codigo_municipio") %>%
  left_join(df_economia, by = "codigo_municipio") %>%
  left_join(df_indigena, by = "codigo_municipio") %>%
  left_join(df_quilombola, by = "codigo_municipio") %>%
  mutate(
    acai_area_media_2015_2023 = replace_na(acai_area_media_2015_2023, 0),
    cana_area_media_2001_2023 = replace_na(cana_area_media_2001_2023, 0),
    pop_indigena_2022         = replace_na(pop_indigena_2022, 0),
    pop_quilombola_2022       = replace_na(pop_quilombola_2022, 0)
  )

cat("Calculando Área Territorial e Finalizando Matriz Relativa...\n")

arq_pop <- file.path(diretorio_desf, "POP_IBGE_MUNICIPIO_TRATADA.csv")
df_pop_2022 <- read_csv(arq_pop, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  filter(ANO_OBITO == "2022") %>%
  group_by(codigo_municipio = COD_MUN) %>%
  summarise(populacao_2022 = sum(as.numeric(populacao_residente), na.rm = TRUE), .groups = "drop")

arq_mapa <- file.path(diretorio_desf, "TAXAS_MORTALIDADE_CHAGAS_PRONTAS.gpkg")
df_area <- st_read(arq_mapa, quiet = TRUE) %>%
  mutate(
    codigo_municipio = as.character(as.integer(code_muni)),
    area_hectares = as.numeric(st_area(geom)) / 10000
  ) %>%
  st_drop_geometry() %>%
  select(codigo_municipio, area_hectares)

df_sidra_final <- df_sidra_bruto %>%
  left_join(df_pop_2022, by = "codigo_municipio") %>%
  left_join(df_area, by = "codigo_municipio") %>%
  mutate(
    pib_per_capita_2022 = if_else(populacao_2022 > 0, (pib_total_2022 * 1000) / populacao_2022, NA_real_),
    pct_indigena_2022   = if_else(populacao_2022 > 0, (pop_indigena_2022 / populacao_2022) * 100, 0),
    pct_quilombola_2022 = if_else(populacao_2022 > 0, (pop_quilombola_2022 / populacao_2022) * 100, 0),
    pct_area_acai       = if_else(area_hectares > 0, (acai_area_media_2015_2023 / area_hectares) * 100, 0),
    pct_area_cana       = if_else(area_hectares > 0, (cana_area_media_2001_2023 / area_hectares) * 100, 0)
  ) %>%
  select(
    -populacao_2022, 
    -area_hectares, 
    -pib_total_2022,
    -acai_area_media_2015_2023, 
    -cana_area_media_2001_2023, 
    -pop_indigena_2022, 
    -pop_quilombola_2022
  )

write_csv(df_sidra_final, arquivo_saida)
cat("\nProcessamento SIDRA concluído! Apenas métricas relativas (taxas) mantidas. Salvo em:", arquivo_saida, "\n")