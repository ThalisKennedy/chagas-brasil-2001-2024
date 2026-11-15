# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/Portal de Dados Abertos do SUS/Portal_de_Dados_Abertos_do_SUS_v10.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Portal de Dados Abertos do SUS"))

library(tidyverse)
library(data.table)

pasta_sus   <- paste0(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Bancos/Portal de Dados Abertos do SUS/Dados"), "/")
caminho_censo <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Censo 2022/Censo2022_Agregado_Municipios.csv")
caminho_pop   <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Desfechos/POP_IBGE_MUNICIPIO_TRATADA.csv")

df_censo <- fread(caminho_censo, colClasses = list(character = "codigo_municipio")) %>%
  mutate(codigo_ibge6 = substr(codigo_municipio, 1, 6)) %>%
  select(codigo_ibge6) %>% distinct()

pop_mun_ano <- fread(caminho_pop, colClasses = list(character = "COD_MUN")) %>%
  rename(codigo_ibge7 = COD_MUN, ano = ANO_OBITO) %>%
  mutate(codigo_ibge6 = substr(codigo_ibge7, 1, 6)) %>%
  group_by(codigo_ibge6, ano) %>%
  summarise(populacao = sum(populacao_residente, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# SEÇÃO 1: SISAGUA (Cadastro de População Abastecida)
# ==============================================================================
cat("\nProcessando SISAGUA...\n")
df_sisagua_bruto <- fread(paste0(pasta_sus, "cadastro_populacao_abastecida.csv"), sep = ";", encoding = "Latin-1")

df_sisagua_final <- df_sisagua_bruto %>%
  rename(codigo_ibge7 = CO_MUNICIPIO_IBGE, ano = NU_ANO, pop_abastecida = NU_VALOR_POPULACAO_ESTIMADA) %>%
  filter(ano >= 2001, ano <= 2024) %>%
  mutate(
    codigo_ibge7 = ifelse(codigo_ibge7 >= 530000 & codigo_ibge7 <= 530099, 530010, codigo_ibge7),
    codigo_ibge6 = substr(as.character(codigo_ibge7), 1, 6)
  ) %>%
  group_by(codigo_ibge6, ano) %>%
  summarise(pop_abastecida_total = sum(pop_abastecida, na.rm = TRUE), .groups = "drop") %>%
  left_join(pop_mun_ano, by = c("codigo_ibge6", "ano")) %>%
  mutate(sisagua_cobertura_ano = 100 * pop_abastecida_total / populacao) %>%
  group_by(codigo_ibge6) %>%
  summarise(sisagua_cobertura_media = mean(sisagua_cobertura_ano, na.rm = TRUE), .groups = "drop") %>%
  right_join(df_censo, by = "codigo_ibge6") %>%
  mutate(sisagua_cobertura_media = replace_na(sisagua_cobertura_media, 0))

# ==============================================================================
# SEÇÃO 2: RIPSA (Capacidade Instalada e Recursos Humanos)
# ==============================================================================
cat("Processando RIPSA...\n")
df_rh     <- fread(paste0(pasta_sus, "ripsa003rc.csv"), sep = ",", quote = "") %>% rename_with(~ gsub('"', '', tolower(.x)))
df_leitos <- fread(paste0(pasta_sus, "ripsa004rc.csv"), sep = ",", quote = "") %>% rename_with(~ gsub('"', '', tolower(.x)))
df_equip  <- fread(paste0(pasta_sus, "ripsa014rc.csv"), sep = ",", quote = "") %>% rename_with(~ gsub('"', '', tolower(.x)))

df_ripsa_final <- df_rh %>%
  select(municipio = matches("^municipio"), ano = matches("^ano"), enfermeiros = matches("enfermeiros$")) %>%
  full_join(df_leitos %>% select(municipio = matches("^municipio"), ano = matches("^ano"), leitos_sus = matches("leitos sus$")), by = c("municipio", "ano")) %>%
  full_join(df_equip %>% select(municipio = matches("^municipio"), ano = matches("^ano"), raiox = matches("raio x simples$"), ultrassom = matches("ultrassom$")), by = c("municipio", "ano")) %>%
  mutate(across(c(enfermeiros, leitos_sus, raiox, ultrassom), as.numeric), codigo_ibge6 = as.character(municipio)) %>%
  filter(!is.na(codigo_ibge6), ano >= 2001, ano <= 2024, !grepl("0000$", codigo_ibge6)) %>%
  left_join(pop_mun_ano, by = c("codigo_ibge6", "ano")) %>%
  filter(populacao > 0) %>%
  mutate(
    tx_enferm_ano = (enfermeiros / populacao) * 1000,
    tx_leitos_sus_ano = (leitos_sus / populacao) * 1000, 
    tx_rx_ano = (raiox / populacao) * 1000,
    tx_usg_ano = (ultrassom / populacao) * 1000
  ) %>%
  group_by(codigo_ibge6) %>%
  summarise(
    ripsa_tx_enfermeiros_media = mean(tx_enferm_ano, na.rm = TRUE),
    ripsa_tx_leitos_sus_media = mean(tx_leitos_sus_ano, na.rm = TRUE),
    ripsa_tx_raiox_media = mean(tx_rx_ano, na.rm = TRUE),
    ripsa_tx_usg_media = mean(tx_usg_ano, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(df_censo, by = "codigo_ibge6") %>%
  mutate(across(starts_with("ripsa_"), ~ replace_na(.x, 0)))

# ==============================================================================
# SEÇÃO 3: SISAB (Previne Brasil - Atenção Básica)
# ==============================================================================
cat("Processando SISAB...\n")
df_sisab_bruto <- bind_rows(
  fread(paste0(pasta_sus, "sisab_indicador_desempenho_2024Q1.csv")),
  fread(paste0(pasta_sus, "sisab_indicador_desempenho_2024Q2.csv")),
  fread(paste0(pasta_sus, "sisab_indicador_desempenho_2024Q3.csv"))
) %>% rename_with(~ trimws(tolower(.x)))

df_sisab_final <- df_sisab_bruto %>%
  filter(!is.na(ibge), !is.na(co_tipo_indicador), visao_equipe == "validas") %>%
  group_by(codigo_ibge6 = as.character(ibge), ds_quadrimestre, co_tipo_indicador) %>%
  summarise(
    numerador_soma = sum(as.numeric(numerador), na.rm = TRUE),
    denominador_soma = sum(as.numeric(denominador_utilizador), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(percentual_quad = ifelse(denominador_soma > 0, 100 * numerador_soma / denominador_soma, NA_real_)) %>%
  group_by(codigo_ibge6, co_tipo_indicador) %>%
  summarise(media_anual = mean(percentual_quad, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = co_tipo_indicador, values_from = media_anual) %>%
  rename(any_of(c(
    sisab_prenatal_6consultas = "10",
    sisab_prenatal_sifilis_hiv = "20",
    sisab_prenatal_odonto = "30",
    sisab_citopatologico = "40",
    sisab_vacina_polio_penta = "50",
    sisab_hipertensao_pa = "60",
    sisab_diabetes_hemo_glicada = "70"
  ))) %>%
  mutate(sisab_isf_global = rowMeans(select(., starts_with("sisab_")), na.rm = TRUE)) %>%
  right_join(df_censo, by = "codigo_ibge6") %>%
  mutate(across(starts_with("sisab_"), ~ replace_na(.x, 0)))

# ==============================================================================
# SEÇÃO 4: PROGRAMA MAIS MÉDICOS
# ==============================================================================
cat("Processando Mais Médicos...\n")
df_mm_final <- fread(paste0(pasta_sus, "ppf_mais_medicos_serie_historica.csv"), encoding = "Latin-1") %>%
  rename_with(~ trimws(tolower(.x))) %>%
  filter(!is.na(ibge), nchar(as.character(ibge)) == 6) %>%
  mutate(codigo_ibge6 = as.character(ibge), ano = as.integer(substr(dt_referencia, 7, 10))) %>%
  filter(ano >= 2001, ano <= 2024) %>%
  group_by(codigo_ibge6, ano) %>%
  summarise(prof_total = sum(total_prof_ativos, na.rm = TRUE), .groups = "drop") %>%
  left_join(pop_mun_ano, by = c("codigo_ibge6", "ano")) %>%
  mutate(mm_por10k_ano = 10000 * prof_total / populacao) %>%
  group_by(codigo_ibge6) %>%
  summarise(maismedicos_media_10k = mean(mm_por10k_ano, na.rm = TRUE), .groups = "drop") %>%
  right_join(df_censo, by = "codigo_ibge6") %>%
  mutate(maismedicos_media_10k = replace_na(maismedicos_media_10k, 0))

# ==============================================================================
# SEÇÃO 5: SIOPS (Despesas em Saúde Deflacionadas)
# ==============================================================================
cat("Processando SIOPS (Deflacionamento IPCA)...\n")
arquivos_siops <- list.files(pasta_sus, pattern = "^SIOPS.*\\.csv$", full.names = TRUE)

ler_siops <- function(caminho) {
  fread(caminho, encoding = "Latin-1") %>%
    select(ano = 1, codigo_ibge6 = 4, subfuncao = 8, codigo = 9, fase = 11, valor = 12) %>%
    mutate(codigo_ibge6 = as.character(codigo_ibge6), valor = as.numeric(gsub(",", ".", valor))) %>%
    filter(grepl("Liquidada", fase, ignore.case = TRUE), codigo %in% c("3.0.00.00.00.00", "4.0.00.00.00.00"))
}

tabela_ipca <- tibble(
  ano = 2018:2024,
  fator_ipca = c(1.3878, 1.3305, 1.2729, 1.1566, 1.0933, 1.0450, 1.0000)
)

soma_ou_na <- function(valor, cond) if (any(cond)) sum(valor[cond], na.rm = TRUE) else NA_real_

df_siops_final <- bind_rows(lapply(arquivos_siops, ler_siops)) %>%
  left_join(tabela_ipca, by = "ano") %>%
  mutate(valor_real = valor * fator_ipca) %>%
  group_by(codigo_ibge6, ano) %>%
  summarise(
    despesa_total      = sum(valor_real, na.rm = TRUE),
    despesa_ab         = soma_ou_na(valor_real, grepl("301", subfuncao)),
    despesa_vigilancia = soma_ou_na(valor_real, grepl("304|305", subfuncao)),
    .groups = "drop"
  ) %>%
  left_join(pop_mun_ano, by = c("codigo_ibge6", "ano")) %>%
  mutate(
    siops_pc_total_ano      = despesa_total / populacao,
    siops_pc_ab_ano         = despesa_ab / populacao,
    siops_pc_vigilancia_ano = despesa_vigilancia / populacao
  ) %>%
  group_by(codigo_ibge6) %>%
  summarise(
    siops_pc_total_media      = mean(siops_pc_total_ano, na.rm = TRUE),
    siops_pc_ab_media         = mean(siops_pc_ab_ano, na.rm = TRUE),
    siops_pc_vigilancia_media = mean(siops_pc_vigilancia_ano, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(df_censo, by = "codigo_ibge6")

# ==============================================================================
# SEÇÃO 6: RAZÃO DE CONSULTAS (APS E ESPECIALIZADA)
# ==============================================================================
cat("Processando Consultas (APS e Especializada)...\n")

aps_agg <- fread(paste0(pasta_sus, "mgdi_ms_4iu.csv"), encoding = "Latin-1") %>%
  rename_with(tolower) %>%
  filter(!is.na(co_ibge), ds_item_categoria == "Total") %>%
  mutate(codigo_ibge6 = as.character(substr(co_ibge, 1, 6))) %>%
  group_by(codigo_ibge6, co_anomes) %>%
  summarise(total_consultas_mes = sum(as.numeric(vl_indicador_calculado_mun), na.rm = TRUE), .groups = "drop") %>%
  group_by(codigo_ibge6) %>%
  summarise(razao_consultas_aps_media = mean(total_consultas_mes, na.rm = TRUE), .groups = "drop")

esp_agg <- fread(paste0(pasta_sus, "mgdi_ms_0bx.csv"), encoding = "Latin-1") %>%
  rename_with(tolower) %>%
  filter(!is.na(co_ibge)) %>%
  mutate(codigo_ibge6 = as.character(substr(co_ibge, 1, 6))) %>%
  group_by(codigo_ibge6, co_anomes) %>%
  summarise(total_consultas_mes = sum(as.numeric(vl_indicador_calculado_mun), na.rm = TRUE), .groups = "drop") %>%
  group_by(codigo_ibge6) %>%
  summarise(razao_consultas_esp_media = mean(total_consultas_mes, na.rm = TRUE), .groups = "drop")

df_consultas_final <- df_censo %>%
  left_join(aps_agg, by = "codigo_ibge6") %>%
  left_join(esp_agg, by = "codigo_ibge6") %>%
  mutate(across(starts_with("razao_"), ~ replace_na(.x, 0)))

# ==============================================================================
# SEÇÃO 7: FUSÃO FINAL E EXPORTAÇÃO
# ==============================================================================
cat("\nExecutando Fusão do Master Dataset...\n")

df_final_sus <- df_censo %>%
  left_join(df_sisagua_final, by = "codigo_ibge6") %>%
  left_join(df_ripsa_final, by = "codigo_ibge6") %>%
  left_join(df_sisab_final, by = "codigo_ibge6") %>%
  left_join(df_mm_final, by = "codigo_ibge6") %>%
  left_join(df_siops_final, by = "codigo_ibge6") %>%
  left_join(df_consultas_final, by = "codigo_ibge6")

arquivo_saida <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Portal_Dados_Abertos_SUS_CHAGAS_FINAL.csv")
write_csv(df_final_sus, arquivo_saida)

cat("\nArquivo final estruturado e salvo em:", arquivo_saida, "\n")