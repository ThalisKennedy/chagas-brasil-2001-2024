# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/CIDACS/CIDACS v13.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/CIDACS"))

library(tidyverse)
library(arrow)

pasta_cidacs   <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Bancos/CIDACS/Dados")
arq_mestre     <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Censo 2022/Censo2022_Agregado_Municipios.csv")
arq_pop        <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Desfechos/POP_IBGE_MUNICIPIO_TRATADA.csv")
pasta_saida    <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/CIDACS")

cat("\nCarregando Gabarito e Matriz Populacional Anual...\n")
df_gabarito <- read_csv(arq_mestre, col_select = c("codigo_municipio"), col_types = cols(.default = "c")) %>%
  mutate(codigo_ibge6 = str_sub(codigo_municipio, 1, 6))

pop_mun_ano <- read_csv(arq_pop, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  mutate(
    codigo_ibge6 = str_sub(COD_MUN, 1, 6),
    ano_num = as.numeric(ANO_OBITO),
    populacao = as.numeric(populacao_residente)
  ) %>%
  group_by(codigo_ibge6, ano_num) %>%
  summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")

pop_2023 <- pop_mun_ano %>% filter(ano_num == 2023) %>% select(codigo_ibge6, populacao)

# ==============================================================================
# SESSÃO 1: Características Urbanísticas do Entorno (Censo 2022)
# ==============================================================================
cat("[1/11] Processando CIDACS Entorno...\n")
arq_cidacs <- list.files(pasta_cidacs, pattern = "\\.csv$", full.names = TRUE)[1]

df_cidacs_final <- df_gabarito %>%
  left_join(
    read_csv(arq_cidacs, show_col_types = FALSE) %>%
      mutate(codigo_ibge6 = str_sub(as.character(cd_mun), 1, 6)) %>%
      mutate(
        cidacs_indice_precariedade_urbana_2022 = ((v05007/v05000) + (v05010/v05000) + (v05013/v05000) + (v05022/v05000)) / 4 * 100,
        cidacs_perc_arborizacao_densa_2022     = (v05033/v05000) * 100
      ) %>% select(codigo_ibge6, starts_with("cidacs_")),
    by = "codigo_ibge6"
  ) %>%
  mutate(across(starts_with("cidacs_"), ~ replace_na(.x, 0)))

# ==============================================================================
# SESSÃO 2: CNES - Equipamentos de Saúde (Taxa/100k)
# ==============================================================================
cat("[2/11] Processando CNES Equipamentos...\n")
arq_equip <- list.files(pasta_cidacs, pattern = "CNES_Equipamentos.*\\.csv$", full.names = TRUE)[1]

df_equip_final <- df_gabarito %>%
  left_join(
    arrow::open_dataset(arq_equip, format = "csv") %>%
      filter(COMPETEN == 202312) %>%
      filter(IND_SUS == 1) %>%
      select(CODUFMUN_7, QT_USO) %>%
      collect() %>%
      mutate(codigo_ibge6 = str_sub(as.character(CODUFMUN_7), 1, 6), qt_uso = as.numeric(QT_USO)) %>%
      group_by(codigo_ibge6) %>%
      summarise(eqp_sus = sum(qt_uso, na.rm = TRUE), .groups = "drop") %>%
      left_join(pop_2023, by = "codigo_ibge6") %>%
      mutate(tx_equipamentos_sus_100k_2023 = if_else(populacao > 0, (eqp_sus / populacao) * 100000, 0)) %>%
      select(codigo_ibge6, tx_equipamentos_sus_100k_2023),
    by = "codigo_ibge6"
  ) %>% mutate(tx_equipamentos_sus_100k_2023 = replace_na(tx_equipamentos_sus_100k_2023, 0))

# ==============================================================================
# SESSÃO 3: CNES - Leitos Hospitalares (Taxa/100k)
# ==============================================================================
cat("[3/11] Processando CNES Leitos...\n")
arq_leitos <- list.files(pasta_cidacs, pattern = "CNES_Leitos.*\\.csv$", full.names = TRUE)[1]

df_leitos_final <- df_gabarito %>%
  left_join(
    arrow::open_dataset(arq_leitos, format = "csv") %>%
      filter(COMPETEN == 202312) %>% 
      filter(QT_SUS > 0) %>%
      select(CODUFMUN_7, QT_SUS) %>%
      collect() %>%
      mutate(codigo_ibge6 = str_sub(as.character(CODUFMUN_7), 1, 6), qt_sus = as.numeric(QT_SUS)) %>%
      group_by(codigo_ibge6) %>%
      summarise(leitos_sus = sum(qt_sus, na.rm = TRUE), .groups = "drop") %>%
      left_join(pop_2023, by = "codigo_ibge6") %>%
      mutate(tx_leitos_sus_100k_2023 = if_else(populacao > 0, (leitos_sus / populacao) * 100000, 0)) %>%
      select(codigo_ibge6, tx_leitos_sus_100k_2023),
    by = "codigo_ibge6"
  ) %>% mutate(tx_leitos_sus_100k_2023 = replace_na(tx_leitos_sus_100k_2023, 0))

# ==============================================================================
# SESSÃO 4: CNES - Profissionais de Saúde APS (Taxa/10k)
# ==============================================================================
cat("[4/11] Processando CNES Profissionais...\n")
arq_prof <- list.files(pasta_cidacs, pattern = "CNES_Profissionais.*\\.csv$", full.names = TRUE)[1]

df_prof_final <- df_gabarito %>%
  left_join(
    arrow::open_dataset(arq_prof, format = "csv") %>%
      filter(COMPETEN == 202312) %>% 
      filter(PROF_SUS == 1) %>%
      filter(CBO %in% c(225142, 225170)) %>%
      select(CODUFMUN_7, any_of(c("HORA_AMB", "HORAHOSP", "HORAOUTR"))) %>%
      collect() %>%
      mutate(
        codigo_ibge6 = str_sub(as.character(CODUFMUN_7), 1, 6),
        h_amb = if("HORA_AMB" %in% names(.)) as.numeric(HORA_AMB) else 0,
        h_hsp = if("HORAHOSP" %in% names(.)) as.numeric(HORAHOSP) else 0,
        h_out = if("HORAOUTR" %in% names(.)) as.numeric(HORAOUTR) else 0,
        h_tot = replace_na(h_amb,0) + replace_na(h_hsp,0) + replace_na(h_out,0)
      ) %>%
      group_by(codigo_ibge6) %>%
      summarise(horas_aps = sum(h_tot, na.rm = TRUE), .groups = "drop") %>%
      left_join(pop_2023, by = "codigo_ibge6") %>%
      mutate(tx_horas_medicos_aps_sus_10k_2023 = if_else(populacao > 0, (horas_aps / populacao) * 10000, 0)) %>%
      select(codigo_ibge6, tx_horas_medicos_aps_sus_10k_2023),
    by = "codigo_ibge6"
  ) %>% mutate(tx_horas_medicos_aps_sus_10k_2023 = replace_na(tx_horas_medicos_aps_sus_10k_2023, 0))

# ==============================================================================
# SESSÃO 5 e 6: IVM e SPEI 
# ==============================================================================
cat("[5/11 e 6/11] Processando Índices IVM e SPEI...\n")
arq_ivm  <- list.files(pasta_cidacs, pattern = ".*ndice_de_Vulnerabilidade_Municipal.*\\.csv$", full.names = TRUE)[1]
arq_spei <- list.files(pasta_cidacs, pattern = ".*ndice_Padronizado_de_Precipita.*\\.csv$", full.names = TRUE)[1]

df_ivm_final <- df_gabarito %>%
  left_join(
    read_csv(arq_ivm, show_col_types = FALSE) %>%
      filter(anoIVM == 2022) %>%
      mutate(codigo_ibge6 = str_sub(codigo_ibge, 1, 6)) %>%
      select(codigo_ibge6, ivm_global_2022 = ivm, ivm_economia_2022 = ivm_economia_local) %>%
      distinct(codigo_ibge6, .keep_all = TRUE),
    by = "codigo_ibge6"
  )

df_spei_final <- df_gabarito %>%
  left_join(
    read_csv(arq_spei, col_types = cols(.default = "c")) %>%
      mutate(ano_num = as.numeric(ano), spei_m = as.numeric(spei_gamma_12)) %>%
      filter(ano_num >= 2001 & ano_num <= 2024) %>%
      group_by(codigo_ibge6 = str_sub(cod_mun_7, 1, 6)) %>%
      summarise(spei_media_2001_2024 = mean(spei_m, na.rm = TRUE), .groups = "drop"),
    by = "codigo_ibge6"
  )

# ==============================================================================
# SESSÃO 7, 8 e 9: Chuva, Temp e Umidade via Arrow
# ==============================================================================
cat("[7/11 a 9/11] Processando Variáveis Climáticas...\n")
arq_chuva <- list.files(pasta_cidacs, pattern = ".*Precipita.*chuva_di.*\\.csv$", full.names = TRUE)[1]
arq_temp  <- list.files(pasta_cidacs, pattern = ".*Temperatura_m.*\\.csv$", full.names = TRUE)[1]
arq_umid  <- list.files(pasta_cidacs, pattern = ".*Umidade_relativa_di.*\\.csv$", full.names = TRUE)[1]

df_chuva_final <- df_gabarito %>% left_join(
  arrow::open_dataset(arq_chuva, format = "csv") %>%
    select(cod_mun_7, ano, precipitacao) %>% filter(as.numeric(ano) >= 2001 & as.numeric(ano) <= 2024) %>% collect() %>%
    group_by(codigo_ibge6 = str_sub(cod_mun_7, 1, 6)) %>%
    summarise(precipitacao_media_anual_2001_2024 = mean(as.numeric(precipitacao), na.rm=TRUE) * 365.25, .groups = "drop"),
  by = "codigo_ibge6")

df_temp_final <- df_gabarito %>% left_join(
  arrow::open_dataset(arq_temp, format = "csv") %>%
    select(cod_mun_7, ano, temperatura_media, temperatura_maxima, temperatura_minima) %>%
    filter(as.numeric(ano) >= 2001 & as.numeric(ano) <= 2023) %>% collect() %>%
    group_by(codigo_ibge6 = str_sub(cod_mun_7, 1, 6)) %>%
    summarise(
      temp_media_2001_2023 = mean(as.numeric(temperatura_media), na.rm=TRUE),
      temp_max_2001_2023   = mean(as.numeric(temperatura_maxima), na.rm=TRUE),
      temp_min_2001_2023   = mean(as.numeric(temperatura_minima), na.rm=TRUE), .groups = "drop"
    ), by = "codigo_ibge6")

df_umidade_final <- df_gabarito %>% left_join(
  arrow::open_dataset(arq_umid, format = "csv") %>%
    select(cod_mun_7, ano, umidade_relativa) %>% filter(as.numeric(ano) >= 2001 & as.numeric(ano) <= 2023) %>% collect() %>%
    group_by(codigo_ibge6 = str_sub(cod_mun_7, 1, 6)) %>%
    summarise(umidade_media_2001_2023 = mean(as.numeric(umidade_relativa), na.rm=TRUE), .groups = "drop"),
  by = "codigo_ibge6")

# ==============================================================================
# SESSÃO 10: Programa Cisternas (Taxa/10k)
# ==============================================================================
cat("[10/11] Processando Cisternas...\n")
arq_cis <- list.files(pasta_cidacs, pattern = ".*Programa_Cisternas.*\\.csv$", full.names = TRUE)[1]

df_cisternas_final <- df_gabarito %>%
  left_join(
    read_csv(arq_cis, col_types = cols(.default = "c")) %>%
      mutate(codigo_ibge6 = str_sub(codigo_ibge, 1, 6)) %>%
      group_by(codigo_ibge6) %>%
      summarise(
        c_con = max(as.numeric(qtd_cisternas_entregues_consumo), na.rm=TRUE),
        c_pro = max(as.numeric(qtd_cisternas_entregues_producao), na.rm=TRUE),
        c_esc = max(as.numeric(qtd_cisternas_entregues_escolar), na.rm=TRUE), .groups = "drop"
      ) %>%
      mutate(
        c_con = if_else(is.infinite(c_con), 0, c_con),
        c_pro = if_else(is.infinite(c_pro), 0, c_pro),
        c_esc = if_else(is.infinite(c_esc), 0, c_esc),
        tot_cist = c_con + c_pro + c_esc
      ) %>%
      left_join(pop_mun_ano %>% filter(ano_num == 2022), by = "codigo_ibge6") %>%
      mutate(tx_cisternas_10k_2004_2023 = if_else(populacao > 0, (tot_cist / populacao) * 10000, 0)) %>%
      select(codigo_ibge6, tx_cisternas_10k_2004_2023),
    by = "codigo_ibge6"
  ) %>% mutate(tx_cisternas_10k_2004_2023 = replace_na(tx_cisternas_10k_2004_2023, 0))

# ==============================================================================
# SESSÃO 11: Programa Bolsa Família (Deflacionado e em Taxa/1k)
# ==============================================================================
cat("[11/11] Processando Bolsa Família...\n")
arq_bolsa <- list.files(pasta_cidacs, pattern = ".*Relat.*rio_de_pagamentos_do_bolsa_fam.*\\.csv$", full.names = TRUE)[1]

tabela_ipca_bf <- tibble(
  ano_num = 2004:2024,
  fator_ipca = c(2.62, 2.48, 2.40, 2.30, 2.17, 2.08, 1.96, 1.84, 1.74, 1.64, 1.54, 1.39, 1.31, 1.27, 1.23, 1.18, 1.13, 1.02, 0.97, 0.93, 1.00) 
)

df_bolsa_final <- df_gabarito %>%
  left_join(
    read_csv(arq_bolsa, col_types = cols(.default = "c")) %>%
      mutate(codigo_ibge6 = str_sub(codigo_ibge, 1, 6), ano_num = as.numeric(`_ANO`)) %>%
      filter(ano_num >= 2004 & ano_num <= 2024) %>%
      left_join(tabela_ipca_bf, by = "ano_num") %>%
      left_join(pop_mun_ano, by = c("codigo_ibge6", "ano_num")) %>%
      mutate(
        vlr_real = as.numeric(pbf_vlr_medio_benef_f) * fator_ipca,
        tx_fam = if_else(populacao > 0, (as.numeric(qtd_familias_beneficiarias_bolsa_familia_s) / populacao) * 1000, 0)
      ) %>%
      group_by(codigo_ibge6) %>%
      summarise(
        bf_tx_familias_1k_2004_2024 = mean(tx_fam, na.rm = TRUE),
        bf_valor_medio_real_2004_2024 = mean(vlr_real, na.rm = TRUE), .groups = "drop"
      ),
    by = "codigo_ibge6"
  ) %>%
  mutate(
    bf_tx_familias_1k_2004_2024 = replace_na(bf_tx_familias_1k_2004_2024, 0),
    bf_valor_medio_real_2004_2024 = replace_na(bf_valor_medio_real_2004_2024, 0)
  )

# ==============================================================================
# FUSÃO FINAL DO DATASET CIDACS
# ==============================================================================
cat("\nConsolidando o Master Dataset do CIDACS...\n")

df_master_cidacs <- df_gabarito %>%
  left_join(df_cidacs_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_equip_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_leitos_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_prof_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_ivm_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_spei_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_chuva_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_temp_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_umidade_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_cisternas_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  left_join(df_bolsa_final %>% select(-any_of("codigo_municipio")), by = "codigo_ibge6") %>%
  select(codigo_municipio, everything(), -codigo_ibge6)

arquivo_saida_final <- file.path(pasta_saida, "CIDACS_Agregado_Municipios_FINAL_v13.csv")
write_csv(df_master_cidacs, arquivo_saida_final)

cat("Arquivo salvo com sucesso em:", arquivo_saida_final, "\n")
glimpse(df_master_cidacs)