# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/CIDACS/03_diagnostico_cidacs.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/CIDACS"))

# ==============================================================================
# 03_diagnostico_cidacs.R   -   SOMENTE LEITURA (não altera nenhum dado)
#
# Objetivo: (1) localizar a coluna de competência/data nos 3 arquivos "massivos"
#           do CNES (Equipamentos, Leitos, Profissionais) e testar se filtrar a
#           uma única competência resolve a inflação de ~285x encontrada;
#           (2) inspecionar a estrutura bruta do arquivo de Cisternas para
#           confirmar se os valores são acumulados ou incrementais.
# Saída   : diagnostico_cidacs.txt. Envie esse arquivo.
# ==============================================================================
library(tidyverse)
library(arrow)

pasta <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/CIDACS")

sink("diagnostico_cidacs.txt", split = TRUE)

bloco <- function(titulo, expr) {
  cat("\n", strrep("=", 72), "\n", titulo, "\n", strrep("=", 72), "\n", sep = "")
  tryCatch(expr, error = function(e) cat("ERRO neste bloco:", conditionMessage(e), "\n"))
}

SP <- "3550308"  # São Paulo-SP: referência real ~15.409 leitos SUS em 2023 (CNES/CFM)

# ------------------------------------------------------------------------------
bloco("1. CNES Leitos - schema completo e procura por coluna de competência", {
  arq <- list.files(pasta, pattern = "CNES_Leitos.*\\.csv$", full.names = TRUE)[1]
  cat("Arquivo:", arq, "\n\n")
  ds <- arrow::open_dataset(arq, format = "csv")
  cat("Colunas disponíveis:\n"); print(names(ds))

  col_comp <- grep("COMPET|COMPETEN|ANO|MES|NU_COMP|DT_|CMPT", names(ds), value = TRUE, ignore.case = TRUE)
  cat("\nCandidatas a coluna de competência/data:", paste(col_comp, collapse = ", "), "\n")

  if (length(col_comp) > 0) {
    for (cc in col_comp) {
      cat("\n--- Distribuição de", cc, "(10 valores mais frequentes) ---\n")
      tab <- ds %>% select(all_of(cc)) %>% collect() %>% count(.data[[cc]], sort = TRUE)
      print(head(tab, 10))
    }
  }

  cat("\n--- Amostra de 5 linhas cruas (todas as colunas) ---\n")
  print(head(collect(ds), 5))
})

# ------------------------------------------------------------------------------
bloco("2. CNES Leitos - teste de correção: São Paulo numa ÚNICA competência", {
  arq <- list.files(pasta, pattern = "CNES_Leitos.*\\.csv$", full.names = TRUE)[1]
  ds  <- arrow::open_dataset(arq, format = "csv")
  col_comp <- grep("COMPET|COMPETEN|NU_COMP|CMPT", names(ds), value = TRUE, ignore.case = TRUE)[1]

  if (!is.na(col_comp)) {
    ultima <- ds %>% select(all_of(col_comp)) %>% collect() %>% pull(1) %>% max(na.rm = TRUE)
    cat("Coluna de competência usada:", col_comp, "| última competência encontrada:", ultima, "\n")

    teste <- ds %>%
      filter(CODUFMUN_7 == SP, .data[[col_comp]] == ultima) %>%
      select(CODUFMUN_7, QT_SUS, all_of(col_comp)) %>%
      collect()

    cat("Linhas de São Paulo nessa competência:", nrow(teste), "\n")
    cat("Soma de QT_SUS (deveria ficar perto de 15.409, o total real em 2023):",
        sum(as.numeric(teste$QT_SUS), na.rm = TRUE), "\n")
  } else {
    cat("Nenhuma coluna de competência óbvia encontrada — inspecione a lista de colunas do bloco 1 manualmente.\n")
  }
})

# ------------------------------------------------------------------------------
bloco("3. CNES Equipamentos e Profissionais - mesma checagem de schema", {
  for (padrao in c("CNES_Equipamentos.*\\.csv$", "CNES_Profissionais.*\\.csv$")) {
    arq <- list.files(pasta, pattern = padrao, full.names = TRUE)[1]
    if (is.na(arq)) { cat("Não encontrado:", padrao, "\n"); next }
    cat("\n---", basename(arq), "---\n")
    ds <- arrow::open_dataset(arq, format = "csv")
    cat("Colunas:", paste(names(ds), collapse = " | "), "\n")
    col_comp <- grep("COMPET|COMPETEN|NU_COMP|CMPT", names(ds), value = TRUE, ignore.case = TRUE)
    cat("Candidatas a competência:", paste(col_comp, collapse = ", "), "\n")
  }
})

# ------------------------------------------------------------------------------
bloco("4. Cisternas - estrutura bruta: os valores são acumulados ou incrementais?", {
  arq <- list.files(pasta, pattern = "Programa_Cisternas.*\\.csv$", full.names = TRUE)[1]
  cat("Arquivo:", arq, "\n")
  d <- read_csv(arq, col_types = cols(.default = "c"))
  cat("Colunas:", paste(names(d), collapse = " | "), "\nLinhas:", nrow(d), "\n")

  linhas_mun <- d %>% count(codigo_ibge, sort = TRUE)
  cat("\nLinhas por município: mediana =", median(linhas_mun$n), "| máx =", max(linhas_mun$n), "\n")

  mun_teste <- linhas_mun$codigo_ibge[1]  # o município com mais linhas no arquivo
  cat("\nTodas as linhas do município com mais registros (", mun_teste, "), na ordem em que aparecem:\n", sep = "")
  print(d %>% filter(codigo_ibge == mun_teste) %>%
          select(codigo_ibge, any_of(c("ano", "data", "qtd_cisternas_entregues_consumo",
                                        "qtd_cisternas_entregues_producao", "qtd_cisternas_entregues_escolar"))))
  cat("\nSe os valores só crescem ao longo das linhas -> são acumulados (max() está certo).\n")
  cat("Se os valores oscilam/reiniciam -> são incrementais (deveria ser sum(), não max()).\n")
})

sink()
cat("Diagnóstico salvo em:", file.path(getwd(), "diagnostico_cidacs.txt"), "\n")
