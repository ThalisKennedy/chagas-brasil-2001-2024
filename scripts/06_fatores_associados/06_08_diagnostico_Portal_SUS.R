# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/Portal de Dados Abertos do SUS/02_diagnostico_portal_sus.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Portal de Dados Abertos do SUS"))

library(tidyverse)
library(data.table)

base <- paste0(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Bancos/Portal de Dados Abertos do SUS/Dados"), "/")
f    <- function(x) paste0(base, x)

sink("diagnostico_portal_sus.txt", split = TRUE)

bloco <- function(titulo, expr) {
  cat("\n", strrep("=", 72), "\n", titulo, "\n", strrep("=", 72), "\n", sep = "")
  tryCatch(expr, error = function(e) cat("ERRO neste bloco:", conditionMessage(e), "\n"))
}
norm <- function(d) { names(d) <- tolower(trimws(gsub('"', "", names(d)))); d }
num  <- function(x) suppressWarnings(as.numeric(gsub('"', "", x)))
periodo <- function(d) {
  for (cn in grep("ano|mes|comp|data|period", names(d), value = TRUE)) {
    v <- d[[cn]]
    cat("   ", cn, ": ", as.character(min(v, na.rm = TRUE)), " -> ", as.character(max(v, na.rm = TRUE)),
        " (", n_distinct(v), " valores distintos)\n", sep = "")
  }
}

bloco("1. SISAB (Previne Brasil) - 2024 Q1", {
  q1 <- norm(fread(f("sisab_indicador_desempenho_2024Q1.csv")))
  cat("Colunas:", paste(names(q1), collapse = " | "), "\nLinhas:", nrow(q1), "\n\nPrimeiras linhas:\n")
  print(head(q1, 4))
  cat("\nTipos de indicador (co_tipo_indicador):\n"); print(table(q1$co_tipo_indicador, useNA = "ifany"))

  dup <- q1 %>% count(ibge, ds_quadrimestre, co_tipo_indicador) %>% filter(n > 1)
  cat("\nCombinações município x quadrimestre x tipo com MAIS DE 1 linha:", nrow(dup), "\n")
  if (nrow(dup) > 0) {
    print(summary(dup$n))
    cat("Exemplo de combinação repetida:\n")
    print(q1 %>% filter(ibge == dup$ibge[1], co_tipo_indicador == dup$co_tipo_indicador[1]))
  }
  cat("\nMunicípio 110001 - todas as linhas:\n")
  print(q1 %>% filter(as.character(ibge) == "110001"))
  cat("\nDistribuição de 'percentual':\n"); print(summary(num(q1$percentual)))
  cat("Linhas com percentual > 100:", sum(num(q1$percentual) > 100, na.rm = TRUE), "\n")
})

bloco("2. SIOPS - colunas por ano, fases, códigos e conversão numérica", {
  arqs <- list.files(base, pattern = "^SIOPS", full.names = TRUE)
  cat("Arquivos SIOPS encontrados:", length(arqs), "\n")
  for (a in arqs) {
    d <- fread(a, encoding = "Latin-1", nrows = 3)
    cat(basename(a), "\n   colunas nas posições 1,4,8,9,11,12:",
        paste(names(d)[c(1, 4, 8, 9, 11, 12)], collapse = " | "), "\n")
  }
  d <- fread(arqs[length(arqs)], encoding = "Latin-1")
  cat("\nÚltimo ano - todas as colunas:", paste(names(d), collapse = " | "), "\n")
  s <- d %>% select(ano = 1, codigo_ibge6 = 4, subfuncao = 8, codigo = 9, fase = 11, valor = 12)
  cat("\nFases:\n"); print(table(s$fase))
  cat("\nCódigos de natureza (coluna 9) - 15 mais frequentes:\n"); print(head(sort(table(s$codigo), decreasing = TRUE), 15))
  cat("\nSubfunções (coluna 8) - valores distintos (até 40):\n"); print(head(unique(s$subfuncao), 40))
  v      <- suppressWarnings(as.numeric(gsub(",", ".", s$valor)))
  falhas <- is.na(v) & !is.na(s$valor) & trimws(as.character(s$valor)) != ""
  cat("\nValores que NÃO viraram número após gsub(',', '.'):", sum(falhas), "\n")
  if (any(falhas)) print(head(unique(s$valor[falhas]), 10))
  for (m in c("351350", "520017")) {     
    cat("\nSubfunções declaradas pelo município ", m, ":\n", sep = "")
    print(unique(s$subfuncao[as.character(s$codigo_ibge6) == m]))
  }
})

bloco("3. Consultas (mgdi_ms_4iu = APS; mgdi_ms_0bx = especializada)", {
  for (arq in c("mgdi_ms_4iu.csv", "mgdi_ms_0bx.csv")) {
    d <- norm(fread(f(arq), encoding = "Latin-1"))
    cat("\n---", arq, "---\nColunas:", paste(names(d), collapse = " | "), "\nLinhas:", nrow(d), "\n")
    print(head(d, 3))
    cat("co_anomes: de", min(d$co_anomes, na.rm = TRUE), "a", max(d$co_anomes, na.rm = TRUE),
        "| meses distintos:", n_distinct(d$co_anomes), "| municípios distintos:", n_distinct(d$co_ibge), "\n")
    m <- d %>% count(co_ibge, co_anomes)
    cat("Linhas por município-mês: mediana =", median(m$n), "| máx =", max(m$n), "\n")
    print(summary(num(d$vl_indicador_calculado_mun)))
    cat("Municípios ausentes em relação aos 5.570:", 5570 - n_distinct(d$co_ibge), "\n")
  }
})

bloco("4. SISAGUA - estrutura e soma nacional da população abastecida por ano", {
  arq <- f("cadastro_populacao_abastecida.csv")
  cab <- fread(arq, sep = ";", encoding = "Latin-1", nrows = 3)
  cat("Colunas:", paste(names(cab), collapse = " | "), "\n"); print(cab)
  sa <- fread(arq, sep = ";", encoding = "Latin-1",
              select = c("CO_MUNICIPIO_IBGE", "NU_ANO", "NU_VALOR_POPULACAO_ESTIMADA"))
  cat("\nLinhas por ano:\n"); print(table(sa$NU_ANO))
  tot <- sa %>% filter(NU_ANO >= 2001, NU_ANO <= 2024) %>% group_by(NU_ANO) %>%
    summarise(soma_pop_abastecida = sum(NU_VALOR_POPULACAO_ESTIMADA, na.rm = TRUE), .groups = "drop")
  cat("\nSoma nacional da população abastecida por ano (referência: ~203 milhões em 2022):\n")
  print(as.data.frame(tot))
  ma <- sa %>% count(CO_MUNICIPIO_IBGE, NU_ANO)
  cat("\nLinhas por município-ano: mediana =", median(ma$n), "| p95 =", quantile(ma$n, .95), "| máx =", max(ma$n), "\n")
})

bloco("5. RIPSA 003 (recursos humanos) - o que é a coluna 'agentes de saúde'?", {
  rh <- fread(f("ripsa003rc.csv"), sep = ",", quote = "")
  names(rh) <- tolower(gsub('"', "", names(rh)))
  cat("Colunas:", paste(names(rh), collapse = " | "), "\n")
  col_mun <- grep("^municipio", names(rh), value = TRUE)[1]
  col_ano <- grep("^ano", names(rh), value = TRUE)[1]
  col_ag  <- grep("agentes de sa", names(rh), value = TRUE)[1]
  cat("Coluna usada como 'agentes de saúde':", col_ag, "\n")
  tot <- data.frame(mun = as.character(rh[[col_mun]]), ano = num(rh[[col_ano]]), ag = num(rh[[col_ag]])) %>%
    filter(!grepl("0000$", mun), ano >= 2010, ano <= 2024) %>%
    group_by(ano) %>% summarise(total_pais = sum(ag, na.rm = TRUE), .groups = "drop")
  cat("\nTotal nacional da coluna por ano (ACS no país: da ordem de 260-270 mil):\n"); print(as.data.frame(tot))
})

bloco("6. Mais Médicos e eMulti - colunas, período e cobertura de municípios", {
  mm <- norm(fread(f("ppf_mais_medicos_serie_historica.csv"), encoding = "Latin-1"))
  cat("--- Mais Médicos ---\nColunas:", paste(names(mm), collapse = " | "), "\nLinhas:", nrow(mm), "\n")
  print(head(mm, 3)); periodo(mm)
  cat("Nº de dígitos do código 'ibge':\n"); print(table(nchar(as.character(mm$ibge))))
  cat("Linhas por município: mediana =", median(table(mm$ibge)), "\n")

  em <- norm(fread(f("emultcust.csv")))
  cat("\n--- eMulti ---\nColunas:", paste(names(em), collapse = " | "), "\nLinhas:", nrow(em), "\n")
  print(head(em, 3)); periodo(em)
  cat("Linhas por município (co_ibge): mediana =", median(table(em$co_ibge)), "\n")
})

sink()
cat("Diagnóstico salvo em:", file.path(getwd(), "diagnostico_portal_sus.txt"), "\n")
