# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 6 - Série Temporal Interrompida/STI_exportar_pendencias_v2.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 6 - Série Temporal Interrompida"))

# =============================================================================
# STI – EXPORTAÇÃO DO QUE FALTA (Resultados 5.7 e Apêndice H)
# Script AUTÔNOMO: refaz os mesmos modelos dos scripts CHAGBR 6.1, 6.3 e DOBR 6.4
# (mesmas variáveis, mesmos marcos, mesma especificação) e salva o que faltava.
#
# v2 (19/10/2026):
#   - lê os .rds com os nomes que estão hoje na pasta (ver arq_aguda e arq_obitos);
#   - modela só Brasil + 5 regiões (a região ignorada entra apenas no total do Brasil);
#   - séries mensais com os 288 meses (jan/2001 a dez/2024);
#   - salva ordens, coeficientes e Ljung-Box dos dois SARIMA antes das curvas
#     contrafactuais (bootstrap), que são a parte demorada;
#   - na BN, registra também o theta inicial e os casos por ano de cada localidade
#     (para explicar falhas de ajuste, como a da Região Sul);
#   - grava uma conferência dos bancos e o sessionInfo() em STI_saidas.
#
# A população é lida da pasta do IBGE, como nos seus scripts.
# Saída: pasta STI_saidas.
# =============================================================================

pasta         <- file.path(DIR_DADOS, "Análises/Parte 6 - Série Temporal Interrompida")
caminho_dados <- paste0(file.path(DIR_DADOS, "IBGE/IBGE_POP_2001_2024"), "/")
arq_aguda     <- "CHAGAS_AGUDA_FILTRADO_2_2001_2024.rds"
arq_obitos    <- "MORT_CHAGAS_FINAL_2_TRATADO_COMPLETO.rds"
GERAR_CURVAS_SARIMA <- TRUE   # FALSE pula os contrafactuais do SARIMA (1.000 simulações; só servem às figuras)

library(tidyverse)
library(tsibble)
library(lubridate)
library(fable)
library(fabletools)
library(feasts)
library(MASS)
library(broom)
library(writexl)
select <- dplyr::select; filter <- dplyr::filter   # o MASS mascara o select do dplyr

setwd(pasta)
dir.create("STI_saidas", showWarnings = FALSE)
inicio <- Sys.time()

pop_padrao_br2022 <- readRDS(paste0(caminho_dados, "pop_padrao_br2022.rds"))
POP_IBGE_TRATADA  <- readRDS(paste0(caminho_dados, "POP_IBGE_REGIÃO_TRATADA.rds"))
POP_IBGE_TRATADA  <- bind_rows(
  POP_IBGE_TRATADA,
  POP_IBGE_TRATADA %>% group_by(ANO_OBITO, FAIXA_QUINQUENAL) %>%
    summarise(populacao_residente = sum(populacao_residente, na.rm = TRUE), .groups = "drop") %>%
    mutate(REGIAO = "Brasil"))
locs_modelo <- unique(as.character(POP_IBGE_TRATADA$REGIAO))   # Brasil + 5 regiões

CHAGAS_AGUDA_FILTRADO_2001_2024 <- readRDS(file.path(pasta, arq_aguda))
MORT_CHAGAS_FINAL_TRATADO       <- readRDS(file.path(pasta, arq_obitos))

# --- Conferência dos bancos (comparar com a Tabela D1) ------------------------
conferir <- function(d, idade, data, nome) {
  cat("\n==", nome, "==\n")
  cat("Registros:", nrow(d), "| sem idade:", sum(is.na(d[[idade]])),
      "| datas:", format(min(d[[data]], na.rm = TRUE)), "a", format(max(d[[data]], na.rm = TRUE)),
      "| datas NA:", sum(is.na(d[[data]])), "\n")
  print(table(Regiao = d$REGIAO_RESIDENCIA, useNA = "ifany"))
  fora <- setdiff(unique(as.character(d$REGIAO_RESIDENCIA)), locs_modelo)
  n_fora <- sum(as.character(d$REGIAO_RESIDENCIA) %in% fora | is.na(d$REGIAO_RESIDENCIA))
  cat("Fora das 5 regiões da população (entram só no Brasil):", n_fora, "\n")
  if (n_fora > 0.01 * nrow(d)) stop("Os nomes de REGIAO_RESIDENCIA não batem com os da população (", nome, ").")
}
conf <- capture.output({
  conferir(CHAGAS_AGUDA_FILTRADO_2001_2024, "IDADE_FINAL", "DT_NOTIFIC", "DC aguda (Tabela D1: 7.531 casos)")
  conferir(MORT_CHAGAS_FINAL_TRATADO, "IDADE_OBITO", "DATA_OBITO", "Óbitos (Tabela D1: 147.573 com idade + 138 sem idade)")
})
writeLines(conf, "STI_saidas/Conferencia_bancos.txt"); cat(conf, sep = "\n")

faixa <- function(idade) cut(idade, breaks = c(seq(0, 80, by = 5), Inf),
                             labels = pop_padrao_br2022$FAIXA_QUINQUENAL, right = FALSE)
zera <- function(d, vars) mutate(d, across(all_of(vars), ~ 0))
v1 <- c("nivel_int1", "tendencia_int1"); v2 <- c("nivel_int2", "tendencia_int2"); v3 <- c("nivel_int3", "tendencia_int3")

# =============================================================================
# A) BINOMIAL NEGATIVA – DC aguda, trimestral (igual ao CHAGBR 6.3)
# =============================================================================
CASOS_TRIMESTRE_FAIXA <- CHAGAS_AGUDA_FILTRADO_2001_2024 %>%
  filter(!is.na(IDADE_FINAL)) %>%
  mutate(data_trimestre = yearquarter(DT_NOTIFIC), ano = year(DT_NOTIFIC),
         localidade = as.character(REGIAO_RESIDENCIA), FAIXA_QUINQUENAL = faixa(IDADE_FINAL)) %>%
  filter(!is.na(data_trimestre)) %>%
  group_by(localidade, data_trimestre, ano, FAIXA_QUINQUENAL) %>%
  summarise(casos = n(), .groups = "drop")
CASOS_TRIMESTRE_FAIXA <- bind_rows(
  CASOS_TRIMESTRE_FAIXA,
  CASOS_TRIMESTRE_FAIXA %>% group_by(data_trimestre, ano, FAIXA_QUINQUENAL) %>%
    summarise(casos = sum(casos), .groups = "drop") %>% mutate(localidade = "Brasil"))

dq1 <- yearquarter("2005 Q1"); dq2 <- yearquarter("2015 Q1"); dq3 <- yearquarter("2020 Q1")

CASOS_CHAGAS_STI <- tidyr::expand_grid(
    localidade = locs_modelo,
    data_trimestre = yearquarter(seq(as.Date("2001-01-01"), as.Date("2024-12-01"), by = "quarter")),
    FAIXA_QUINQUENAL = unique(pop_padrao_br2022$FAIXA_QUINQUENAL)) %>%
  mutate(ano = year(data_trimestre), trimestre_sazonal = as.factor(quarter(data_trimestre))) %>%
  left_join(CASOS_TRIMESTRE_FAIXA, by = c("localidade", "data_trimestre", "ano", "FAIXA_QUINQUENAL")) %>%
  left_join(POP_IBGE_TRATADA, by = c("localidade" = "REGIAO", "ano" = "ANO_OBITO", "FAIXA_QUINQUENAL")) %>%
  mutate(casos = replace_na(casos, 0)) %>%
  group_by(localidade) %>% mutate(tempo = dense_rank(data_trimestre)) %>% ungroup() %>%
  mutate(nivel_int1 = if_else(data_trimestre >= dq1, 1, 0),
         tendencia_int1 = if_else(data_trimestre >= dq1, tempo - max(tempo[data_trimestre < dq1]), 0),
         nivel_int2 = if_else(data_trimestre >= dq2, 1, 0),
         tendencia_int2 = if_else(data_trimestre >= dq2, tempo - max(tempo[data_trimestre < dq2]), 0),
         nivel_int3 = if_else(data_trimestre >= dq3, 1, 0),
         tendencia_int3 = if_else(data_trimestre >= dq3, tempo - max(tempo[data_trimestre < dq3]), 0))

TAXA_REAL_TRIM <- CASOS_CHAGAS_STI %>%
  left_join(pop_padrao_br2022, by = "FAIXA_QUINQUENAL") %>%
  mutate(casos_esp = (casos / populacao_residente) * peso_pop_padrao) %>%
  group_by(localidade, data_trimestre) %>%
  summarise(taxa = sum(casos_esp, na.rm = TRUE) / sum(peso_pop_padrao, na.rm = TRUE) * 1e5, .groups = "drop")

pred_padronizada <- function(modelo, novos) {
  d <- novos %>% left_join(pop_padrao_br2022, by = "FAIXA_QUINQUENAL") %>% mutate(populacao_residente = peso_pop_padrao)
  p <- predict(modelo, newdata = d, type = "link", se.fit = TRUE)
  d %>% mutate(e = exp(p$fit), i = exp(p$fit - 1.96 * p$se.fit), s = exp(p$fit + 1.96 * p$se.fit)) %>%
    group_by(data_trimestre) %>%
    summarise(valor = sum(e) / sum(peso_pop_padrao) * 1e5, ic_inf = sum(i) / sum(peso_pop_padrao) * 1e5,
              ic_sup = sum(s) / sum(peso_pop_padrao) * 1e5, .groups = "drop")
}

formula_bn <- casos ~ tempo + nivel_int1 + tendencia_int1 + nivel_int2 + tendencia_int2 +
  nivel_int3 + tendencia_int3 + trimestre_sazonal + FAIXA_QUINQUENAL + offset(log(populacao_residente))

COEF_BN <- list(); DIAG_BN <- list(); CURVAS_BN <- list(); RESUMOS_BN <- character(0)
for (loc in locs_modelo) {
  df <- CASOS_CHAGAS_STI %>% filter(localidade == loc)
  avisos <- character(0); erro <- NA_character_
  m <- withCallingHandlers(
    tryCatch(glm.nb(formula_bn, data = df, link = log),
             error = function(e) { erro <<- conditionMessage(e); NULL }),
    warning = function(w) { avisos <<- c(avisos, conditionMessage(w)); invokeRestart("muffleWarning") })
  # Diagnóstico de falhas: theta inicial que o glm.nb estima a partir do ajuste de Poisson.
  # Valores próximos de zero indicam superdispersão extrema (ex.: um surto isolado na série).
  p0 <- tryCatch(suppressWarnings(glm(formula_bn, data = df, family = poisson)), error = function(e) NULL)
  theta0 <- if (is.null(p0)) NA_real_ else
    tryCatch(suppressWarnings(as.numeric(theta.ml(p0$y, fitted(p0), limit = 25))), error = function(e) NA_real_)
  diag <- tibble(localidade = loc, ajustou = !is.null(m), erro = erro,
                 avisos = paste(unique(avisos), collapse = " | "),
                 theta = if (is.null(m)) NA else m$theta, ep_theta = if (is.null(m)) NA else m$SE.theta,
                 th_warn = if (is.null(m) || is.null(m$th.warn)) NA_character_ else m$th.warn,
                 convergiu = if (is.null(m)) NA else m$converged,
                 theta_inicial_poisson = theta0,
                 n_casos = sum(df$casos), linhas_sem_pop = sum(is.na(df$populacao_residente)))
  casos_ano <- df %>% group_by(ano) %>% summarise(casos = sum(casos), .groups = "drop")
  RESUMOS_BN <- c(RESUMOS_BN, paste0("\n========== ", loc, " =========="),
                  if (is.null(m)) paste("ERRO:", erro) else capture.output(summary(m)),
                  if (length(avisos)) paste("AVISOS:", paste(unique(avisos), collapse = " | ")),
                  paste("theta inicial (Poisson):", signif(theta0, 3)),
                  paste("Casos por ano:", paste(casos_ano$ano, casos_ano$casos, sep = "=", collapse = " ")))
  if (!is.null(m)) {
    COEF_BN[[loc]] <- tidy(m) %>% filter(str_detect(term, "^(tempo|nivel_int|tendencia_int)")) %>%
      mutate(localidade = loc, RTI = exp(estimate),
             IC_inf = exp(estimate - 1.96 * std.error), IC_sup = exp(estimate + 1.96 * std.error))
    r <- residuals(m, type = "pearson")
    rt <- df[as.integer(names(r)), ] %>% mutate(r = r) %>% group_by(data_trimestre) %>% summarise(r = sum(r), .groups = "drop")
    lb <- Box.test(rt$r, lag = 8, type = "Ljung-Box")
    diag <- diag %>% mutate(lb_Q_lag8 = unname(lb$statistic), lb_p = lb$p.value)
    CURVAS_BN[[loc]] <- bind_rows(
      TAXA_REAL_TRIM %>% filter(localidade == loc) %>% transmute(data_trimestre, valor = taxa, serie = "observada"),
      pred_padronizada(m, df) %>% mutate(serie = "ajustada"),
      pred_padronizada(m, df %>% filter(data_trimestre >= dq1, data_trimestre < dq2) %>% zera(c(v1, v2, v3))) %>% mutate(serie = "contrafactual_2005"),
      pred_padronizada(m, df %>% filter(data_trimestre >= dq2, data_trimestre < dq3) %>% zera(c(v2, v3))) %>% mutate(serie = "contrafactual_2015"),
      pred_padronizada(m, df %>% filter(data_trimestre >= dq3) %>% zera(v3)) %>% mutate(serie = "contrafactual_2020")
    ) %>% mutate(localidade = loc, data = as.Date(data_trimestre)) %>% select(-data_trimestre)
  }
  DIAG_BN[[loc]] <- diag
}
write_xlsx(bind_rows(COEF_BN),   "STI_saidas/Resultados_Coeficientes_BN_Incidencia.xlsx")
write_xlsx(bind_rows(DIAG_BN),   "STI_saidas/Diagnostico_BN_Incidencia.xlsx")
write_xlsx(bind_rows(CURVAS_BN), "STI_saidas/Curvas_BN_Incidencia.xlsx")
writeLines(RESUMOS_BN, "STI_saidas/Resumo_modelos_BN_Incidencia.txt")
print(bind_rows(DIAG_BN))
message("BN pronta (", round(difftime(Sys.time(), inicio, units = "mins"), 1), " min).")

# =============================================================================
# B e C) SARIMA das taxas mensais padronizadas (iguais ao CHAGBR 6.1 e DOBR 6.4)
# =============================================================================
dm1 <- yearmonth("2005-01"); dm2 <- yearmonth("2015-01"); dm3 <- yearmonth("2020-03")
dm_ini <- yearmonth("2001-01"); dm_fim <- yearmonth("2024-12")

serie_mensal <- function(dados, idade, data) {
  b <- dados %>% filter(!is.na({{ idade }})) %>%
    mutate(data_mes = yearmonth({{ data }}), localidade = as.character(REGIAO_RESIDENCIA),
           FAIXA_QUINQUENAL = faixa({{ idade }})) %>%
    filter(!is.na(data_mes), data_mes >= dm_ini, data_mes <= dm_fim) %>%
    group_by(localidade, data_mes, FAIXA_QUINQUENAL) %>% summarise(n = n(), .groups = "drop")
  b <- bind_rows(b, b %>% group_by(data_mes, FAIXA_QUINQUENAL) %>% summarise(n = sum(n), .groups = "drop") %>%
                   mutate(localidade = "Brasil")) %>%
    filter(localidade %in% locs_modelo)
  b %>% complete(localidade, data_mes, FAIXA_QUINQUENAL, fill = list(n = 0)) %>%
    mutate(ano = year(data_mes)) %>%
    left_join(POP_IBGE_TRATADA, by = c("localidade" = "REGIAO", "ano" = "ANO_OBITO", "FAIXA_QUINQUENAL")) %>%
    left_join(pop_padrao_br2022, by = "FAIXA_QUINQUENAL") %>%
    mutate(esp = (n / populacao_residente) * peso_pop_padrao) %>%
    group_by(localidade, data_mes) %>%
    summarise(taxa_padronizada = sum(esp, na.rm = TRUE) / sum(peso_pop_padrao, na.rm = TRUE) * 1e5, .groups = "drop") %>%
    as_tsibble(index = data_mes, key = localidade) %>%
    fill_gaps(taxa_padronizada = 0, .start = dm_ini, .end = dm_fim) %>%
    group_by(localidade) %>%
    mutate(tempo = row_number(),
           nivel_int1 = if_else(data_mes >= dm1, 1, 0),
           tendencia_int1 = if_else(data_mes >= dm1, tempo - max(tempo[data_mes < dm1]), 0),
           nivel_int2 = if_else(data_mes >= dm2, 1, 0),
           tendencia_int2 = if_else(data_mes >= dm2, tempo - max(tempo[data_mes < dm2]), 0),
           nivel_int3 = if_else(data_mes >= dm3, 1, 0),
           tendencia_int3 = if_else(data_mes >= dm3, tempo - max(tempo[data_mes < dm3]), 0)) %>%
    ungroup()
}

ajustar_sarima <- function(serie, prefixo) {
  fit <- serie %>% model(sarima_mits = ARIMA(log(taxa_padronizada + 0.001) ~ tempo +
                                               nivel_int1 + tendencia_int1 + nivel_int2 + tendencia_int2 +
                                               nivel_int3 + tendencia_int3 + pdq() + PDQ(period = 12),
                                             stepwise = FALSE, approximation = FALSE))
  ordens <- tibble(localidade = fit$localidade, modelo = purrr::map_chr(fit$sarima_mits, fabletools::model_sum))
  write_xlsx(ordens, paste0("STI_saidas/Ordens_SARIMA_", prefixo, ".xlsx"))
  writeLines(capture.output(print(fit, width = 200)), paste0("STI_saidas/Print_SARIMA_", prefixo, ".txt"))
  write_xlsx(tidy(fit), paste0("STI_saidas/Coeficientes_completos_SARIMA_", prefixo, ".xlsx"))
  write_xlsx(augment(fit) %>% features(.innov, ljung_box, lag = 24, dof = 0),
             paste0("STI_saidas/LjungBox_SARIMA_", prefixo, ".xlsx"))
  print(ordens)
  message("SARIMA ", prefixo, " ajustado (", round(difftime(Sys.time(), inicio, units = "mins"), 1), " min).")
  fit
}

curvas_sarima <- function(fit, serie, prefixo) {
  c1 <- serie %>% filter(data_mes >= dm1, data_mes < dm2) %>% zera(c(v1, v2, v3))
  c2 <- serie %>% filter(data_mes >= dm2, data_mes < dm3) %>% zera(c(v2, v3))
  c3 <- serie %>% filter(data_mes >= dm3) %>% zera(v3)
  cf <- bind_rows(
    forecast(fit, new_data = c1, bootstrap = TRUE, times = 1000) %>% as_tibble() %>% mutate(serie = "contrafactual_2005"),
    forecast(fit, new_data = c2, bootstrap = TRUE, times = 1000) %>% as_tibble() %>% mutate(serie = "contrafactual_2015"),
    forecast(fit, new_data = c3, bootstrap = TRUE, times = 1000) %>% as_tibble() %>% mutate(serie = "contrafactual_2020")) %>%
    mutate(ic = hilo(taxa_padronizada, 95)) %>% unpack_hilo(ic) %>%
    transmute(localidade, data = as.Date(data_mes), valor = .mean, ic_inf = ic_lower, ic_sup = ic_upper, serie)
  curvas <- bind_rows(
    serie %>% as_tibble() %>% transmute(localidade, data = as.Date(data_mes), valor = taxa_padronizada, serie = "observada"),
    fitted(fit) %>% as_tibble() %>% transmute(localidade, data = as.Date(data_mes), valor = .fitted, serie = "ajustada"),
    cf)
  write_xlsx(curvas, paste0("STI_saidas/Curvas_SARIMA_", prefixo, ".xlsx"))
  message("Curvas SARIMA ", prefixo, " salvas (", round(difftime(Sys.time(), inicio, units = "mins"), 1), " min).")
}

SERIE_AGUDA  <- serie_mensal(CHAGAS_AGUDA_FILTRADO_2001_2024, IDADE_FINAL, DT_NOTIFIC)
SERIE_OBITOS <- serie_mensal(MORT_CHAGAS_FINAL_TRATADO, IDADE_OBITO, DATA_OBITO)
print(SERIE_AGUDA %>% as_tibble() %>% count(localidade, name = "meses"))

FIT_STI_INC <- ajustar_sarima(SERIE_AGUDA, "Aguda")    # = FIT_STI_INC do CHAGBR 6.1
FIT_STI     <- ajustar_sarima(SERIE_OBITOS, "Obitos")  # = FIT_STI do DOBR 6.4

if (GERAR_CURVAS_SARIMA) {
  curvas_sarima(FIT_STI_INC, SERIE_AGUDA, "Aguda")
  curvas_sarima(FIT_STI, SERIE_OBITOS, "Obitos")
}

writeLines(capture.output(sessionInfo()), "STI_saidas/sessionInfo.txt")
message("Pronto (", round(difftime(Sys.time(), inicio, units = "mins"), 1), " min). Saídas em ", file.path(pasta, "STI_saidas"))
