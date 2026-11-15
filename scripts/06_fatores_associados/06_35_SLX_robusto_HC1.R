# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final/Conserto_1_SLX_Robusto_HC1.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final"))

# =============================================================================
# CONSERTO 1 - SLX COM ERROS-PADRAO ROBUSTOS (HC1) + EFEITOS POR DESVIO-PADRAO
# Parte 5.7 V4 - os tres desfechos. NAO refaz nenhum modelo espacial por ML.
# -----------------------------------------------------------------------------
# POR QUE ESTE SCRIPT EXISTE
#   O bloco "robusto a heterocedasticidade" da rodada de 02/10 degenerou:
#
#   (a) SDM por 2SLS - "Rho" saiu 0,0000 nos TRES desfechos (contra 0,787 na
#       mortalidade, 0,753 na aguda e 0,466 na cronica por maxima
#       verossimilhanca). As colunas WX entram como regressores e ao mesmo
#       tempo compoem o conjunto de instrumentos que o stsls monta a partir da
#       propria matriz do modelo; com isso o rho deixa de ser identificado.
#       E com rho = 0 a decomposicao de LeSage & Pace colapsa em
#       direto = beta, indireto = theta, total = beta + theta - ou seja, um
#       SLX com outro nome. Confere na aguda, variavel por variavel, contra a
#       Tabela 10 do Word.
#
#   (b) SAR por 2SLS - funcionou SO na mortalidade. Na aguda e na cronica
#       todos os p ficaram acima de 0,94, porque o sanduiche HC0 e dominado
#       por poucos municipios extremos (10 municipios concentram 68% da
#       variancia na aguda; 3 concentram 69% na cronica).
#
# O QUE ESTE SCRIPT FAZ
#   Reestima o MESMO SLX que ja esta na Tabela 10 do Word (X + WX nas 5
#   defasagens do SDM, ajustado por lm), trocando apenas a matriz de variancia
#   pelo HC1 de White. Mesma especificacao, mesmas estimativas pontuais: muda
#   so a inferencia. Entrega direto/indireto/total com EP, p e IC95%, sem peso
#   e ponderado pela populacao, lado a lado com a versao de EP classicos para
#   voce poder mostrar o quanto a inferencia muda.
#   Entrega tambem: (i) os efeitos do SAR e do SDM por ML reexpressos por
#   desvio-padrao do regressor - necessario para o spei, cuja amplitude e de
#   0,06 e por isso o coeficiente por unidade sai em centenas; (ii) um
#   diagnostico de concentracao do desfecho, para a secao de limitacoes.
#
# COMO USAR
#   Conferir dir_saida abaixo e rodar o arquivo inteiro (source).
#   Nada e sobrescrito: toda saida nova leva sufixo _HC1, _PorDP ou
#   _Concentracao.
#
# TEMPO: ~5 min por desfecho. O poly2nb e o unico passo lento (~4 min).
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(sf, spdep, sandwich, dplyr, purrr, readr, tibble)

# >>> CONFERIR ESTE CAMINHO <<<
dir_saida <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final")

SUFIXOS <- c("Mortalidade", "Aguda", "Cronica")

arquivo_log <- file.path(dir_saida, sprintf("LOG_Conserto1_SLX_HC1_%s.txt",
                                            format(Sys.time(), "%Y%m%d_%H%M%S")))
log_msg <- function(...) {
  msg <- sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...))
  cat(msg, "\n"); cat(msg, "\n", file = arquivo_log, append = TRUE); flush.console()
}

# ---- efeitos de um SLX a partir de coef() e de uma matriz de variancia -----
# Num SLX nao ha (I - rho W)^-1: o efeito direto de x e o proprio beta, o
# indireto e o theta da defasagem e o total e a soma. O EP do total sai do
# metodo delta: Var(b + t) = Var(b) + Var(t) + 2 Cov(b, t). A unica diferenca
# em relacao ao script original e QUAL matriz V entra aqui.
efeitos_slx <- function(m, V, vars_durbin, rotulo) {
  b <- coef(m)
  z <- stats::qnorm(0.975)
  p2 <- function(e, s) 2 * stats::pnorm(-abs(e / s))
  purrr::map_dfr(vars_durbin, function(v) {
    ln <- paste0("lag_", v)
    if (!(v %in% names(b)) || !(ln %in% names(b))) return(NULL)
    if (is.na(b[[v]]) || is.na(b[[ln]]))           return(NULL)
    if (!(v %in% rownames(V)) || !(ln %in% rownames(V))) return(NULL)
    dir <- unname(b[v]); ind <- unname(b[ln]); tot <- dir + ind
    s_d <- sqrt(V[v, v]); s_i <- sqrt(V[ln, ln])
    s_t <- sqrt(V[v, v] + V[ln, ln] + 2 * V[v, ln])
    tibble(
      Variavel        = v,
      Especificacao   = rotulo,
      Efeito_Direto   = dir, EP_Direto   = s_d, P_Direto   = p2(dir, s_d),
      IC_Direto_inf   = dir - z * s_d, IC_Direto_sup   = dir + z * s_d,
      Efeito_Indireto = ind, EP_Indireto = s_i, P_Indireto = p2(ind, s_i),
      IC_Indireto_inf = ind - z * s_i, IC_Indireto_sup = ind + z * s_i,
      Efeito_Total    = tot, EP_Total    = s_t, P_Total    = p2(tot, s_t),
      IC_Total_inf    = tot - z * s_t, IC_Total_sup    = tot + z * s_t)
  })
}

assimetria <- function(x) { x <- x[!is.na(x)]; n <- length(x)
  m <- mean(x); s <- sqrt(sum((x - m)^2) / (n - 1)); sum(((x - m) / s)^3) / n }
curtose <- function(x) { x <- x[!is.na(x)]; n <- length(x)
  m <- mean(x); s <- sqrt(sum((x - m)^2) / (n - 1)); sum(((x - m) / s)^4) / n - 3 }

processar <- function(sfx) {

  dir_d <- file.path(dir_saida, sfx)
  log_msg("==== ", sfx, " ====")

  # A configuracao vem do proprio checkpoint da rodada: garante que covariaveis
  # e defasagens sejam EXATAMENTE as usadas em 02/10, sem redigitar nada.
  arq_chk <- file.path(dir_d, paste0("Checkpoint_Parte57_V4_", sfx, ".rds"))
  if (!file.exists(arq_chk)) stop("checkpoint nao encontrado: ", arq_chk)
  chk <- readRDS(arq_chk)
  cfg <- chk$cfg
  vars_durbin <- chk$vars_durbin
  log_msg("   desfecho: ", cfg$desfecho, " | ", length(cfg$covariaveis), " covariaveis")
  log_msg("   defasagens: ", paste(vars_durbin, collapse = ", "))

  arq_gpkg <- file.path(dir_d, paste0("Diagnostico_", sfx, "_GWR_e_SDM.gpkg"))
  if (!file.exists(arq_gpkg)) stop("gpkg nao encontrado: ", arq_gpkg)
  mapa <- sf::st_read(arq_gpkg, quiet = TRUE)
  log_msg("   gpkg lido: ", nrow(mapa), " municipios")
  if (nrow(mapa) != chk$n)
    log_msg("   !! ATENCAO: gpkg tem ", nrow(mapa), " linhas e o checkpoint diz n = ", chk$n)

  faltam <- setdiff(c(cfg$desfecho, cfg$covariaveis), names(mapa))
  if (length(faltam) > 0) stop("colunas ausentes no gpkg: ", paste(faltam, collapse = ", "))

  sf::sf_use_s2(FALSE)
  t0 <- Sys.time()
  nb <- spdep::poly2nb(mapa, queen = TRUE)
  lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  log_msg("   matriz Queen reconstruida em ",
          round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min | ",
          sum(spdep::card(nb) == 0), " municipio(s) sem vizinho")

  dados <- sf::st_drop_geometry(mapa)

  WX <- sapply(vars_durbin, function(v)
    spdep::lag.listw(lw, dados[[v]], zero.policy = TRUE))
  colnames(WX) <- paste0("lag_", vars_durbin)
  dados_aug <- cbind(dados, as.data.frame(WX))

  form_slx <- stats::as.formula(paste(
    cfg$desfecho, "~",
    paste(c(cfg$covariaveis, colnames(WX)), collapse = " + ")))

  # ---- SLX sem peso: EP classico x EP robusto HC1 --------------------------
  m_sem <- stats::lm(form_slx, data = dados_aug)
  V_cl  <- stats::vcov(m_sem)
  V_hc  <- sandwich::vcovHC(m_sem, type = "HC1")

  saida <- dplyr::bind_rows(
    efeitos_slx(m_sem, V_cl, vars_durbin, "SLX sem peso - EP classico"),
    efeitos_slx(m_sem, V_hc, vars_durbin, "SLX sem peso - EP robusto HC1"))

  # ---- SLX ponderado pela populacao, tambem com HC1 ------------------------
  # 'pop_media' e coluna de dados_aug, entao lm() resolve o peso dentro de
  # data e o problema de ambiente da rodada anterior nao reaparece.
  if ("pop_media" %in% names(dados_aug) &&
      !any(is.na(dados_aug$pop_media)) && all(dados_aug$pop_media > 0)) {
    m_com  <- stats::lm(form_slx, data = dados_aug, weights = pop_media)
    V_cl_p <- stats::vcov(m_com)
    V_hc_p <- sandwich::vcovHC(m_com, type = "HC1")
    saida <- dplyr::bind_rows(saida,
      efeitos_slx(m_com, V_cl_p, vars_durbin, "SLX ponderado - EP classico"),
      efeitos_slx(m_com, V_hc_p, vars_durbin, "SLX ponderado - EP robusto HC1"))
  } else {
    log_msg("   !! pop_media ausente, com NA ou <= 0 no gpkg: versao ponderada pulada")
  }

  saida$Matriz <- "Queen"
  saida$Desfecho <- cfg$rotulo
  readr::write_csv(saida, file.path(dir_d, paste0("Impactos_SLX_HC1_", sfx, ".csv")))
  log_msg("   gravado: Impactos_SLX_HC1_", sfx, ".csv")

  # comparacao classico x robusto, na mesma ordem de variaveis
  a <- saida[saida$Especificacao == "SLX sem peso - EP classico", ]
  b <- saida[saida$Especificacao == "SLX sem peso - EP robusto HC1", ]
  b <- b[match(a$Variavel, b$Variavel), ]
  log_msg("   razao EP robusto / EP classico (efeito total): ",
          paste(sprintf("%s=%.2f", a$Variavel, b$EP_Total / a$EP_Total), collapse = " | "))
  log_msg("   perdem significancia no total com HC1: ",
          sum(a$P_Total < 0.05 & b$P_Total >= 0.05), " de ", nrow(a),
          " | ganham: ", sum(a$P_Total >= 0.05 & b$P_Total < 0.05))

  # ---- efeitos por desvio-padrao do regressor ------------------------------
  dp <- vapply(cfg$covariaveis, function(v) stats::sd(dados[[v]], na.rm = TRUE), numeric(1))
  tab_dp <- tibble::tibble(Variavel = names(dp), DP_do_regressor = as.numeric(dp),
                           Minimo = vapply(cfg$covariaveis, function(v) min(dados[[v]], na.rm = TRUE), numeric(1)),
                           Maximo = vapply(cfg$covariaveis, function(v) max(dados[[v]], na.rm = TRUE), numeric(1)))

  reescalar <- function(arquivo, rotulo) {
    cam <- file.path(dir_d, arquivo)
    if (!file.exists(cam)) { log_msg("   (sem ", arquivo, ")"); return(NULL) }
    d <- readr::read_csv(cam, show_col_types = FALSE)
    precisa <- c("Variavel", "Efeito_Direto", "Efeito_Indireto", "Efeito_Total")
    if (!all(precisa %in% names(d))) { log_msg("   (", arquivo, " sem as colunas esperadas)"); return(NULL) }
    if (!"Matriz" %in% names(d)) d$Matriz <- NA_character_
    for (cc in c("IC_Total_inf", "IC_Total_sup")) if (!cc %in% names(d)) d[[cc]] <- NA_real_
    d %>%
      dplyr::inner_join(tab_dp, by = "Variavel") %>%
      dplyr::mutate(
        Modelo = rotulo,
        Direto_porDP   = Efeito_Direto   * DP_do_regressor,
        Indireto_porDP = Efeito_Indireto * DP_do_regressor,
        Total_porDP    = Efeito_Total    * DP_do_regressor,
        IC_Total_inf_porDP = IC_Total_inf * DP_do_regressor,
        IC_Total_sup_porDP = IC_Total_sup * DP_do_regressor) %>%
      dplyr::select(dplyr::any_of(c("Modelo", "Matriz", "Variavel", "DP_do_regressor",
                    "Minimo", "Maximo", "Direto_porDP", "P_Direto",
                    "Indireto_porDP", "P_Indireto", "Total_porDP",
                    "IC_Total_inf_porDP", "IC_Total_sup_porDP", "P_Total")))
  }

  por_dp <- dplyr::bind_rows(
    reescalar(paste0("Impactos_SAR_", sfx, ".csv"), "SAR (ML)"),
    reescalar(paste0("Impactos_SDM_Parcial_", sfx, ".csv"), "SDM parcial (ML)"))
  if (!is.null(por_dp) && nrow(por_dp) > 0) {
    readr::write_csv(por_dp, file.path(dir_d, paste0("Impactos_PorDP_", sfx, ".csv")))
    log_msg("   gravado: Impactos_PorDP_", sfx, ".csv")
  }

  # ---- concentracao do desfecho (secao de limitacoes) ---------------------
  y <- dados[[cfg$desfecho]]
  svar <- sum((y - mean(y))^2)
  parcela <- function(k) 100 * sum((sort(y, decreasing = TRUE)[1:k] - mean(y))^2) / svar
  conc <- tibble::tibble(
    Desfecho = cfg$rotulo, n = length(y),
    Media = mean(y), DP = stats::sd(y), Mediana = stats::median(y),
    P99 = unname(stats::quantile(y, 0.99)), Maximo = max(y),
    Razao_max_mediana = max(y) / stats::median(y),
    Assimetria = assimetria(y), Curtose_excesso = curtose(y),
    Pct_variancia_3_maiores  = parcela(3),
    Pct_variancia_10_maiores = parcela(10),
    Pct_variancia_50_maiores = parcela(50))
  readr::write_csv(conc, file.path(dir_d, paste0("Diagnostico_Concentracao_", sfx, ".csv")))
  log_msg("   concentracao: variancia nos 10 maiores = ",
          round(conc$Pct_variancia_10_maiores, 1), "% | curtose = ", round(conc$Curtose_excesso, 1))

  rm(nb, lw, mapa, dados, dados_aug); gc(verbose = FALSE)
  list(slx = saida, por_dp = por_dp, concentracao = conc)
}

res <- list()
for (sfx in SUFIXOS) {
  res[[sfx]] <- tryCatch(processar(sfx),
                         error = function(e) { log_msg(">> FALHA em ", sfx, ": ", conditionMessage(e)); NULL })
}

# ---- consolidados na raiz ---------------------------------------------------
cons_slx <- dplyr::bind_rows(lapply(res, function(x) if (!is.null(x)) x$slx))
if (nrow(cons_slx) > 0) {
  readr::write_csv(cons_slx, file.path(dir_saida, "Impactos_SLX_HC1_Tres_Desfechos.csv"))
  log_msg("gravado: Impactos_SLX_HC1_Tres_Desfechos.csv")
}
cons_conc <- dplyr::bind_rows(lapply(res, function(x) if (!is.null(x)) x$concentracao))
if (nrow(cons_conc) > 0) {
  readr::write_csv(cons_conc, file.path(dir_saida, "Diagnostico_Concentracao_Tres_Desfechos.csv"))
  log_msg("gravado: Diagnostico_Concentracao_Tres_Desfechos.csv")
}
log_msg("FIM do Conserto 1")
