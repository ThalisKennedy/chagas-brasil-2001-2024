# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final/Conserto_2_GWR.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final"))

# =============================================================================
# CONSERTO 2 - DOIS REPAROS NO GWR
# Parte 5.7 V4. Nenhum modelo global e refeito; nada antigo e sobrescrito.
# -----------------------------------------------------------------------------
# BLOCO A - STEPWISE DA MORTALIDADE (o que falhou em 02/10)
#   Na rodada, gwr.model.selection morreu com "inv(): matrix is singular"
#   porque recebeu a banda da especificacao principal: 79 vizinhos. Numa
#   janela de 79 municipios, candidatas com muitos zeros estruturais
#   (pct_quilombola_2022, pct_indigena_2022, densidade_familias_assentadas_km2)
#   ficam constantes e o X'X local fica singular. A prova de que a causa era a
#   banda e que o MESMO bloco rodou sem erro na aguda (banda 2.809) e na
#   cronica (banda 449). Este bloco repete a selecao com a banda da
#   especificacao 'alternativa' (259) e, se ainda falhar, com a da 'original'
#   (3.119). A banda usada fica registrada na saida - e preciso declarar no
#   texto que a selecao foi feita com banda fixa diferente da principal.
#
# BLOCO B - GWR DA AGUDA SEM O ACAI  (opcional: muda a especificacao)
#   pct_area_acai e zero em 94,0% dos municipios (so 333 tem valor, 274 deles
#   na Amazonia Legal). Resultado na rodada: o coeficiente local vai de -21,3
#   a +356,3, e |coeficiente| > 10 em 906 municipios - TODOS eles com
#   acai = 0. E a mesma objecao de identificacao que o script ja aplica ao
#   amazonia_legal_flag via vars_fora_gwr, so que aplicada a variavel que de
#   fato dirige o modelo da aguda. Este bloco reajusta o GWR da aguda
#   excluindo o acai, para voce ter a alternativa na mao. Se preferir manter a
#   especificacao original e apenas declarar a limitacao, deixe o bloco
#   desligado - os numeros acima ja sustentam o argumento.
#
# COMO USAR
#   Conferir dir_saida, escolher os blocos nas chaves abaixo e rodar o arquivo.
#
# TEMPO: Bloco A ~8 min. Bloco B ~8 min (12 min com o I de Moran).
# MEMORIA: a matriz de distancias ocupa ~250 MB por desfecho; os blocos rodam
#   em sequencia com rm()+gc() entre eles.
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(sf, sp, spdep, GWmodel, dplyr, purrr, readr, tibble)

# >>> CONFERIR ESTE CAMINHO <<<
dir_saida <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final")

FAZER_BLOCO_A <- TRUE    # stepwise da mortalidade com banda maior
FAZER_BLOCO_B <- TRUE    # GWR da aguda sem pct_area_acai
CALCULAR_MORAN <- TRUE   # I de Moran dos residuos no bloco B (+4 min: poly2nb)

BANDAS_STEPWISE <- c(259, 3119)  # 79 falhou; tenta na ordem e para na primeira que der
N_CAND_STEPWISE <- 10
CRS_PROJETADO  <- 5880
CRS_GEOGRAFICO <- 4674

arquivo_log <- file.path(dir_saida, sprintf("LOG_Conserto2_GWR_%s.txt",
                                            format(Sys.time(), "%Y%m%d_%H%M%S")))
log_msg <- function(...) {
  msg <- sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...))
  cat(msg, "\n"); cat(msg, "\n", file = arquivo_log, append = TRUE); flush.console()
}

# ---- preparo comum: le o gpkg e monta sp_pts + matriz de distancias --------
# Reproduz exatamente o preparo da rodada original: centroide calculado em CRS
# PROJETADO (EPSG:5880) e devolvido em lon/lat, com gw.dist(longlat = TRUE).
# Sem isso a banda nao seria comparavel com a da Tabela 13.
preparar <- function(sfx) {
  dir_d <- file.path(dir_saida, sfx)
  arq_chk <- file.path(dir_d, paste0("Checkpoint_Parte57_V4_", sfx, ".rds"))
  if (!file.exists(arq_chk)) stop("checkpoint nao encontrado: ", arq_chk)
  chk <- readRDS(arq_chk); cfg <- chk$cfg

  arq_gpkg <- file.path(dir_d, paste0("Diagnostico_", sfx, "_GWR_e_SDM.gpkg"))
  if (!file.exists(arq_gpkg)) stop("gpkg nao encontrado: ", arq_gpkg)
  sf::sf_use_s2(FALSE)
  mapa <- sf::st_read(arq_gpkg, quiet = TRUE)
  log_msg("   gpkg: ", nrow(mapa), " municipios")

  cent <- suppressWarnings(
    sf::st_centroid(sf::st_geometry(sf::st_transform(mapa, CRS_PROJETADO))))
  coords <- sf::st_coordinates(sf::st_transform(cent, CRS_GEOGRAFICO))

  dados <- sf::st_drop_geometry(mapa)
  faltam <- setdiff(c(cfg$desfecho, cfg$covariaveis), names(dados))
  if (length(faltam) > 0) stop("colunas ausentes no gpkg: ", paste(faltam, collapse = ", "))

  dados_gwr <- dados[, c(cfg$desfecho, cfg$covariaveis), drop = FALSE]
  dados_gwr$coord_x <- coords[, 1]; dados_gwr$coord_y <- coords[, 2]
  sp_pts <- as(sf::st_as_sf(dados_gwr, coords = c("coord_x", "coord_y"),
                            crs = CRS_GEOGRAFICO), "Spatial")

  log_msg("   pre-calculando a matriz de distancias (km)...")
  dMat <- GWmodel::gw.dist(coords, longlat = TRUE)

  ranking <- NULL
  arq_rk <- file.path(dir_d, paste0("Ranking_LASSO_", sfx, ".csv"))
  if (file.exists(arq_rk)) ranking <- readr::read_csv(arq_rk, show_col_types = FALSE)

  list(cfg = cfg, dir_d = dir_d, mapa = mapa, dados = dados,
       sp_pts = sp_pts, dMat = dMat, ranking = ranking, n = nrow(mapa))
}

# ---- ajuste de um GWR, com os mesmos diagnosticos da rodada ----------------
ajustar_gwr <- function(P, vars, nome_spec, listw = NULL) {
  cfg <- P$cfg; n <- P$n
  f <- stats::as.formula(paste(cfg$desfecho, "~", paste(vars, collapse = " + ")))
  log_msg("   GWR [", nome_spec, "]: ", paste(vars, collapse = " + "))

  ols5 <- stats::lm(f, data = P$dados)
  k <- length(stats::coef(ols5)) + 1
  aicc_ols <- stats::AIC(ols5) + (2 * k * (k + 1)) / (n - k - 1)

  bw <- GWmodel::bw.gwr(f, data = P$sp_pts, approach = "AICc",
                        kernel = "bisquare", adaptive = TRUE, dMat = P$dMat)
  log_msg("      banda otima: ", bw, " vizinhos (", round(100 * bw / n, 1), "% da amostra)")

  g <- GWmodel::gwr.basic(f, data = P$sp_pts, bw = bw, kernel = "bisquare",
                          adaptive = TRUE, dMat = P$dMat)
  d <- g$GW.diagnostic
  pega <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  aicc_gwr <- if (!is.null(d$gw.AICc)) d$gw.AICc else d$AICc

  sdf <- as.data.frame(g$SDF)
  if ("y" %in% names(sdf)) {
    ok <- max(abs(sdf$y - P$dados[[cfg$desfecho]]), na.rm = TRUE) < 1e-8
    log_msg("      ordem das linhas do SDF ", ifelse(ok, "conferida [OK]", "NAO confere - checar"))
  }

  resumo <- tibble::tibble(
    Especificacao = nome_spec, Variaveis = paste(vars, collapse = " + "),
    n = n, Banda_vizinhos = bw, Pct_amostra = 100 * bw / n,
    Kernel = "bisquare adaptativo",
    AICc_OLS_mesmas_vars = aicc_ols, AICc_GWR = aicc_gwr,
    Delta_AICc = aicc_ols - aicc_gwr,
    ENP = pega(d$enp), EDF = pega(d$edf),
    R2_GWR = pega(d$gw.R2), R2_ajustado_GWR = pega(d$gwR2.adj),
    R2_OLS_mesmas_vars = summary(ols5)$r.squared,
    Ganho_AICc = ifelse((aicc_ols - aicc_gwr) > 3, "sim", "nao"))

  tadj <- tryCatch(as.data.frame(GWmodel::gwr.t.adjust(g)$SDF),
                   error = function(e) { log_msg("      !! gwr.t.adjust falhou: ", conditionMessage(e)); NULL })
  if (!is.null(tadj)) {
    cols_p <- grep("_p($|_)", names(tadj), value = TRUE)
    pct <- vapply(cols_p, function(cc) 100 * mean(tadj[[cc]] < 0.05, na.rm = TRUE), numeric(1))
    log_msg("      % de municipios com p < 0,05 (bruto e ajustado):")
    for (i in seq_along(pct)) log_msg("         ", cols_p[i], ": ", round(pct[i], 1), "%")
  }

  moran <- NULL
  if (!is.null(listw)) {
    moran <- tryCatch({
      mg <- spdep::moran.test(g$SDF$residual, listw, zero.policy = TRUE, alternative = "two.sided")
      mo <- spdep::moran.test(stats::residuals(ols5), listw, zero.policy = TRUE, alternative = "two.sided")
      tibble::tibble(
        Modelo = c(sprintf("OLS (%d variaveis do GWR)", length(vars)),
                   sprintf("GWR [%s]", nome_spec)),
        Moran_I = c(unname(mo$estimate[1]), unname(mg$estimate[1])),
        P_bilateral = c(mo$p.value, mg$p.value), Especificacao = nome_spec)
    }, error = function(e) NULL)
    if (!is.null(moran))
      log_msg("      I de Moran: OLS = ", round(moran$Moran_I[1], 4),
              " | GWR = ", round(moran$Moran_I[2], 4))
  }

  # amplitude dos coeficientes locais - e o que diz se a superficie e legivel
  amp <- purrr::map_dfr(vars, function(v) {
    if (!v %in% names(sdf)) return(NULL)
    x <- sdf[[v]]
    tibble::tibble(Variavel = v, Minimo = min(x, na.rm = TRUE),
                   Q1 = unname(stats::quantile(x, .25, na.rm = TRUE)),
                   Mediana = stats::median(x, na.rm = TRUE),
                   Q3 = unname(stats::quantile(x, .75, na.rm = TRUE)),
                   Maximo = max(x, na.rm = TRUE),
                   Pct_negativos = 100 * mean(x < 0, na.rm = TRUE),
                   Especificacao = nome_spec)
  })

  list(resumo = resumo, sdf = sdf, tadj = tadj, moran = moran, amplitude = amp, bw = bw)
}

# =============================================================================
# BLOCO A - stepwise da mortalidade com banda maior
# =============================================================================
if (FAZER_BLOCO_A) {
  log_msg("======== BLOCO A: stepwise do GWR - Mortalidade ========")
  resA <- tryCatch({
    P <- preparar("Mortalidade")
    if (is.null(P$ranking)) stop("Ranking_LASSO_Mortalidade.csv nao encontrado")
    cand <- utils::head(setdiff(P$ranking$Variavel, P$cfg$vars_fora_gwr), N_CAND_STEPWISE)
    log_msg("   candidatas (", length(cand), "): ", paste(cand, collapse = ", "))

    sel <- NULL; bw_usada <- NA
    for (bw in BANDAS_STEPWISE) {
      log_msg("   tentando gwr.model.selection com banda fixa = ", bw,
              " (", length(cand) * (length(cand) + 1) / 2, " ajustes)")
      sel <- tryCatch(
        GWmodel::gwr.model.selection(
          DeVar = P$cfg$desfecho, InDeVars = cand, data = P$sp_pts,
          bw = bw, approach = "AICc", adaptive = TRUE,
          kernel = "bisquare", dMat = P$dMat),
        error = function(e) { log_msg("      falhou com banda ", bw, ": ", conditionMessage(e)); NULL })
      if (!is.null(sel)) { bw_usada <- bw; break }
    }
    if (is.null(sel)) stop("a selecao falhou em todas as bandas tentadas")
    log_msg("   selecao concluida com banda fixa = ", bw_usada)

    modelos <- sel[[1]]
    crit <- as.matrix(sel[[2]])
    valores <- as.numeric(crit[, ncol(crit)])
    if (length(valores) != length(modelos))
      stop("estrutura inesperada: ", length(modelos), " modelos e ", length(valores), " criterios")

    tabela <- purrr::map_dfr(seq_along(modelos), function(i) {
      v <- modelos[[i]][[2]]
      tibble::tibble(Posicao = i, n_variaveis = length(v),
                     Variaveis = paste(v, collapse = " + "), AICc = valores[i])
    }) %>% dplyr::arrange(AICc) %>%
      dplyr::mutate(Banda_fixa_da_selecao = bw_usada,
                    Nota = "banda fixa 259 (especificacao 'alternativa'); com 79 a selecao e singular")
    readr::write_csv(tabela, file.path(P$dir_d, "GWR_Stepwise_AICc_Tabela_Mortalidade.csv"))
    log_msg("   gravado: GWR_Stepwise_AICc_Tabela_Mortalidade.csv")

    melhor <- modelos[[which.min(valores)]][[2]]
    log_msg("   melhor modelo por AICc (", length(melhor), " variaveis): ",
            paste(melhor, collapse = ", "))

    # ajusta o GWR do modelo escolhido, com banda reotimizada (como na rodada)
    g <- ajustar_gwr(P, melhor, "stepwise")
    readr::write_csv(g$resumo, file.path(P$dir_d, "Parametros_GWR_Stepwise_Mortalidade.csv"))
    readr::write_csv(g$amplitude, file.path(P$dir_d, "GWR_Amplitude_Coef_stepwise_Mortalidade.csv"))
    if (!is.null(g$tadj))
      readr::write_csv(g$tadj, file.path(P$dir_d, "GWR_P_Ajustados_stepwise_Mortalidade.csv"))
    readr::write_csv(g$sdf, file.path(P$dir_d, "GWR_SDF_stepwise_Mortalidade.csv"))
    log_msg("   gravadas as saidas do GWR stepwise")

    rm(P); gc(verbose = FALSE)
    TRUE
  }, error = function(e) { log_msg(">> FALHA no BLOCO A: ", conditionMessage(e)); FALSE })
}

# =============================================================================
# BLOCO B - GWR da aguda sem pct_area_acai
# =============================================================================
if (FAZER_BLOCO_B) {
  log_msg("======== BLOCO B: GWR da Aguda sem pct_area_acai ========")
  resB <- tryCatch({
    P <- preparar("Aguda")
    if (is.null(P$ranking)) stop("Ranking_LASSO_Aguda.csv nao encontrado")

    fora <- unique(c(P$cfg$vars_fora_gwr, "pct_area_acai"))
    vars <- utils::head(setdiff(P$ranking$Variavel, fora), 5)
    log_msg("   excluidas do GWR: ", paste(fora, collapse = ", "))

    lw <- NULL
    if (CALCULAR_MORAN) {
      log_msg("   reconstruindo a matriz Queen para o I de Moran...")
      nb <- spdep::poly2nb(P$mapa, queen = TRUE)
      lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
    }

    g <- ajustar_gwr(P, vars, "principal_sem_acai", listw = lw)

    readr::write_csv(g$resumo,    file.path(P$dir_d, "Parametros_GWR_sem_acai_Aguda.csv"))
    readr::write_csv(g$amplitude, file.path(P$dir_d, "GWR_Amplitude_Coef_sem_acai_Aguda.csv"))
    readr::write_csv(g$sdf,       file.path(P$dir_d, "GWR_SDF_sem_acai_Aguda.csv"))
    if (!is.null(g$tadj))
      readr::write_csv(g$tadj,  file.path(P$dir_d, "GWR_P_Ajustados_sem_acai_Aguda.csv"))
    if (!is.null(g$moran))
      readr::write_csv(g$moran, file.path(P$dir_d, "Tabela_Moran_GWR_sem_acai_Aguda.csv"))
    log_msg("   gravadas as saidas do GWR sem acai")

    # comparacao direta com a especificacao principal original
    arq_old <- file.path(P$dir_d, "Parametros_GWR_Consolidado_Aguda.csv")
    if (file.exists(arq_old)) {
      old <- readr::read_csv(arq_old, show_col_types = FALSE)
      cmp <- dplyr::bind_rows(old, g$resumo)
      readr::write_csv(cmp, file.path(P$dir_d, "Parametros_GWR_Consolidado_Aguda_com_sem_acai.csv"))
      log_msg("   gravado: Parametros_GWR_Consolidado_Aguda_com_sem_acai.csv")
    }

    rm(P); gc(verbose = FALSE)
    TRUE
  }, error = function(e) { log_msg(">> FALHA no BLOCO B: ", conditionMessage(e)); FALSE })
}

log_msg("FIM do Conserto 2")
