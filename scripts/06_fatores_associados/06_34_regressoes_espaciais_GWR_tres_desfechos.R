# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final/Completo - 5.7 - Regressoes Espaciais.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final"))

# =============================================================================
# CHAGAS - PARTE 5.7 - REGRESSAO ESPACIAL E GWR - V4 - TRES DESFECHOS (GLOBAL)
# -----------------------------------------------------------------------------
# Mortalidade (SIM) + DC Aguda (SINAN) + DC Cronica (e-SUS) em uma unica rodada.
# Todas as saidas vao para a pasta "Regressao Espacial - Final", em subpastas
# por desfecho. Log mestre, sessionInfo e resumo comparativo ficam na raiz.
#
# ---------------------------------------------------------------------------
# NOVIDADES DESTA VERSAO (V4) EM RELACAO AOS SCRIPTS V2 (um por desfecho)
# ---------------------------------------------------------------------------
# CORRECOES QUE MUDAM NUMEROS
#   [1.1] KNN-5 com centroides em CRS projetado (EPSG:5880) e distancia de
#         grande circulo (longlat = TRUE). Antes a vizinhanca era euclidiana
#         em GRAUS -> vizinhos distorcidos no Norte e no Sul.
#   [1.2] LASSO com model.matrix (agrupa niveis de fator corretamente) e
#         nfolds explicito.
#   [1.3] file.path() no lugar de setwd() dentro das funcoes.
#   [1.4] Amostra por complete.cases da propria formula (nao mais pela coluna
#         de residuos herdada de outra etapa) + anti_join do join com o shape
#         + teste de 6/7 digitos olhando TODAS as linhas, nao a primeira.
#
# EXTRACOES QUE O V2 CALCULAVA E DESCARTAVA
#   [2.1] Erro-padrao de rho / lambda.           [2.2] bptest.Sarlm completo.
#   [2.3] VIF completo do SDM + cor(X, WX).      [2.4] ENP e EDF do GWR.
#   [2.5] lambda.min, lambda.1se e nao-zeros.    [2.6] IC95% dos impactos.
#   [2.7] gl corrigido (conta o sigma^2).        [2.9] I de Moran bilateral.
#   [2.10] Teste de Anselin-Kelejian sobre o stsls.
#
# ACRESCIMOS DE ROBUSTEZ
#   [3.1] Modelos PONDERADOS pela populacao do municipio (RDS do IBGE).
#   [3.2] Impactos ROBUSTOS: SAR via stsls e SDM via stsls com WX explicito e
#         decomposicao manual de LeSage & Pace (com validacao embutida).
#   [3.3] Sensibilidade a outliers nos modelos espaciais (sem 3 / sem 10).
#   [3.4] Sensibilidade a municipios sem vizinhos (ilhas oceanicas).
#   [3.5] gwr.collin.diagno + gwr.t.adjust + teste F de Leung (F123.test).
#   [S1]  GWR sem a dummy regional (coeficiente local nao identificado em
#         janelas sem variacao do regressor). O SDM MANTEM a dummy defasada:
#         em WX ela e uma medida de contiguidade regional bem definida.
#   [S4]  GWR tambem com as 5 variaveis de maior |t| no OLS (sensibilidade ao
#         criterio de selecao).
#   [S5]  GWR tambem com selecao STEPWISE por AICc (gwr.model.selection).
#   [S8]  lm.RStests dentro do proprio script (justificativa SAR vs SEM).
#   [S9]  Tabela de Moran com o OLS das MESMAS 5 variaveis do GWR.
#
# NAO INCLUIDOS (decisao de desenho): gwr.robust, gwr.mixed, MGWR.
# NAO INCLUIDO (testado e descartado): denominador efetivo da taxa padronizada
#   como peso alternativo - correlacao de 0,9995 (em log) com a populacao
#   media, ou seja, seria o mesmo peso com outro nome e uma pergunta a mais
#   na banca ("qual populacao-padrao?"). Fica so a ponderacao por populacao.
#
# EXECUCAO DESATENDIDA: cada bloco pesado esta dentro de tryCatch; uma falha
# e registrada no log e o script continua. Cada desfecho grava CSV + .rds de
# checkpoint assim que termina, entao uma falha no terceiro nao perde os dois
# primeiros.
#
# ---------------------------------------------------------------------------
# V4.1 - CORRECOES APOS A RODADA DE 01/10 (noite)
# ---------------------------------------------------------------------------
#  (a) PONDERACAO FALHAVA com "objeto 'w' nao encontrado". lm() e errorsarlm()
#      avaliam 'weights' dentro de data e depois em environment(formula), nunca
#      no frame de quem chama; a formula nascia em processar_desfecho(), onde
#      'w' nao existe. Agora o ambiente da formula e reapontado.
#  (b) TESTE F DE LEUNG DESLIGADO. Ficou mais de 3h42 sem terminar em n = 5.566
#      e segurou o resto do script. EXECUTAR_F123 agora e FALSE por padrao.
#  (c) COLINEARIDADE LOCAL virou o ULTIMO passo de cada desfecho, com gravacao
#      propria. Na rodada anterior 1h40 de calculo ficou so na memoria e se
#      perdeu porque o teste F travou logo depois.
#  (d) METODO_ML passou de "eigen" (25 a 34 min por matriz) para "Matrix".
#  (e) VALIDACAO DOS IMPACTOS DO DURBIN reprovava a toa: a tolerancia de 1e-6
#      era menor que o proprio erro de truncamento da serie (6e-5 observado).
#      Passou para 1e-3, que ainda pega erro de formula de verdade.
#  (f) anselin.kelejian nao existe no spdep 1.4.2; so e chamada se existir.
#  (g) PULAR_CONCLUIDOS permite retomar sem refazer desfecho ja finalizado.
# =============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, sf, sp, spdep, spatialreg, lmtest, GWmodel,
               flextable, officer, broom, car, glmnet, MASS, purrr)

select <- dplyr::select   # evita o MASS::select

# =============================================================================
# 0. PARAMETROS GERAIS  >>> CONFERIR OS CAMINHOS ANTES DE RODAR <<<
# =============================================================================

dir_dados     <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo")
dir_shapefile <- file.path(DIR_DADOS, "Análises/Parte 4 - Análise Espacial/Shapes")
dir_populacao <- file.path(DIR_DADOS, "IBGE/IBGE_POP_2001_2024")

# RAIZ DE SAIDA: tudo vai para aqui, em subpastas por desfecho.
dir_saida <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final")

SEMENTE        <- 2026
R_IMPACTOS     <- 2000   # simulacoes para o IC dos impactos (era 500, sem IC)
CRS_PROJETADO  <- 5880   # SIRGAS 2000 / Brazil Polyconic (so para os centroides)
CRS_GEOGRAFICO <- 4674   # SIRGAS 2000 geografico (CRS do shape do IBGE)
# ATENCAO - NAO TROCAR POR "Matrix"/"LU" SEM LER ISTO.
# Os metodos esparsos sao MUITO mais rapidos (0,1 min contra 25 min por matriz
# em n = 5.566) e dao exatamente as mesmas estimativas de rho, beta e logLik.
# Mas o manual do spatialreg e explicito: "Asymptotic standard errors are only
# computed when method='eigen'". Com qualquer outro metodo o controle 'fdHess'
# entra sozinho e TODOS os erros-padrao viram aproximacao por diferencas
# finitas - inclusive a matriz de covariancia de onde o impacts() sorteia as
# 2.000 simulacoes, ou seja, os p e os IC95% dos efeitos diretos, indiretos e
# totais. Na rodada de 02/10 o EP de rho saiu 0,0080 com "Matrix" contra
# 0,0099 com "eigen" (24% de diferenca) no MESMO ajuste.
# Como o "Matrix" ainda por cima nao aceita matriz assimetrica (KNN-5), a
# tabela sairia com Queen por Hessiana numerica e KNN-5 analitico - dois
# calculos diferentes lado a lado. Por isso: eigen, e paciencia com o relogio.
METODO_ML      <- "eigen"
# So entram se o "eigen" falhar; o log avisa em caixa alta que os EP daquele
# ajuste sao aproximados.
METODOS_ALT    <- c("LU", "Matrix")

# --- chaves liga/desliga dos blocos pesados ---------------------------------
EXECUTAR_PONDER    <- TRUE   # [3.1] modelos ponderados pela populacao
EXECUTAR_OUTLIER   <- TRUE   # [3.3][3.4] sensibilidade sem 3 / sem 10 / sem ilhas
# [3.5] gwr.collin.diagno: ~1h40 POR DESFECHO. Roda por ULTIMO, depois de tudo
# ja estar gravado. Aceita TRUE (todos), FALSE (nenhum) ou um vetor com as
# chaves de CONFIGS. Por padrao so a mortalidade, que e o unico desfecho em que
# o GWR e interpretado substantivamente - nos outros dois ele ja e reportado
# como nao informativo e o diagnostico nao muda a conclusao.
EXECUTAR_COLLIN    <- c("mortalidade")
EXECUTAR_TADJUST   <- TRUE   # [3.5] gwr.t.adjust (barato)
EXECUTAR_F123      <- FALSE  # [3.5] teste F de Leung: DESLIGADO. Em n = 5.566 exige a
                             # matriz chapeu n x n (248 MB) e tracos de produtos dela
                             # (~1,7e11 operacoes). Na rodada de 01/10 passou de 3h42
                             # sem terminar e travou o resto do script. Nao religar
                             # sem antes testar numa subamostra.
EXECUTAR_STEPWISE  <- TRUE   # [S5] gwr.model.selection
N_CAND_STEPWISE    <- 10     # candidatas do stepwise: 10 -> 55 ajustes de GWR por desfecho
DIAG_SO_PRINCIPAL  <- TRUE   # collin/t.adjust so na especificacao principal
PULAR_CONCLUIDOS   <- FALSE  # TRUE pula o desfecho que ja tiver Checkpoint_*.rds gravado

options(stringsAsFactors = FALSE)
if (!dir.exists(dir_saida)) dir.create(dir_saida, recursive = TRUE)

arquivo_log <- file.path(dir_saida, sprintf("LOG_Parte57_V4_%s.txt",
                                            format(Sys.time(), "%Y%m%d_%H%M%S")))

log_msg <- function(...) {
  msg <- sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...))
  cat(msg, "\n"); cat(msg, "\n", file = arquivo_log, append = TRUE); flush.console()
}
log_bloco <- function(titulo) {
  log_msg(strrep("=", 72)); log_msg(titulo); log_msg(strrep("=", 72))
}
seguro <- function(expr, rotulo, valor_falha = NULL) {
  tryCatch(expr, error = function(e) {
    log_msg(">> FALHA em [", rotulo, "]: ", conditionMessage(e)); valor_falha
  })
}
escrever <- function(objeto, cfg, nome) {
  if (is.null(objeto)) return(invisible(NULL))
  if (is.data.frame(objeto) && nrow(objeto) == 0) return(invisible(NULL))
  seguro({
    readr::write_csv(objeto, file.path(cfg$dir_exportacao, nome))
    log_msg("   gravado: ", nome)
  }, paste("escrita de", nome))
}
# Remove colunas constantes-inuteis das saidas do GWR (ex.: CV_Score sempre 0)
limpar_colunas_vazias <- function(d) {
  if (is.null(d)) return(NULL)
  d <- as.data.frame(d)
  manter <- vapply(d, function(x) {
    if (!is.numeric(x)) return(TRUE)
    !(all(is.na(x)) || (all(!is.na(x)) && all(x == 0)))
  }, logical(1))
  d[, manter, drop = FALSE]
}

# Algumas versoes do spatialreg nao aceitam 'weights' no lagsarlm.
# O script detecta isso em tempo de execucao e cai para o SEM ponderado.
LAG_ACEITA_W <- "weights" %in% names(formals(spatialreg::lagsarlm))
SEM_ACEITA_W <- "weights" %in% names(formals(spatialreg::errorsarlm))

# Rede de seguranca para os ajustes ML: tenta METODO_ML e, se a versao ou a
# plataforma nao der conta, tenta os METODOS_ALT em ordem em vez de derrubar o
# desfecho inteiro. Qualquer recuo e registrado em CAIXA ALTA, porque fora do
# "eigen" os erros-padrao daquele ajuste sao aproximados (ver nota no bloco 0).
# Argumentos explicitos de proposito: repassar via '...' mexeria na forma como
# o spatialreg resolve os nomes dos argumentos.
tentar_metodos <- function(chamar, rotulo) {
  metodos <- unique(c(METODO_ML, METODOS_ALT))
  erro1 <- NULL
  for (i in seq_along(metodos)) {
    r <- tryCatch(chamar(metodos[i]),
                  error = function(e) { if (is.null(erro1)) erro1 <<- conditionMessage(e); NULL })
    if (!is.null(r)) {
      if (i > 1) {
        log_msg("   !! ATENCAO: '", metodos[1], "' nao serve aqui (", erro1, ")")
        log_msg("   !! ", rotulo, " estimado com '", metodos[i],
                "' - os ERROS-PADRAO deste ajuste sao APROXIMADOS (Hessiana numerica),")
        log_msg("   !! e isso afeta os p e os IC dos impactos. Conferir antes de publicar.")
      }
      return(r)
    }
  }
  stop("nenhum metodo de estimacao funcionou para ", rotulo, ": ", erro1)
}
fit_lag <- function(form, dados, listw, Durbin = NULL, rotulo = "modelo lag") {
  tentar_metodos(function(met) {
    if (is.null(Durbin))
      lagsarlm(form, data = dados, listw = listw, method = met, zero.policy = TRUE)
    else
      lagsarlm(form, data = dados, listw = listw, Durbin = Durbin,
               method = met, zero.policy = TRUE)
  }, rotulo)
}
fit_err <- function(form, dados, listw, rotulo = "modelo de erro") {
  tentar_metodos(function(met)
    errorsarlm(form, data = dados, listw = listw, method = met, zero.policy = TRUE), rotulo)
}

# =============================================================================
# 1. CONFIGURACAO DOS TRES DESFECHOS
# =============================================================================
# pop_anos: anos usados para o PESO. O peso e a populacao media anual do
# municipio no periodo do desfecho (mortalidade e aguda cobrem 2001-2024;
# a cronica e um corte de 2023).

CONFIGS <- list(

  mortalidade = list(
    rotulo      = "Mortalidade (SIM 2001-2024)",
    sufixo      = "Mortalidade",
    arquivo_csv = "Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Mortalidade_global.csv",
    desfecho    = "mort_taxa_padronizada_suavizada",
    col_residuo = "residuos_ols_mortalidade",
    pop_anos    = 2001:2024,
    vars_fora_gwr = c("amazonia_legal_flag"),
    covariaveis = c(
      "pct_negros", "amazonia_legal_flag", "pct_analfabetismo_15mais",
      "densidade_familias_assentadas_km2", "pct_quilombola_2022",
      "ivm_global_2022", "pct_indigena_2022", "perc_media_agro_19_21",
      "spei_media_2001_2024", "mapbiomas_fogo_media_historica_perc",
      "mapbiomas_vegsec_media_historica_perc", "mapbiomas_desmatamento_media_anual_perc",
      "tx_horas_medicos_aps_sus_10k_2023", "ripsa_tx_enfermeiros_media",
      "ripsa_tx_raiox_media", "razao_consultas_aps_media", "razao_consultas_esp_media",
      "adensamento_medio", "pct_rural", "cidacs_indice_precariedade_urbana_2022",
      "sisagua_cobertura_media", "pct_nasc_fora_municipio", "pct_nasc_outra_regiao",
      "pct_subsistencia_14mais", "pct_area_cana", "pct_parede_taipa")
  ),

  aguda = list(
    rotulo      = "DC Aguda (SINAN 2001-2024)",
    sufixo      = "Aguda",
    arquivo_csv = "Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Aguda_global.csv",
    desfecho    = "aguda_taxa_padronizada_suavizada",
    col_residuo = "residuos_ols_aguda",
    pop_anos    = 2001:2024,
    vars_fora_gwr = c("amazonia_legal_flag"),
    covariaveis = c(
      "amazonia_legal_flag", "densidade_familias_assentadas_km2", "pct_indigena_2022",
      "perc_media_agro_19_21", "mapbiomas_vegsec_media_historica_perc",
      "mapbiomas_desmatamento_media_anual_perc", "cobertura_aps",
      "ripsa_tx_enfermeiros_media", "sisab_isf_global", "razao_consultas_aps_media",
      "adensamento_medio", "pct_rural", "sisagua_cobertura_media",
      "pct_nasc_fora_municipio", "pct_nasc_outra_regiao", "pct_subsistencia_14mais",
      "pct_area_acai", "pct_area_cana", "pct_parede_taipa")
  ),

  cronica = list(
    rotulo      = "DC Cronica (e-SUS 2023)",
    sufixo      = "Cronica",
    arquivo_csv = "Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Cronica_global.csv",
    desfecho    = "cronica_taxa_padronizada_suavizada",
    col_residuo = "residuos_ols_cronica",
    pop_anos    = 2023,
    vars_fora_gwr = c("amazonia_legal_flag"),
    covariaveis = c(
      "amazonia_legal_flag", "pct_quilombola_2022", "densidade_familias_assentadas_km2",
      "spei_media_2001_2024", "mapbiomas_fogo_media_historica_perc",
      "mapbiomas_vegsec_media_historica_perc", "mapbiomas_desmatamento_media_anual_perc",
      "sisab_isf_global", "pct_esgoto_inadequado",
      "pct_nasc_fora_municipio", "pct_parede_taipa")
  )
)

for (nm in names(CONFIGS)) {
  CONFIGS[[nm]]$chave <- nm
  CONFIGS[[nm]]$dir_exportacao <- file.path(dir_saida, CONFIGS[[nm]]$sufixo)
}

# EXECUTAR_COLLIN aceita TRUE, FALSE ou um vetor de chaves de CONFIGS
roda_collin <- function(cfg) {
  if (isTRUE(EXECUTAR_COLLIN)) return(TRUE)
  if (isFALSE(EXECUTAR_COLLIN)) return(FALSE)
  isTRUE(cfg$chave %in% EXECUTAR_COLLIN)
}

# =============================================================================
# 2. FUNCOES
# =============================================================================

# ---- 2.1 [3.1] Populacao do IBGE (RDS por faixa quinquenal) ----------------
# Estrutura esperada: COD_MUN (7 digitos), ANO_OBITO, FAIXA_QUINQUENAL,
# populacao_residente. 5.570 municipios x 24 anos x 17 faixas.
carregar_populacao <- function() {
  arqs <- list.files(dir_populacao, pattern = "\\.rds$", ignore.case = TRUE, full.names = TRUE)
  alvo <- arqs[grepl("POP_IBGE_MUNICIPIO_TRATADA", basename(arqs), ignore.case = TRUE)]
  if (length(alvo) == 0) alvo <- arqs
  if (length(alvo) == 0) stop("Nenhum .rds encontrado em ", dir_populacao)
  pop <- readRDS(alvo[1])
  log_msg("populacao: ", basename(alvo[1]), " | ", nrow(pop), " linhas")

  nomes <- names(pop)
  cod <- nomes[grepl("^(COD_MUN|codigo_municipio|cod_mun)$", nomes, ignore.case = TRUE)][1]
  ano <- nomes[grepl("^(ANO_OBITO|ANO|ano)$", nomes, ignore.case = TRUE)][1]
  val <- nomes[grepl("^(populacao_residente|populacao|pop)$", nomes, ignore.case = TRUE)][1]
  if (any(is.na(c(cod, ano, val))))
    stop("Nao identifiquei as colunas do RDS de populacao. Colunas: ", paste(nomes, collapse = ", "))

  pop <- tibble(COD_MUN = as.character(pop[[cod]]),
                ANO     = as.integer(pop[[ano]]),
                POP     = as.numeric(pop[[val]]))
  log_msg("populacao: ", dplyr::n_distinct(pop$COD_MUN), " municipios | anos ",
          min(pop$ANO), "-", max(pop$ANO))
  pop
}

# Peso = populacao media anual no periodo do desfecho.
peso_populacional <- function(pop, anos, largura_chave) {
  pop %>%
    filter(ANO %in% anos) %>%
    group_by(COD_MUN, ANO) %>% summarise(p = sum(POP, na.rm = TRUE), .groups = "drop") %>%
    group_by(COD_MUN) %>% summarise(pop_media = mean(p), .groups = "drop") %>%
    mutate(chave_ibge = substr(COD_MUN, 1, largura_chave)) %>%
    group_by(chave_ibge) %>% summarise(pop_media = sum(pop_media), .groups = "drop")
}

# ---- 2.2 Leitura, join conferido e definicao da amostra --------------------
carregar_dados <- function(cfg, mapa_br, pop) {

  df <- readr::read_csv(file.path(dir_dados, cfg$arquivo_csv), show_col_types = FALSE) %>%
    mutate(ibge_csv_str = as.character(codigo_municipio))

  # [1.4] o teste de 6 vs 7 digitos olha TODAS as linhas, nao so a primeira
  larguras <- sort(unique(nchar(df$ibge_csv_str)))
  log_msg("   larguras do codigo IBGE no CSV: ", paste(larguras, collapse = "/"))
  if (length(larguras) != 1 || !larguras %in% c(6, 7))
    stop("Codigos IBGE com largura inesperada: ", paste(larguras, collapse = "/"))
  largura <- larguras
  mapa_key <- mapa_br %>% mutate(chave_ibge = substr(CD_MUN, 1, largura))

  # [1.4] anti_join nos dois sentidos
  sem_poligono <- df %>% anti_join(st_drop_geometry(mapa_key),
                                   by = c("ibge_csv_str" = "chave_ibge"))
  sem_dado <- st_drop_geometry(mapa_key) %>%
    anti_join(df, by = c("chave_ibge" = "ibge_csv_str"))
  log_msg("   CSV sem poligono: ", nrow(sem_poligono),
          " | poligonos sem dado: ", nrow(sem_dado))
  if (nrow(sem_poligono) > 0)
    escrever(sem_poligono %>% select(any_of(c("codigo_municipio", "ibge_csv_str"))),
             cfg, paste0("Diagnostico_Join_CSV_sem_poligono_", cfg$sufixo, ".csv"))
  if (nrow(sem_dado) > 0)
    escrever(sem_dado %>% select(any_of(c("CD_MUN", "NM_MUN", "SIGLA_UF", "chave_ibge"))),
             cfg, paste0("Diagnostico_Join_Poligono_sem_CSV_", cfg$sufixo, ".csv"))

  mapa_df <- mapa_key %>% inner_join(df, by = c("chave_ibge" = "ibge_csv_str"))
  log_msg("   apos o join: ", nrow(mapa_df), " municipios")

  # [3.1] peso populacional
  pesos_pop <- peso_populacional(pop, cfg$pop_anos, largura)
  antes <- nrow(mapa_df)
  mapa_df <- mapa_df %>% left_join(pesos_pop, by = "chave_ibge")
  n_sem_pop <- sum(is.na(mapa_df$pop_media))
  log_msg("   peso populacional (media anual ", min(cfg$pop_anos), "-", max(cfg$pop_anos),
          "): ", antes - n_sem_pop, " com populacao | ", n_sem_pop, " sem")
  if (n_sem_pop > 0)
    escrever(st_drop_geometry(mapa_df) %>% filter(is.na(pop_media)) %>%
               select(any_of(c("CD_MUN", "NM_MUN", "SIGLA_UF", "chave_ibge"))),
             cfg, paste0("Diagnostico_Municipios_Sem_Populacao_", cfg$sufixo, ".csv"))

  # [1.4] amostra por complete.cases das variaveis da PROPRIA formula
  vars_modelo <- c(cfg$desfecho, cfg$covariaveis)
  faltantes <- setdiff(vars_modelo, names(mapa_df))
  if (length(faltantes) > 0) stop("Colunas ausentes: ", paste(faltantes, collapse = ", "))

  dados_mod <- st_drop_geometry(mapa_df)[, vars_modelo, drop = FALSE]
  ok <- stats::complete.cases(dados_mod)

  if (any(!ok)) {
    perdidos <- dados_mod[!ok, , drop = FALSE]
    motivo <- tibble(variavel = vars_modelo,
                     n_na = vapply(vars_modelo, function(v) sum(is.na(perdidos[[v]])), integer(1))) %>%
      filter(n_na > 0) %>% arrange(desc(n_na))
    log_msg("   excluidos por NA: ", sum(!ok), " municipios")
    for (i in seq_len(nrow(motivo))) log_msg("      ", motivo$variavel[i], ": ", motivo$n_na[i], " NA")
    escrever(motivo, cfg, paste0("Diagnostico_Amostra_NA_", cfg$sufixo, ".csv"))
  }

  if (!is.null(cfg$col_residuo) && cfg$col_residuo %in% names(mapa_df)) {
    n_antigo <- sum(!is.na(mapa_df[[cfg$col_residuo]]))
    log_msg("   n pelo criterio antigo (!is.na residuo): ", n_antigo,
            " | n pelo criterio novo (complete.cases): ", sum(ok),
            ifelse(n_antigo == sum(ok), "  [identicos]", "  [DIFEREM - conferir]"))
  }

  mapa_df <- mapa_df[ok, ]
  log_msg("   amostra final: ", nrow(mapa_df), " municipios")
  mapa_df
}

# ---- 2.3 Matrizes de pesos (CORRECAO DO KNN-5) -----------------------------
construir_pesos <- function(mapa_df) {
  sf_use_s2(FALSE)
  vizinhos_queen <- poly2nb(mapa_df, queen = TRUE)
  W_queen <- nb2listw(vizinhos_queen, style = "W", zero.policy = TRUE)
  ids_ilhas <- which(card(vizinhos_queen) == 0)
  log_msg("   Queen: ", length(ids_ilhas), " municipio(s) sem vizinho")

  # [1.1] centroide em CRS PROJETADO, devolvido em lon/lat, e knearneigh com
  # longlat = TRUE (distancia de grande circulo, nao graus decimais).
  cent_proj  <- suppressWarnings(st_centroid(st_geometry(st_transform(mapa_df, CRS_PROJETADO))))
  coords_geo <- st_coordinates(st_transform(cent_proj, CRS_GEOGRAFICO))
  W_knn5 <- nb2listw(knn2nb(knearneigh(coords_geo, k = 5, longlat = TRUE)),
                     style = "W", zero.policy = TRUE)

  list(queen = W_queen, knn5 = W_knn5, coords = coords_geo,
       n_ilhas = length(ids_ilhas), ids_ilhas = ids_ilhas)
}

# ---- 2.4 LASSO (ordenamento) + ranking alternativo por |t| -----------------
rodar_lasso <- function(mapa_df, cfg) {

  dados <- st_drop_geometry(mapa_df)[, cfg$covariaveis, drop = FALSE]
  mm <- stats::model.matrix(~ ., data = dados)[, -1, drop = FALSE]   # [1.2]
  y  <- mapa_df[[cfg$desfecho]]

  set.seed(SEMENTE)
  cv_lasso <- cv.glmnet(scale(mm), y, alpha = 1, nfolds = 10)
  co <- coef(cv_lasso, s = "lambda.min")
  bruto <- data.frame(Termo = rownames(co), Coeficiente = as.numeric(co)) %>%
    filter(Termo != "(Intercept)")

  mapear <- function(termo) {
    hits <- cfg$covariaveis[vapply(cfg$covariaveis, function(v) startsWith(termo, v), logical(1))]
    if (length(hits) == 0) return(termo)
    hits[which.max(nchar(hits))]
  }
  bruto$Variavel <- vapply(bruto$Termo, mapear, character(1))

  ranking <- bruto %>%
    group_by(Variavel) %>%
    summarise(Coeficiente = Coeficiente[which.max(abs(Coeficiente))], .groups = "drop") %>%
    filter(Coeficiente != 0) %>% arrange(desc(abs(Coeficiente))) %>%
    mutate(Posicao = row_number())

  n_nz <- function(s) sum(as.numeric(coef(cv_lasso, s = s))[-1] != 0)   # [2.5]
  params <- tibble(lambda_min = cv_lasso$lambda.min, lambda_1se = cv_lasso$lambda.1se,
                   n_nao_zero_min = n_nz("lambda.min"), n_nao_zero_1se = n_nz("lambda.1se"),
                   n_candidatas = length(cfg$covariaveis), n_folds = 10,
                   erro_cv_min = min(cv_lasso$cvm))
  log_msg("   LASSO: lambda.min = ", signif(params$lambda_min, 4),
          " | nao-zeros = ", params$n_nao_zero_min, "/", params$n_candidatas,
          " | com lambda.1se = ", params$n_nao_zero_1se)

  form_ols <- as.formula(paste(cfg$desfecho, "~", paste(cfg$covariaveis, collapse = " + ")))
  ols <- lm(form_ols, data = st_drop_geometry(mapa_df))
  co_ols <- coef(ols)[-1]; dp_x <- apply(mm, 2, sd)
  comum <- intersect(names(co_ols), names(dp_x))
  padronizado <- tibble(Termo = comum, OLS_padronizado = co_ols[comum] * dp_x[comum]) %>%
    left_join(bruto %>% select(Termo, Coeficiente), by = "Termo") %>%
    mutate(Razao = Coeficiente / OLS_padronizado)
  comparacao <- tibble(correlacao = suppressWarnings(cor(padronizado$Coeficiente,
                                                         padronizado$OLS_padronizado)),
                       razao_mediana = median(padronizado$Razao, na.rm = TRUE),
                       razao_minima  = min(padronizado$Razao, na.rm = TRUE),
                       razao_maxima  = max(padronizado$Razao, na.rm = TRUE))
  log_msg("   LASSO vs OLS padronizado: r = ", round(comparacao$correlacao, 4),
          " | razao mediana = ", round(comparacao$razao_mediana, 4))

  ranking_t <- broom::tidy(ols) %>% filter(term != "(Intercept)") %>%     # [S4]
    mutate(Variavel = vapply(term, mapear, character(1))) %>%
    group_by(Variavel) %>% summarise(t = statistic[which.max(abs(statistic))], .groups = "drop") %>%
    arrange(desc(abs(t))) %>% mutate(Posicao = row_number())

  list(ranking = ranking, bruto = bruto, params = params, comparacao = comparacao,
       padronizado = padronizado, ranking_t = ranking_t, ols = ols)
}

# ---- 2.5 Impactos com IC95% ------------------------------------------------
impactos_com_ic <- function(modelo, trMat, rotulo) {
  set.seed(SEMENTE)
  imp <- impacts(modelo, tr = trMat, R = R_IMPACTOS, zstats = TRUE)
  s <- summary(imp, zstats = TRUE, short = TRUE)
  base <- data.frame(
    Variavel        = stringr::str_replace(attr(imp, "bnames"), " dy/dx", ""),
    Efeito_Direto   = as.numeric(s$res$direct),   P_Direto   = as.numeric(s$pzmat[, 1]),
    Efeito_Indireto = as.numeric(s$res$indirect), P_Indireto = as.numeric(s$pzmat[, 2]),
    Efeito_Total    = as.numeric(s$res$total),    P_Total    = as.numeric(s$pzmat[, 3]))
  ic <- seguro({                                                           # [2.6]
    sr <- imp$sres; q <- function(m, p) apply(as.matrix(m), 2, quantile, probs = p, na.rm = TRUE)
    data.frame(IC_Direto_inf   = q(sr$direct, .025),   IC_Direto_sup   = q(sr$direct, .975),
               IC_Indireto_inf = q(sr$indirect, .025), IC_Indireto_sup = q(sr$indirect, .975),
               IC_Total_inf    = q(sr$total, .025),    IC_Total_sup    = q(sr$total, .975))
  }, paste("IC dos impactos -", rotulo))
  if (!is.null(ic) && nrow(ic) == nrow(base)) base <- cbind(base, ic)
  else log_msg("   aviso: IC dos impactos indisponivel para ", rotulo)
  base
}

# ---- 2.6 [3.2] Impactos do Durbin a partir de coeficientes arbitrarios -----
# LeSage & Pace: S_r(W) = (I - rho W)^-1 (beta_r I + theta_r W)
#   direto = n^-1 tr(S_r) = beta sum(rho^q T_q) + theta sum(rho^q T_{q+1})
#   total  = (beta_r + theta_r) / (1 - rho)        [W padronizada por linha]
impactos_durbin_manual <- function(rho, beta, theta, tracos, n, total_fechado = TRUE) {
  Tq <- as.numeric(tracos)
  if (max(abs(Tq), na.rm = TRUE) > 10) Tq <- Tq / n   # normaliza se vier tr(W^q) bruto
  Tq <- c(1, Tq)                                      # T_0 = tr(I)/n = 1
  m <- length(Tq); g <- rho^(0:(m - 1))
  direto <- beta * sum(g * Tq) + theta * sum(g[-m] * Tq[-1])
  # W padronizada por linha: as linhas de W^q somam 1, entao o efeito total e
  # (beta+theta)/(1-rho) na forma fechada OU (beta+theta)*sum(rho^q) na serie
  # truncada nos mesmos m tracos do efeito direto. As duas diferem por
  # rho^(m+1)/(1-rho) - desprezivel com rho baixo, mas ~0,07% com rho = 0,79.
  # Qual delas o spatialreg usa e DECIDIDO na validacao contra o impacts(),
  # nao chutado aqui.
  total <- if (total_fechado) (beta + theta) / (1 - rho) else (beta + theta) * sum(g)
  data.frame(direto = direto, indireto = total - direto, total = total)
}

# ---- 2.7 Modelos globais ---------------------------------------------------
estimar_modelos_globais <- function(dados_df, listw, nome_w, cfg,
                                    form_completa, form_durbin, vars_durbin) {

  log_bloco(sprintf("%s | matriz %s", cfg$rotulo, nome_w))
  n <- nrow(dados_df); res <- list(); t_ml <- Sys.time()

  ols <- lm(form_completa, data = dados_df)
  sar <- fit_lag(form_completa, dados_df, listw, rotulo = paste("SAR", nome_w))
  sem <- fit_err(form_completa, dados_df, listw, rotulo = paste("SEM", nome_w))
  sdm <- fit_lag(form_completa, dados_df, listw, Durbin = form_durbin,
                 rotulo = paste("SDM parcial", nome_w))
  log_msg("   modelos ML estimados (metodo ", METODO_ML, ") em ",
          round(as.numeric(difftime(Sys.time(), t_ml, units = "mins")), 1), " min")

  # ---- [S8] testes LM/RS robustos (Anselin): justificativa SAR vs SEM ------
  res$rstests <- seguro({
    rs <- tryCatch(spdep::lm.RStests(ols, listw, test = "all", zero.policy = TRUE),
                   error = function(e) spdep::lm.LMtests(ols, listw, test = "all", zero.policy = TRUE))
    purrr::map_dfr(names(rs), function(k) tibble(
      Teste = k, Estatistica = unname(rs[[k]]$statistic),
      gl = unname(rs[[k]]$parameter), P_Valor = unname(rs[[k]]$p.value)))
  }, paste("lm.RStests", nome_w))
  if (!is.null(res$rstests)) {
    res$rstests$Matriz <- nome_w
    log_msg("   RStests: ", paste(sprintf("%s=%.1f", res$rstests$Teste,
                                          res$rstests$Estatistica), collapse = " | "))
  }

  # ---- [2.7] gl correto + [2.1] erro-padrao de rho/lambda -----------------
  gl <- function(m) as.integer(attr(logLik(m), "df"))
  pega <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  res$tabela_modelos <- data.frame(
    Modelo = c("OLS", "SAR", "SEM", "SDM (Parcial)"),
    gl     = c(gl(ols), gl(sar), gl(sem), gl(sdm)),
    gl_antigo_coef = c(length(coef(ols)), length(coef(sar)), length(coef(sem)), length(coef(sdm))),
    AIC    = c(AIC(ols), AIC(sar), AIC(sem), AIC(sdm)),
    BIC    = c(BIC(ols), BIC(sar), BIC(sem), BIC(sdm)),
    LogLik = c(as.numeric(logLik(ols)), as.numeric(logLik(sar)),
               as.numeric(logLik(sem)), as.numeric(logLik(sdm))),
    Rho_Lambda    = c(NA, sar$rho, sem$lambda, sdm$rho),
    EP_Rho_Lambda = c(NA, pega(sar$rho.se), pega(sem$lambda.se), pega(sdm$rho.se)),
    Matriz = nome_w) %>% mutate(Delta_AIC = AIC - min(AIC)) %>% arrange(AIC)
  log_msg("   rho(SAR) = ", round(sar$rho, 4), " (EP = ",
          ifelse(is.null(sar$rho.se), "NA", round(sar$rho.se, 4)), ")")

  res$lr_sdm_sar <- seguro(LR.Sarlm(sdm, sar), "LR SDM vs SAR")

  # ---- [2.9] Moran uni e bilateral ----------------------------------------
  moran_linha <- function(residuos, rotulo) {
    g <- moran.test(residuos, listw, zero.policy = TRUE, alternative = "greater")
    b <- moran.test(residuos, listw, zero.policy = TRUE, alternative = "two.sided")
    tibble(Modelo = rotulo, Moran_I = unname(g$estimate[1]),
           P_unilateral = g$p.value, P_bilateral = b$p.value)
  }
  res$tabela_moran <- bind_rows(
    moran_linha(residuals(ols), sprintf("OLS (%d variaveis)", length(cfg$covariaveis))),
    moran_linha(residuals(sar), "SAR"), moran_linha(residuals(sem), "SEM"),
    moran_linha(residuals(sdm), "SDM (Parcial)")) %>% mutate(Matriz = nome_w)

  # ---- [2.2] Breusch-Pagan completo ---------------------------------------
  res$bptest <- seguro({
    b1 <- lmtest::bptest(ols); b2 <- spatialreg::bptest.Sarlm(sar); b3 <- spatialreg::bptest.Sarlm(sdm)
    tibble(Modelo = c("OLS", "SAR", "SDM (Parcial)"),
           Estatistica = c(unname(b1$statistic), unname(b2$statistic), unname(b3$statistic)),
           gl = c(unname(b1$parameter), unname(b2$parameter), unname(b3$parameter)),
           P_Valor = c(b1$p.value, b2$p.value, b3$p.value), Matriz = nome_w)
  }, paste("bptest", nome_w))
  if (!is.null(res$bptest))
    log_msg("   Breusch-Pagan p: ", paste(sprintf("%s=%.4g", res$bptest$Modelo,
                                                  res$bptest$P_Valor), collapse = " | "))

  # ---- [2.3] VIF completo do SDM + correlacao X vs WX ---------------------
  res$vif <- seguro({
    Xsdm <- as.data.frame(sdm$X) %>% select(-any_of("(Intercept)"))
    v <- car::vif(lm(dados_df[[cfg$desfecho]] ~ ., data = Xsdm))
    tibble(Termo = names(v), VIF = as.numeric(v), Matriz = nome_w) %>% arrange(desc(VIF))
  }, paste("VIF do SDM", nome_w))
  if (!is.null(res$vif))
    log_msg("   VIF maximo do SDM parcial: ", round(max(res$vif$VIF), 2),
            " | acima de 10: ", sum(res$vif$VIF > 10))

  res$cor_x_wx <- seguro({
    purrr::map_dfr(vars_durbin, function(v) {
      x <- dados_df[[v]]
      tibble(Variavel = v, Cor_X_WX = cor(x, lag.listw(listw, x, zero.policy = TRUE),
                                          use = "complete.obs"), Matriz = nome_w)
    }) %>% arrange(desc(abs(Cor_X_WX)))
  }, paste("cor X-WX", nome_w))

  # ---- Impactos por maxima verossimilhanca --------------------------------
  trMat <- trW(as(listw, "CsparseMatrix"), type = "mult")
  res$trMat <- trMat
  res$impactos_sar <- seguro(impactos_com_ic(sar, trMat, paste("SAR", nome_w)),
                             paste("impactos SAR", nome_w))
  res$impactos_sdm <- seguro(impactos_com_ic(sdm, trMat, paste("SDM", nome_w)),
                             paste("impactos SDM", nome_w))
  if (!is.null(res$impactos_sar)) res$impactos_sar$Matriz <- nome_w
  if (!is.null(res$impactos_sdm)) res$impactos_sdm$Matriz <- nome_w

  # ---- [3.2] SAR robusto (stsls): coeficientes E impactos -----------------
  res$sar_robusto <- seguro({
    sr <- stsls(form_completa, data = dados_df, listw = listw, zero.policy = TRUE, robust = TRUE)
    co <- as.data.frame(summary(sr)$Coef) %>% tibble::rownames_to_column("term")
    colnames(co) <- c("term", "estimate", "std.error", "statistic", "p.value")
    co$Matriz <- nome_w

    imp_rob <- seguro({
      set.seed(SEMENTE)
      ir <- impacts(sr, tr = trMat, R = R_IMPACTOS)
      s <- summary(ir, zstats = TRUE, short = TRUE)
      data.frame(Variavel = stringr::str_replace(attr(ir, "bnames"), " dy/dx", ""),
                 Efeito_Direto = as.numeric(s$res$direct),     P_Direto = as.numeric(s$pzmat[, 1]),
                 Efeito_Indireto = as.numeric(s$res$indirect), P_Indireto = as.numeric(s$pzmat[, 2]),
                 Efeito_Total = as.numeric(s$res$total),       P_Total = as.numeric(s$pzmat[, 3]),
                 Matriz = nome_w)
    }, paste("impactos do SAR robusto", nome_w))

    # [2.10] Anselin-Kelejian: nao existe no spdep 1.4.2 (confirmado na rodada de
    # 01/10). So tenta se o pacote realmente exportar a funcao, para nao poluir o log.
    ak <- NULL
    if ("anselin.kelejian" %in% getNamespaceExports("spdep")) {
      ak <- seguro({
        a <- spdep::anselin.kelejian(sr, listw = listw, zero.policy = TRUE)
        tibble(Estatistica = unname(a$statistic), P_Valor = unname(a$p.value), Matriz = nome_w)
      }, paste("Anselin-Kelejian", nome_w))
    }

    log_msg("   SAR robusto (stsls) ok", ifelse(is.null(imp_rob), " [sem impactos]", " [com impactos]"))
    list(coeficientes = co, impactos = imp_rob, anselin_kelejian = ak)
  }, paste("stsls SAR", nome_w))

  # ---- [3.2] SDM robusto: stsls com WX explicito + impactos manuais -------
  res$sdm_robusto <- seguro({
    WX <- sapply(vars_durbin, function(v) lag.listw(listw, dados_df[[v]], zero.policy = TRUE))
    colnames(WX) <- paste0("lag_", vars_durbin)
    dados_aug <- cbind(dados_df, as.data.frame(WX))
    form_aug <- as.formula(paste(deparse(form_completa, width.cutoff = 500), collapse = " "))
    form_aug <- update(form_aug, as.formula(paste("~ . +", paste(colnames(WX), collapse = " + "))))

    sr <- stsls(form_aug, data = dados_aug, listw = listw, zero.policy = TRUE, robust = TRUE)
    co <- as.data.frame(summary(sr)$Coef) %>% tibble::rownames_to_column("term")
    colnames(co) <- c("term", "estimate", "std.error", "statistic", "p.value")
    co$Matriz <- nome_w

    # --- validacao da rotina manual contra o impacts() do spatialreg -------
    # Aplica a rotina manual aos coeficientes de MAXIMA VEROSSIMILHANCA e exige
    # que ela reproduza o impacts() do pacote. So se reproduzir e que a mesma
    # rotina e aplicada aos coeficientes 2SLS. Testa as duas convencoes de
    # efeito total e fica com a que o pacote usa.
    b_ml <- coef(sdm); rho_ml <- unname(b_ml["rho"]); b_ml <- b_ml[names(b_ml) != "rho"]
    monta <- function(fechado) purrr::map_dfr(vars_durbin, function(v) {
      ln <- paste0("lag.", v)
      th <- if (ln %in% names(b_ml)) unname(b_ml[ln]) else 0
      impactos_durbin_manual(rho_ml, unname(b_ml[v]), th, trMat, n, fechado) %>%
        mutate(Variavel = v)
    })
    valido <- FALSE; total_fechado <- TRUE
    if (!is.null(res$impactos_sdm)) {
      ref <- res$impactos_sdm %>% select(Variavel, Efeito_Direto, Efeito_Total)
      escala <- max(1, max(abs(c(ref$Efeito_Direto, ref$Efeito_Total)), na.rm = TRUE))
      avalia <- function(fechado) {
        cmp <- monta(fechado) %>% inner_join(ref, by = "Variavel")
        max(abs(c(cmp$direto - cmp$Efeito_Direto,
                  cmp$total  - cmp$Efeito_Total)), na.rm = TRUE) / escala
      }
      d_fech <- seguro(avalia(TRUE),  "validacao do Durbin (total fechado)",  Inf)
      d_ser  <- seguro(avalia(FALSE), "validacao do Durbin (total em serie)", Inf)
      total_fechado <- isTRUE(d_fech <= d_ser)
      dif <- min(d_fech, d_ser)
      # Tolerancia de 1e-3 relativo. Os dois lados truncam a serie de potencias
      # nos mesmos 30 tracos do trW, e com rho ~ 0,79 a cauda rho^31 sozinha ja
      # vale ~7e-4, entao 1e-6 era estrito demais: na rodada de 01/10 a diferenca
      # ficou em 6e-5 (ou seja, os dois concordam) e a validacao reprovou a toa.
      # Um erro de formula de verdade apareceria em ordem de grandeza ou em sinal,
      # muito acima de 1e-3.
      valido <- is.finite(dif) && dif < 1e-3
      log_msg("   validacao dos impactos manuais do Durbin: diferenca relativa = ",
              signif(dif, 3), " (fechado ", signif(d_fech, 3), " / serie ", signif(d_ser, 3), ")",
              " | convencao adotada: ", ifelse(total_fechado, "forma fechada", "serie truncada"),
              ifelse(valido, "  [OK]", "  [FALHOU - impactos robustos do SDM NAO exportados]"))
    } else log_msg("   validacao dos impactos manuais: impossivel (impacts do SDM ausente)")

    imp_rob <- NULL
    if (valido) imp_rob <- seguro({
      co_v <- sr$coefficients; i_rho <- which(names(co_v) == "Rho")
      rho_r <- unname(co_v[i_rho])
      set.seed(SEMENTE)
      amostras <- MASS::mvrnorm(R_IMPACTOS, mu = co_v, Sigma = sr$var)
      p_emp <- function(x) 2 * min(mean(x <= 0), mean(x >= 0))
      purrr::map_dfr(vars_durbin, function(v) {
        iv <- which(names(co_v) == v); il <- which(names(co_v) == paste0("lag_", v))
        if (length(iv) == 0 || length(il) == 0) return(NULL)
        pont <- impactos_durbin_manual(rho_r, unname(co_v[iv]), unname(co_v[il]),
                                       trMat, n, total_fechado)
        sim <- t(apply(amostras, 1, function(z)
          as.numeric(impactos_durbin_manual(z[i_rho], z[iv], z[il], trMat, n, total_fechado))))
        tibble(Variavel = v,
               Efeito_Direto = pont$direto,
               IC_Direto_inf = quantile(sim[, 1], .025), IC_Direto_sup = quantile(sim[, 1], .975),
               P_Direto = p_emp(sim[, 1]),
               Efeito_Indireto = pont$indireto,
               IC_Indireto_inf = quantile(sim[, 2], .025), IC_Indireto_sup = quantile(sim[, 2], .975),
               P_Indireto = p_emp(sim[, 2]),
               Efeito_Total = pont$total,
               IC_Total_inf = quantile(sim[, 3], .025), IC_Total_sup = quantile(sim[, 3], .975),
               P_Total = p_emp(sim[, 3]), Matriz = nome_w)
      })
    }, paste("simulacao dos impactos robustos do SDM", nome_w))

    list(coeficientes = co, impactos = imp_rob, validacao_ok = valido)
  }, paste("stsls SDM", nome_w))

  res$residuos_sar <- residuals(sar)
  res$residuos_sdm <- residuals(sdm)
  res
}

# ---- 2.8a Impactos de um modelo SLX (X + WX) pelo metodo delta -------------
# Num SLX nao ha (I - rho W)^-1: o efeito direto de x e o proprio beta, o
# indireto e o theta da defasagem e o total e a soma. O EP do total sai do
# metodo delta: Var(b+t) = Var(b) + Var(t) + 2 Cov(b,t). Tudo vem de coef() e
# vcov(), sem depender da estrutura interna de nenhum objeto do spatialreg.
impactos_slx <- function(m, vars_durbin, rotulo) {
  b <- coef(m); V <- vcov(m)
  purrr::map_dfr(vars_durbin, function(v) {
    ln <- paste0("lag_", v)
    if (!(v %in% names(b)) || !(ln %in% names(b))) return(NULL)
    if (is.na(b[[v]]) || is.na(b[[ln]])) return(NULL)
    dir <- unname(b[v]); ind <- unname(b[ln]); tot <- dir + ind
    se_d <- sqrt(V[v, v]); se_i <- sqrt(V[ln, ln])
    se_t <- sqrt(V[v, v] + V[ln, ln] + 2 * V[v, ln])
    p2 <- function(e, s) 2 * stats::pnorm(-abs(e / s))
    tibble(Variavel = v, Especificacao = rotulo,
           Efeito_Direto = dir,   EP_Direto = se_d,   P_Direto = p2(dir, se_d),
           Efeito_Indireto = ind, EP_Indireto = se_i, P_Indireto = p2(ind, se_i),
           Efeito_Total = tot,    EP_Total = se_t,    P_Total = p2(tot, se_t),
           IC_Total_inf = tot - 1.959964 * se_t, IC_Total_sup = tot + 1.959964 * se_t)
  })
}

# ---- 2.8 [3.1] Modelos ponderados pela populacao ---------------------------
# ATENCAO: o spatialreg 1.4.3 NAO aceita 'weights' no lagsarlm (so no
# errorsarlm). O script detecta isso e entrega tres coisas:
#   (i)   OLS ponderado - leitura de direcao, roda sempre;
#   (ii)  SAR ponderado se a versao permitir, senao SEM ponderado;
#   (iii) par SLX sem peso x SLX com peso - MESMA especificacao (X + WX nas 5
#         defasagens do SDM), estimada por lm, que aceita peso nativamente e da
#         efeitos direto/indireto/total comparaveis entre si. E este par que
#         isola o efeito da ponderacao com a especificacao fixa; o SLX nao e o
#         SAR, entao entra como sensibilidade, nao como substituto do modelo
#         de referencia.
modelos_ponderados <- function(dados_df, listw, nome_w, cfg, form_completa, trMat,
                               vars_durbin) {

  w <- dados_df$pop_media
  if (is.null(w) || any(is.na(w)) || any(w <= 0)) {
    log_msg("   populacao ausente, com NA ou <= 0; ponderacao pulada"); return(NULL)
  }
  saida <- list()

  # CORRECAO (rodada de 01/10 falhou aqui com "objeto 'w' nao encontrado").
  # lm() e errorsarlm() avaliam o argumento 'weights' dentro de 'data' e, se nao
  # achar, em environment(formula) - NUNCA no frame de quem chamou. A formula foi
  # criada em processar_desfecho(), onde 'w' nao existe. Reapontar o ambiente da
  # formula para este frame resolve de uma vez para todos os ajustes ponderados.
  form_p <- form_completa
  environment(form_p) <- environment()

  # OLS ponderado (sempre)
  ols_p <- lm(form_p, data = dados_df, weights = w)
  saida$ols_ponderado <- broom::tidy(ols_p) %>% mutate(Matriz = nome_w, Modelo = "OLS ponderado")

  # Os ajustes ponderados NAO passam por fit_lag/fit_err: 'w' precisa continuar
  # visivel na cadeia de ambientes, entao a rede de seguranca e montada aqui
  # dentro, onde 'w' existe.
  com_fallback <- function(ajusta, rotulo) tentar_metodos(ajusta, rotulo)

  if (LAG_ACEITA_W) {
    m <- com_fallback(function(met)
      lagsarlm(form_p, data = dados_df, listw = listw, weights = w,
               method = met, zero.policy = TRUE), "SAR ponderado")
    tipo <- "SAR ponderado (lagsarlm, weights = populacao)"
    log_msg("   ", tipo, ": rho = ", round(m$rho, 4))
    saida$impactos <- seguro(impactos_com_ic(m, trMat, paste("SAR ponderado", nome_w)),
                             paste("impactos do SAR ponderado", nome_w))
    if (!is.null(saida$impactos)) saida$impactos$Matriz <- nome_w
    par_esp <- m$rho; ep_esp <- if (is.null(m$rho.se)) NA else m$rho.se
  } else if (SEM_ACEITA_W) {
    m <- com_fallback(function(met)
      errorsarlm(form_p, data = dados_df, listw = listw, weights = w,
                 method = met, zero.policy = TRUE), "SEM ponderado")
    tipo <- "SEM ponderado (errorsarlm, weights = populacao) - esta versao do spatialreg nao aceita weights no lagsarlm"
    log_msg("   !! ", tipo)
    log_msg("   !! no SEM nao ha transbordamento: a saida sao COEFICIENTES, nao impactos")
    saida$coeficientes <- as.data.frame(summary(m)$Coef) %>%
      tibble::rownames_to_column("term") %>%
      setNames(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
      mutate(Matriz = nome_w)
    par_esp <- m$lambda; ep_esp <- if (is.null(m$lambda.se)) NA else m$lambda.se
  } else {
    log_msg("   !! nem lagsarlm nem errorsarlm aceitam weights nesta versao; so o OLS ponderado saiu")
    m <- NULL; tipo <- "apenas OLS ponderado"; par_esp <- NA; ep_esp <- NA
  }

  saida$resumo <- tibble(
    Matriz = nome_w, Modelo = tipo,
    Rho_ou_Lambda = par_esp, EP = ep_esp,
    LogLik = if (is.null(m)) NA_real_ else as.numeric(logLik(m)),
    Peso = sprintf("populacao media anual %d-%d", min(cfg$pop_anos), max(cfg$pop_anos)),
    Nota = "AIC do modelo ponderado NAO e comparavel ao do nao ponderado; entra como sensibilidade")

  # ---- par SLX sem peso x SLX com peso, mesma especificacao ---------------
  # Construo as colunas WX a mao e ajusto por lm: o peso entra nativamente e os
  # efeitos saem por metodo delta, sem depender da estrutura interna de nenhum
  # objeto do spatialreg. E a comparacao que isola o efeito da ponderacao.
  saida$slx <- seguro({
    WX <- sapply(vars_durbin, function(v) lag.listw(listw, dados_df[[v]], zero.policy = TRUE))
    colnames(WX) <- paste0("lag_", vars_durbin)
    dados_aug <- cbind(dados_df, as.data.frame(WX))
    form_slx <- update(form_p, as.formula(paste("~ . +", paste(colnames(WX), collapse = " + "))))
    environment(form_slx) <- environment()   # garante que 'w' continue visivel

    slx_sem <- lm(form_slx, data = dados_aug)
    slx_com <- lm(form_slx, data = dados_aug, weights = w)

    out <- bind_rows(
      impactos_slx(slx_sem, vars_durbin, "SLX sem peso"),
      impactos_slx(slx_com, vars_durbin, "SLX ponderado pela populacao")) %>%
      mutate(Matriz = nome_w)

    a <- out %>% filter(Especificacao == "SLX sem peso") %>%
      select(Variavel, tot_sem = Efeito_Total, p_sem = P_Total)
    b <- out %>% filter(Especificacao == "SLX ponderado pela populacao") %>%
      select(Variavel, tot_com = Efeito_Total, p_com = P_Total)
    cmp <- inner_join(a, b, by = "Variavel")
    if (nrow(cmp) > 0)
      log_msg("   SLX sem peso x ponderado (", nome_w, "): ", nrow(cmp),
              " variaveis | trocaram de sinal: ",
              sum(sign(cmp$tot_sem) != sign(cmp$tot_com), na.rm = TRUE),
              " | perderam significancia: ",
              sum(cmp$p_sem < 0.05 & cmp$p_com >= 0.05, na.rm = TRUE),
              " | ganharam: ",
              sum(cmp$p_sem >= 0.05 & cmp$p_com < 0.05, na.rm = TRUE))
    out
  }, paste("SLX ponderado", nome_w))

  saida
}

# ---- 2.9 [3.3][3.4] Sensibilidades da amostra ------------------------------
sensibilidade_subamostra <- function(mapa_df, cfg, form_completa, manter, rotulo) {
  sub <- mapa_df[manter, ]
  lw <- nb2listw(poly2nb(sub, queen = TRUE), style = "W", zero.policy = TRUE)
  sar <- fit_lag(form_completa, st_drop_geometry(sub), lw, rotulo = paste("SAR", rotulo))
  tr <- trW(as(lw, "CsparseMatrix"), type = "mult")
  imp <- impactos_com_ic(sar, tr, rotulo)
  imp$Cenario <- rotulo; imp$n <- nrow(sub); imp$Rho <- sar$rho
  log_msg("   ", rotulo, ": n = ", nrow(sub), " | rho = ", round(sar$rho, 4))
  imp
}

# ---- 2.10 GWR (uma especificacao) ------------------------------------------
rodar_gwr <- function(sp_pts, dMat, formula_gwr, nome_spec, cfg, listw_queen, dados_df,
                      diagnosticos = TRUE) {

  vars <- all.vars(formula_gwr)[-1]
  log_msg("   GWR [", nome_spec, "]: ", paste(vars, collapse = " + "))
  n <- nrow(dados_df)

  ols5 <- lm(formula_gwr, data = dados_df)
  k <- length(coef(ols5)) + 1
  aicc_ols <- AIC(ols5) + (2 * k * (k + 1)) / (n - k - 1)

  bw <- bw.gwr(formula_gwr, data = sp_pts, approach = "AICc",
               kernel = "bisquare", adaptive = TRUE, dMat = dMat)
  log_msg("      banda otima: ", bw, " vizinhos (", round(100 * bw / n, 1), "% da amostra)")

  g <- gwr.basic(formula_gwr, data = sp_pts, bw = bw, kernel = "bisquare",
                 adaptive = TRUE, dMat = dMat)
  d <- g$GW.diagnostic
  aicc_gwr <- if (!is.null(d$gw.AICc)) d$gw.AICc else d$AICc
  pega <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)

  resumo <- tibble(                                                      # [2.4]
    Especificacao = nome_spec, Variaveis = paste(vars, collapse = " + "),
    n = n, Banda_vizinhos = bw, Pct_amostra = 100 * bw / n,
    Kernel = "bisquare adaptativo",
    AICc_OLS_mesmas_vars = aicc_ols, AICc_GWR = aicc_gwr, Delta_AICc = aicc_ols - aicc_gwr,
    ENP = pega(d$enp), EDF = pega(d$edf),
    R2_GWR = pega(d$gw.R2), R2_ajustado_GWR = pega(d$gwR2.adj),
    R2_OLS_mesmas_vars = summary(ols5)$r.squared,
    Ganho_AICc = ifelse((aicc_ols - aicc_gwr) > 3, "sim", "nao"))

  sdf <- limpar_colunas_vazias(as.data.frame(g$SDF))                     # [4.7]

  # [4.3] conferencia da ordem das linhas antes de qualquer bind_cols
  ordem_ok <- seguro({
    if (!"y" %in% names(as.data.frame(g$SDF))) NA
    else max(abs(as.data.frame(g$SDF)$y - dados_df[[cfg$desfecho]]), na.rm = TRUE) < 1e-8
  }, "conferencia de ordem do SDF")
  if (isTRUE(ordem_ok)) log_msg("      ordem das linhas do SDF conferida [OK]")
  else log_msg("      !! ordem das linhas do SDF NAO conferida - checar antes de usar o gpkg")

  # [S9] Moran do GWR AO LADO do OLS com as MESMAS variaveis
  moran <- seguro({
    mg <- moran.test(g$SDF$residual, listw_queen, zero.policy = TRUE, alternative = "two.sided")
    mo <- moran.test(residuals(ols5), listw_queen, zero.policy = TRUE, alternative = "two.sided")
    tibble(Modelo = c(sprintf("OLS (%d variaveis do GWR)", length(vars)),
                      sprintf("GWR [%s]", nome_spec)),
           Moran_I = c(unname(mo$estimate[1]), unname(mg$estimate[1])),
           P_bilateral = c(mo$p.value, mg$p.value))
  }, paste("Moran do GWR", nome_spec))

  tadj <- NULL; leung <- NULL
  # A colinearidade local (gwr.collin.diagno) NAO roda aqui: leva ~1h40 por
  # desfecho e, na rodada de 01/10, o resultado ficou preso na memoria sem nunca
  # ser gravado porque o teste F travou logo depois. Agora ela e o ULTIMO passo
  # do desfecho, com gravacao propria - ver diagnostico_colinearidade_gwr().

  if (diagnosticos && EXECUTAR_TADJUST) {                                # [3.5]
    tadj <- seguro({ as.data.frame(gwr.t.adjust(g)$SDF) }, paste("gwr.t.adjust", nome_spec))
    if (!is.null(tadj)) {
      cols_p <- grep("_p($|_)", names(tadj), value = TRUE)
      if (length(cols_p) > 0) {
        pct <- vapply(cols_p, function(cc) 100 * mean(tadj[[cc]] < 0.05, na.rm = TRUE), numeric(1))
        log_msg("      % de municipios com p < 0,05 (bruto e ajustado):")
        for (i in seq_along(pct)) log_msg("         ", cols_p[i], ": ", round(pct[i], 1), "%")
      }
    }
  }

  if (diagnosticos && EXECUTAR_F123) {                                   # [3.5]
    leung <- seguro({
      gF <- gwr.basic(formula_gwr, data = sp_pts, bw = bw, kernel = "bisquare",
                      adaptive = TRUE, dMat = dMat, F123.test = TRUE)
      ft <- gF$Ftests; lst <- list()
      if (!is.null(ft$F1.test)) lst$F1 <- as.data.frame(t(as.matrix(ft$F1.test)))
      if (!is.null(ft$F2.test)) lst$F2 <- as.data.frame(t(as.matrix(ft$F2.test)))
      if (!is.null(ft$F3.test)) lst$F3 <- as.data.frame(ft$F3.test) %>%
          tibble::rownames_to_column("Termo")
      rm(gF); gc(verbose = FALSE); lst
    }, paste("F123.test de Leung", nome_spec))
    if (!is.null(leung) && !is.null(leung$F3)) {
      log_msg("      teste F3 de Leung obtido (variacao espacial por coeficiente)")
    } else {
      log_msg("      teste F de Leung indisponivel (provavel limite de memoria). ",
              "Argumento substituto: banda de ", round(100 * bw / n, 1), "% da amostra ",
              "ja indica processo quase global.")
    }
  }

  list(resumo = resumo, sdf = sdf, moran = moran, tadj = tadj, leung = leung,
       bw = bw, vars = vars, formula = formula_gwr, diagnosticos = diagnosticos)
}

# ---- 2.10b [3.5] Colinearidade local: ULTIMO passo do desfecho -------------
# Separada de rodar_gwr de proposito. E o bloco mais lento do script (~1h40 em
# n = 5.566) e roda depois que CSV, gpkg, Word e checkpoint ja estao em disco.
# Se for interrompida, perde-se so este arquivo.
diagnostico_colinearidade_gwr <- function(sp_pts, dMat, cfg, gwr_res) {
  alvos <- names(gwr_res)[vapply(gwr_res, function(x) !is.null(x) && isTRUE(x$diagnosticos),
                                 logical(1))]
  if (length(alvos) == 0) return(invisible(NULL))
  resumos <- list()
  for (nm in alvos) {
    x <- gwr_res[[nm]]
    t0 <- Sys.time()
    log_msg("   colinearidade local [", nm, "] - bloco lento, tudo o mais ja esta gravado")
    collin <- seguro({
      as.data.frame(gwr.collin.diagno(x$formula, data = sp_pts, bw = x$bw,
                                      kernel = "bisquare", adaptive = TRUE, dMat = dMat)$SDF)
    }, paste("gwr.collin.diagno", nm))
    if (is.null(collin)) next
    escrever(collin, cfg, paste0("GWR_Colinearidade_Local_", nm, "_", cfg$sufixo, ".csv"))
    if ("local_CN" %in% names(collin)) {
      cn <- collin$local_CN
      resumos[[nm]] <- tibble(
        Especificacao = nm, Banda = x$bw,
        CN_mediana = median(cn, na.rm = TRUE),
        CN_p90 = unname(quantile(cn, .9, na.rm = TRUE)),
        CN_maximo = max(cn, na.rm = TRUE),
        Pct_CN_acima_10 = 100 * mean(cn > 10, na.rm = TRUE),
        Pct_CN_acima_30 = 100 * mean(cn > 30, na.rm = TRUE),
        Minutos = as.numeric(difftime(Sys.time(), t0, units = "mins")))
      log_msg("      numero de condicao local: mediana = ", round(median(cn, na.rm = TRUE), 1),
              " | % acima de 30 = ", round(100 * mean(cn > 30, na.rm = TRUE), 1),
              "% | ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
    }
  }
  escrever(bind_rows(resumos), cfg, paste0("GWR_Colinearidade_Resumo_", cfg$sufixo, ".csv"))
}

# ---- 2.11 [S5] Selecao stepwise por AICc (gwr.model.selection) -------------
# Forward stepwise do proprio GWmodel, com BANDA FIXA (a da especificacao
# principal) para nao reotimizar a banda a cada candidato. Com 10 candidatas
# sao 55 ajustes de GWR; sem banda fixa seriam 55 buscas de banda.
stepwise_gwr <- function(sp_pts, dMat, cfg, candidatas, bw_fixo) {
  log_msg("   GWR stepwise (AICc) sobre ", length(candidatas), " candidatas, banda fixa = ", bw_fixo)
  sel <- gwr.model.selection(DeVar = cfg$desfecho, InDeVars = candidatas,
                             data = sp_pts, bw = bw_fixo, approach = "AICc",
                             adaptive = TRUE, kernel = "bisquare", dMat = dMat)
  modelos <- sel[[1]]
  # sel[[2]] e uma matriz cuja ULTIMA coluna traz o criterio (o exemplo do
  # proprio GWmodel usa model.sel[[2]][, 2]). Confere o alinhamento antes de usar.
  crit <- as.matrix(sel[[2]])
  valores <- as.numeric(crit[, ncol(crit)])
  if (length(valores) != length(modelos))
    stop("gwr.model.selection devolveu ", length(modelos), " modelos e ",
         length(valores), " valores de criterio - estrutura inesperada")
  tabela <- purrr::map_dfr(seq_along(modelos), function(i) {
    v <- modelos[[i]][[2]]
    tibble(Posicao = i, n_variaveis = length(v),
           Variaveis = paste(v, collapse = " + "), AICc = valores[i])
  }) %>% arrange(AICc)
  melhor <- modelos[[which.min(valores)]][[2]]
  log_msg("      melhor modelo por AICc (", length(melhor), " variaveis): ",
          paste(melhor, collapse = ", "))
  list(tabela = tabela, vars = melhor)
}

# =============================================================================
# 3. PIPELINE DE UM DESFECHO
# =============================================================================

processar_desfecho <- function(cfg, mapa_br, pop) {

  log_bloco(paste("INICIANDO:", cfg$rotulo))
  t0 <- Sys.time()
  if (!dir.exists(cfg$dir_exportacao)) dir.create(cfg$dir_exportacao, recursive = TRUE)
  sfx <- cfg$sufixo

  # ---- 3.1 dados e pesos ---------------------------------------------------
  mapa_df  <- carregar_dados(cfg, mapa_br, pop)
  dados_df <- st_drop_geometry(mapa_df)
  pesos    <- construir_pesos(mapa_df)
  form_completa <- as.formula(paste(cfg$desfecho, "~",
                                    paste(cfg$covariaveis, collapse = " + ")))

  # ---- 3.2 LASSO / ordenamento --------------------------------------------
  log_msg("-- Ordenamento das variaveis (LASSO com lambda.min)")
  lasso <- rodar_lasso(mapa_df, cfg)
  escrever(lasso$ranking,     cfg, paste0("Ranking_LASSO_", sfx, ".csv"))
  escrever(lasso$params,      cfg, paste0("Parametros_LASSO_", sfx, ".csv"))
  escrever(lasso$comparacao,  cfg, paste0("LASSO_vs_OLS_padronizado_", sfx, ".csv"))
  escrever(lasso$ranking_t,   cfg, paste0("Ranking_por_t_OLS_", sfx, ".csv"))

  # Defasagens do SDM: top-5 do LASSO COMO ESTAVA (inclusive a dummy). Em WX a
  # dummy e uma medida de contiguidade regional bem definida - a objecao de
  # identificacao vale para o GWR, nao para o Durbin.
  vars_durbin <- head(lasso$ranking$Variavel, 5)
  form_durbin <- as.formula(paste("~", paste(vars_durbin, collapse = " + ")))
  log_msg("   defasagens do SDM (top-5 LASSO): ", paste(vars_durbin, collapse = ", "))

  specs <- list(
    principal   = head(setdiff(lasso$ranking$Variavel,   cfg$vars_fora_gwr), 5),  # [S1]
    alternativa = head(setdiff(lasso$ranking_t$Variavel, cfg$vars_fora_gwr), 5),  # [S4]
    original    = vars_durbin)
  specs <- specs[!duplicated(vapply(specs, paste, character(1), collapse = "|"))]
  for (nm in names(specs)) log_msg("   GWR[", nm, "]: ", paste(specs[[nm]], collapse = ", "))

  # ---- 3.3 modelos globais, Queen e KNN-5 ---------------------------------
  res_queen <- estimar_modelos_globais(dados_df, pesos$queen, "Queen", cfg,
                                       form_completa, form_durbin, vars_durbin)
  res_knn5  <- estimar_modelos_globais(dados_df, pesos$knn5, "KNN-5", cfg,
                                       form_completa, form_durbin, vars_durbin)
  junta <- function(campo) bind_rows(res_queen[[campo]], res_knn5[[campo]])

  escrever(junta("tabela_modelos"), cfg, paste0("Tabela_Modelos_Espaciais_", sfx, ".csv"))
  escrever(junta("tabela_moran"),   cfg, paste0("Tabela_Moran_Residuos_", sfx, ".csv"))
  escrever(junta("rstests"),        cfg, paste0("Testes_LM_Robustos_", sfx, ".csv"))
  escrever(junta("bptest"),         cfg, paste0("Breusch_Pagan_", sfx, ".csv"))
  escrever(junta("vif"),            cfg, paste0("VIF_SDM_Parcial_", sfx, ".csv"))
  escrever(junta("cor_x_wx"),       cfg, paste0("Correlacao_X_WX_", sfx, ".csv"))
  escrever(junta("impactos_sar"),   cfg, paste0("Impactos_SAR_", sfx, ".csv"))
  escrever(junta("impactos_sdm"),   cfg, paste0("Impactos_SDM_Parcial_", sfx, ".csv"))

  escrever(bind_rows(res_queen$sar_robusto$coeficientes, res_knn5$sar_robusto$coeficientes),
           cfg, paste0("Coeficientes_SAR_Robusto_", sfx, ".csv"))
  escrever(bind_rows(res_queen$sar_robusto$impactos, res_knn5$sar_robusto$impactos),
           cfg, paste0("Impactos_SAR_Robusto_", sfx, ".csv"))
  escrever(bind_rows(res_queen$sar_robusto$anselin_kelejian, res_knn5$sar_robusto$anselin_kelejian),
           cfg, paste0("Teste_Anselin_Kelejian_", sfx, ".csv"))
  escrever(bind_rows(res_queen$sdm_robusto$coeficientes, res_knn5$sdm_robusto$coeficientes),
           cfg, paste0("Coeficientes_SDM_Robusto_", sfx, ".csv"))
  escrever(bind_rows(res_queen$sdm_robusto$impactos, res_knn5$sdm_robusto$impactos),
           cfg, paste0("Impactos_SDM_Robusto_", sfx, ".csv"))

  # ---- 3.4 [3.1] ponderacao por populacao ---------------------------------
  pond <- NULL
  if (EXECUTAR_PONDER) {
    log_msg("-- Modelos ponderados pela populacao do municipio")
    pq <- seguro(modelos_ponderados(dados_df, pesos$queen, "Queen", cfg, form_completa,
                                    res_queen$trMat, vars_durbin), "modelo ponderado Queen")
    pk <- seguro(modelos_ponderados(dados_df, pesos$knn5, "KNN-5", cfg, form_completa,
                                    res_knn5$trMat, vars_durbin), "modelo ponderado KNN-5")
    escrever(bind_rows(pq$resumo, pk$resumo),               cfg, paste0("Resumo_Ponderado_", sfx, ".csv"))
    escrever(bind_rows(pq$impactos, pk$impactos),           cfg, paste0("Impactos_SAR_Ponderado_", sfx, ".csv"))
    escrever(bind_rows(pq$coeficientes, pk$coeficientes),   cfg, paste0("Coeficientes_SEM_Ponderado_", sfx, ".csv"))
    escrever(bind_rows(pq$ols_ponderado, pk$ols_ponderado), cfg, paste0("Coeficientes_OLS_Ponderado_", sfx, ".csv"))
    escrever(bind_rows(pq$slx, pk$slx),                     cfg, paste0("Impactos_SLX_Peso_vs_SemPeso_", sfx, ".csv"))
    pond <- pq
  }

  # ---- 3.5 [3.3][3.4] sensibilidades da amostra ---------------------------
  if (EXECUTAR_OUTLIER) {
    log_msg("-- Sensibilidade a extremos e a ilhas (SAR-Queen)")
    y <- dados_df[[cfg$desfecho]]
    cen <- list(sem_3_maiores  = !(seq_along(y) %in% order(y, decreasing = TRUE)[1:3]),
                sem_10_maiores = !(seq_along(y) %in% order(y, decreasing = TRUE)[1:10]))
    if (pesos$n_ilhas > 0) cen$sem_ilhas <- !(seq_along(y) %in% pesos$ids_ilhas)
    sens <- purrr::map_dfr(names(cen), function(nm)
      seguro(sensibilidade_subamostra(mapa_df, cfg, form_completa, cen[[nm]], nm),
             paste("sensibilidade", nm)))
    escrever(sens, cfg, paste0("Sensibilidade_Amostra_SAR_Queen_", sfx, ".csv"))
  }

  # ---- 3.6 GWR ------------------------------------------------------------
  log_msg("-- GWR")
  dados_gwr <- dados_df[, c(cfg$desfecho, cfg$covariaveis), drop = FALSE]
  dados_gwr$coord_x <- pesos$coords[, 1]; dados_gwr$coord_y <- pesos$coords[, 2]
  sp_pts <- as(st_as_sf(dados_gwr, coords = c("coord_x", "coord_y"), crs = CRS_GEOGRAFICO),
               "Spatial")
  log_msg("   pre-calculando a matriz de distancias (km)...")
  dMat <- gw.dist(pesos$coords, longlat = TRUE)

  gwr_res <- list()
  for (nm in names(specs)) {
    f <- as.formula(paste(cfg$desfecho, "~", paste(specs[[nm]], collapse = " + ")))
    gwr_res[[nm]] <- seguro(
      rodar_gwr(sp_pts, dMat, f, nm, cfg, pesos$queen, dados_df,
                diagnosticos = (!DIAG_SO_PRINCIPAL) || nm == "principal"),
      paste("GWR", nm))
  }

  # [S5] stepwise, usando a banda da principal como banda fixa da selecao
  if (EXECUTAR_STEPWISE && !is.null(gwr_res$principal)) {
    sw <- seguro({
      cand <- head(setdiff(lasso$ranking$Variavel, cfg$vars_fora_gwr), N_CAND_STEPWISE)
      stepwise_gwr(sp_pts, dMat, cfg, cand, gwr_res$principal$bw)
    }, "gwr.model.selection")
    if (!is.null(sw)) {
      escrever(sw$tabela, cfg, paste0("GWR_Stepwise_AICc_Tabela_", sfx, ".csv"))
      ja_tem <- vapply(specs, function(v) setequal(v, sw$vars), logical(1))
      if (any(ja_tem)) {
        log_msg("   stepwise escolheu o mesmo conjunto de [", names(specs)[which(ja_tem)[1]],
                "] - nao refaz o ajuste")
      } else {
        f <- as.formula(paste(cfg$desfecho, "~", paste(sw$vars, collapse = " + ")))
        gwr_res$stepwise <- seguro(
          rodar_gwr(sp_pts, dMat, f, "stepwise", cfg, pesos$queen, dados_df,
                    diagnosticos = !DIAG_SO_PRINCIPAL), "GWR stepwise")
      }
    }
  }

  # [S15] tabela consolidada de parametros do GWR
  tab_gwr <- bind_rows(lapply(gwr_res, function(x) if (!is.null(x)) x$resumo))
  escrever(tab_gwr, cfg, paste0("Parametros_GWR_Consolidado_", sfx, ".csv"))

  tab_moran_gwr <- bind_rows(lapply(names(gwr_res), function(nm) {
    x <- gwr_res[[nm]]; if (is.null(x) || is.null(x$moran)) return(NULL)
    x$moran %>% mutate(Especificacao = nm)
  }))
  escrever(tab_moran_gwr, cfg, paste0("Tabela_Moran_GWR_vs_OLS5_", sfx, ".csv"))

  for (nm in names(gwr_res)) {
    x <- gwr_res[[nm]]; if (is.null(x)) next
    escrever(x$tadj,   cfg, paste0("GWR_P_Ajustados_", nm, "_", sfx, ".csv"))
    if (!is.null(x$leung$F3)) escrever(x$leung$F3, cfg, paste0("GWR_Teste_F3_Leung_", nm, "_", sfx, ".csv"))
    if (!is.null(x$leung$F1)) escrever(x$leung$F1, cfg, paste0("GWR_Teste_F1_Leung_", nm, "_", sfx, ".csv"))
    if (!is.null(x$leung$F2)) escrever(x$leung$F2, cfg, paste0("GWR_Teste_F2_Leung_", nm, "_", sfx, ".csv"))
  }

  # ---- 3.7 camada espacial completa (QGIS) --------------------------------
  seguro({
    principal <- if (!is.null(gwr_res$principal)) gwr_res$principal else gwr_res[[1]]
    mapa_out <- mapa_df
    mapa_out$residuos_sar_queen <- res_queen$residuos_sar
    mapa_out$residuos_sdm_queen <- res_queen$residuos_sdm
    cols_gwr <- principal$sdf %>% select(-any_of(c("coords.x1", "coords.x2"))) %>%
      rename_with(~ paste0("gwr_", .x))
    mapa_out <- bind_cols(mapa_out, cols_gwr)
    st_write(mapa_out,
             dsn = file.path(cfg$dir_exportacao, paste0("Diagnostico_", sfx, "_GWR_e_SDM.gpkg")),
             layer = "resultados_gwr", driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
    readr::write_csv(st_drop_geometry(mapa_out),
                     file.path(cfg$dir_exportacao, paste0("Resultados_Completos_", sfx, "_GWR_SDM.csv")))
    log_msg("   camada espacial (gpkg) e CSV completo gravados")
  }, "exportacao espacial")

  # ---- 3.8 Word ABNT ------------------------------------------------------
  seguro(gerar_word(cfg, res_queen, res_knn5, tab_gwr, tab_moran_gwr, lasso, pond),
         "geracao do Word")

  # ---- 3.9 checkpoint -----------------------------------------------------
  saida <- list(cfg = cfg, n = nrow(mapa_df), specs = specs, vars_durbin = vars_durbin,
                lasso = lasso[c("ranking", "params", "comparacao", "ranking_t")],
                queen = res_queen[c("tabela_modelos", "tabela_moran", "rstests", "bptest",
                                    "vif", "cor_x_wx", "impactos_sar", "impactos_sdm")],
                knn5  = res_knn5[c("tabela_modelos", "tabela_moran", "impactos_sar", "impactos_sdm")],
                gwr = tab_gwr, moran_gwr = tab_moran_gwr)
  saveRDS(saida, file.path(cfg$dir_exportacao, paste0("Checkpoint_Parte57_V4_", sfx, ".rds")))
  log_msg("CHECKPOINT gravado: ", cfg$rotulo, " esta completo em disco apos ",
          round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
          " min. O que vier depois e extra.")

  # ---- 3.10 colinearidade local: por ultimo, de proposito -----------------
  if (roda_collin(cfg)) {
    seguro(diagnostico_colinearidade_gwr(sp_pts, dMat, cfg, gwr_res), "colinearidade local")
  } else {
    log_msg("   colinearidade local nao solicitada para este desfecho (EXECUTAR_COLLIN)")
  }

  rm(dMat, sp_pts, gwr_res, mapa_df, dados_df); gc(verbose = FALSE)
  log_msg("CONCLUIDO: ", cfg$rotulo, " em ",
          round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
  saida
}

# =============================================================================
# 4. RELATORIO WORD
# =============================================================================

gerar_word <- function(cfg, res_queen, res_knn5, tab_gwr, tab_moran_gwr, lasso, pond) {

  set_flextable_defaults(font.family = "Times New Roman", font.size = 10,
                         decimal.mark = ",", big.mark = ".")
  ft <- function(d, titulo, digitos = 4) {
    d <- as.data.frame(d); num <- names(d)[vapply(d, is.numeric, logical(1))]
    f <- flextable(d)
    if (length(num) > 0) f <- colformat_double(f, j = num, digits = digitos)
    f %>% autofit() %>% border_remove() %>%
      hline_top(border = fp_border(color = "black", width = 1.5), part = "header") %>%
      hline_bottom(border = fp_border(color = "black", width = 1), part = "header") %>%
      hline_bottom(border = fp_border(color = "black", width = 1.5), part = "body") %>%
      set_caption(caption = titulo)
  }
  add <- function(doc, d, titulo, digitos = 4) {
    if (is.null(d) || nrow(as.data.frame(d)) == 0) return(doc)
    body_add_flextable(doc, ft(d, titulo, digitos), align = "center") %>%
      body_add_par("", style = "Normal")
  }
  lr_txt <- function(r) {
    if (is.null(r$lr_sdm_sar)) return("Teste LR SDM vs SAR: indisponivel")
    sprintf("Teste LR SDM (parcial) vs SAR, 5 gl: estatistica = %s; p = %s",
            format(round(unname(r$lr_sdm_sar$statistic), 3), decimal.mark = ","),
            ifelse(r$lr_sdm_sar$p.value < 0.001, "< 0,001",
                   format(round(r$lr_sdm_sar$p.value, 4), decimal.mark = ",")))
  }

  doc <- read_docx() %>%
    body_add_par(paste0("Resultados Espaciais Globais - ", cfg$rotulo), style = "heading 1") %>%
    body_add_par(paste0("Taxa padronizada suavizada global. Matriz Queen (principal) e KNN-5 ",
                        "(sensibilidade), esta ultima reconstruida com centroides em CRS ",
                        "projetado (SIRGAS 2000 / Policonica, EPSG:5880) e distancia de grande ",
                        "circulo."), style = "Normal") %>%

    body_add_par("1. Justificativa da forma espacial (testes LM/RS robustos)", style = "heading 2") %>%
    add(bind_rows(res_queen$rstests, res_knn5$rstests),
        "Testes de multiplicador de Lagrange sobre os residuos do OLS (Anselin).") %>%

    body_add_par("2. Selecao de modelos", style = "heading 2") %>%
    add(bind_rows(res_queen$tabela_modelos, res_knn5$tabela_modelos),
        "Modelos espaciais globais: gl, AIC, BIC, log-verossimilhanca, rho/lambda e erro-padrao.", 3) %>%
    body_add_par(lr_txt(res_queen), style = "Normal") %>%
    body_add_par(paste0("Nota: gl inclui o parametro de variancia; a coluna gl_antigo_coef ",
                        "reproduz a contagem usada nas versoes anteriores."), style = "Normal") %>%

    body_add_par("3. Autocorrelacao residual", style = "heading 2") %>%
    add(bind_rows(res_queen$tabela_moran, res_knn5$tabela_moran),
        "I de Moran dos residuos (p unilateral e bilateral).") %>%

    body_add_par("4. Heterocedasticidade", style = "heading 2") %>%
    add(bind_rows(res_queen$bptest, res_knn5$bptest),
        "Teste de Breusch-Pagan (versao espacial para SAR e SDM).") %>%
    add(bind_rows(res_queen$sar_robusto$anselin_kelejian, res_knn5$sar_robusto$anselin_kelejian),
        "Teste de Anselin-Kelejian sobre os residuos do SAR estimado por 2SLS.") %>%
    body_add_break() %>%

    body_add_par("5. Impactos por maxima verossimilhanca (Queen), com IC95%", style = "heading 2") %>%
    add(res_queen$impactos_sar, "Efeitos diretos, indiretos e totais do SAR - matriz Queen.") %>%
    add(res_queen$impactos_sdm, "Efeitos diretos, indiretos e totais do SDM parcial - matriz Queen.") %>%
    body_add_break() %>%

    body_add_par("6. Estimacao robusta a heterocedasticidade (2SLS)", style = "heading 2") %>%
    add(res_queen$sar_robusto$impactos, "Efeitos do SAR sob estimacao robusta (stsls) - matriz Queen.") %>%
    add(res_queen$sdm_robusto$impactos,
        paste0("Efeitos do SDM parcial sob estimacao robusta: decomposicao de LeSage & Pace ",
               "aplicada aos coeficientes 2SLS, validada contra o impacts() do spatialreg.")) %>%
    add(res_queen$sdm_robusto$coeficientes,
        "Coeficientes do SDM parcial por 2SLS robusto (defasagens WX explicitas) - matriz Queen.") %>%
    body_add_break() %>%

    body_add_par("7. Sensibilidade: ponderacao pela populacao", style = "heading 2") %>%
    body_add_par(paste0("Peso = populacao media anual do municipio no periodo do desfecho ",
                        "(IBGE). A variancia de uma taxa e inversamente proporcional ao ",
                        "denominador, de modo que a ponderacao e a resposta direta a ",
                        "heterocedasticidade dependente do nivel. O AIC do modelo ponderado ",
                        "NAO e comparavel ao do nao ponderado: a tabela entra como ",
                        "sensibilidade, nao na comparacao de modelos."), style = "Normal")
  if (!is.null(pond)) {
    doc <- doc %>% add(pond$resumo, "Parametro espacial do modelo ponderado.") %>%
      add(pond$slx,
          paste0("Comparacao direta do efeito da ponderacao: modelo SLX (X + WX nas ",
                 "mesmas defasagens do SDM parcial) sem peso e ponderado pela ",
                 "populacao, matriz Queen. Efeito direto = beta, indireto = theta, ",
                 "total = beta + theta, com erro-padrao do total pelo metodo delta. ",
                 "O SLX nao substitui o SAR: serve para isolar o efeito do peso com ",
                 "a especificacao mantida fixa.")) %>%
      add(pond$impactos, "Efeitos do modelo espacial ponderado pela populacao - matriz Queen.") %>%
      add(pond$coeficientes, "Coeficientes do modelo espacial ponderado - matriz Queen.") %>%
      add(pond$ols_ponderado, "Coeficientes do OLS ponderado pela populacao - leitura de direcao.")
  }

  doc <- doc %>% body_add_break() %>%
    body_add_par("8. Modelo local (GWR): parametros e justificativa", style = "heading 2") %>%
    add(tab_gwr, paste0("Parametros do GWR por especificacao: banda, kernel, AICc contra o OLS ",
                        "com as MESMAS variaveis, ENP/EDF, R2 e colinearidade local."), 2) %>%
    body_add_par(paste0("Especificacoes: 'principal' exclui a dummy regional, cujo coeficiente ",
                        "local nao e identificado em janelas sem variacao do regressor; ",
                        "'alternativa' usa as 5 variaveis de maior |t| no OLS; 'original' ",
                        "reproduz o top-5 do ordenamento por coeficiente padronizado; ",
                        "'stepwise' usa a selecao forward por AICc do proprio GWmodel ",
                        "(gwr.model.selection), com banda fixa. O AICc sempre compara o GWR ao ",
                        "OLS com as MESMAS variaveis, nunca ao modelo global completo."),
                 style = "Normal") %>%
    add(tab_moran_gwr, paste0("I de Moran dos residuos do GWR e do OLS com as mesmas variaveis ",
                              "(comparacao pareada, matriz Queen).")) %>%
    body_add_break() %>%

    body_add_par("9. Ordenamento das variaveis (LASSO)", style = "heading 2") %>%
    add(lasso$params, "Parametros do LASSO: lambda, coeficientes nao nulos e erro de validacao cruzada.", 5) %>%
    add(lasso$comparacao, "Concordancia entre o LASSO e o OLS padronizado.") %>%
    body_add_par(paste0("Nota: com lambda.min, n elevado em relacao ao numero de preditores, o ",
                        "LASSO nao exclui variaveis. Funciona como ORDENAMENTO por coeficiente ",
                        "padronizado, nao como selecao. A etapa e exploratoria e o SDM parcial e ",
                        "o GWR herdam essa condicao - por isso a tabela 8 traz tambem as ",
                        "especificacoes por |t| e por stepwise AICc."), style = "Normal") %>%
    body_add_break() %>%

    body_add_par("10. Sensibilidade: matriz KNN-5", style = "heading 2") %>%
    add(res_knn5$impactos_sar, "Efeitos do SAR - matriz KNN-5.") %>%
    add(res_knn5$impactos_sdm, "Efeitos do SDM parcial - matriz KNN-5.")

  alvo <- file.path(cfg$dir_exportacao,
                    paste0("Resultados_Globais_", cfg$sufixo, "_Espacial_ABNT.docx"))
  print(doc, target = alvo)
  log_msg("   Word gravado: ", basename(alvo))
}

# =============================================================================
# 5. EXECUCAO
# =============================================================================

log_bloco("PARTE 5.7 V4 - TRES DESFECHOS - INICIO")
log_msg("R ", getRversion(), " | spdep ", as.character(packageVersion("spdep")),
        " | spatialreg ", as.character(packageVersion("spatialreg")),
        " | GWmodel ", as.character(packageVersion("GWmodel")))
log_msg("lagsarlm aceita weights: ", LAG_ACEITA_W,
        " | errorsarlm aceita weights: ", SEM_ACEITA_W)
log_msg("saida: ", dir_saida)

seguro({                                                                  # [4.6]
  writeLines(capture.output(sessionInfo()), file.path(dir_saida, "sessionInfo_Parte57_V4.txt"))
}, "sessionInfo")

mapa_br <- st_read(file.path(dir_shapefile, "BR_Municipios_2024.shp"), quiet = TRUE) %>%
  mutate(CD_MUN = as.character(CD_MUN))
log_msg("shapefile: ", nrow(mapa_br), " poligonos | CRS = ", st_crs(mapa_br)$epsg)

pop <- seguro(carregar_populacao(), "leitura da populacao")
if (is.null(pop)) {
  log_msg("!! populacao nao carregada - a ponderacao sera pulada nos tres desfechos")
  EXECUTAR_PONDER <- FALSE
  pop <- tibble(COD_MUN = character(), ANO = integer(), POP = numeric())
}

resultados <- list()
for (nome in names(CONFIGS)) {
  cfg_i <- CONFIGS[[nome]]
  chk <- file.path(cfg_i$dir_exportacao, paste0("Checkpoint_Parte57_V4_", cfg_i$sufixo, ".rds"))
  if (PULAR_CONCLUIDOS && file.exists(chk)) {
    log_msg("PULANDO ", cfg_i$rotulo, " - checkpoint ja existe (PULAR_CONCLUIDOS = TRUE)")
    resultados[[nome]] <- seguro(readRDS(chk), paste("leitura do checkpoint", nome))
    next
  }
  resultados[[nome]] <- seguro(processar_desfecho(cfg_i, mapa_br, pop),
                               paste("desfecho", nome))
  gc(verbose = FALSE)
}

# ---- resumo comparativo dos tres -------------------------------------------
seguro({
  # extrator tolerante: nunca devolve vetor de comprimento 0 dentro do tibble
  um <- function(x, vazio = NA) if (length(x) == 0 || all(is.na(x))) vazio else x[1]
  comp <- purrr::map_dfr(names(resultados), function(nm) {
    r <- resultados[[nm]]; if (is.null(r) || is.null(r$queen$tabela_modelos)) return(NULL)
    tm <- r$queen$tabela_modelos
    mo <- r$queen$tabela_moran
    gwr_p <- if (!is.null(r$gwr) && nrow(r$gwr) > 0)
      r$gwr %>% filter(Especificacao == "principal") else NULL
    vazio_gwr <- is.null(gwr_p) || nrow(gwr_p) == 0
    tibble(
      Desfecho = r$cfg$rotulo, n = r$n, Variaveis = length(r$cfg$covariaveis),
      Melhor_AIC_Queen = um(tm$Modelo[which.min(tm$AIC)], NA_character_),
      Rho_SAR_Queen    = um(tm$Rho_Lambda[tm$Modelo == "SAR"], NA_real_),
      EP_Rho_SAR_Queen = um(tm$EP_Rho_Lambda[tm$Modelo == "SAR"], NA_real_),
      Moran_OLS_Queen  = um(mo$Moran_I[1], NA_real_),
      Moran_SAR_Queen  = um(mo$Moran_I[2], NA_real_),
      Defasagens_SDM   = paste(r$vars_durbin, collapse = "; "),
      GWR_principal    = paste(r$specs$principal, collapse = "; "),
      GWR_banda        = if (vazio_gwr) NA_real_ else um(gwr_p$Banda_vizinhos, NA_real_),
      GWR_banda_pct    = if (vazio_gwr) NA_real_ else um(gwr_p$Pct_amostra, NA_real_),
      GWR_ganho_AICc   = if (vazio_gwr) NA_character_ else um(gwr_p$Ganho_AICc, NA_character_))
  })
  readr::write_csv(comp, file.path(dir_saida, "Resumo_Comparativo_Tres_Desfechos_V4.csv"))
  log_msg("resumo comparativo gravado em ", dir_saida)
}, "resumo comparativo")

log_bloco("FIM")
log_msg("log completo em: ", arquivo_log)
