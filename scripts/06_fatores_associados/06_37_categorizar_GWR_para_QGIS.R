# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final/Categorizar_GWR_para_QGIS.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final"))

# =============================================================================
# CATEGORIZAÇÃO DOS RESULTADOS DO GWR PARA MAPEAMENTO NO QGIS
# Projeto: Doença de Chagas nos municípios do Brasil — Parte 5.7
# Versão 2.0 — 03/10/2026
# -----------------------------------------------------------------------------
# O QUE ESTE SCRIPT FAZ
#
#   1. Lê a camada de resultados do GWR (.gpkg) de cada desfecho.
#   2. Detecta sozinho as variáveis do modelo local, a partir das colunas
#      gwr_<var>, gwr_<var>_SE e gwr_<var>_TV.
#   3. Monta o p-valor local de cada coeficiente: prioriza o CSV de p ajustados
#      do gwr.t.adjust (casando por código do município quando existir, senão
#      pela ordem das linhas); se não achar, calcula do t local e aplica a
#      correção escolhida (Bonferroni, BH ou BY).
#   4. Cria, para cada variável, cinco colunas prontas para a simbologia:
#
#        <var>__cls   classe do coeficiente local, todos os municípios
#        <var>__msk   a mesma classe, com "Não significativo" em branco
#                     -> É ESTE o mapa recomendado
#        <var>__psig  significância em 4 níveis, formato Oliveira et al.
#                     (Não significativo / p < 0,05 / p < 0,01 / p < 0,001)
#        <var>__sig   significância DIRECIONAL em 7 níveis (sinal + p)
#        <var>__tvc   classe do t local, formato Moroskoski et al.
#                     (< -1,96 / -1,96 a 0 / 0 a 1,96 / > 1,96)
#      mais <var>__p, o p-valor usado, para conferência e filtros no QGIS.
#
#   5. Categoriza também o R² local (marcando os negativos), o número de
#      condição local (corte visual em 30), os resíduos do modelo de referência
#      e a taxa do desfecho — as quatro figuras da seção 7.6 do Resumo.
#   6. Escreve um GPKG por desfecho, um .qml por mapa e, opcionalmente, grava
#      os estilos DENTRO do GPKG (tabela layer_styles), para aparecerem em
#      "Carregar estilo -> Do banco de dados" no QGIS.
#   7. Exporta a tabela de legendas (intervalos exatos, com n e %) e o resumo
#      de significância por variável, que alimenta a Tabela 6 do corpo.
#
# -----------------------------------------------------------------------------
# DE ONDE VEM CADA FORMATO (os artigos que você mandou)
#
#   * Oliveira et al. 2024, mortalidade materna (Ciênc. Saúde Coletiva) —
#     por variável, um mapa de coeficiente ao lado de um mapa de significância.
#     Coeficiente em 5 classes com uma faixa central estreita em torno de zero
#     (-0,01 a 0,01); significância em "Não significativo / p<0,05 / p<0,01 /
#     p<0,001". É o que CLASSE_ZERO/P_ZERO e a coluna __psig reproduzem.
#
#   * Moroskoski et al. 2025, feminicídio no Paraná (Ciênc. Saúde Coletiva) —
#     duas colunas de mapas: coeficiente e valor de t, com o t em
#     "< -1,96 / -1,96 a 0 / 0 a 1,96 / > 1,96". É a coluna __tvc, idêntica.
#     Lá o coeficiente é cortado em 5 classes ancoradas em zero SEM faixa
#     central: para esse formato, basta CLASSE_ZERO <- FALSE.
#     Atenção à cor: eles usam vermelho para negativo e azul para positivo,
#     o inverso da convenção aqui — se quiser igualar, INVERTER_DIVERGENTE.
#
#   * Sousa et al. 2022, tuberculose no Ceará (Rev. Bras. Epidemiol.) e
#     Anjos et al. 2020, Zika e chikungunya no Recife — mesma lógica de
#     painéis por variável.
#
#   * A classe "Não significativo" em branco (a coluna __msk) vai além do que
#     esses artigos fazem: eles colorem o coeficiente inteiro e deixam a
#     significância no mapa ao lado. É a escolha certa aqui. Com 4,5% a 9,4%
#     de municípios significativos na mortalidade e 0% a 5,6% na crônica
#     depois da correção, um mapa todo colorido seria lido como achado.
#     É também o que a seção 7.6 do seu Resumo recomenda — lá em cinza; aqui
#     o padrão é branco, e troca-se em COR_NAO_SIGNIF.
#
#   * As quebras são por QUANTIS, não por intervalos iguais nem quebras
#     naturais. Com coeficientes locais de -1.254 a +1.136 (spei na crônica)
#     ou de -21,3 a +356,3 (açaí na aguda), qualquer esquema de amplitude joga
#     99% dos municípios num balde só.
#
#   * Com QUEBRAS_SO_SIGNIF = TRUE, as classes do mapa mascarado saem só dos
#     municípios significativos, para que as poucas áreas coloridas tenham
#     contraste interno em vez de virarem um tom só.
#
# -----------------------------------------------------------------------------
# COMO USAR
#
#   1. Ajuste PASTA_BASE no bloco 0 (e, se precisar, os padrões de arquivo).
#   2. Rode o script inteiro. Ele não escreve nada na pasta de entrada.
#      Se não achar a camada de algum desfecho, ele LISTA no log os .gpkg que
#      existem na pasta, para você corrigir PADRAO_CAMADA.
#   3. No QGIS: abra Mapas_GWR_<Desfecho>.gpkg e, em
#      Propriedades da camada -> Simbologia -> Estilo -> Carregar estilo,
#      escolha "Do banco de dados" (se GRAVAR_ESTILOS_NO_GPKG = TRUE) ou
#      "Do arquivo", apontando para a pasta estilos/.
#
#   Exporte sempre em GPKG, nunca em SHP: os nomes de coluna passam de 10
#   caracteres e seriam truncados.
# =============================================================================


## ===========================================================================
## 0. CONFIGURAÇÃO
## ===========================================================================

# Pasta onde estão as saídas da Parte 5.7.
# Apontada para a pasta-mãe, e não para "GWR - Mapas", de propósito: a busca é
# recursiva, então assim ela acha tanto os .gpkg (que estão em GWR - Mapas)
# quanto os CSV de p ajustados / colinearidade / parâmetros, que ficam fora
# dela. Use barras normais, mesmo no Windows.
PASTA_BASE <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final")

# Pasta de saída (criada se não existir)
PASTA_SAIDA <- file.path(PASTA_BASE, "GWR - Mapas", "Mapas QGIS")

# Sufixos dos desfechos, como aparecem nos nomes dos arquivos
DESFECHOS <- c("Mortalidade", "Aguda", "Cronica")

# --- Localização dos arquivos (regex, busca recursiva a partir de PASTA_BASE) -
PADRAO_CAMADA  <- "Diagnostico_.*_GWR_e_SDM\\.gpkg$"  # geometria + colunas do GWR
PADRAO_P_AJUST <- "P_Ajustad.*\\.csv$"                # saída do gwr.t.adjust
PADRAO_COLLIN  <- "Colinearidade_Local.*\\.csv$"      # saída do gwr.collin.diagno
PADRAO_PARAMS  <- "Parametros_GWR_Consolidado.*\\.csv$"  # banda e ENP

# Se houver mais de uma especificação de GWR por desfecho (principal,
# alternativa, original, stepwise), ponha aqui o nome da que vai nos mapas.
# O script só considera arquivos cujo nome contenha este texto.
# Deixe NA para ignorar o filtro.
ESPECIFICACAO <- "principal"   # o .gpkg traz a especificacao principal
                               # (na aguda, a COM acai). Outras: "sem_acai",
                               # "stepwise", "alternativa", "original".
                               # Use NA para nao filtrar.

# Prefixo a REMOVER do nome da variável ao batizar as colunas de saída, se ele
# existir. A detecção em si não depende dele: o script procura qualquer coluna
# terminada em _TV cuja coluna-base exista. Deixe "" para não remover nada.
PREFIXO_GWR <- "gwr_"

# Colunas auxiliares por desfecho. ATENÇÃO: os três .gpkg carregam as TRÊS
# taxas e os TRÊS resíduos lado a lado (mort_, aguda_, cronica_), então deixar
# o script "detectar sozinho" faria ele mapear a taxa da mortalidade no mapa
# da aguda. Por isso o nome é explícito.
COL_TAXA_POR_DESFECHO <- c(
  Mortalidade = "mort_taxa_padronizada_suavizada",
  Aguda       = "aguda_taxa_padronizada_suavizada",
  Cronica     = "cronica_taxa_padronizada_suavizada")

# Resíduo do MODELO DE REFERÊNCIA de cada desfecho (Resumo, seção 1.1):
# mortalidade SAR-Queen, DC aguda SDM parcial-Queen, DC crônica SAR-Queen.
# Mapear residuos_sar_queen na aguda seria mapear o resíduo do modelo errado.
COL_RESID_POR_DESFECHO <- c(
  Mortalidade = "residuos_sar_queen",
  Aguda       = "residuos_sdm_queen",
  Cronica     = "residuos_sar_queen")

COL_TAXA  <- NA           # força uma coluna de taxa para todos os desfechos
COL_RESID <- NA           # idem para o resíduo
COL_ID    <- NA           # código do município, para casar camada x CSV de p

# Variáveis que o Resumo diz para NÃO mapear como coeficiente (seção 7.6).
# O script continua gerando as colunas, mas marca o mapa no log, no LEIA-ME
# e na coluna "nao_mapear" da tabela de legendas.
VARIAVEIS_NAO_MAPEAR <- c("pct_area_acai",          # aguda: |coef|>10 em 906 mun., todos com açaí = 0
                          "spei_media_2001_2024")   # crônica: 0% signif.; amplitude -1.254 a +1.136

# --- Critério de significância -----------------------------------------------
# "arquivo" = usa o CSV de p ajustados do gwr.t.adjust (recomendado)
# "bonf" / "bh" / "by" = calcula do t local e aplica a correção aqui
# "bruto"  = sem correção (só para a tabela de comparação do apêndice)
CRITERIO_P <- "arquivo"

# Qual coluna procurar no CSV de p ajustados, em ordem de preferência:
# "fb" Fotheringham-Byrne, "bonf" Bonferroni, "by" Benjamini-Yekutieli,
# "bh" Benjamini-Hochberg, "bruto" sem correção
PREFERENCIA_P_ARQUIVO <- c("fb", "bonf", "by", "bh")

# Se o arquivo de p ajustados não for encontrado, cai para este método
FALLBACK_P <- "bonf"

# Nível usado na máscara (acima disso -> "Não significativo")
ALFA <- 0.05

# Cortes dos mapas de significância (__psig e __sig)
CORTES_P <- c(0.05, 0.01, 0.001)

# --- Classes do coeficiente ---------------------------------------------------
N_CLASSES_LADO    <- 2      # 2 por lado + classe central = 5 classes
CLASSE_ZERO       <- TRUE   # faixa central "próxima de zero" (formato Oliveira)
P_ZERO            <- 0.10   # a faixa central cobre os 10% menores |coeficiente|
QUEBRAS_SO_SIGNIF <- TRUE   # no mapa mascarado, classes só sobre os significativos

# --- Paletas ------------------------------------------------------------------
COR_NAO_SIGNIF <- "#FFFFFF"   # branco. O Resumo sugere cinza: use "#E6E6E6"
COR_SEM_DADO   <- "#D9D9D9"
COR_CONTORNO   <- "#595959"
LARG_CONTORNO  <- 0.06        # mm

# Extremos da rampa divergente. Padrão ColorBrewer RdBu: negativo frio (azul),
# positivo quente (vermelho). TRUE inverte, como em Moroskoski et al.
RAMPA_NEGATIVA      <- c("#D1E5F0", "#2166AC")
RAMPA_POSITIVA      <- c("#FDDBC7", "#B2182B")
COR_CLASSE_ZERO     <- "#F7F7F7"
INVERTER_DIVERGENTE <- FALSE

# Significância não direcional, 4 níveis (formato Oliveira et al.)
PALETA_PSIG <- c(COR_NAO_SIGNIF, "#C6DBEF", "#4292C6", "#08519C")

# Significância direcional, 7 níveis
PALETA_SIG <- c("#2166AC", "#67A9CF", "#D1E5F0",
                COR_NAO_SIGNIF,
                "#FDDBC7", "#EF8A62", "#B2182B")

# Classe de t, 4 níveis (formato Moroskoski et al.)
PALETA_TV <- c("#2166AC", "#D1E5F0", "#FDDBC7", "#B2182B")

# R² local (claro -> escuro) e cor da classe negativa
PALETA_R2       <- c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B")
COR_R2_NEGATIVO <- "#D7301F"

# Número de condição local: frio até 30, quente acima (corte do Resumo)
CORTES_CN <- c(10, 20, 30, 50)
PALETA_CN <- c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C")

# Taxa do desfecho
PALETA_TAXA <- c("#FFF5EB", "#FDD0A2", "#FD8D3C", "#D94801", "#7F2704")

# --- Saídas --------------------------------------------------------------------
GRAVAR_ESTILOS_NO_GPKG <- TRUE   # grava os .qml na tabela layer_styles do GPKG
GERAR_PREVIA           <- TRUE   # PNGs rápidos para conferir as classes
PREVIA_APENAS          <- "__msk$|R2|CN_local|Residuo|Taxa"  # filtro das prévias
SO_COLUNAS_MAPA        <- FALSE  # TRUE descarta as colunas originais não usadas
QML_FORMATO_LEGADO     <- FALSE  # TRUE usa <prop k= v=> (QGIS 3.0 a 3.8)
VERSAO_QGIS_QML        <- "3.34.0"


## ===========================================================================
## 1. UTILITÁRIOS
## ===========================================================================

suppressPackageStartupMessages(library(sf))

LOG_LINHAS <- character(0)

msg <- function(...) {
  linha <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste0(..., collapse = ""))
  LOG_LINHAS <<- c(LOG_LINHAS, linha)
  cat(linha, "\n", sep = "")
  invisible(NULL)
}

aviso <- function(...) msg("!! ATENCAO: ", ...)

criar_pasta <- function(p) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

# Remove acentos sem depender do locale nem da codificação do arquivo.
# Duas armadilhas que este bloco evita, as duas silenciosas:
#   * iconv(to = "ASCII//TRANSLIT") devolve NA para QUALQUER caractere
#     não-ASCII quando o locale é C — todos os .qml viravam "NA.qml" e a busca
#     por "Crônica" não casava com arquivo nenhum;
#   * chartr() conta bytes, não caracteres, num locale de byte único, então um
#     par de strings acentuadas do mesmo tamanho dispara "'old' is longer
#     than 'new'".
# Por isso: escapes \u explícitos (independentes da codificação do .R) e
# substituição byte a byte, que é segura porque UTF-8 é autossincronizável.
ACENTOS <- c(
  "á" = "a", "à" = "a", "â" = "a", "ã" = "a",
  "ä" = "a", "å" = "a",
  "Á" = "A", "À" = "A", "Â" = "A", "Ã" = "A",
  "Ä" = "A", "Å" = "A",
  "é" = "e", "è" = "e", "ê" = "e", "ë" = "e",
  "É" = "E", "È" = "E", "Ê" = "E", "Ë" = "E",
  "í" = "i", "ì" = "i", "î" = "i", "ï" = "i",
  "Í" = "I", "Ì" = "I", "Î" = "I", "Ï" = "I",
  "ó" = "o", "ò" = "o", "ô" = "o", "õ" = "o",
  "ö" = "o",
  "Ó" = "O", "Ò" = "O", "Ô" = "O", "Õ" = "O",
  "Ö" = "O",
  "ú" = "u", "ù" = "u", "û" = "u", "ü" = "u",
  "Ú" = "U", "Ù" = "U", "Û" = "U", "Ü" = "U",
  "ç" = "c", "Ç" = "C", "ñ" = "n", "Ñ" = "N",
  "ý" = "y", "ÿ" = "y", "Ý" = "Y",
  "‐" = "-", "‑" = "-", "‒" = "-", "–" = "-",
  "—" = "-", "―" = "-", "−" = "-",
  "º" = "o", "ª" = "a", "²" = "2", "³" = "3")

sem_acento <- function(x) {
  y <- as.character(x)
  Encoding(y) <- "UTF-8"          # NÃO usar enc2utf8(): num locale C ele
                                  # converte "ã" no texto literal "<c3><a3>"
  for (k in names(ACENTOS)) {
    y <- gsub(k, ACENTOS[[k]], y, fixed = TRUE, useBytes = TRUE)
  }
  y <- gsub("[^ -~]", "_", y, useBytes = TRUE)   # o que sobrou de não-ASCII
  y[is.na(y)] <- ""
  y
}

nome_arquivo <- function(x) {
  y <- gsub("[^A-Za-z0-9_.-]+", "_", sem_acento(x))
  y <- gsub("_+", "_", y)
  y <- gsub("^_|_$", "", y)
  y[!nzchar(y)] <- "mapa"
  y
}

# --- Escrita de texto sempre em UTF-8, sem depender do locale -----------------

escrever_texto <- function(linhas, arq, bom = FALSE) {
  x <- as.character(linhas)
  # Declarar a codificação e gravar os bytes como estão. enc2utf8() aqui
  # corromperia os acentos num locale C ("Não" -> "N<c3><a3>o"), o que deixa
  # o .qml inválido como XML e o QGIS recusa o estilo inteiro.
  Encoding(x) <- "UTF-8"
  con <- file(arq, open = "wb")
  on.exit(close(con))
  if (bom) writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeLines(x, con, useBytes = TRUE)
  invisible(arq)
}

# CSV com ";" e vírgula decimal, em UTF-8 com BOM para o Excel ler os acentos
escrever_csv <- function(x, arq) {
  linhas <- utils::capture.output(
    utils::write.csv2(x, file = stdout(), row.names = FALSE, na = ""))
  escrever_texto(linhas, arq, bom = TRUE)
}

# Lista arquivos da pasta de ENTRADA, ignorando o que o próprio script já
# gravou na pasta de saída (senão uma segunda rodada se lê a si mesma).
listar_entrada <- function(pasta, padrao) {
  arqs <- list.files(pasta, pattern = padrao, recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  if (!length(arqs)) return(arqs)
  fora <- normalizePath(PASTA_SAIDA, winslash = "/", mustWork = FALSE)
  arqs[!startsWith(normalizePath(arqs, winslash = "/", mustWork = FALSE), fora)]
}

localizar <- function(pasta, padrao, sufixo) {
  arqs <- listar_entrada(pasta, padrao)
  if (!length(arqs)) return(NA_character_)
  bn   <- tolower(sem_acento(basename(arqs)))
  alvo <- tolower(sem_acento(sufixo))
  arqs <- arqs[grepl(alvo, bn, fixed = TRUE)]
  if (!is.na(ESPECIFICACAO) && length(arqs)) {
    esp  <- tolower(sem_acento(ESPECIFICACAO))
    keep <- grepl(esp, tolower(sem_acento(basename(arqs))), fixed = TRUE)
    if (any(keep)) arqs <- arqs[keep]
  }
  if (!length(arqs)) return(NA_character_)
  if (length(arqs) > 1) {
    aviso(length(arqs), " arquivos casam '", padrao, "' para ", sufixo,
          "; usando o mais recente: ",
          basename(arqs[which.max(file.mtime(arqs))]),
          " (use ESPECIFICACAO para escolher)")
  }
  arqs[which.max(file.mtime(arqs))]
}

ler_csv_flex <- function(arq) {
  tenta <- function(f) tryCatch(f(arq, stringsAsFactors = FALSE,
                                  check.names = FALSE),
                                error = function(e) NULL,
                                warning = function(w) NULL)
  a <- tenta(utils::read.csv)
  b <- tenta(utils::read.csv2)
  na_ <- if (is.null(a)) 0L else ncol(a)
  nb_ <- if (is.null(b)) 0L else ncol(b)
  if (max(na_, nb_) == 0L) return(NULL)
  if (nb_ > na_) b else a
}

# --- Formatação numérica em padrão brasileiro ---------------------------------

# Casas decimais a partir da LARGURA das classes, não do menor valor: é o que
# dá legenda legível ("0,00 a 0,19") em vez de "0,00027 a 0,18661".
digitos_auto <- function(br) {
  w <- diff(br)
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(2L)
  min(6L, max(0L, as.integer(ceiling(-log10(min(w)))) + 1L))
}

fmt_br <- function(x, dig = 2L) {
  s <- trimws(formatC(x, format = "f", digits = dig,
                      big.mark = ".", decimal.mark = ","))
  sub("^-(0(,0+)?)$", "\\1", s)          # evita "-0,00"
}

# --- Quebras divergentes ancoradas em zero ------------------------------------

quebras_divergentes <- function(x, n_lado = 2L, classe_zero = TRUE, p_zero = 0.10) {
  x <- x[is.finite(x)]
  if (length(x) < 5L || length(unique(x)) < 3L) return(NULL)

  z <- 0
  if (classe_zero) {
    z <- as.numeric(stats::quantile(abs(x), p_zero, na.rm = TRUE))
    if (!is.finite(z) || z <= 0) z <- 0
  }

  neg <- x[x < -z]
  pos <- x[x >  z]
  br  <- numeric(0)

  if (length(unique(neg)) >= 2L) {
    k <- min(n_lado, length(unique(neg)))
    q <- as.numeric(stats::quantile(neg, probs = seq(0, 1, length.out = k + 1L)))
    q[length(q)] <- -z
    br <- c(br, q)
  } else if (length(neg)) {
    br <- c(br, min(neg), -z)
  }

  if (z > 0) br <- c(br, if (length(neg)) -z, if (length(pos)) z)

  if (length(unique(pos)) >= 2L) {
    k <- min(n_lado, length(unique(pos)))
    q <- as.numeric(stats::quantile(pos, probs = seq(0, 1, length.out = k + 1L)))
    q[1] <- z
    br <- c(br, q)
  } else if (length(pos)) {
    br <- c(br, z, max(pos))
  }

  br <- sort(unique(br))
  if (length(br) < 2L) return(NULL)
  br[1]          <- min(br[1], min(x))
  br[length(br)] <- max(br[length(br)], max(x))
  br
}

rotulos_intervalo <- function(br) {
  d0 <- digitos_auto(br)
  for (d in seq(d0, 8L)) {
    r <- paste0(fmt_br(br[-length(br)], d), " a ", fmt_br(br[-1], d))
    if (!anyDuplicated(r)) return(r)
  }
  paste0(seq_len(length(br) - 1L), ") ",
         fmt_br(br[-length(br)], 8L), " a ", fmt_br(br[-1], 8L))
}

classificar <- function(x, br, rotulos = NULL) {
  if (is.null(br) || length(br) < 2L) {
    return(list(valor = rep(NA_character_, length(x)),
                rotulos = character(0), quebras = NULL))
  }
  if (is.null(rotulos)) rotulos <- rotulos_intervalo(br)
  eps <- max(diff(range(br)) * 1e-9, .Machine$double.eps)
  brc <- br
  brc[1] <- brc[1] - eps
  brc[length(brc)] <- brc[length(brc)] + eps
  idx <- cut(x, breaks = brc, labels = FALSE, include.lowest = TRUE, right = TRUE)
  out <- rep(NA_character_, length(x))
  ok  <- !is.na(idx)
  out[ok] <- rotulos[idx[ok]]
  list(valor = out, rotulos = rotulos, quebras = br)
}

interpolar <- function(paleta, n) {
  if (n <= 0) return(character(0))
  if (n == length(paleta)) return(paleta)
  grDevices::colorRampPalette(paleta)(n)
}

# Cores coerentes com o sinal: negativas de um lado, positivas do outro, e a
# classe que contém o zero em cinza claro.
paleta_para_quebras <- function(br) {
  n     <- length(br) - 1L
  inf   <- br[-length(br)]; sup <- br[-1]
  cores <- rep(COR_CLASSE_ZERO, n)
  i_zer <- which(inf < 0 & sup > 0)
  i_neg <- setdiff(which(sup <= 0), i_zer)
  i_pos <- setdiff(which(inf >= 0), i_zer)
  rneg <- if (INVERTER_DIVERGENTE) RAMPA_POSITIVA else RAMPA_NEGATIVA
  rpos <- if (INVERTER_DIVERGENTE) RAMPA_NEGATIVA else RAMPA_POSITIVA
  if (length(i_neg)) cores[i_neg] <- rev(interpolar(rneg, length(i_neg)))
  if (length(i_pos)) cores[i_pos] <- interpolar(rpos, length(i_pos))
  cores
}

# Valores, rótulos e cores, já sem as classes vazias (que poluiriam a legenda)
construir_mapa <- function(valor, rotulos, cores, lim_inf = NULL, lim_sup = NULL) {
  cont <- vapply(rotulos, function(r) sum(valor == r, na.rm = TRUE), integer(1))
  k <- cont > 0L
  if (!any(k)) k <- rep(TRUE, length(rotulos))
  list(valor   = valor,
       rotulos = unname(rotulos[k]),
       cores   = unname(cores[k]),
       n       = unname(cont[k]),
       lim_inf = if (is.null(lim_inf)) rep(NA_real_, sum(k)) else unname(lim_inf[k]),
       lim_sup = if (is.null(lim_sup)) rep(NA_real_, sum(k)) else unname(lim_sup[k]))
}


## ===========================================================================
## 2. GERAÇÃO DE ESTILOS QML
## ===========================================================================

esc_xml <- function(x) {
  x <- gsub("&", "&amp;",  x, fixed = TRUE)
  x <- gsub("<", "&lt;",   x, fixed = TRUE)
  x <- gsub(">", "&gt;",   x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&apos;", x, fixed = TRUE)
}

hex_rgba <- function(hex, alpha = 255) {
  m <- grDevices::col2rgb(hex)
  paste(m[1, 1], m[2, 1], m[3, 1], alpha, sep = ",")
}

simbolo_fill <- function(idx, cor, contorno = COR_CONTORNO, larg = LARG_CONTORNO) {
  props <- c(
    color              = hex_rgba(cor),
    joinstyle          = "bevel",
    offset             = "0,0",
    offset_unit        = "MM",
    outline_color      = hex_rgba(contorno),
    outline_style      = "solid",
    outline_width      = formatC(larg, format = "f", digits = 2),
    outline_width_unit = "MM",
    style              = "solid"
  )
  corpo <- if (QML_FORMATO_LEGADO) {
    paste0('          <prop k="', names(props), '" v="', props, '"/>',
           collapse = "\n")
  } else {
    paste0('          <Option type="Map">\n',
           paste0('            <Option name="', names(props),
                  '" type="QString" value="', props, '"/>', collapse = "\n"),
           '\n          </Option>')
  }
  paste0('      <symbol type="fill" name="', idx,
         '" alpha="1" clip_to_extent="1" force_rhr="0">\n',
         '        <layer class="SimpleFill" locked="0" pass="0" enabled="1">\n',
         corpo, '\n',
         '        </layer>\n',
         '      </symbol>')
}

qml_categorizado <- function(campo, rotulos, cores,
                             rotulo_sem_dado = "Sem dado",
                             cor_sem_dado = COR_SEM_DADO) {
  stopifnot(length(rotulos) == length(cores), length(rotulos) > 0)
  n <- length(rotulos)
  simbolos <- c(vapply(seq_len(n), function(i) simbolo_fill(i - 1L, cores[i]),
                       character(1)),
                simbolo_fill(n, cor_sem_dado))
  cats <- c(vapply(seq_len(n), function(i) sprintf(
              '      <category render="true" symbol="%d" value="%s" label="%s"/>',
              i - 1L, esc_xml(rotulos[i]), esc_xml(rotulos[i])), character(1)),
            sprintf('      <category render="true" symbol="%d" value="" label="%s"/>',
                    n, esc_xml(rotulo_sem_dado)))

  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    "<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>\n",
    '<qgis version="', VERSAO_QGIS_QML, '" styleCategories="Symbology">\n',
    '  <renderer-v2 type="categorizedSymbol" attr="', esc_xml(campo),
    '" forceraster="0" symbollevels="0" enableorderby="0">\n',
    '    <categories>\n', paste(cats, collapse = "\n"), '\n    </categories>\n',
    '    <symbols>\n', paste(simbolos, collapse = "\n"), '\n    </symbols>\n',
    '    <source-symbol>\n', simbolo_fill(0L, cor_sem_dado),
    '\n    </source-symbol>\n',
    '  </renderer-v2>\n',
    '  <blendMode>0</blendMode>\n',
    '  <featureBlendMode>0</featureBlendMode>\n',
    '  <layerGeometryType>2</layerGeometryType>\n',
    '</qgis>\n')
}

# Grava o estilo dentro do GPKG (tabela layer_styles), para o QGIS oferecer em
# "Carregar estilo -> Do banco de dados".
gravar_estilo_gpkg <- function(gpkg, camada, col_geom, nome_estilo, qml,
                               descricao = "") {
  if (!requireNamespace("RSQLite", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) return(FALSE)
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), gpkg)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    # Nome real da coluna de geometria: o GDAL costuma gravar "geom", mas pode
    # ser "geometry" conforme a camada de origem. Perguntar evita um estilo
    # registrado para uma coluna que não existe.
    gc <- try(DBI::dbGetQuery(con,
      "SELECT column_name FROM gpkg_geometry_columns WHERE table_name = ?",
      params = list(camada)), silent = TRUE)
    if (!inherits(gc, "try-error") && NROW(gc) > 0) col_geom <- gc[[1]][1]
    DBI::dbExecute(con, paste(
      "CREATE TABLE IF NOT EXISTS layer_styles (",
      "id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "f_table_catalog TEXT, f_table_schema TEXT, f_table_name TEXT,",
      "f_geometry_column TEXT, styleName TEXT, styleQML TEXT, styleSLD TEXT,",
      "useAsDefault BOOLEAN, description TEXT, owner TEXT, ui TEXT,",
      "update_time DATETIME DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ','now')))"))
    DBI::dbExecute(con,
      "DELETE FROM layer_styles WHERE f_table_name = ? AND styleName = ?",
      params = list(camada, nome_estilo))
    DBI::dbExecute(con, paste(
      "INSERT INTO layer_styles",
      "(f_table_catalog, f_table_schema, f_table_name, f_geometry_column,",
      " styleName, styleQML, styleSLD, useAsDefault, description, owner, ui)",
      "VALUES ('', '', ?, ?, ?, ?, NULL, 0, ?, '', NULL)"),
      params = list(camada, col_geom, nome_estilo, qml, descricao))
    ja <- DBI::dbGetQuery(con, "SELECT table_name FROM gpkg_contents")$table_name
    if (!"layer_styles" %in% ja) {
      DBI::dbExecute(con, paste(
        "INSERT INTO gpkg_contents (table_name, data_type, identifier, description)",
        "VALUES ('layer_styles', 'attributes', 'layer_styles', 'Estilos QGIS')"))
    }
    TRUE
  }, error = function(e) { aviso("layer_styles: ", conditionMessage(e)); FALSE })
}


## ===========================================================================
## 3. DETECÇÃO DE COLUNAS E CÁLCULO DOS p
## ===========================================================================

# Acha os pares (coeficiente, t) sem depender de um prefixo fixo: procura toda
# coluna terminada em _TV cuja coluna-base exista. Assim funciona tanto se o
# SDF foi gravado como "gwr_pct_negros"/"gwr_pct_negros_TV" quanto como
# "pct_negros"/"pct_negros_TV". Devolve o nome limpo (que vira <var>__msk etc.)
# junto com os nomes reais das colunas de origem.
detectar_variaveis <- function(nomes, prefixo = PREFIXO_GWR) {
  tvs <- grep("_TV$", nomes, value = TRUE)
  if (!length(tvs)) return(NULL)
  base <- sub("_TV$", "", tvs)
  ok   <- base %in% nomes
  if (any(!ok)) {
    aviso("coluna _TV sem o coeficiente correspondente: ",
          paste(utils::head(base[!ok], 8), collapse = ", "))
  }
  base <- base[ok]; tvs <- tvs[ok]
  if (!length(base)) return(NULL)
  limpo <- if (nzchar(prefixo))
    sub(paste0("^", prefixo), "", base, ignore.case = TRUE) else base
  limpo[!nzchar(limpo)] <- base[!nzchar(limpo)]
  if (anyDuplicated(limpo)) limpo <- base      # o prefixo não era redundante
  data.frame(var = limpo, col_b = base, col_tv = tvs, stringsAsFactors = FALSE)
}

detectar_coluna <- function(nomes, padroes) {
  nn <- tolower(nomes)
  for (p in padroes) {
    hit <- which(grepl(p, nn))
    if (length(hit)) return(nomes[hit[1]])
  }
  NA_character_
}

# CD_MUN e chave_ibge vêm como TEXTO no .gpkg; codigo_municipio vem como REAL
# e casa pior num join. Por isso os de texto primeiro.
PADROES_ID <- c("^cd_?mun$", "^chave_ibge$", "^cod.*ibge", "^cd_?mun",
                "^geocod", "^codigo_?mun", "^cod_?mun", "^ibge", "^id_?mun")

MARCADORES_P <- list(
  fb    = c("p_fb", "_fb", "fotheringham", "byrne", "silva"),
  bonf  = c("p_bo", "bonf", "_bo"),
  by    = c("p_by", "yekutieli", "_by"),
  bh    = c("p_bh", "hochberg", "fdr", "_bh"),
  bruto = c("p_value", "p_bruto", "pvalor", "p_valor", "pval")
)

# Procura a coluna de p da variável v. Primeiro exige que o nome COMECE com a
# variável (evita que "pct_negros" case com "pct_negros_2022"), depois afrouxa.
achar_col_p <- function(nomes_csv, var, chave) {
  nn <- tolower(nomes_csv)
  vv <- tolower(var)
  for (cand in list(which(startsWith(nn, vv)),
                    which(grepl(vv, nn, fixed = TRUE)))) {
    if (!length(cand)) next
    for (m in MARCADORES_P[[chave]]) {
      hit <- cand[grepl(m, nn[cand], fixed = TRUE)]
      if (length(hit)) {
        if (length(hit) > 1) {
          aviso("mais de uma coluna de p casa '", var, "' + '", m,
                "'; usando ", nomes_csv[hit[1]])
        }
        return(nomes_csv[hit[1]])
      }
    }
  }
  NA_character_
}

p_de_tv <- function(tv, gl = Inf) {
  if (is.finite(gl) && gl > 1) 2 * stats::pt(-abs(tv), df = gl)
  else 2 * stats::pnorm(-abs(tv))
}

ajustar_p <- function(p, metodo) {
  m <- sum(is.finite(p))
  switch(metodo,
    bruto = p,
    bonf  = pmin(1, p * m),
    bh    = stats::p.adjust(p, method = "BH"),
    by    = stats::p.adjust(p, method = "BY"),
    stop("metodo de ajuste desconhecido: ", metodo))
}

montar_p <- function(dados, tabv, arq_p, gl) {
  vars <- tabv$var
  n <- nrow(dados)
  P <- matrix(NA_real_, nrow = n, ncol = length(vars),
              dimnames = list(NULL, vars))
  origem <- stats::setNames(rep(NA_character_, length(vars)), vars)

  tabp <- NULL
  if (identical(CRITERIO_P, "arquivo") && !is.na(arq_p) && file.exists(arq_p)) {
    tabp <- ler_csv_flex(arq_p)
    if (!is.null(tabp) && nrow(tabp) != n) {
      aviso("CSV de p ajustados tem ", nrow(tabp), " linhas e a camada tem ", n,
            " - arquivo ignorado; o p sera calculado pelo t local")
      tabp <- NULL
    }
    # Casa por codigo do municipio quando os dois lados tiverem um
    if (!is.null(tabp)) {
      id_cam <- if (!is.na(COL_ID)) COL_ID else detectar_coluna(names(dados), PADROES_ID)
      id_csv <- detectar_coluna(names(tabp), PADROES_ID)
      if (!is.na(id_cam) && !is.na(id_csv)) {
        a <- as.character(dados[[id_cam]]); b <- as.character(tabp[[id_csv]])
        if (!anyDuplicated(b) && all(a %in% b)) {
          tabp <- tabp[match(a, b), , drop = FALSE]
          msg("p ajustado: casado por ", id_csv, " x ", id_cam)
        } else {
          aviso("codigos do CSV de p nao batem com os da camada - usando a ORDEM das linhas")
        }
      } else {
        msg("p ajustado: sem coluna de codigo - casado pela ORDEM das linhas")
      }
    }
  }

  for (i in seq_along(vars)) {
    v  <- vars[i]
    tv <- as.numeric(dados[[tabv$col_tv[i]]])
    if (!is.null(tabp)) {
      for (chave in PREFERENCIA_P_ARQUIVO) {
        col <- achar_col_p(names(tabp), v, chave)
        if (!is.na(col)) {
          P[, v] <- suppressWarnings(as.numeric(tabp[[col]]))
          origem[v] <- paste0("arquivo:", chave, " (", col, ")")
          break
        }
      }
    }
    if (all(is.na(P[, v]))) {
      metodo <- if (CRITERIO_P %in% c("bonf", "bh", "by", "bruto")) CRITERIO_P
                else FALLBACK_P
      P[, v] <- ajustar_p(p_de_tv(tv, gl), metodo)
      origem[v] <- paste0("calculado:", metodo,
                          if (is.finite(gl)) paste0(" (gl=", round(gl), ")")
                          else " (normal)")
    }
  }
  list(P = P, origem = origem)
}

# --- Significância ------------------------------------------------------------

NIVEIS_PSIG <- c("Não significativo", "p < 0,05", "p < 0,01", "p < 0,001")

sig_simples <- function(p, cortes = CORTES_P) {
  out <- rep(NA_character_, length(p))
  ok  <- is.finite(p)
  out[ok]                   <- NIVEIS_PSIG[1]
  out[ok & p < cortes[1]]   <- NIVEIS_PSIG[2]
  out[ok & p < cortes[2]]   <- NIVEIS_PSIG[3]
  out[ok & p < cortes[3]]   <- NIVEIS_PSIG[4]
  out
}

NIVEIS_SIG <- c("Negativo (p < 0,001)", "Negativo (p < 0,01)",
                "Negativo (p < 0,05)", "Não significativo",
                "Positivo (p < 0,05)", "Positivo (p < 0,01)",
                "Positivo (p < 0,001)")

sig_direcional <- function(b, p, cortes = CORTES_P) {
  out <- rep("Não significativo", length(b))
  neg <- is.finite(b) & b < 0 & is.finite(p)
  pos <- is.finite(b) & b > 0 & is.finite(p)
  out[neg & p < cortes[1]] <- NIVEIS_SIG[3]
  out[neg & p < cortes[2]] <- NIVEIS_SIG[2]
  out[neg & p < cortes[3]] <- NIVEIS_SIG[1]
  out[pos & p < cortes[1]] <- NIVEIS_SIG[5]
  out[pos & p < cortes[2]] <- NIVEIS_SIG[6]
  out[pos & p < cortes[3]] <- NIVEIS_SIG[7]
  out[!is.finite(b) | !is.finite(p)] <- NA_character_
  out
}

NIVEIS_TV <- c("< -1,96", "-1,96 a 0", "0 a 1,96", "> 1,96")

classe_tv <- function(tv) {
  out <- rep(NA_character_, length(tv))
  ok  <- is.finite(tv)
  out[ok & tv <  -1.96]              <- NIVEIS_TV[1]
  out[ok & tv >= -1.96 & tv <  0]    <- NIVEIS_TV[2]
  out[ok & tv >=  0    & tv <= 1.96] <- NIVEIS_TV[3]
  out[ok & tv >   1.96]              <- NIVEIS_TV[4]
  out
}


## ===========================================================================
## 4. PROCESSAMENTO DE UM DESFECHO
## ===========================================================================

processar_desfecho <- function(sufixo) {

  msg(strrep("-", 72))
  msg("DESFECHO: ", sufixo)

  arq_camada <- localizar(PASTA_BASE, PADRAO_CAMADA,  sufixo)
  arq_p      <- localizar(PASTA_BASE, PADRAO_P_AJUST, sufixo)
  arq_collin <- localizar(PASTA_BASE, PADRAO_COLLIN,  sufixo)
  arq_params <- localizar(PASTA_BASE, PADRAO_PARAMS,  sufixo)

  if (is.na(arq_camada)) {
    aviso("camada nao encontrada para ", sufixo, " - desfecho pulado")
    todos <- basename(listar_entrada(PASTA_BASE, "\\.gpkg$"))
    if (length(todos)) {
      msg("   .gpkg existentes na pasta (ajuste PADRAO_CAMADA):")
      for (a in utils::head(todos, 25)) msg("     - ", a)
    } else {
      msg("   nenhum .gpkg encontrado em '", PASTA_BASE, "' - confira PASTA_BASE")
    }
    return(NULL)
  }
  msg("camada    : ", basename(arq_camada))
  msg("p ajustado: ", if (is.na(arq_p)) "(NAO ENCONTRADO)" else basename(arq_p))
  msg("colinearid: ", if (is.na(arq_collin)) "(nao encontrado)" else basename(arq_collin))
  msg("parametros: ", if (is.na(arq_params)) "(nao encontrado)" else basename(arq_params))
  if (is.na(arq_p)) {
    cands <- basename(listar_entrada(PASTA_BASE, "\\.csv$"))
    cands <- cands[grepl(tolower(sem_acento(sufixo)),
                         tolower(sem_acento(cands)), fixed = TRUE)]
    if (length(cands)) {
      msg("   sem o CSV de p ajustados, a significancia sai do t local + ",
          FALLBACK_P, ". CSVs deste desfecho na pasta (ajuste PADRAO_P_AJUST):")
      for (a in utils::head(cands, 20)) msg("     - ", a)
    }
  }

  g <- sf::st_read(arq_camada, quiet = TRUE)
  n <- nrow(g)
  col_geom <- attr(g, "sf_column")
  msg("feicoes   : ", n)

  nomes <- names(g)
  tabv  <- detectar_variaveis(nomes)
  if (is.null(tabv) || !nrow(tabv)) {
    aviso("nenhuma coluna terminada em _TV encontrada - desfecho pulado")
    msg("   colunas da camada (para ajustar a deteccao):")
    for (bloco in split(nomes, ceiling(seq_along(nomes) / 6))) {
      msg("     ", paste(bloco, collapse = " | "))
    }
    return(NULL)
  }
  vars <- tabv$var
  msg("variaveis : ", paste(vars, collapse = ", "))
  msg("            lidas de ", tabv$col_b[1], " / ", tabv$col_tv[1], " (e analogas)")

  # ENP, para os graus de liberdade do p bruto
  enp <- NA_real_
  if (!is.na(arq_params) && file.exists(arq_params)) {
    tp <- ler_csv_flex(arq_params)
    if (!is.null(tp) && nrow(tp)) {
      # O Parametros_GWR_Consolidado_* traz UMA LINHA POR ESPECIFICAÇÃO
      # (principal, alternativa, original, stepwise, sem_acai). Ler a primeira
      # linha às cegas pegaria o ENP da especificação errada.
      lin <- 1L
      if (nrow(tp) > 1L && !is.na(ESPECIFICACAO)) {
        col_esp <- detectar_coluna(names(tp),
                     c("especific", "^modelo$", "modelo", "spec", "^nome$"))
        if (!is.na(col_esp)) {
          hit <- which(grepl(tolower(sem_acento(ESPECIFICACAO)),
                             tolower(sem_acento(as.character(tp[[col_esp]]))),
                             fixed = TRUE))
          if (length(hit)) {
            lin <- hit[1]
            msg("parametros: linha '", tp[[col_esp]][lin], "'")
          } else {
            aviso("'", ESPECIFICACAO, "' nao achada em ", basename(arq_params),
                  " - lendo a 1a linha")
          }
        }
      }
      col_enp <- detectar_coluna(names(tp), c("^enp$", "enp", "effective.*param"))
      if (!is.na(col_enp)) enp <- suppressWarnings(as.numeric(tp[[col_enp]][lin]))
      col_bw <- detectar_coluna(names(tp), c("^banda$", "banda", "bandwidth", "^bw$"))
      if (!is.na(col_bw)) msg("banda     : ", tp[[col_bw]][lin], " vizinhos")
    }
  }
  gl <- if (is.finite(enp) && enp < n - 2) n - enp else Inf
  msg("ENP       : ", if (is.finite(enp)) round(enp, 1) else "(nao lido)",
      " | gl do p bruto: ", if (is.finite(gl)) round(gl) else "normal")

  pp     <- montar_p(g, tabv, arq_p, gl)
  P      <- pp$P
  origem <- pp$origem

  legenda <- list()
  estilos <- list()
  resumo  <- list()

  registrar <- function(nome, campo, mapa, descricao, tipo, nao_mapear = FALSE) {
    estilos[[nome]] <<- list(campo = campo, rotulos = mapa$rotulos,
                             cores = mapa$cores, descricao = descricao)
    legenda[[length(legenda) + 1L]] <<- data.frame(
      desfecho = sufixo, mapa = tipo, coluna = campo,
      ordem = seq_along(mapa$rotulos), classe = mapa$rotulos,
      limite_inferior = mapa$lim_inf, limite_superior = mapa$lim_sup,
      n = mapa$n, pct = round(100 * mapa$n / n, 2),
      cor = mapa$cores, nao_mapear = nao_mapear,
      stringsAsFactors = FALSE)
  }

  ## -------------------------------------------------- uma variável por vez
  for (i in seq_along(vars)) {
    v   <- vars[i]
    b   <- as.numeric(g[[tabv$col_b[i]]])
    tv  <- as.numeric(g[[tabv$col_tv[i]]])
    p   <- P[, v]
    sig <- is.finite(p) & p < ALFA
    bloq <- v %in% VARIAVEIS_NAO_MAPEAR

    g[[paste0(v, "__p")]] <- p

    # 1. classe do coeficiente, todos os municípios
    br <- quebras_divergentes(b, N_CLASSES_LADO, CLASSE_ZERO, P_ZERO)
    cl <- classificar(b, br)
    if (length(cl$rotulos)) {
      col_cls <- paste0(v, "__cls")
      g[[col_cls]] <- cl$valor
      m <- construir_mapa(cl$valor, cl$rotulos, paleta_para_quebras(br),
                          br[-length(br)], br[-1])
      registrar(paste0(v, " - coeficiente"), col_cls, m,
                paste0("Coeficiente local de ", v, " (", sufixo, ")"),
                "coeficiente", bloq)
    } else {
      aviso("'", v, "': coeficiente local sem variacao suficiente ",
            "(", length(unique(b[is.finite(b)])), " valores distintos) - ",
            "sem mapa de coeficiente; so __psig, __sig e __tvc")
    }

    # 2. coeficiente mascarado pela significância  <- mapa recomendado
    base_msk <- if (QUEBRAS_SO_SIGNIF && sum(sig, na.rm = TRUE) >= 10L) b[sig] else b
    br_m <- quebras_divergentes(base_msk, N_CLASSES_LADO, CLASSE_ZERO, P_ZERO)
    if (is.null(br_m)) br_m <- br
    if (!is.null(br_m)) {
      cl_m  <- classificar(b, br_m)
      valor <- cl_m$valor
      valor[!sig | is.na(sig)] <- "Não significativo"
      valor[!is.finite(b)]     <- NA_character_
      col_msk <- paste0(v, "__msk")
      g[[col_msk]] <- valor
      m <- construir_mapa(valor,
                          c("Não significativo", cl_m$rotulos),
                          c(COR_NAO_SIGNIF, paleta_para_quebras(br_m)),
                          c(NA_real_, br_m[-length(br_m)]),
                          c(NA_real_, br_m[-1]))
      registrar(paste0(v, " - coef mascarado"), col_msk, m,
                paste0("Coeficiente local de ", v,
                       " com os não significativos em branco (", sufixo, ")"),
                "coef_mascarado", bloq)
    }

    # 3. significância em 4 níveis (formato Oliveira et al.)
    col_psig <- paste0(v, "__psig")
    g[[col_psig]] <- sig_simples(p)
    m <- construir_mapa(g[[col_psig]], NIVEIS_PSIG, PALETA_PSIG)
    registrar(paste0(v, " - significância 4 níveis"), col_psig, m,
              paste0("Significância local de ", v, " (", sufixo, ")"),
              "significancia_4")

    # 4. significância direcional em 7 níveis
    col_sig <- paste0(v, "__sig")
    g[[col_sig]] <- sig_direcional(b, p)
    m <- construir_mapa(g[[col_sig]], NIVEIS_SIG, PALETA_SIG)
    registrar(paste0(v, " - significância direcional"), col_sig, m,
              paste0("Significância local direcional de ", v, " (", sufixo, ")"),
              "significancia_7")

    # 5. classe de t (formato Moroskoski et al.)
    col_tv <- paste0(v, "__tvc")
    g[[col_tv]] <- classe_tv(tv)
    m <- construir_mapa(g[[col_tv]], NIVEIS_TV, PALETA_TV)
    registrar(paste0(v, " - t local"), col_tv, m,
              paste0("t local de ", v, " (", sufixo, ")"), "t_local")

    # resumo por variável
    p_bruto <- p_de_tv(tv, gl)
    resumo[[length(resumo) + 1L]] <- data.frame(
      desfecho = sufixo, variavel = v, n = sum(is.finite(b)),
      coef_min      = min(b, na.rm = TRUE),
      coef_q1       = unname(stats::quantile(b, 0.25, na.rm = TRUE)),
      coef_mediana  = stats::median(b, na.rm = TRUE),
      coef_q3       = unname(stats::quantile(b, 0.75, na.rm = TRUE)),
      coef_max      = max(b, na.rm = TRUE),
      pct_negativos    = 100 * mean(b < 0, na.rm = TRUE),
      pct_sig_bruto    = 100 * mean(p_bruto < ALFA, na.rm = TRUE),
      pct_sig_criterio = 100 * mean(sig, na.rm = TRUE),
      pct_sig_positivo = 100 * mean(sig & b > 0, na.rm = TRUE),
      pct_sig_negativo = 100 * mean(sig & b < 0, na.rm = TRUE),
      mediana_coef_sig = if (any(sig, na.rm = TRUE))
                           stats::median(b[sig], na.rm = TRUE) else NA_real_,
      origem_p = unname(origem[v]),
      nao_mapear = bloq,
      row.names = NULL, stringsAsFactors = FALSE)

    msg(sprintf("  %-44s sig %5.1f%%  (bruto %5.1f%%)  %s%s",
                substr(v, 1, 44), 100 * mean(sig, na.rm = TRUE),
                100 * mean(p_bruto < ALFA, na.rm = TRUE), origem[[v]],
                if (bloq) "  << NAO MAPEAR (secao 7.6)" else ""))
  }

  ## ------------------------------------------------------ R² local (Fig. 3)
  col_r2 <- detectar_coluna(nomes, c("^gwr_local_r2$", "local_r2"))
  if (!is.na(col_r2)) {
    r2  <- as.numeric(g[[col_r2]])
    pos <- r2[is.finite(r2) & r2 >= 0]
    br2 <- if (length(unique(pos)) >= 5L)
      unique(as.numeric(stats::quantile(pos, seq(0, 1, length.out = 6)))) else NULL
    if (!is.null(br2) && length(br2) >= 2L) {
      cl2   <- classificar(r2, br2)
      valor <- cl2$valor
      valor[is.finite(r2) & r2 < 0] <- "Negativo (ajuste local pior que a média)"
      g[["Local_R2__cls"]] <- valor
      m <- construir_mapa(valor,
                          c("Negativo (ajuste local pior que a média)", cl2$rotulos),
                          c(COR_R2_NEGATIVO, interpolar(PALETA_R2, length(cl2$rotulos))),
                          c(NA_real_, br2[-length(br2)]), c(NA_real_, br2[-1]))
      registrar("R² local", "Local_R2__cls", m,
                paste0("R² local do GWR (", sufixo, ")"), "local_r2")
      msg("R2 local  : ", sum(r2 < 0, na.rm = TRUE),
          " municipios com R2 local negativo")
    }
  }

  ## ------------------------------------ número de condição local (Fig. 4)
  if (!is.na(arq_collin) && file.exists(arq_collin)) {
    tc <- ler_csv_flex(arq_collin)
    if (!is.null(tc) && nrow(tc) == n) {
      col_cn <- detectar_coluna(names(tc),
                  c("local_cn", "condition", "numero.*condic", "^cn$"))
      if (!is.na(col_cn)) {
        cn  <- suppressWarnings(as.numeric(tc[[col_cn]]))
        brc <- sort(unique(c(min(cn, na.rm = TRUE), CORTES_CN,
                             max(cn, na.rm = TRUE))))
        brc <- brc[is.finite(brc)]
        clc <- classificar(cn, brc)
        if (length(clc$rotulos)) {
          g[["CN_local__cls"]] <- clc$valor
          m <- construir_mapa(clc$valor, clc$rotulos,
                              interpolar(PALETA_CN, length(clc$rotulos)),
                              brc[-length(brc)], brc[-1])
          registrar("Número de condição local", "CN_local__cls", m,
                    paste0("Número de condição local, corte em 30 (", sufixo, ")"),
                    "cn_local")
          msg("CN local  : mediana ", round(stats::median(cn, na.rm = TRUE), 1),
              " | ", round(100 * mean(cn > 30, na.rm = TRUE), 1), "% acima de 30")
        }
      }
    } else if (!is.null(tc)) {
      aviso("CSV de colinearidade local com ", nrow(tc),
            " linhas, incompativel com a camada - ignorado")
    }
  }

  ## --------------------------------------- resíduos do modelo (Fig. 2)
  col_res <- if (!is.na(COL_RESID)) COL_RESID else
             if (sufixo %in% names(COL_RESID_POR_DESFECHO) &&
                 COL_RESID_POR_DESFECHO[[sufixo]] %in% nomes)
               COL_RESID_POR_DESFECHO[[sufixo]] else
    detectar_coluna(nomes, c("^residuos_(sar|sdm)_queen$", "residuo.*queen",
                             "^residuos_(sar|sdm)", "^gwr_residual$",
                             "^residual$", "^residuo$", "^gwr_residuo$"))
  if (!is.na(col_res)) {
    rs  <- as.numeric(g[[col_res]])
    brr <- quebras_divergentes(rs, 2L, TRUE, 0.10)
    clr <- classificar(rs, brr)
    if (length(clr$rotulos)) {
      g[["Residuo__cls"]] <- clr$valor
      m <- construir_mapa(clr$valor, clr$rotulos, paleta_para_quebras(brr),
                          brr[-length(brr)], brr[-1])
      registrar("Resíduos do modelo de referência", "Residuo__cls", m,
                paste0("Resíduos do modelo de referência - ", col_res,
                       " (", sufixo, ")"), "residuo")
      msg("residuos  : coluna ", col_res)
    }
  }

  ## ------------------------------------------ taxa do desfecho (Fig. 1)
  col_taxa <- if (!is.na(COL_TAXA)) COL_TAXA else
              if (sufixo %in% names(COL_TAXA_POR_DESFECHO) &&
                  COL_TAXA_POR_DESFECHO[[sufixo]] %in% nomes)
                COL_TAXA_POR_DESFECHO[[sufixo]] else
    detectar_coluna(nomes, c("taxa.*suav", "suav.*taxa", "^taxa", "^tx_",
                             "^gwr_y$", "^y$"))
  if (!is.na(col_taxa)) {
    tx  <- as.numeric(g[[col_taxa]])
    brt <- unique(as.numeric(stats::quantile(tx, seq(0, 1, length.out = 6),
                                             na.rm = TRUE)))
    if (length(brt) >= 3L) {
      clt <- classificar(tx, brt)
      g[["Taxa__cls"]] <- clt$valor
      m <- construir_mapa(clt$valor, clt$rotulos,
                          interpolar(PALETA_TAXA, length(clt$rotulos)),
                          brt[-length(brt)], brt[-1])
      registrar("Taxa do desfecho", "Taxa__cls", m,
                paste0("Taxa padronizada suavizada - ", col_taxa,
                       " (", sufixo, ")"), "taxa")
      msg("taxa      : coluna ", col_taxa)
    }
  }

  ## --------------------------------------------------------------- escrita
  pasta_d <- criar_pasta(file.path(PASTA_SAIDA, sufixo))
  pasta_e <- criar_pasta(file.path(pasta_d, "estilos"))

  if (SO_COLUNAS_MAPA) {
    ident  <- detectar_coluna(names(g), PADROES_ID)
    nomem  <- detectar_coluna(names(g), c("^nome", "^municipio", "^nm_mun"))
    manter <- unique(c(ident, nomem,
                       grep("__(cls|msk|psig|sig|tvc|p)$", names(g), value = TRUE),
                       col_geom))
    g <- g[, manter[!is.na(manter)]]
  }

  camada  <- "mapas_gwr"
  arq_out <- file.path(pasta_d, paste0("Mapas_GWR_", sufixo, ".gpkg"))
  if (file.exists(arq_out)) file.remove(arq_out)
  sf::st_write(g, arq_out, layer = camada, quiet = TRUE, append = FALSE)
  msg("gravado   : ", basename(arq_out), " (camada '", camada, "')")

  n_qml <- 0L; n_db <- 0L; usados <- character(0)
  for (nm in names(estilos)) {
    e    <- estilos[[nm]]
    base <- nome_arquivo(nm)
    if (base %in% usados) base <- paste0(base, "_", length(usados) + 1L)
    usados <- c(usados, base)
    qml <- qml_categorizado(e$campo, e$rotulos, e$cores)
    escrever_texto(qml, file.path(pasta_e, paste0(base, ".qml")))
    n_qml <- n_qml + 1L
    if (GRAVAR_ESTILOS_NO_GPKG &&
        gravar_estilo_gpkg(arq_out, camada, "geom", nm, qml, e$descricao)) {
      n_db <- n_db + 1L
    }
  }
  msg("estilos   : ", n_qml, " arquivos .qml",
      if (GRAVAR_ESTILOS_NO_GPKG) paste0(" | ", n_db, " dentro do GPKG") else "")

  leg <- do.call(rbind, legenda)
  escrever_csv(leg, file.path(pasta_d, paste0("Legenda_Classes_", sufixo, ".csv")))

  res <- do.call(rbind, resumo)
  escrever_csv(res, file.path(pasta_d,
                              paste0("Significancia_Resumo_", sufixo, ".csv")))

  ## --------------------------------------------------------------- prévia
  if (GERAR_PREVIA) {
    pasta_p <- criar_pasta(file.path(pasta_d, "previa"))
    alvos <- names(estilos)[vapply(estilos, function(e)
      grepl(PREVIA_APENAS, e$campo), logical(1))]
    for (nm in alvos) {
      e <- estilos[[nm]]
      vcol <- g[[e$campo]]
      if (is.null(vcol)) next
      cores <- stats::setNames(e$cores, e$rotulos)[vcol]
      cores[is.na(cores)] <- COR_SEM_DADO
      arq_png <- file.path(pasta_p, paste0(nome_arquivo(nm), ".png"))
      tryCatch({
        grDevices::png(arq_png, width = 1400, height = 1250, res = 150)
        graphics::par(mar = c(0, 0, 2.5, 0))
        plot(sf::st_geometry(g), col = cores, border = NA,
             main = paste0(sufixo, " - ", nm))
        graphics::legend("bottomright", legend = e$rotulos, fill = e$cores,
                         bty = "n", cex = 0.6, border = "grey40")
        grDevices::dev.off()
      }, error = function(err) {
        try(grDevices::dev.off(), silent = TRUE)
        aviso("previa de '", nm, "': ", conditionMessage(err))
      })
    }
    msg("previa    : ", length(alvos), " PNGs em previa/")
  }

  list(resumo = res, legenda = leg)
}


## ===========================================================================
## 5. EXECUÇÃO
## ===========================================================================

criar_pasta(PASTA_SAIDA)
msg(strrep("=", 72))
msg("CATEGORIZACAO DOS MAPAS DO GWR PARA O QGIS")
msg("criterio de significancia: ", CRITERIO_P,
    " | preferencia no arquivo: ", paste(PREFERENCIA_P_ARQUIVO, collapse = " > "),
    " | alfa = ", ALFA)
msg("classes do coeficiente: ate ", 2 * N_CLASSES_LADO + as.integer(CLASSE_ZERO),
    " (quantis ancorados em zero",
    if (CLASSE_ZERO) ", com faixa central" else "", ")")
if (!is.na(ESPECIFICACAO)) msg("especificacao: ", ESPECIFICACAO)

saidas <- list()
for (d in DESFECHOS) {
  saidas[[d]] <- tryCatch(processar_desfecho(d), error = function(e) {
    aviso(">> FALHA em ", d, ": ", conditionMessage(e)); NULL
  })
}

resumos <- do.call(rbind, lapply(saidas, function(s) s$resumo))
if (!is.null(resumos)) {
  escrever_csv(resumos, file.path(PASTA_SAIDA,
                                  "Significancia_Resumo_Tres_Desfechos.csv"))
  msg(strrep("-", 72))
  msg("resumo consolidado gravado - alimenta a Tabela 6 do corpo")
}

leia <- c(
  "MAPAS DO GWR - COMO USAR NO QGIS",
  "",
  "Um GPKG por desfecho, em Mapas_GWR_<Desfecho>.gpkg (camada 'mapas_gwr').",
  "",
  "COLUNAS CRIADAS, por variavel:",
  "  <var>__cls   classe do coeficiente local, todos os municipios",
  "  <var>__msk   o mesmo, com 'Nao significativo' em branco  <- RECOMENDADO",
  "  <var>__psig  significancia em 4 niveis (formato Oliveira et al.):",
  "               Nao significativo / p < 0,05 / p < 0,01 / p < 0,001",
  "  <var>__sig   significancia direcional em 7 niveis (sinal + p)",
  "  <var>__tvc   classe do t local (formato Moroskoski et al.):",
  "               < -1,96 / -1,96 a 0 / 0 a 1,96 / > 1,96",
  "  <var>__p     o p-valor usado no criterio",
  "Mais: Local_R2__cls, CN_local__cls, Residuo__cls, Taxa__cls.",
  "",
  "COMO MONTAR A FIGURA (e o que os artigos de referencia fazem):",
  "  Oliveira et al. e Moroskoski et al. poem, por variavel, DOIS paineis lado",
  "  a lado: o coeficiente e, ao lado, a significancia (ou o t). Para repetir:",
  "    painel A = <var>__cls  (ou __msk) ; painel B = <var>__psig (ou __tvc)",
  "",
  "APLICAR O ESTILO:",
  "  Propriedades da camada -> Simbologia -> Estilo (rodape) -> Carregar estilo",
  "    - 'Do banco de dados': lista os estilos gravados dentro do proprio GPKG",
  "    - 'Do arquivo': aponte para a pasta estilos/ e escolha o .qml",
  "  Se um .qml nao carregar, abra o script, troque QML_FORMATO_LEGADO para",
  "  TRUE (formato <prop>, das versoes 3.0 a 3.8 do QGIS) e rode de novo.",
  "",
  "LEGENDAS: Legenda_Classes_<Desfecho>.csv traz os intervalos exatos de cada",
  "classe, com n e % de municipios - sao esses numeros que vao na legenda da",
  "figura e no texto. A coluna 'cor' repete o hexadecimal usado no .qml.",
  "",
  "VARIAVEIS MARCADAS 'nao_mapear' (secao 7.6 do Resumo Analitico):",
  paste0("  ", paste(VARIAVEIS_NAO_MAPEAR, collapse = ", ")),
  "  As colunas existem, mas o Resumo desaconselha publicar o mapa de",
  "  coeficiente dessas duas (acai: |coef| > 10 em 906 municipios, todos com",
  "  acai = 0; spei: 0% significativo apos correcao).",
  "",
  "ATENCAO: exporte sempre em GPKG, nunca em shapefile - os nomes de coluna",
  "passam de 10 caracteres e seriam truncados."
)
escrever_texto(leia, file.path(PASTA_SAIDA, "LEIA-ME_QGIS.txt"))
escrever_texto(LOG_LINHAS, file.path(PASTA_SAIDA, "Log_Categorizacao_GWR.txt"))
msg("fim")
