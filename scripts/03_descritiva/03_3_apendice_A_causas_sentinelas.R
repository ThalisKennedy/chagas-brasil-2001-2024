# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SIM/DOBR - CHAGAS/APENDICE_A_sentinelas_SIM_completo.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SIM/DOBR - CHAGAS"))

# =============================================================================
# APÊNDICE A – ANÁLISE DE SENSIBILIDADE DA MORTALIDADE (CAUSAS SENTINELAS)
# Refeita com o SIM completo, 2001–2024 (script único)
# -----------------------------------------------------------------------------
# POR QUE REFAZER
#   O script anterior (DOBR - CHAGAS - FINAL_5 - SENSIBILIDADE.R) procurava as
#   causas sentinelas dentro do próprio extrato de óbitos com menção à DC. Os 25
#   "sentinelas" eram óbitos com a DC registrada só no campo ATESTADO (por isso
#   ficaram fora da "amostra principal" de 147.564, que varria apenas a causa
#   básica e as linhas). Todos mencionavam a DC, de modo que a comparação não
#   avaliava o sub-registro descrito na seção 4.4.
#
# O QUE ESTE SCRIPT FAZ
#   Grupo DC (amostra principal): os 147.711 óbitos com menção à DC das Tabelas
#     13 a 15 (MORT_CHAGAS_FINAL_2_TRATADO_COMPLETO.rds), sem alteração.
#   Grupo sentinela: óbitos de 2001 a 2024 com CAUSA BÁSICA I42.0, I42.9, I44
#     (I44.0 a I44.7), I46.1, I49.9 ou I51.7 e SEM menção à DC (B57, I41.2,
#     I98.1, K23.1 ou K93.1) em nenhum campo de causa da DO (causa básica,
#     causa básica original, linhas A a D, Parte II e ATESTADO). É o critério
#     do indicador de mortalidade por causas sentinelas do Boletim
#     Epidemiológico especial de abril de 2022 (BRASIL, 2022a): causa básica,
#     retirados os registros com DC como causa associada. O I50 segue fora.
#   As variáveis do grupo sentinela seguem as regras do script
#   DOBR - CHAGAS - FINAL_5.R. Para conferir, as mesmas regras são aplicadas aos
#   óbitos com menção à DC encontrados no SIM bruto e comparadas com a base
#   .rds (aba conf_dc_bruto_vs_rds): se bater, os dois grupos foram construídos
#   do mesmo jeito.
#   Tabela A3: as "Principais Comorbidades" têm como denominador os óbitos com
#   ao menos uma comorbidade, como na Tabela 15.
#   Análise descritiva, sem valor-p, como no apêndice original.
#
# COMO RODAR
#   1. Ajuste as pastas da seção 0.
#   2. pasta_sim: arquivos do SIM-DO de 2001 a 2024 (DOBR2001.dbc ... DOBR2024.dbc,
#      ou DO{UF}{ano}.dbc; também lê .dbf, .csv, .rds e .parquet). Deixe só um
#      arquivo por ano (ou um por UF e ano): se houver DOBR e arquivos por UF,
#      o script usa só os DOBR; arquivos DOFET, DOEXT, DOINF e DOMAT são
#      ignorados. Se a pasta não existir ou não tiver arquivos, os dados são
#      baixados do DATASUS com o pacote microdatasus, UF por UF (demora algumas
#      horas).
#   3. Cada arquivo (ou ano baixado) já processado fica salvo em
#      pasta_saida/cache_sim. Se o R parar no meio, é só rodar de novo: o script
#      continua de onde parou. Para refazer do zero, apague essa pasta.
#   4. Antes de usar os números, confira as abas que começam com "conf_":
#      conf_obitos_por_ano – óbitos lidos por ano (compare com o TabNet) e os
#        óbitos com menção à DC achados no SIM bruto x base .rds (esperado: 147.711);
#      conf_dc_bruto_vs_rds – distribuição de cada variável nos dois; diferenças
#        grandes indicam regra diferente da usada na base .rds.
#
# SAÍDAS (pasta_saida)
#   Apendice_A_numeros.xlsx – abas A1, A2 e A3 no formato das tabelas, aba
#     "impacto" (óbitos sentinelas em relação aos óbitos com menção à DC, por
#     ano, região e código) e abas de conferência;
#   Tabela_A1.docx a Tabela_A3.docx – se o pacote gt estiver instalado;
#   Sentinelas_SIM_2001_2024.rds – os óbitos sentinelas, já tratados.
# =============================================================================

library(tidyverse)
library(lubridate)
library(gtsummary)
library(writexl)

theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")

# -----------------------------------------------------------------------------
# 0. CONFIGURAÇÃO – ajuste aqui
# -----------------------------------------------------------------------------
if (!exists("pasta_bases")) {
  pasta_bases <- file.path(DIR_DADOS, "Análises/Parte 2 - Análises Descritivas")
}
# Pasta com os arquivos do SIM-DO 2001–2024 (pode ter subpastas)
if (!exists("pasta_sim")) {
  pasta_sim <- file.path(DIR_DADOS, "SIM")
}
if (!exists("pasta_saida")) pasta_saida <- file.path(pasta_bases, "Apêndice A")

arq_sim_dc <- "MORT_CHAGAS_FINAL_2_TRATADO_COMPLETO.rds"
anos       <- 2001:2024

# Códigos (CID-10 sem ponto)
regex_dc        <- "B57|I412|I981|K231|K931"
regex_sentinela <- "^(I420|I429|I44|I461|I499|I517)"
codigos_sentinela <- c(
  "I42.0 – Cardiomiopatia dilatada"                     = "^I420",
  "I42.9 – Cardiomiopatia não especificada"             = "^I429",
  "I44 – Bloqueio atrioventricular e do ramo esquerdo"  = "^I44",
  "I46.1 – Morte súbita (de origem) cardíaca"           = "^I461",
  "I49.9 – Arritmia cardíaca não especificada"          = "^I499",
  "I51.7 – Cardiomegalia"                               = "^I517"
)

grupos <- c(dc = "Amostra Principal (menção à DC)", sentinela = "Causas Sentinelas (sem menção à DC)")

campos_dc    <- c("CAUSABAS", "LINHAA", "LINHAB", "LINHAC", "LINHAD", "LINHAII", "ATESTADO")
vars_brutas  <- c("DTOBITO", "DTNASC", "SEXO", "RACACOR", "ESTCIV", "ESC", "OCUP", "NATURAL",
                  "CODMUNNATU", "CODMUNRES", "CODMUNOCOR", "LOCOCOR", "ASSISTMED", "NECROPSIA",
                  campos_dc, "CAUSABAS_O")
termos_para_anular <- c("Ignorado/Em branco", "Ignorado")

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
pasta_cache <- file.path(pasta_saida, "cache_sim")
dir.create(pasta_cache, showWarnings = FALSE)

localizar <- function(arquivo) {
  candidatos <- unique(c(arquivo, file.path(pasta_bases, arquivo),
                         file.path(pasta_bases, "Apêndice D", arquivo)))
  achado <- candidatos[file.exists(candidatos)][1]
  if (is.na(achado)) stop("Arquivo não encontrado: ", arquivo, call. = FALSE)
  achado
}

# =============================================================================
# 1. LEITURA DO SIM COMPLETO, ARQUIVO POR ARQUIVO
#    De cada arquivo ficam só as colunas usadas e só os óbitos com menção à DC
#    ou com causa básica sentinela; o resto é descartado na hora (memória).
# =============================================================================

limpar_cid <- function(x) str_remove_all(str_to_upper(coalesce(as.character(x), "")), "[^A-Z0-9]")

ler_dbc <- function(arq) {
  if (requireNamespace("microdatasus", quietly = TRUE) &&
      "read_dbc" %in% getNamespaceExports("microdatasus")) {
    return(microdatasus::read_dbc(arq))
  }
  if (requireNamespace("read.dbc", quietly = TRUE)) return(read.dbc::read.dbc(arq, as.is = TRUE))
  stop("Para ler arquivos .dbc, instale o microdatasus 3.0 ou mais recente ",
       "(remotes::install_github('rfsaldanha/microdatasus')) ou o pacote read.dbc.", call. = FALSE)
}

ler_arquivo <- function(arq) {
  ext <- tolower(tools::file_ext(arq))
  df <- switch(
    ext,
    dbc = ler_dbc(arq),
    dbf = foreign::read.dbf(arq, as.is = TRUE),
    csv = {
      sep <- if (str_detect(readLines(arq, n = 1, warn = FALSE), ";")) ";" else ","
      cab <- names(readr::read_delim(arq, delim = sep, n_max = 0, col_types = readr::cols(.default = "c"),
                                     progress = FALSE))
      readr::read_delim(arq, delim = sep, col_types = readr::cols(.default = "c"),
                        col_select = all_of(cab[toupper(cab) %in% vars_brutas]),
                        locale = readr::locale(encoding = "latin1"), progress = FALSE)
    },
    rds = readRDS(arq),
    parquet = arrow::read_parquet(arq),
    stop("Formato não suportado: ", arq, call. = FALSE)
  )
  df <- as_tibble(df)
  names(df) <- toupper(names(df))
  df %>% select(any_of(vars_brutas)) %>% mutate(across(everything(), as.character))
}

# Marca e filtra um bloco do SIM bruto
#   MENCAO_DC : código da DC na causa básica, nas linhas ou no ATESTADO
#               (mesmo universo da amostra principal de 147.711)
#   SENTINELA : causa básica sentinela e nenhuma menção à DC, inclusive na
#               causa básica original (CAUSABAS_O)
filtrar_bloco <- function(df) {
  for (v in setdiff(vars_brutas, names(df))) df[[v]] <- NA_character_
  dc_campos <- Reduce(`|`, lapply(campos_dc, function(v) str_detect(limpar_cid(df[[v]]), regex_dc)))
  dc_cb_original <- str_detect(limpar_cid(df$CAUSABAS_O), regex_dc)
  df %>%
    mutate(
      ANO_ARQUIVO = suppressWarnings(as.integer(str_sub(str_pad(DTOBITO, 8, pad = "0"), 5, 8))),
      MENCAO_DC = dc_campos,
      DC_SO_CB_ORIGINAL = dc_cb_original & !dc_campos,
      SENTINELA = str_detect(limpar_cid(CAUSABAS), regex_sentinela) & !dc_campos & !dc_cb_original
    )
}

processar <- function(df, origem) {
  df <- filtrar_bloco(df)
  list(
    dados   = df %>% filter(MENCAO_DC | SENTINELA | DC_SO_CB_ORIGINAL) %>% mutate(ORIGEM = origem),
    contagem = df %>% count(ANO = ANO_ARQUIVO, name = "obitos_lidos") %>% mutate(origem = origem)
  )
}

# --- arquivos locais ----------------------------------------------------------
arquivos_sim <- if (dir.exists(pasta_sim)) {
  list.files(pasta_sim, pattern = "\\.(dbc|dbf|csv|rds|parquet)$", ignore.case = TRUE,
             full.names = TRUE, recursive = TRUE)
} else character(0)
nomes <- basename(arquivos_sim)
arquivos_sim <- arquivos_sim[
  !str_detect(toupper(nomes), "^DO(FET|EXT|INF|MAT)") &           # fetais e subconjuntos do DO
  !str_detect(toupper(nomes), "^MORT_CHAGAS") &                    # bases já tratadas
  !str_starts(normalizePath(arquivos_sim, winslash = "/", mustWork = FALSE),
              fixed(normalizePath(pasta_saida, winslash = "/", mustWork = FALSE)))
]
if (any(str_detect(toupper(basename(arquivos_sim)), "^DOBR"))) {
  por_uf <- str_detect(toupper(basename(arquivos_sim)), "^DO[A-Z]{2}\\d{2,4}\\.") &
            !str_detect(toupper(basename(arquivos_sim)), "^DOBR")
  if (any(por_uf)) message("Há arquivos DOBR e por UF na pasta: usando só os DOBR, para não contar óbitos em dobro.")
  arquivos_sim <- arquivos_sim[!por_uf]
}

# só nomes no formato DO{UF ou BR}{ANO}.ext -- derruba a "DOBR - Completa.dbf"
# e qualquer outro arquivo que não seja um ano bruto do SIM
nomes <- toupper(basename(arquivos_sim))
arquivos_sim <- arquivos_sim[str_detect(nomes, "^DO(BR|[A-Z]{2})\\d{4}\\.(DBC|DBF|CSV|RDS|PARQUET)$")]

# um arquivo só por UF+ano: prefere .dbc; só cai pro .dbf (ou csv/rds/parquet)
# se não existir .dbc daquele ano -- sem isso, anos com os dois formatos
# contam cada óbito duas vezes
info <- tibble(arq = arquivos_sim, nome = toupper(basename(arquivos_sim))) %>%
  mutate(chave = str_remove(nome, "\\.[A-Z]+$"),
         prioridade = match(str_extract(nome, "[A-Z]+$"), c("DBC", "DBF", "CSV", "RDS", "PARQUET"))) %>%
  arrange(chave, prioridade) %>%
  distinct(chave, .keep_all = TRUE)
arquivos_sim <- info$arq

for (arq in arquivos_sim) {
  relativo <- str_remove(normalizePath(arq, winslash = "/", mustWork = FALSE),
                         fixed(paste0(normalizePath(pasta_sim, winslash = "/", mustWork = FALSE), "/")))
  cache <- file.path(pasta_cache, paste0(str_replace_all(relativo, "[^A-Za-z0-9]", "_"), ".rds"))
  cat(sprintf("%-16s %8.1f MB  %s\n", basename(arq), file.info(arq)$size / 1e6,
              if (file.exists(cache)) "OK" else ">>> ESTE <<<"))
}

ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS", "MT", "PA",
         "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO")

if (length(arquivos_sim)) {
  message(length(arquivos_sim), " arquivo(s) do SIM em ", pasta_sim, ":\n  ",
          paste(basename(arquivos_sim), collapse = ", "))
  blocos <- map(arquivos_sim, function(arq) {
    relativo <- str_remove(normalizePath(arq, winslash = "/", mustWork = FALSE),
                           fixed(paste0(normalizePath(pasta_sim, winslash = "/", mustWork = FALSE), "/")))
    cache <- file.path(pasta_cache, paste0(str_replace_all(relativo, "[^A-Za-z0-9]", "_"), ".rds"))
    if (file.exists(cache)) return(readRDS(cache))
    message("Lendo ", basename(arq), " ...")
    res <- processar(ler_arquivo(arq), basename(arq))
    saveRDS(res, cache)
    gc()
    res
  })
} else {
  message("Nenhum arquivo do SIM em pasta_sim; baixando do DATASUS com o microdatasus.")
  if (!requireNamespace("microdatasus", quietly = TRUE)) {
    stop("Instale o microdatasus: remotes::install_github('rfsaldanha/microdatasus')", call. = FALSE)
  }
  baixar <- function(ano, uf, tentativas = 3) {
    for (k in seq_len(tentativas)) {
      df <- tryCatch(microdatasus::fetch_datasus(year_start = ano, year_end = ano, uf = uf,
                                                 information_system = "SIM-DO"),
                     error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) return(df)
      Sys.sleep(10)
    }
    stop("Falha ao baixar o SIM-DO de ", uf, " ", ano, ". Rode o script de novo: ",
         "o que já foi baixado fica no cache.", call. = FALSE)
  }
  blocos <- map(anos, function(ano) {
    partes <- map(ufs, function(uf) {
      cache <- file.path(pasta_cache, paste0("SIM_DO_", ano, "_", uf, ".rds"))
      if (file.exists(cache)) return(readRDS(cache))
      message("Baixando SIM-DO ", uf, " ", ano, " ...")
      df <- baixar(ano, uf)
      names(df) <- toupper(names(df))
      res <- processar(df %>% select(any_of(vars_brutas)) %>% mutate(across(everything(), as.character)),
                       paste0("DO", uf, ano))
      saveRDS(res, cache)
      rm(df); gc()
      res
    })
    list(dados = bind_rows(map(partes, "dados")), contagem = bind_rows(map(partes, "contagem")))
  })
}

SIM_FILTRADO <- bind_rows(map(blocos, "dados")) %>% filter(ANO_ARQUIVO %in% anos)
CONTAGEM_LIDA <- bind_rows(map(blocos, "contagem"))
rm(blocos); gc()

# =============================================================================
# 2. VARIÁVEIS – mesmas regras do script DOBR - CHAGAS - FINAL_5.R
# =============================================================================

uf_nomes <- c("11" = "Rondônia", "12" = "Acre", "13" = "Amazonas", "14" = "Roraima", "15" = "Pará",
              "16" = "Amapá", "17" = "Tocantins", "21" = "Maranhão", "22" = "Piauí", "23" = "Ceará",
              "24" = "Rio Grande do Norte", "25" = "Paraíba", "26" = "Pernambuco", "27" = "Alagoas",
              "28" = "Sergipe", "29" = "Bahia", "31" = "Minas Gerais", "32" = "Espírito Santo",
              "33" = "Rio de Janeiro", "35" = "São Paulo", "41" = "Paraná", "42" = "Santa Catarina",
              "43" = "Rio Grande do Sul", "50" = "Mato Grosso do Sul", "51" = "Mato Grosso",
              "52" = "Goiás", "53" = "Distrito Federal")
regioes <- c("1" = "Norte", "2" = "Nordeste", "3" = "Sudeste", "4" = "Sul", "5" = "Centro-Oeste")

derivar_variaveis <- function(df) {
  df %>%
    mutate(
      # Datas e idade
      DATA_OBITO  = dmy(str_pad(DTOBITO, 8, pad = "0"), quiet = TRUE),
      DATA_NASC   = dmy(str_pad(DTNASC, 8, pad = "0"), quiet = TRUE),
      IDADE_OBITO = floor(time_length(interval(DATA_NASC, DATA_OBITO), "years")),
      ANO_OBITO   = year(DATA_OBITO),

      # Sexo, raça/cor e estado civil
      SEXO = case_when(str_trim(SEXO) == "1" ~ "Masculino",
                       str_trim(SEXO) == "2" ~ "Feminino",
                       TRUE ~ "Ignorado/Em branco"),
      RACA_NUM = suppressWarnings(as.numeric(RACACOR)),
      RACACOR  = case_when(RACA_NUM == 1 ~ "Branca", RACA_NUM == 2 ~ "Preta", RACA_NUM == 3 ~ "Amarela",
                           RACA_NUM == 4 ~ "Parda", RACA_NUM == 5 ~ "Indígena",
                           TRUE ~ "Ignorado/Em branco"),
      ESTCIV_NUM   = suppressWarnings(as.numeric(ESTCIV)),
      ESTADO_CIVIL = case_when(ESTCIV_NUM == 1 ~ "Solteiro", ESTCIV_NUM == 2 ~ "Casado",
                               ESTCIV_NUM == 3 ~ "Viúvo", ESTCIV_NUM == 4 ~ "Separado/Divorciado",
                               ESTCIV_NUM == 5 ~ "União Estável", TRUE ~ "Ignorado/Em branco"),

      # Faixa etária
      FAIXA_ETARIA = as.character(cut(IDADE_OBITO, breaks = c(0, 46, 60, 75, Inf),
                                       labels = c("0 a 45 anos", "46 a 59 anos", "60 a 74 anos",
                                                  "75 anos ou mais"), right = FALSE)),
      FAIXA_ETARIA = replace_na(FAIXA_ETARIA, "Ignorado/Em branco"),

      # Residência e naturalidade
      CODMUNRES  = substr(as.character(CODMUNRES), 1, 6),
      CODMUNOCOR = substr(as.character(CODMUNOCOR), 1, 6),
      COD_UF_RESID = substr(CODMUNRES, 1, 2),
      REGIAO_RESIDENCIA = coalesce(unname(regioes[substr(COD_UF_RESID, 1, 1)]), "Ignorado/Em branco"),
      COD_UF_NASC = case_when(
        !is.na(CODMUNNATU) & nchar(CODMUNNATU) >= 2 &
          !substr(CODMUNNATU, 1, 2) %in% c("00", "99") ~ substr(CODMUNNATU, 1, 2),
        !is.na(NATURAL) & substr(NATURAL, 1, 1) == "8" & nchar(NATURAL) >= 3 ~ substr(NATURAL, 2, 3),
        !is.na(NATURAL) & nchar(NATURAL) >= 2 &
          !substr(NATURAL, 1, 2) %in% c("00", "80", "99") ~ substr(NATURAL, 1, 2),
        TRUE ~ NA_character_
      ),
      # No script original a UF de nascimento passa pelo nome da UF; códigos
      # fora das 27 UFs viram "Ignorado/Em branco"
      COD_UF_NASC = if_else(COD_UF_NASC %in% names(uf_nomes), COD_UF_NASC, NA_character_),
      UF_NASCIMENTO = coalesce(unname(uf_nomes[COD_UF_NASC]), "Ignorado/Em branco"),
      CLASSIFICACAO_MIGRATORIA = case_when(
        is.na(COD_UF_NASC) | is.na(COD_UF_RESID) ~ "Ignorado/Em branco",
        substr(COD_UF_NASC, 1, 1) != substr(COD_UF_RESID, 1, 1) ~ "Migrante Interregional",
        COD_UF_NASC != COD_UF_RESID ~ "Migrante Interestadual",
        COD_UF_NASC == COD_UF_RESID ~ "Não Migrante",
        TRUE ~ "Ignorado/Em branco"
      ),

      # Escolaridade
      ESCOLARIDADE = case_when(
        IDADE_OBITO < 5 ~ "Não se aplica",
        ESC == "1" ~ "Nenhuma",
        ESC %in% c("2", "3") ~ "1 a 7 anos",
        ESC == "4" ~ "8 a 11 anos",
        ESC == "5" ~ "12 anos ou mais",
        TRUE ~ "Ignorado/Em branco"
      ),

      # Local de ocorrência e fluxo assistencial
      LOCAL_OCORRENCIA = case_when(
        LOCOCOR == "1" ~ "Hospital",
        LOCOCOR == "2" ~ "Outros Estabelecimento de Saúde",
        LOCOCOR == "3" ~ "Domicílio",
        LOCOCOR == "4" ~ "Via Pública",
        LOCOCOR == "5" ~ "Outros",
        TRUE ~ "Ignorado/Em branco"
      ),
      MUN_RES_6  = str_pad(CODMUNRES, width = 6, pad = "0", side = "left"),
      MUN_OCOR_6 = str_pad(CODMUNOCOR, width = 6, pad = "0", side = "left"),
      FLUXO_ASSISTENCIA = case_when(
        is.na(CODMUNRES) | is.na(CODMUNOCOR) ~ "Ignorado/Em branco",
        MUN_RES_6 == "000000" | MUN_OCOR_6 == "000000" ~ "Ignorado/Em branco",
        MUN_RES_6 == MUN_OCOR_6 ~ "Sem Deslocamento",
        substr(MUN_RES_6, 1, 2) == substr(MUN_OCOR_6, 1, 2) ~ "Deslocamento Intermunicipal",
        substr(MUN_RES_6, 1, 1) == substr(MUN_OCOR_6, 1, 1) ~ "Deslocamento Interestadual",
        substr(MUN_RES_6, 1, 1) != substr(MUN_OCOR_6, 1, 1) ~ "Deslocamento Interregional",
        TRUE ~ "Ignorado/Em branco"
      ),

      # Perfil ocupacional (CBO de 3 dígitos até 2005; CBO 2002 a partir de 2006)
      OCUP_NUM = str_remove_all(as.character(OCUP), "[^0-9]"),
      DIG1 = substr(OCUP_NUM, 1, 1),
      GRANDE_GRUPO_OCUP = case_when(
        is.na(OCUP) | OCUP_NUM %in% c("000000", "999999", "99999", "0", "") ~ "Ignorado/Em branco",
        OCUP_NUM %in% c("999991", "999992", "999994") ~ "Inativos/Sem Ocupação",
        OCUP_NUM == "999993" ~ "Aposentado/Pensionista",
        ANO_OBITO <= 2005 & DIG1 %in% c("0", "1") ~ "Profissionais das Ciências e Artes",
        ANO_OBITO <= 2005 & DIG1 == "2" ~ "Membros Superiores/Dirigentes",
        ANO_OBITO <= 2005 & DIG1 == "3" ~ "Serviços Administrativos",
        ANO_OBITO <= 2005 & DIG1 %in% c("4", "5") ~ "Trabalhadores de Serviços/Comércio",
        ANO_OBITO <= 2005 & DIG1 == "6" ~ "Trabalhadores Agropecuários/Florestais/Pesca",
        ANO_OBITO <= 2005 & DIG1 %in% c("7", "8", "9") ~ "Trabalhadores da Produção Industrial",
        ANO_OBITO >= 2006 & DIG1 == "0" ~ "Membros das Forças Armadas/Polícia/Bombeiros",
        ANO_OBITO >= 2006 & DIG1 == "1" ~ "Membros Superiores/Dirigentes/Poder Público",
        ANO_OBITO >= 2006 & DIG1 == "2" ~ "Profissionais das Ciências e Artes",
        ANO_OBITO >= 2006 & DIG1 == "3" ~ "Técnicos de Nível Médio",
        ANO_OBITO >= 2006 & DIG1 == "4" ~ "Serviços Administrativos",
        ANO_OBITO >= 2006 & DIG1 == "5" ~ "Trabalhadores de Serviços/Comércio",
        ANO_OBITO >= 2006 & DIG1 == "6" ~ "Trabalhadores Agropecuários/Florestais/Pesca",
        ANO_OBITO >= 2006 & DIG1 %in% c("7", "8", "9") ~ "Trabalhadores da Produção Industrial",
        TRUE ~ "Ignorado/Em branco"
      ),
      PERFIL_OCUPACIONAL = case_when(
        GRANDE_GRUPO_OCUP == "Trabalhadores Agropecuários/Florestais/Pesca" ~ "Agropecuário/Rural",
        GRANDE_GRUPO_OCUP %in% c("Membros Superiores/Dirigentes/Poder Público", "Membros Superiores/Dirigentes",
                                 "Profissionais das Ciências e Artes", "Técnicos de Nível Médio") ~
          "Não Manual/Intelectual/Técnico",
        GRANDE_GRUPO_OCUP %in% c("Serviços Administrativos", "Trabalhadores de Serviços/Comércio",
                                 "Trabalhadores da Produção Industrial",
                                 "Membros das Forças Armadas/Polícia/Bombeiros") ~
          "Manual Urbano/Serviços/Industrial",
        GRANDE_GRUPO_OCUP == "Aposentado/Pensionista" ~ "Aposentado/Pensionista",
        GRANDE_GRUPO_OCUP == "Inativos/Sem Ocupação" ~ "Inativos/Sem Ocupação",
        TRUE ~ "Ignorado/Em branco"
      ),

      # Comorbidades: causa básica e linhas A a D e Parte II (sem o ATESTADO)
      DIAG_TEMP = str_remove_all(str_to_upper(paste(CAUSABAS, LINHAA, LINHAB, LINHAC, LINHAD, LINHAII,
                                                    sep = " ")), "[^A-Z0-9]"),
      TEM_HIPERTENSAO = as.numeric(str_detect(DIAG_TEMP, "I10|I11|I12|I13|I14|I15")),
      TEM_DIABETES    = as.numeric(str_detect(DIAG_TEMP, "E10|E11|E12|E13|E14")),
      TEM_ISQUEMICA   = as.numeric(str_detect(DIAG_TEMP, "I20|I21|I22|I23|I24|I25")),
      TEM_AVC         = as.numeric(str_detect(DIAG_TEMP, "I60|I61|I62|I63|I64|I69")),
      TEM_NEOPLASIA   = as.numeric(str_detect(DIAG_TEMP, "C[0-9]")),
      TEM_RENAL       = as.numeric(str_detect(DIAG_TEMP, "N17|N18|N19")),
      QTD_COMORBIDADES = TEM_HIPERTENSAO + TEM_DIABETES + TEM_ISQUEMICA + TEM_AVC + TEM_NEOPLASIA + TEM_RENAL,
      PERFIL_COMORBIDADE = case_when(QTD_COMORBIDADES == 0 ~ "Nenhuma Identificada",
                                     QTD_COMORBIDADES == 1 ~ "Uma Comorbidade",
                                     QTD_COMORBIDADES >= 2 ~ "Multimorbidade (2 ou mais)",
                                     TRUE ~ "Ignorado/Em branco"),

      # Tipo de menção (só para conferir o grupo DC do SIM bruto)
      TIPO_MENCAO = if_else(str_detect(limpar_cid(CAUSABAS), regex_dc), "Causa Básica", "Causa Associada"),

      # Código sentinela da causa básica
      CODIGO_SENTINELA = {
        cb <- limpar_cid(CAUSABAS)
        out <- rep(NA_character_, length(cb))
        for (k in seq_along(codigos_sentinela)) {
          out[is.na(out) & str_detect(cb, codigos_sentinela[[k]])] <- names(codigos_sentinela)[k]
        }
        out
      }
    ) %>%
    select(-RACA_NUM, -ESTCIV_NUM, -MUN_RES_6, -MUN_OCOR_6, -OCUP_NUM, -DIG1, -DIAG_TEMP)
}

# Rótulos finais (os mesmos das Tabelas 13 a 15) para as duas origens
harmonizar <- function(df) {
  troca <- function(x, mapa) { x <- as.character(x); i <- x %in% names(mapa); x[i] <- unname(mapa[x[i]]); x }
  df %>%
    mutate(
      LOCAL_OCORRENCIA = troca(LOCAL_OCORRENCIA, c("Outros Estab. Saúde" = "Outros Estabelecimentos de Saúde",
                                                   "Outros Estabelecimento de Saúde" = "Outros Estabelecimentos de Saúde")),
      PERFIL_COMORBIDADE = troca(PERFIL_COMORBIDADE, c("Multimorbidade (2+)" = "Multimorbidade (2 ou mais)")),
      PERFIL_OCUPACIONAL = troca(PERFIL_OCUPACIONAL, c("Dona de Casa/Estudante" = "Inativos/Sem Ocupação")),
      CLASSIFICACAO_MIGRATORIA = troca(CLASSIFICACAO_MIGRATORIA, c("Migrante Interregional" = "Migrante Inter-regional")),
      FLUXO_ASSISTENCIA = troca(FLUXO_ASSISTENCIA, c("Deslocamento Interregional" = "Deslocamento Inter-regional"))
    )
}

niveis <- list(
  SEXO               = c("Masculino", "Feminino"),
  RACACOR            = c("Branca", "Parda", "Preta", "Amarela", "Indígena"),
  FAIXA_ETARIA       = c("0 a 45 anos", "46 a 59 anos", "60 a 74 anos", "75 anos ou mais"),
  ESCOLARIDADE       = c("Nenhuma", "1 a 7 anos", "8 a 11 anos", "12 anos ou mais", "Não se aplica"),
  ESTADO_CIVIL       = c("Solteiro", "Casado", "Viúvo", "Separado/Divorciado", "União Estável"),
  PERFIL_OCUPACIONAL = c("Aposentado/Pensionista", "Manual Urbano/Serviços/Industrial", "Agropecuário/Rural",
                         "Não Manual/Intelectual/Técnico", "Inativos/Sem Ocupação"),
  REGIAO_RESIDENCIA  = c("Sudeste", "Centro-Oeste", "Nordeste", "Sul", "Norte"),
  CLASSIFICACAO_MIGRATORIA = c("Não Migrante", "Migrante Interestadual", "Migrante Inter-regional"),
  FLUXO_ASSISTENCIA  = c("Sem Deslocamento", "Deslocamento Intermunicipal", "Deslocamento Interestadual",
                         "Deslocamento Inter-regional"),
  LOCAL_OCORRENCIA   = c("Hospital", "Domicílio", "Outros Estabelecimentos de Saúde", "Via Pública", "Outros"),
  PERFIL_COMORBIDADE = c("Nenhuma Identificada", "Uma Comorbidade", "Multimorbidade (2 ou mais)")
)

# "Ignorado/Em branco" vira NA e as categorias ficam na ordem das tabelas
preparar <- function(df) {
  df <- harmonizar(df)
  for (v in names(niveis)) {
    x <- as.character(df[[v]])
    x[x %in% termos_para_anular] <- NA
    df[[v]] <- factor(x, levels = unique(c(niveis[[v]], sort(setdiff(unique(x[!is.na(x)]), niveis[[v]])))))
  }
  com_comorb <- df$QTD_COMORBIDADES >= 1
  for (v in c("TEM_HIPERTENSAO", "TEM_ISQUEMICA", "TEM_RENAL", "TEM_DIABETES", "TEM_AVC", "TEM_NEOPLASIA")) {
    df[[sub("TEM_", "C_", v)]] <- if_else(com_comorb, df[[v]] == 1, NA)
  }
  df
}

# =============================================================================
# 3. OS DOIS GRUPOS
# =============================================================================

MORT_DC <- readRDS(localizar(arq_sim_dc))

SENTINELAS <- SIM_FILTRADO %>%
  filter(SENTINELA) %>%
  derivar_variaveis() %>%
  filter(ANO_OBITO %in% anos)

DC_BRUTO <- SIM_FILTRADO %>%
  filter(MENCAO_DC) %>%
  derivar_variaveis() %>%
  filter(ANO_OBITO %in% anos)

saveRDS(SENTINELAS, file.path(pasta_saida, "Sentinelas_SIM_2001_2024.rds"))

colunas_A <- c(names(niveis), "C_HIPERTENSAO", "C_ISQUEMICA", "C_RENAL", "C_DIABETES", "C_AVC", "C_NEOPLASIA")
DC_A   <- preparar(MORT_DC)
SENT_A <- preparar(SENTINELAS)
dados_A <- bind_rows(
  DC_A   %>% select(all_of(colunas_A)) %>% mutate(GRUPO = grupos[["dc"]]),
  SENT_A %>% select(all_of(colunas_A)) %>% mutate(GRUPO = grupos[["sentinela"]])
) %>%
  mutate(GRUPO = factor(GRUPO, levels = unname(grupos)))

message("Óbitos – amostra principal: ", nrow(MORT_DC), " | causas sentinelas: ", nrow(SENTINELAS),
        " | DC no SIM bruto: ", nrow(DC_BRUTO), " (esperado: ", nrow(MORT_DC), ")")

# =============================================================================
# 4. NÚMEROS DAS TABELAS A1 A A3 (formato da dissertação)
# =============================================================================

fmt_n   <- function(x) style_number(x, big.mark = ".", decimal.mark = ",")
fmt_pct <- function(x) style_number(x * 100, digits = 1, big.mark = ".", decimal.mark = ",")
celula  <- function(x, tot) paste0(fmt_n(x), " (", fmt_pct(if (tot > 0) x / tot else 0), "%)")

# Uma linha por rótulo de variável (n válido no total e em cada grupo) e uma
# por categoria, "n (%)". Dicotômicas: só a linha "sim", com o denominador de
# quem tem a informação (nas comorbidades, quem teve ao menos uma).
numeros_A <- function(dados, variaveis, rotulos, dicotomicas = character(0), bloco_dic = NULL) {
  gr <- levels(dados$GRUPO)
  linha_rotulo <- function(rot, d) {
    tibble(rotulo = rot, n_total = fmt_n(nrow(d)), total = "",
           n_dc = fmt_n(sum(d$GRUPO == gr[1])), dc = "",
           n_sentinela = fmt_n(sum(d$GRUPO == gr[2])), sentinela = "")
  }
  out <- list()
  for (v in variaveis) {
    d <- dados %>% filter(!is.na(.data[[v]]))
    if (v %in% dicotomicas) {
      if (!is.null(bloco_dic) && v == dicotomicas[1]) out[[length(out) + 1]] <- linha_rotulo(bloco_dic, d)
      sim <- d %>% filter(.data[[v]])
      out[[length(out) + 1]] <- tibble(
        rotulo = rotulos[[v]], n_total = "", total = celula(nrow(sim), nrow(d)),
        n_dc = "", dc = celula(sum(sim$GRUPO == gr[1]), sum(d$GRUPO == gr[1])),
        n_sentinela = "", sentinela = celula(sum(sim$GRUPO == gr[2]), sum(d$GRUPO == gr[2])))
    } else {
      out[[length(out) + 1]] <- linha_rotulo(rotulos[[v]], d)
      for (nv in levels(dados[[v]])) {
        out[[length(out) + 1]] <- tibble(
          rotulo = nv, n_total = "", total = celula(sum(d[[v]] == nv), nrow(d)),
          n_dc = "", dc = celula(sum(d[[v]] == nv & d$GRUPO == gr[1]), sum(d$GRUPO == gr[1])),
          n_sentinela = "", sentinela = celula(sum(d[[v]] == nv & d$GRUPO == gr[2]), sum(d$GRUPO == gr[2])))
      }
    }
  }
  bind_rows(out) %>%
    rename("Variável" = rotulo, n = n_total, `Total Avaliado` = total,
           `n ` = n_dc, !!grupos[["dc"]] := dc, `n  ` = n_sentinela, !!grupos[["sentinela"]] := sentinela)
}

vars_A1 <- c(SEXO = "Sexo", RACACOR = "Raça/Cor", FAIXA_ETARIA = "Faixa Etária", ESCOLARIDADE = "Escolaridade",
             ESTADO_CIVIL = "Estado Civil", PERFIL_OCUPACIONAL = "Perfil Ocupacional")
vars_A2 <- c(REGIAO_RESIDENCIA = "Região de Residência", CLASSIFICACAO_MIGRATORIA = "Perfil Migratório",
             FLUXO_ASSISTENCIA = "Fluxo Assistencial", LOCAL_OCORRENCIA = "Local de Ocorrência do Óbito")
vars_A3 <- c(PERFIL_COMORBIDADE = "Perfil de Comorbidades", C_HIPERTENSAO = "Hipertensão",
             C_ISQUEMICA = "Doença Arterial Coronariana", C_RENAL = "Doença Renal", C_DIABETES = "Diabetes",
             C_AVC = "Acidente Vascular Cerebral", C_NEOPLASIA = "Neoplasias")
dic_A3  <- c("C_HIPERTENSAO", "C_ISQUEMICA", "C_RENAL", "C_DIABETES", "C_AVC", "C_NEOPLASIA")

cabecalho <- tibble(
  "Variável" = "Grupo de análise (N)", n = "", `Total Avaliado` = fmt_n(nrow(dados_A)),
  `n ` = "", !!grupos[["dc"]] := fmt_n(sum(dados_A$GRUPO == grupos[["dc"]])),
  `n  ` = "", !!grupos[["sentinela"]] := fmt_n(sum(dados_A$GRUPO == grupos[["sentinela"]])))

numeros <- list(
  A1 = bind_rows(cabecalho, numeros_A(dados_A, names(vars_A1), vars_A1)),
  A2 = bind_rows(cabecalho, numeros_A(dados_A, names(vars_A2), vars_A2)),
  A3 = bind_rows(cabecalho, numeros_A(dados_A, names(vars_A3), vars_A3, dic_A3,
                                      bloco_dic = "Principais Comorbidades"))
)

# =============================================================================
# 5. IMPACTO E CONFERÊNCIAS
# =============================================================================

somar_total <- function(t) {
  t %>%
    mutate(across(c(obitos_DC, obitos_sentinela), ~ replace_na(.x, 0L))) %>%
    bind_rows(tibble(chave = "Total", obitos_DC = sum(.$obitos_DC), obitos_sentinela = sum(.$obitos_sentinela))) %>%
    mutate(sentinela_por_100_DC = style_number(100 * obitos_sentinela / obitos_DC, digits = 1,
                                               big.mark = ".", decimal.mark = ","))
}
impacto <- list(
  impacto_por_ano = full_join(
    MORT_DC %>% count(chave = as.character(ANO_OBITO), name = "obitos_DC"),
    SENTINELAS %>% count(chave = as.character(ANO_OBITO), name = "obitos_sentinela"), by = "chave") %>%
    arrange(chave) %>% somar_total() %>% rename(Ano = chave),
  impacto_por_regiao = full_join(
    DC_A %>% count(chave = as.character(REGIAO_RESIDENCIA), name = "obitos_DC"),
    SENT_A %>% count(chave = as.character(REGIAO_RESIDENCIA), name = "obitos_sentinela"), by = "chave") %>%
    mutate(chave = replace_na(chave, "Ignorado/Em branco")) %>% somar_total() %>% rename(Regiao = chave),
  impacto_por_codigo = SENTINELAS %>% count(Codigo = CODIGO_SENTINELA, name = "obitos_sentinela") %>%
    mutate(pct = fmt_pct(obitos_sentinela / sum(obitos_sentinela)))
)

# Óbitos lidos por ano (todas as causas): compare com o TabNet para ver se o
# SIM está completo e sem arquivos repetidos
conf_ano <- CONTAGEM_LIDA %>%
  filter(ANO %in% anos) %>%
  group_by(ANO) %>% summarise(obitos_lidos_todas_causas = sum(obitos_lidos), .groups = "drop") %>%
  full_join(DC_BRUTO %>% count(ANO = ANO_OBITO, name = "DC_SIM_bruto"), by = "ANO") %>%
  full_join(MORT_DC %>% count(ANO = as.integer(ANO_OBITO), name = "DC_base_rds"), by = "ANO") %>%
  full_join(SENTINELAS %>% count(ANO = ANO_OBITO, name = "sentinelas"), by = "ANO") %>%
  arrange(ANO) %>%
  mutate(across(-ANO, ~ replace_na(.x, 0L)), diferenca_DC = DC_SIM_bruto - DC_base_rds)

# Mesmas regras aplicadas aos óbitos com menção à DC do SIM bruto x base .rds
DC_BRUTO_A <- preparar(DC_BRUTO)
comparar_var <- function(v, a = DC_BRUTO_A, b = DC_A) {
  full_join(a %>% count(categoria = as.character(.data[[v]]), name = "SIM_bruto"),
            b %>% count(categoria = as.character(.data[[v]]), name = "base_rds"), by = "categoria") %>%
    mutate(variavel = v, across(c(SIM_bruto, base_rds), ~ replace_na(.x, 0L)),
           diferenca = SIM_bruto - base_rds) %>%
    select(variavel, categoria, SIM_bruto, base_rds, diferenca)
}
conf_dc <- map_dfr(c("TIPO_MENCAO", colunas_A), comparar_var)

conferencias <- list(
  conf_obitos_por_ano   = conf_ano %>% mutate(ANO = as.character(ANO)),
  conf_dc_bruto_vs_rds  = conf_dc,
  conf_excluidos        = tibble(
    situacao = c("Causa básica sentinela com menção à DC em algum campo (fora do grupo sentinela)",
                 "Menção à DC só na causa básica original (CAUSABAS_O)"),
    n = c(sum(str_detect(limpar_cid(SIM_FILTRADO$CAUSABAS), regex_sentinela) & SIM_FILTRADO$MENCAO_DC),
          sum(SIM_FILTRADO$DC_SO_CB_ORIGINAL))),
  conf_arquivos = CONTAGEM_LIDA %>% group_by(origem) %>%
    summarise(obitos_lidos = sum(obitos_lidos), anos = paste(sort(unique(ANO)), collapse = ", "), .groups = "drop")
)

if (abs(nrow(DC_BRUTO) - nrow(MORT_DC)) > 0) {
  message("ATENÇÃO: o SIM bruto tem ", nrow(DC_BRUTO), " óbitos com menção à DC e a base .rds, ",
          nrow(MORT_DC), ". Veja as abas conf_obitos_por_ano e conf_dc_bruto_vs_rds.")
}

# =============================================================================
# 6. EXPORTAR
# =============================================================================

write_xlsx(c(numeros, impacto, conferencias),
           file.path(pasta_saida, "Apendice_A_numeros.xlsx"))

if (requireNamespace("gt", quietly = TRUE)) {
  tabela_A <- function(variaveis, rotulos, titulo, dicotomicas = character(0)) {
    tipo  <- if (length(dicotomicas)) setNames(rep(list("dichotomous"), length(dicotomicas)), dicotomicas) else NULL
    valor <- if (length(dicotomicas)) setNames(rep(list(TRUE), length(dicotomicas)), dicotomicas) else NULL
    tbl_summary(dados_A, by = GRUPO, include = all_of(variaveis), label = as.list(rotulos),
                type = tipo, value = valor,
                statistic = list(all_categorical() ~ "{n} ({p}%)"),
                digits = list(all_categorical() ~ c(0, 1)), missing = "no") %>%
      add_n() %>%
      add_overall(last = FALSE, col_label = "**Total Avaliado**") %>%
      modify_header(label ~ "**Variável**") %>%
      modify_spanning_header(all_stat_cols() ~ "**Grupo de Análise**") %>%
      bold_labels() %>%
      as_gt() %>%
      gt::tab_header(title = titulo) %>%
      gt::tab_source_note(source_note = "Fonte: MS/SVS/DATASUS, 2026.") %>%
      gt::tab_options(table.font.size = 12, table.font.names = "Times New Roman",
                      table.border.top.style = "solid", table.border.top.width = gt::px(2),
                      table.border.top.color = "black", table.border.bottom.style = "solid",
                      table.border.bottom.width = gt::px(2), table.border.bottom.color = "black",
                      column_labels.border.bottom.style = "solid",
                      column_labels.border.bottom.width = gt::px(1),
                      column_labels.border.bottom.color = "black")
  }
  tabelas_gt <- list(
    Tabela_A1 = tabela_A(names(vars_A1), vars_A1,
                         "Tabela A1 – Características sociodemográficas dos óbitos, segundo grupo de análise, Brasil, 2001-2024."),
    Tabela_A2 = tabela_A(names(vars_A2), vars_A2,
                         "Tabela A2 – Características territoriais e assistenciais dos óbitos, segundo grupo de análise, Brasil, 2001-2024."),
    Tabela_A3 = tabela_A(names(vars_A3), vars_A3,
                         "Tabela A3 – Perfil de comorbidades dos óbitos, segundo grupo de análise, Brasil, 2001-2024.",
                         dic_A3)
  )
  iwalk(tabelas_gt, function(g, nome) {
    ok <- tryCatch({ gt::gtsave(g, paste0(nome, ".docx"), path = pasta_saida); TRUE }, error = function(e) FALSE)
    if (!ok) gt::gtsave(g, paste0(nome, ".html"), path = pasta_saida)
  })
}

reset_gtsummary_theme()
message("Pronto. Arquivos em: ", pasta_saida)
