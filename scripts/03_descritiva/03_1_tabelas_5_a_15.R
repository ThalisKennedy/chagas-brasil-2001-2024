# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 2 - Análises Descritivas/Apêndice D/TABELAS_5_A_15_script_unico.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 2 - Análises Descritivas/Apêndice D"))

# =============================================================================
# TABELAS 5 A 15 – ANÁLISE DESCRITIVA DO TEXTO PRINCIPAL (SCRIPT ÚNICO)
# -----------------------------------------------------------------------------
# Refaz as tabelas descritivas do texto principal a partir das MESMAS bases do
# Apêndice D, com as correções de 20/10/2026:
#   1. Testes: qui-quadrado de Pearson (com correção de Yates nas tabelas 2x2,
#      padrão do chisq.test); teste exato de Fisher quando alguma frequência
#      esperada < 5, com valor-p simulado por Monte Carlo (10.000 réplicas) nas
#      tabelas maiores que 2x2. Nos scripts antigos, `all_categorical() ~
#      "chisq.test"` vinha por último no add_p() e anulava as regras de Fisher e
#      de tendência (no gtsummary, a última regra vence; "chisq.test.trend" nem
#      existe no pacote): todas as variáveis tinham recebido qui-quadrado.
#   2. Categorias "Não se aplica"/"Não realizado" ficam na tabela, mas fora do
#      teste (nota 4 das tabelas).
#   3. Valores-p com três casas decimais (o padrão do gtsummary mostra p >= 0,2
#      com uma casa, que virou "0,400" etc. na montagem das tabelas).
#   4. Linhas "Principais ...": indicador sim/não só para quem teve ao menos um
#      item, de modo que % e valor-p usam o mesmo denominador.
#   5. Classificação de autoctonia com as três categorias (o script antigo
#      apagava os não autóctones ao fixar os níveis do fator).
#   6. Ano do diagnóstico (e-SUS) convertido para número antes de categorizar
#      (o script antigo comparava texto com número e jogava os 314 anos em
#      branco em "Antes de 2000").
#   7. e-SUS: uso de medicamento específico recalculado a partir de TRAT_BNZ,
#      HIST_BNZ e TRAT_NFX (como no script DCCRBR23) e modo de suspeita
#      agrupado como nas tabelas; "Grau não informado" tratado como ignorado.
#   8. Semente fixada imediatamente antes de cada teste de Monte Carlo, para o
#      valor-p não depender da ordem das variáveis.
#
# Saídas (pasta_saida):
#   Tabelas_5_a_15_numeros.xlsx – uma aba por tabela, com n, % e valor-p já
#     formatados como na dissertação, uma aba com o teste de cada variável e
#     abas de conferência;
#   Tabela_5.docx ... Tabela_15.docx – se o pacote gt estiver instalado.
# =============================================================================

library(tidyverse)
library(gtsummary)
library(writexl)

theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")

# -----------------------------------------------------------------------------
# 0. CONFIGURAÇÃO – ajuste aqui
# -----------------------------------------------------------------------------
if (!exists("pasta_bases")) {
  pasta_bases <- file.path(DIR_DADOS, "Análises/Parte 2 - Análises Descritivas")
}
if (!exists("pasta_saida")) pasta_saida <- file.path(pasta_bases, "Tabelas 5 a 15")
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

arq_sinan <- "CHAGAS_AGUDA_FILTRADO_2_2001_2024.rds"
arq_esus  <- "CHAGAS_CRONICA_FINAL_TRATADO.rds"
arq_sim   <- "MORT_CHAGAS_FINAL_2_TRATADO_COMPLETO.rds"

semente            <- 2026
replicas_mc        <- 10000
termos_para_anular <- c("Ignorado/Em branco", "Ignorado", "Grau não informado")
fora_do_teste      <- c("Não se aplica", "Não realizado")

localizar <- function(arquivo) {
  candidatos <- unique(c(arquivo, file.path(pasta_bases, arquivo),
                         file.path(pasta_bases, "Apêndice D", arquivo)))
  achado <- candidatos[file.exists(candidatos)][1]
  if (is.na(achado)) stop("Arquivo não encontrado: ", arquivo, call. = FALSE)
  achado
}

# -----------------------------------------------------------------------------
# Funções auxiliares
# -----------------------------------------------------------------------------

# "Ignorado/Em branco" vira NA (fora do denominador), mantendo a ordem dos níveis
anular_ignorados <- function(df) {
  df %>%
    mutate(
      across(where(is.factor),    ~ factor(.x, levels = setdiff(levels(.x), termos_para_anular))),
      across(where(is.character), ~ if_else(.x %in% termos_para_anular, NA_character_, .x))
    )
}

# Ordena e renomeia as categorias. `niveis` pode ter nomes: c("Novo rótulo" =
# "valor na base"). Categorias não previstas são mantidas no fim.
ordenar <- function(x, niveis = NULL) {
  x <- as.character(x)
  if (is.null(niveis)) niveis <- sort(unique(x[!is.na(x)]))
  rotulos <- if (is.null(names(niveis))) niveis else ifelse(names(niveis) == "", niveis, names(niveis))
  x <- unname(ifelse(x %in% niveis, rotulos[match(x, niveis)], x))
  extras <- setdiff(sort(unique(x[!is.na(x)])), rotulos)
  factor(x, levels = unique(c(rotulos, extras)))
}

# Indicador sim/não de um campo-resumo, só para quem teve ao menos um item
detectar <- function(texto, padrao_regex, tem_algum) {
  case_when(
    is.na(tem_algum) | !tem_algum ~ NA,
    is.na(texto)                  ~ NA,
    TRUE ~ str_detect(texto, regex(padrao_regex, ignore_case = TRUE))
  )
}

# Teste usado em todas as variáveis (mesma regra do Apêndice D)
teste_p <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  tab <- tab[!rownames(tab) %in% fora_do_teste, , drop = FALSE]
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return(data.frame(p.value = NA_real_, method = "Não calculado"))
  }
  esperado <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  if (any(esperado < 5)) {
    set.seed(semente)
    p <- stats::fisher.test(tab, simulate.p.value = TRUE, B = replicas_mc)$p.value
    metodo <- if (all(dim(tab) == 2)) "Fisher exato" else "Fisher (Monte Carlo)"
  } else {
    p <- suppressWarnings(stats::chisq.test(tab)$p.value)
    metodo <- if (all(dim(tab) == 2)) "Qui-quadrado (Yates)" else "Qui-quadrado"
  }
  data.frame(p.value = p, method = metodo)
}

p_tres_casas <- function(x) style_pvalue(x, digits = 3)

# Tabela no formato do texto principal (gtsummary)
#   estrato     : coluna dos grupos (faixa etária ou tipo de menção)
#   dicotomicas : indicadores lógicos exibidos só com a linha "Sim"
tabela_texto <- function(dados, estrato, variaveis, rotulos, dicotomicas = character(0),
                         rotulo_estrato, rotulo_total = "**Total**") {
  dados <- dados %>% filter(!is.na(.data[[estrato]]))
  tipo  <- if (length(dicotomicas)) setNames(rep(list("dichotomous"), length(dicotomicas)), dicotomicas) else NULL
  valor <- if (length(dicotomicas)) setNames(rep(list(TRUE), length(dicotomicas)), dicotomicas) else NULL

  tbl_summary(
    data      = dados,
    by        = all_of(estrato),
    include   = all_of(variaveis),
    label     = as.list(rotulos),
    type      = tipo,
    value     = valor,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits    = list(all_categorical() ~ c(0, 1)),
    missing   = "no"
  ) %>%
    add_n() %>%
    add_p(test = everything() ~ teste_p, pvalue_fun = p_tres_casas) %>%
    add_overall(last = FALSE, col_label = rotulo_total) %>%
    modify_header(label ~ "**Variável**") %>%
    modify_spanning_header(all_stat_cols() ~ rotulo_estrato) %>%
    bold_labels()
}

# Números de uma tabela no formato da dissertação: uma linha por rótulo de
# variável (n total, n por grupo e valor-p) e uma por categoria ("n (%)").
# Variáveis dicotômicas ocupam uma única linha, como nas tabelas.
fmt_n   <- function(x) style_number(x, big.mark = ".", decimal.mark = ",")
fmt_pct <- function(x) style_number(x * 100, digits = 1, big.mark = ".", decimal.mark = ",")

numeros_tabela <- function(dados, estrato, variaveis, rotulos, dicotomicas = character(0)) {
  dados  <- dados %>% filter(!is.na(.data[[estrato]]))
  grupos <- levels(droplevels(factor(dados[[estrato]])))
  map_dfr(variaveis, function(v) {
    d   <- dados %>% filter(!is.na(.data[[v]]))
    tst <- teste_p(dados, v, estrato)
    n_g <- map_chr(grupos, ~ fmt_n(sum(d[[estrato]] == .x)))
    if (v %in% dicotomicas) {
      sim <- d %>% filter(.data[[v]] %in% TRUE)
      cel <- function(sub, tot) paste0(fmt_n(nrow(sub)), " (", fmt_pct(nrow(sub) / tot), "%)")
      tibble(variavel = v, tipo = "dicotomica", rotulo = rotulos[[v]],
             n_total = fmt_n(nrow(d)), n_grupos = paste(n_g, collapse = " | "),
             stat_total = cel(sim, nrow(d)),
             stat_grupos = paste(map_chr(grupos, ~ cel(filter(sim, .data[[estrato]] == .x),
                                                        sum(d[[estrato]] == .x))), collapse = " | "),
             p = p_tres_casas(tst$p.value), teste = tst$method)
    } else {
      niveis <- levels(droplevels(factor(d[[v]], levels = levels(factor(dados[[v]])))))
      if (is.factor(dados[[v]])) niveis <- levels(dados[[v]])
      cab <- tibble(variavel = v, tipo = "rotulo", rotulo = rotulos[[v]],
                    n_total = fmt_n(nrow(d)), n_grupos = paste(n_g, collapse = " | "),
                    stat_total = "", stat_grupos = "",
                    p = p_tres_casas(tst$p.value), teste = tst$method)
      cats <- map_dfr(niveis, function(nv) {
        tot <- sum(d[[v]] == nv)
        tibble(variavel = v, tipo = "categoria", rotulo = nv, n_total = "", n_grupos = "",
               stat_total = paste0(fmt_n(tot), " (", fmt_pct(tot / nrow(d)), "%)"),
               stat_grupos = paste(map_chr(grupos, function(g) {
                 ng <- sum(d[[estrato]] == g); x <- sum(d[[v]] == nv & d[[estrato]] == g)
                 paste0(fmt_n(x), " (", fmt_pct(if (ng > 0) x / ng else 0), "%)")
               }), collapse = " | "),
               p = "", teste = "")
      })
      bind_rows(cab, cats)
    }
  })
}

# Denominador das linhas "Principais ..." (quem teve ao menos um item), por grupo
denominador_principais <- function(dados, estrato, indicador, bloco) {
  dados %>%
    filter(!is.na(.data[[estrato]]), !is.na(.data[[indicador]])) %>%
    count(grupo = as.character(.data[[estrato]])) %>%
    { bind_rows(tibble(grupo = "Total", n = sum(.$n)), .) } %>%
    mutate(bloco = bloco, .before = 1)
}

# =============================================================================
# 1. BASES
# =============================================================================

CHAGAS_AGUDA   <- readRDS(localizar(arq_sinan))
CHAGAS_CRONICA <- readRDS(localizar(arq_esus))
MORT_CHAGAS    <- readRDS(localizar(arq_sim))

message("Registros – SINAN: ", nrow(CHAGAS_AGUDA), " | e-SUS: ", nrow(CHAGAS_CRONICA),
        " | SIM: ", nrow(MORT_CHAGAS))

# =============================================================================
# 2. SINAN – DC AGUDA (2001–2024) – TABELAS 5 A 8
# =============================================================================

sinan <- CHAGAS_AGUDA %>%
  mutate(DROGA_FINAL = if_else(DROGA_FINAL %in% "Não se aplica", NA_character_, as.character(DROGA_FINAL))) %>%
  anular_ignorados() %>%
  mutate(
    FAIXA_ETARIA_GRUPO   = ordenar(FAIXA_ETARIA_GRUPO, c("0 a 19 anos", "20 a 59 anos", "60 anos ou mais")),
    SEXO_FINAL           = ordenar(SEXO_FINAL, c("Masculino", "Feminino")),
    RACACOR_FINAL        = ordenar(RACACOR_FINAL, c("Branca", "Parda", "Preta", "Amarela", "Indígena")),
    ESCOLARIDADE_FINAL   = ordenar(ESCOLARIDADE_FINAL, c("Nenhuma", "1 a 7 anos", "8 a 11 anos", "12 anos ou mais", "Não se aplica")),
    PERFIL_OCUPACIONAL   = ordenar(PERFIL_OCUPACIONAL, c("Agropecuário/Rural", "Manual Urbano/Serviços/Industrial",
                                                         "Não Manual/Intelectual/Técnico", "Aposentado/Pensionista",
                                                         "Inativos/Sem Ocupação")),
    OPORTUNIDADE_NOTIFICACAO = ordenar(OPORTUNIDADE_NOTIFICACAO, c("Oportuna (≤ 7 dias)" = "Oportuna (≤7 dias)",
                                                                   "Tardia (> 7 dias)" = "Tardia (>7 dias)")),
    REGIAO_RESIDENCIA    = ordenar(REGIAO_RESIDENCIA, c("Norte", "Nordeste", "Sul", "Sudeste", "Centro-Oeste")),
    FLUXO_ASSISTENCIA    = ordenar(FLUXO_ASSISTENCIA, c("Sem Deslocamento", "Deslocamento Intermunicipal",
                                                        "Deslocamento Interestadual",
                                                        "Deslocamento Inter-regional" = "Deslocamento Interregional")),
    LOCAL_INFECCAO_FINAL = ordenar(LOCAL_INFECCAO_FINAL, c("Domicílio", "Unidade de Hemoterapia", "Laboratório", "Outro")),
    PRESENCA_FINAL       = ordenar(PRESENCA_FINAL, c("Sim", "Não", "Não se aplica" = "Não realizado")),
    TPAUTOCTO_FINAL_2007 = ordenar(TPAUTOCTO_FINAL_2007, c("Autóctone", "Importado" = "Não Autóctone", "Indeterminado")),
    MODO_INFECCAO_FINAL  = ordenar(MODO_INFECCAO_FINAL, c("Oral", "Vetorial", "Acidental", "Vertical/Transplacentária",
                                                          "Transfusional", "Outra")),
    ASSINTOMA_FINAL      = ordenar(ASSINTOMA_FINAL, c("Sim", "Não")),
    USOU_MEDICAMENTO     = ordenar(USOU_MEDICAMENTO, c("Nenhum medicamento", "Sim, somente específico",
                                                       "Sim, somente sintomático", "Sim, ambos")),
    DROGA_FINAL          = ordenar(DROGA_FINAL, c("Benznidazol", "Outra")),
    CRITERIO_FINAL       = ordenar(CRITERIO_FINAL, c("Laboratorial", "Clínico-Epidemiológico", "Clínico", "Outros Critérios")),
    EVOLUCAO_FINAL       = ordenar(EVOLUCAO_FINAL, c("Cura", "Óbito por Chagas", "Óbito por outras causas", "Óbito em investigação")),
    QTD_SINTOMAS_CAT     = ordenar(QTD_SINTOMAS_CAT, c("Nenhum sintoma", "Um sintoma", "Dois sintomas", "Três sintomas",
                                                       "Quatro ou mais sintomas")),
    QTD_SORO_CAT         = ordenar(QTD_SORO_CAT, c("Nenhuma sorologia reagente", "Uma reagente", "Duas reagentes",
                                                   "Três reagentes", "Quatro ou mais reagentes")),
    QTD_PARASITO_CAT     = ordenar(QTD_PARASITO_CAT, c("Nenhum exame positivo", "Um positivo", "Dois positivos",
                                                       "Três ou mais positivos")),
    QTD_MEDIDAS_CAT      = ordenar(QTD_MEDIDAS_CAT, c("Nenhuma medida tomada", "Uma medida", "Duas medidas",
                                                      "Três ou mais medidas")),
    # Quem teve ao menos um item (denominador das linhas "Principais ...")
    TEM_SINTOMA  = !is.na(QTD_SINTOMAS_CAT) & QTD_SINTOMAS_CAT != "Nenhum sintoma",
    TEM_SORO     = !is.na(QTD_SORO_CAT)     & QTD_SORO_CAT     != "Nenhuma sorologia reagente",
    TEM_PARASITO = !is.na(QTD_PARASITO_CAT) & QTD_PARASITO_CAT != "Nenhum exame positivo",
    TEM_MEDIDA   = !is.na(QTD_MEDIDAS_CAT)  & QTD_MEDIDAS_CAT  != "Nenhuma medida tomada"
  ) %>%
  mutate(
    SINTOMA_FEBRE          = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Febre", TEM_SINTOMA),
    SINTOMA_ASTENIA        = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Astenia|Fraqueza|Cansa", TEM_SINTOMA),
    SINTOMA_EDEMA          = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Edema|Inchaço|Face|Pernas", TEM_SINTOMA),
    SINTOMA_ARRITMIA       = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Arritmia|Palpitat|Batedeira", TEM_SINTOMA),
    SINTOMA_DOR            = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Dor|Cefaleia|Cabeca|Corpo", TEM_SINTOMA),
    SINTOMA_HEPATOMEGALIA  = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Hepatoespleno|Hepato|Figado", TEM_SINTOMA),
    SINTOMA_AUMENTO        = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Aumento", TEM_SINTOMA),
    SINTOMA_DIGESTIVO      = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Anorexia|Hiporexia|Vomito|Diarreia|Nausea", TEM_SINTOMA),
    SINTOMA_ESPLENOMEGALIA = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Espleno|Baco", TEM_SINTOMA),
    SINTOMA_ANOREXIA       = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Anorexia", TEM_SINTOMA),
    SINTOMA_ICC            = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "ICC|Congest|Cardia", TEM_SINTOMA),
    SINTOMA_CHAGOMA        = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Chagoma|Inoculacao", TEM_SINTOMA),
    SINTOMA_GANGLIOS       = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Adenomegalia|Ganglio|Adeno", TEM_SINTOMA),
    SINTOMA_HIPOREXIA      = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Hiporexia", TEM_SINTOMA),
    SINTOMA_POLIADENO      = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Poliadeno", TEM_SINTOMA),
    SINTOMA_MENINGO        = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Meningo", TEM_SINTOMA),

    SORO_IFI   = detectar(LISTA_SOROLOGIAS_REAGENTES, "IFI", TEM_SORO),
    SORO_ELISA = detectar(LISTA_SOROLOGIAS_REAGENTES, "ELISA", TEM_SORO),
    SORO_HAI   = detectar(LISTA_SOROLOGIAS_REAGENTES, "HAI", TEM_SORO),

    PARASITO_DIRETO   = detectar(LISTA_PARASITO_POSITIVOS, "Direto|Gota|Fresco", TEM_PARASITO),
    PARASITO_HEMOCULT = detectar(LISTA_PARASITO_POSITIVOS, "Hemo|Cultura", TEM_PARASITO),
    PARASITO_STROUT   = detectar(LISTA_PARASITO_POSITIVOS, "Strout|Micro", TEM_PARASITO),
    PARASITO_XENO     = detectar(LISTA_PARASITO_POSITIVOS, "Xenodiag|Xeno", TEM_PARASITO),
    PARASITO_HISTO    = detectar(LISTA_PARASITO_POSITIVOS, "Histopatologia|Histo", TEM_PARASITO),
    PARASITO_OUTRO    = detectar(LISTA_PARASITO_POSITIVOS, "Outro|PCR", TEM_PARASITO),

    MEDIDA_PESQ_VETOR = detectar(LISTA_MEDIDAS_TOMADAS, "Pesq|Vetor", TEM_MEDIDA),
    MEDIDA_VIGILANCIA = detectar(LISTA_MEDIDAS_TOMADAS, "Vigil[aâ]ncia|Investig|Inquerito|Inquérito|Coleta|Epidemiol", TEM_MEDIDA),
    MEDIDA_EDUCACAO   = detectar(LISTA_MEDIDAS_TOMADAS, "Educa|Orienta", TEM_MEDIDA),
    MEDIDA_BORRIFACAO = detectar(LISTA_MEDIDAS_TOMADAS, "Borrif|Fiscaliza|Inseticida", TEM_MEDIDA),
    MEDIDA_BIOSSEG    = detectar(LISTA_MEDIDAS_TOMADAS, "Biosseg", TEM_MEDIDA)
  )

estrato_sinan <- "**Faixa Etária**"

vars_5 <- c(SEXO_FINAL = "Sexo", RACACOR_FINAL = "Raça/Cor", ESCOLARIDADE_FINAL = "Escolaridade",
            PERFIL_OCUPACIONAL = "Perfil Ocupacional")
vars_6 <- c(OPORTUNIDADE_NOTIFICACAO = "Tempo de Notificação", REGIAO_RESIDENCIA = "Região de Residência",
            FLUXO_ASSISTENCIA = "Fluxo Assistencial", LOCAL_INFECCAO_FINAL = "Local Provável de Infecção",
            PRESENCA_FINAL = "Vestígios de Triatomíneos no Intradomicílio?",
            TPAUTOCTO_FINAL_2007 = "Classificação de Autoctonia (Após 2007)")
vars_7 <- c(MODO_INFECCAO_FINAL = "Modo Provável de Infecção", ASSINTOMA_FINAL = "Paciente Assintomático?",
            USOU_MEDICAMENTO = "Uso de Medicamento?", DROGA_FINAL = "Medicamento Utilizado",
            CRITERIO_FINAL = "Critério de Confirmação", EVOLUCAO_FINAL = "Evolução do Caso")
vars_8 <- c(QTD_SINTOMAS_CAT = "Quantidade de Sintomas", SINTOMA_FEBRE = "Febre", SINTOMA_ASTENIA = "Astenia",
            SINTOMA_EDEMA = "Edema", SINTOMA_ARRITMIA = "Arritmia", SINTOMA_DOR = "Cefaleia",
            SINTOMA_HEPATOMEGALIA = "Hepatomegalia", SINTOMA_AUMENTO = "Aumento de Volume",
            SINTOMA_DIGESTIVO = "Sintomas Digestivos", SINTOMA_ESPLENOMEGALIA = "Esplenomegalia",
            SINTOMA_ANOREXIA = "Anorexia", SINTOMA_ICC = "Insuficiência Cardíaca", SINTOMA_CHAGOMA = "Chagoma",
            SINTOMA_GANGLIOS = "Adenomegalia/Gânglios", SINTOMA_HIPOREXIA = "Hiporexia",
            SINTOMA_POLIADENO = "Poliadenopatia", SINTOMA_MENINGO = "Sinais Meníngeos",
            QTD_SORO_CAT = "Quantidade de Sorologias Reagentes", SORO_IFI = "IFI Reagente",
            SORO_ELISA = "ELISA Reagente", SORO_HAI = "HAI Reagente",
            QTD_PARASITO_CAT = "Quantidade de Parasitológicos Positivos",
            PARASITO_DIRETO = "Exame Direto/Gota Espessa Positivo", PARASITO_HEMOCULT = "Hemocultura Positiva",
            PARASITO_STROUT = "Strout/Micro-hematócrito Positivo", PARASITO_XENO = "Xenodiagnóstico Positivo",
            PARASITO_HISTO = "Histopatologia Positiva", PARASITO_OUTRO = "Outros",
            QTD_MEDIDAS_CAT = "Quantidade de Medidas Tomadas", MEDIDA_PESQ_VETOR = "Pesquisa de Vetor",
            MEDIDA_VIGILANCIA = "Vigilância", MEDIDA_EDUCACAO = "Educação em Saúde",
            MEDIDA_BORRIFACAO = "Fiscalização/Borrifação", MEDIDA_BIOSSEG = "Biossegurança")
dic_8 <- grep("^(SINTOMA|SORO|PARASITO|MEDIDA)_", names(vars_8), value = TRUE)

DEN_8 <- bind_rows(
  denominador_principais(sinan, "FAIXA_ETARIA_GRUPO", "SINTOMA_FEBRE",     "Principais Sintomas"),
  denominador_principais(sinan, "FAIXA_ETARIA_GRUPO", "SORO_IFI",          "Principais Sorologias Reagentes"),
  denominador_principais(sinan, "FAIXA_ETARIA_GRUPO", "PARASITO_DIRETO",   "Principais Parasitológicos Positivos"),
  denominador_principais(sinan, "FAIXA_ETARIA_GRUPO", "MEDIDA_PESQ_VETOR", "Principais Medidas Tomadas")
)

# =============================================================================
# 3. e-SUS NOTIFICA – DC CRÔNICA (2023) – TABELAS 9 A 12
# =============================================================================

esus <- CHAGAS_CRONICA %>%
  mutate(
    ANO_DIAG_NUM = suppressWarnings(as.numeric(as.character(ANO_DIAG))),
    ANO_DIAG_CAT = case_when(
      ANO_DIAG_NUM < 2000                         ~ "Antes de 2000",
      ANO_DIAG_NUM >= 2000 & ANO_DIAG_NUM <= 2009 ~ "2000 a 2009",
      ANO_DIAG_NUM >= 2010 & ANO_DIAG_NUM <= 2019 ~ "2010 a 2019",
      ANO_DIAG_NUM >= 2020                        ~ "2020 em diante",
      TRUE ~ NA_character_
    ),
    DIF_DIAG_NOTIF_CAT = case_when(
      DIF_DIAG_NOTIF == 0                        ~ "Notificação Oportuna (Mesmo ano)",
      DIF_DIAG_NOTIF == 1                        ~ "Notificação Recente (1 ano)",
      DIF_DIAG_NOTIF > 1 & DIF_DIAG_NOTIF <= 10  ~ "Notificação Tardia (2 a 10 anos)",
      DIF_DIAG_NOTIF > 10                        ~ "Notificação Histórica (Mais de 10 anos)",
      TRUE ~ NA_character_
    ),
    QTD_SORO_CAT = case_when(
      is.na(QTD_SOROLOGIAS_REAGENTES)  ~ NA_character_,
      QTD_SOROLOGIAS_REAGENTES == 0    ~ "Nenhuma sorologia reagente",
      QTD_SOROLOGIAS_REAGENTES == 1    ~ "Uma reagente",
      QTD_SOROLOGIAS_REAGENTES == 2    ~ "Duas reagentes",
      QTD_SOROLOGIAS_REAGENTES == 3    ~ "Três reagentes",
      QTD_SOROLOGIAS_REAGENTES >= 4    ~ "Quatro ou mais reagentes"
    ),
    QTD_EXAMES_CAT = case_when(
      is.na(QTD_EXAMES_ALTERADOS)  ~ NA_character_,
      QTD_EXAMES_ALTERADOS == 0    ~ "Nenhum exame alterado",
      QTD_EXAMES_ALTERADOS == 1    ~ "Um alterado",
      QTD_EXAMES_ALTERADOS == 2    ~ "Dois alterados",
      QTD_EXAMES_ALTERADOS == 3    ~ "Três alterados",
      QTD_EXAMES_ALTERADOS >= 4    ~ "Quatro ou mais alterados"
    ),
    # Uso de medicamento específico: mesma regra do script DCCRBR23
    USOU_MEDICAMENTO = case_when(
      TRAT_BNZ_FINAL %in% "Sim" | HIST_BNZ_FINAL %in% "Sim" ~ "Sim, Benznidazol",
      TRAT_NFX_FINAL %in% "Sim" ~ "Sim, Nifurtimox",
      TRAT_BNZ_FINAL %in% "Não" & TRAT_NFX_FINAL %in% "Não" & !HIST_BNZ_FINAL %in% "Sim" ~ "Não",
      TRUE ~ NA_character_
    ),
    # Modo de suspeita agrupado como nas tabelas
    MO_SUSPEIT_FINAL = case_when(
      MO_SUSPEIT_FINAL %in% c("Rastreamento/Busca Ativa", "Outras Buscas Ativas") ~ "Rastreamento/Busca Ativa",
      MO_SUSPEIT_FINAL %in% c("Banco de Sangue", "Triagem de Doador", "Receptor de Transplante",
                              "Pré-Natal", "Serviço HIV/AIDS", "Outros") ~ "Outros",
      TRUE ~ as.character(MO_SUSPEIT_FINAL)
    )
  ) %>%
  anular_ignorados() %>%
  mutate(
    FAIXA_ETARIA_GRUPO = case_when(
      FAIXA_ETARIA %in% c("0 a 45 anos", "46 a 59 anos")     ~ "0 a 59 anos",
      FAIXA_ETARIA %in% c("60 a 74 anos", "75 anos ou mais") ~ "60 anos ou mais",
      TRUE ~ NA_character_
    ),
    FAIXA_ETARIA_GRUPO = ordenar(FAIXA_ETARIA_GRUPO, c("0 a 59 anos", "60 anos ou mais")),
    SEXO_FINAL         = ordenar(SEXO_FINAL, c("Masculino", "Feminino")),
    RACACOR_FINAL      = ordenar(RACACOR_FINAL, c("Branca", "Parda", "Preta", "Amarela", "Indígena")),
    ESCOLARIDADE_FINAL = ordenar(ESCOLARIDADE_FINAL, c("Nenhuma", "1 a 7 anos", "8 a 11 anos", "12 anos ou mais", "Não se aplica")),
    PERFIL_OCUPACIONAL = ordenar(PERFIL_OCUPACIONAL, c("Agropecuário/Rural", "Manual Urbano/Serviços/Industrial",
                                                       "Não Manual/Intelectual/Técnico",
                                                       "Membros das Forças Armadas/Polícia", "Outros/Mal Definidos")),
    COMU_TRAD_FINAL    = ordenar(COMU_TRAD_FINAL, c("Sim", "Não")),
    ANO_DIAG_CAT       = ordenar(ANO_DIAG_CAT, c("Antes de 2000", "2000 a 2009", "2010 a 2019", "2020 em diante")),
    DIF_DIAG_NOTIF_CAT = ordenar(DIF_DIAG_NOTIF_CAT, c("Notificação Oportuna (Mesmo ano)", "Notificação Recente (1 ano)",
                                                       "Notificação Tardia (2 a 10 anos)", "Notificação Histórica (Mais de 10 anos)")),
    REGIAO_RESIDENCIA  = ordenar(REGIAO_RESIDENCIA, c("Sudeste", "Nordeste", "Centro-Oeste", "Sul", "Norte")),
    CLASSIFICACAO_MIGRATORIA = ordenar(CLASSIFICACAO_MIGRATORIA, c("Não Migrante", "Migrante Interestadual",
                                                                   "Migrante Inter-regional" = "Migrante Interregional")),
    FLUXO_ASSISTENCIA  = ordenar(FLUXO_ASSISTENCIA, c("Sem Deslocamento", "Deslocamento Intermunicipal",
                                                      "Deslocamento Interestadual",
                                                      "Deslocamento Inter-regional" = "Deslocamento Interregional")),
    MO_SUSPEIT_FINAL   = ordenar(MO_SUSPEIT_FINAL, c("Rastreamento/Busca Ativa", "Demanda Espontânea: Hospital/Especializado",
                                                     "Demanda Espontânea: UBS", "Outros", "Comitê de Óbito")),
    FORMA_CLINICA      = ordenar(FORMA_CLINICA, c("Cardíaca", "Digestiva",
                                                  "Mista (Cardíaca + Digestiva)" = "Mista (Cardio + Digestiva)", "Nervosa",
                                                  "Não Especificada/Indeterminada" = "Indeterminada/Em investigação")),
    GRAVIDADE_CARDIACA = ordenar(GRAVIDADE_CARDIACA, c("Leve/Moderada", "Avançada")),
    FAMILIA_HISTORICO_DIAG = ordenar(FAMILIA_HISTORICO_DIAG, c("Sim", "Não")),
    USOU_MEDICAMENTO   = ordenar(USOU_MEDICAMENTO, c("Sim, Benznidazol", "Sim, Nifurtimox", "Não")),
    REATIVACAO_FINAL   = ordenar(REATIVACAO_FINAL, c("Sim", "Não")),
    ST_ENCERRA_FINAL   = ordenar(ST_ENCERRA_FINAL, c("Em acompanhamento", "Óbito por DC" = "Óbito por Doença de Chagas",
                                                     "Óbito por outras causas", "Em aberto", "Abandono")),
    PERFIL_COMORBIDADE = ordenar(PERFIL_COMORBIDADE, c("Nenhuma Identificada", "Uma Comorbidade",
                                                       "Multimorbidade (2 ou mais)" = "Multimorbidade (2+)")),
    QTD_SORO_CAT       = ordenar(QTD_SORO_CAT, c("Nenhuma sorologia reagente", "Uma reagente", "Duas reagentes",
                                                 "Três reagentes", "Quatro ou mais reagentes")),
    QTD_EXAMES_CAT     = ordenar(QTD_EXAMES_CAT, c("Nenhum exame alterado", "Um alterado", "Dois alterados",
                                                   "Três alterados", "Quatro ou mais alterados")),
    # Ao menos uma comorbidade: na ficha (perfil) ou só no texto livre (AVC, doença renal, DAC)
    TEM_COMORB = PERFIL_COMORBIDADE %in% c("Uma Comorbidade", "Multimorbidade (2 ou mais)") |
                 (!is.na(RESUMO_CLINICO_PRINCIPAIS) & RESUMO_CLINICO_PRINCIPAIS != "Sem as Principais Comorbidades"),
    TEM_SORO   = !is.na(QTD_SOROLOGIAS_REAGENTES) & QTD_SOROLOGIAS_REAGENTES > 0,
    TEM_EXAME  = !is.na(QTD_EXAMES_ALTERADOS) & QTD_EXAMES_ALTERADOS > 0
  ) %>%
  mutate(
    HAS         = detectar(RESUMO_CLINICO_PRINCIPAIS, "HAS|HIPERTENSAO", TEM_COMORB),
    DM          = detectar(RESUMO_CLINICO_PRINCIPAIS, "DM|DIABETES", TEM_COMORB),
    CARDIOPATIA = detectar(RESUMO_CLINICO_PRINCIPAIS, "Cardiopatia|DAC|ICC|Coracao|Isquemica", TEM_COMORB),
    AVC         = detectar(RESUMO_CLINICO_PRINCIPAIS, "AVC|AVE|Derrame", TEM_COMORB),
    DRC         = detectar(RESUMO_CLINICO_PRINCIPAIS, "DRC|Renal", TEM_COMORB),
    HIV         = detectar(RESUMO_CLINICO_PRINCIPAIS, "HIV|Aids|Imuno", TEM_COMORB),

    SORO_ELISA  = detectar(LISTA_SOROLOGIAS_REAGENTES, "ELISA", TEM_SORO),
    SORO_IFI    = detectar(LISTA_SOROLOGIAS_REAGENTES, "IFI|Imuno", TEM_SORO),
    SORO_HAI    = detectar(LISTA_SOROLOGIAS_REAGENTES, "HAI|Hemaglutinacao", TEM_SORO),
    SORO_QUIMIO = detectar(LISTA_SOROLOGIAS_REAGENTES, "Quimio", TEM_SORO),
    SORO_PCR    = detectar(LISTA_SOROLOGIAS_REAGENTES, "PCR|Molecular", TEM_SORO),

    EXAME_ECG     = detectar(LISTA_EXAMES_ALTERADOS, "ECG|Eletro", TEM_EXAME),
    EXAME_ECO     = detectar(LISTA_EXAMES_ALTERADOS, "Eco\\b|Ecocardiograma", TEM_EXAME),
    EXAME_RX      = detectar(LISTA_EXAMES_ALTERADOS, "RX|Torax|Raio", TEM_EXAME),
    EXAME_ESOFAGO = detectar(LISTA_EXAMES_ALTERADOS, "Esofago|EED", TEM_EXAME),
    EXAME_HOLTER  = detectar(LISTA_EXAMES_ALTERADOS, "Holter", TEM_EXAME),
    EXAME_COLON   = detectar(LISTA_EXAMES_ALTERADOS, "Colon|Enema", TEM_EXAME)
  )

estrato_esus <- "**Faixa Etária**"

vars_9  <- c(SEXO_FINAL = "Sexo", RACACOR_FINAL = "Raça/Cor", ESCOLARIDADE_FINAL = "Escolaridade",
             PERFIL_OCUPACIONAL = "Perfil Ocupacional", COMU_TRAD_FINAL = "Pertence a Comunidade Tradicional?")
vars_10 <- c(ANO_DIAG_CAT = "Ano do Diagnóstico", DIF_DIAG_NOTIF_CAT = "Tempo entre Diagnóstico e Notificação",
             REGIAO_RESIDENCIA = "Região de Residência", CLASSIFICACAO_MIGRATORIA = "Perfil Migratório",
             FLUXO_ASSISTENCIA = "Fluxo Assistencial")
vars_11 <- c(MO_SUSPEIT_FINAL = "Modo de Suspeita", FORMA_CLINICA = "Forma Clínica",
             GRAVIDADE_CARDIACA = "Classificação da Gravidade Cardíaca",
             FAMILIA_HISTORICO_DIAG = "Histórico Familiar de Chagas?",
             USOU_MEDICAMENTO = "Uso de Medicamento Específico?", REATIVACAO_FINAL = "Houve Reativação da Doença?",
             ST_ENCERRA_FINAL = "Situação de Encerramento")
vars_12 <- c(PERFIL_COMORBIDADE = "Perfil de Comorbidades", HAS = "Hipertensão Arterial", DM = "Diabetes Mellitus",
             CARDIOPATIA = "Cardiopatia ou Insuficiência Cardíaca", AVC = "Acidente Vascular Cerebral",
             DRC = "Doença Renal", HIV = "Coinfecção com HIV",
             QTD_SORO_CAT = "Quantidade de Sorologias Reagentes", SORO_ELISA = "ELISA Reagente", SORO_IFI = "IFI Reagente",
             SORO_HAI = "HAI Reagente", SORO_QUIMIO = "Quimioluminescência Reagente", SORO_PCR = "PCR Positivo",
             QTD_EXAMES_CAT = "Quantidade de Exames Alterados", EXAME_ECG = "Eletrocardiograma Alterado",
             EXAME_ECO = "Ecocardiograma Alterado", EXAME_RX = "Radiografia de Tórax Alterada",
             EXAME_ESOFAGO = "Radiografia de Esôfago Alterada", EXAME_HOLTER = "Holter Alterado",
             EXAME_COLON = "Radiografia de Cólon Alterada")
dic_12 <- c("HAS", "DM", "CARDIOPATIA", "AVC", "DRC", "HIV",
            "SORO_ELISA", "SORO_IFI", "SORO_HAI", "SORO_QUIMIO", "SORO_PCR",
            "EXAME_ECG", "EXAME_ECO", "EXAME_RX", "EXAME_ESOFAGO", "EXAME_HOLTER", "EXAME_COLON")

DEN_12 <- bind_rows(
  denominador_principais(esus, "FAIXA_ETARIA_GRUPO", "HAS",        "Principais Comorbidades"),
  denominador_principais(esus, "FAIXA_ETARIA_GRUPO", "SORO_ELISA", "Principais Sorologias Reagentes"),
  denominador_principais(esus, "FAIXA_ETARIA_GRUPO", "EXAME_ECG",  "Principais Exames Alterados")
)

# =============================================================================
# 4. SIM – ÓBITOS COM MENÇÃO À DC (2001–2024) – TABELAS 13 A 15
# =============================================================================

sim <- MORT_CHAGAS %>%
  anular_ignorados() %>%
  mutate(
    TIPO_MENCAO        = ordenar(TIPO_MENCAO, c("Causa Básica", "Causa Associada")),
    SEXO               = ordenar(SEXO, c("Masculino", "Feminino")),
    RACACOR            = ordenar(RACACOR, c("Branca", "Parda", "Preta", "Amarela", "Indígena")),
    FAIXA_ETARIA       = ordenar(FAIXA_ETARIA, c("0 a 45 anos", "46 a 59 anos", "60 a 74 anos", "75 anos ou mais")),
    ESCOLARIDADE       = ordenar(ESCOLARIDADE, c("Nenhuma", "1 a 7 anos", "8 a 11 anos", "12 anos ou mais", "Não se aplica")),
    ESTADO_CIVIL       = ordenar(ESTADO_CIVIL, c("Solteiro", "Casado", "Viúvo", "Separado/Divorciado", "União Estável")),
    PERFIL_OCUPACIONAL = ordenar(PERFIL_OCUPACIONAL, c("Aposentado/Pensionista", "Manual Urbano/Serviços/Industrial",
                                                       "Agropecuário/Rural", "Não Manual/Intelectual/Técnico",
                                                       "Inativos/Sem Ocupação" = "Dona de Casa/Estudante")),
    REGIAO_RESIDENCIA  = ordenar(REGIAO_RESIDENCIA, c("Sudeste", "Centro-Oeste", "Nordeste", "Sul", "Norte")),
    CLASSIFICACAO_MIGRATORIA = ordenar(CLASSIFICACAO_MIGRATORIA, c("Não Migrante", "Migrante Interestadual",
                                                                   "Migrante Inter-regional" = "Migrante Interregional")),
    FLUXO_ASSISTENCIA  = ordenar(FLUXO_ASSISTENCIA, c("Sem Deslocamento", "Deslocamento Intermunicipal",
                                                      "Deslocamento Interestadual",
                                                      "Deslocamento Inter-regional" = "Deslocamento Interregional")),
    LOCAL_OCORRENCIA   = ordenar(LOCAL_OCORRENCIA, c("Hospital", "Domicílio",
                                                     "Outros Estabelecimentos de Saúde" = "Outros Estab. Saúde",
                                                     "Outros Estabelecimentos de Saúde" = "Outros Estabelecimento de Saúde",  # --- rótulo do 01_2 atual (ver README)
                                                     "Via Pública", "Outros")),
    FASE_DOENCA        = ordenar(FASE_DOENCA, c("Aguda", "Crônica")),
    FORMA_CLINICA      = ordenar(FORMA_CLINICA, c("Cardíaca", "Digestiva",
                                                  "Mista (Cardíaca + Digestiva)" = "Mista (Cardio + Digestiva)",
                                                  "Nervosa", "Não Especificada/Indeterminada")),
    PERFIL_COMORBIDADE = ordenar(PERFIL_COMORBIDADE, c("Nenhuma Identificada", "Uma Comorbidade",
                                                       "Multimorbidade (2 ou mais)" = "Multimorbidade (2+)")),
    ASSISTMED          = ordenar(ASSISTMED, c("Sim", "Não")),
    NECROPSIA          = ordenar(NECROPSIA, c("Sim", "Não")),
    TEM_COMORB         = PERFIL_COMORBIDADE %in% c("Uma Comorbidade", "Multimorbidade (2 ou mais)")
  ) %>%
  mutate(
    HAS    = detectar(RESUMO_CLINICO, "HAS|HIPERTENSAO", TEM_COMORB),
    DAC    = detectar(RESUMO_CLINICO, "DAC|CORONARIOPATIA|ISQUEMICA", TEM_COMORB),
    DRC    = detectar(RESUMO_CLINICO, "DRC|RENAL", TEM_COMORB),
    DM     = detectar(RESUMO_CLINICO, "DM|DIABETES", TEM_COMORB),
    AVC    = detectar(RESUMO_CLINICO, "AVC|AVE|DERRAME", TEM_COMORB),
    CANCER = detectar(RESUMO_CLINICO, "CANCER|CÂNCER|NEOPLASIA", TEM_COMORB)
  )

estrato_sim <- "**Tipo de Menção da DC no Óbito**"
total_sim   <- "**Total de menções à DC**"

vars_13 <- c(SEXO = "Sexo", RACACOR = "Raça/Cor", FAIXA_ETARIA = "Faixa Etária", ESCOLARIDADE = "Escolaridade",
             ESTADO_CIVIL = "Estado Civil", PERFIL_OCUPACIONAL = "Perfil Ocupacional")
vars_14 <- c(REGIAO_RESIDENCIA = "Região de Residência", CLASSIFICACAO_MIGRATORIA = "Perfil Migratório",
             FLUXO_ASSISTENCIA = "Fluxo Assistencial", LOCAL_OCORRENCIA = "Local de Ocorrência do Óbito")
vars_15 <- c(FASE_DOENCA = "Fase da Doença", FORMA_CLINICA = "Forma Clínica",
             PERFIL_COMORBIDADE = "Perfil de Comorbidades", HAS = "Hipertensão",
             DAC = "Doença Arterial Coronariana", DRC = "Doença Renal", DM = "Diabetes",
             AVC = "Acidente Vascular Cerebral", CANCER = "Neoplasias",
             ASSISTMED = "Teve Assistência Médica no Óbito?", NECROPSIA = "Realizou Necropsia?")
dic_15 <- c("HAS", "DAC", "DRC", "DM", "AVC", "CANCER")

DEN_15 <- denominador_principais(sim, "TIPO_MENCAO", "HAS", "Principais Comorbidades")

# =============================================================================
# 5. TABELAS (gtsummary) E NÚMEROS
# =============================================================================

especificacao <- list(
  Tabela_5  = list(dados = "sinan", estrato = "FAIXA_ETARIA_GRUPO", vars = vars_5,  dic = character(0)),
  Tabela_6  = list(dados = "sinan", estrato = "FAIXA_ETARIA_GRUPO", vars = vars_6,  dic = character(0)),
  Tabela_7  = list(dados = "sinan", estrato = "FAIXA_ETARIA_GRUPO", vars = vars_7,  dic = character(0)),
  Tabela_8  = list(dados = "sinan", estrato = "FAIXA_ETARIA_GRUPO", vars = vars_8,  dic = dic_8),
  Tabela_9  = list(dados = "esus",  estrato = "FAIXA_ETARIA_GRUPO", vars = vars_9,  dic = character(0)),
  Tabela_10 = list(dados = "esus",  estrato = "FAIXA_ETARIA_GRUPO", vars = vars_10, dic = character(0)),
  Tabela_11 = list(dados = "esus",  estrato = "FAIXA_ETARIA_GRUPO", vars = vars_11, dic = character(0)),
  Tabela_12 = list(dados = "esus",  estrato = "FAIXA_ETARIA_GRUPO", vars = vars_12, dic = dic_12),
  Tabela_13 = list(dados = "sim",   estrato = "TIPO_MENCAO",        vars = vars_13, dic = character(0)),
  Tabela_14 = list(dados = "sim",   estrato = "TIPO_MENCAO",        vars = vars_14, dic = character(0)),
  Tabela_15 = list(dados = "sim",   estrato = "TIPO_MENCAO",        vars = vars_15, dic = dic_15)
)

numeros <- imap(especificacao, function(e, nome) {
  numeros_tabela(get(e$dados), e$estrato, names(e$vars), e$vars, e$dic)
})

testes_usados <- imap_dfr(numeros, function(x, nome) {
  x %>% filter(tipo != "categoria") %>% transmute(tabela = nome, variavel, rotulo, teste, p)
})

if (!exists("gerar_gt") || isTRUE(gerar_gt)) {
  tabelas <- imap(especificacao, function(e, nome) {
    tabela_texto(get(e$dados), e$estrato, names(e$vars), e$vars, e$dic,
                 rotulo_estrato = if (e$dados == "sim") estrato_sim else estrato_sinan,
                 rotulo_total   = if (e$dados == "sim") total_sim else "**Total**")
  })
}

# =============================================================================
# 6. CONFERÊNCIAS (itens da seção 2 das pendências)
# =============================================================================

conferencia <- list(
  autoctonia_por_faixa = sinan %>% filter(!is.na(FAIXA_ETARIA_GRUPO)) %>%
    count(TPAUTOCTO_FINAL_2007, FAIXA_ETARIA_GRUPO) %>%
    pivot_wider(names_from = FAIXA_ETARIA_GRUPO, values_from = n, values_fill = 0),
  esus_ano_notificacao = esus %>% count(ANO_NOTIFICACAO, FAIXA_ETARIA_GRUPO) %>%
    pivot_wider(names_from = FAIXA_ETARIA_GRUPO, values_from = n, values_fill = 0),
  esus_ano_diag_ausente = esus %>% filter(is.na(ANO_DIAG_NUM)) %>% count(FAIXA_ETARIA_GRUPO),
  esus_comorb_so_texto = esus %>% filter(PERFIL_COMORBIDADE == "Nenhuma Identificada", TEM_COMORB) %>%
    select(any_of(c("FAIXA_ETARIA_GRUPO", "REGIAO_RESIDENCIA", "RESUMO_CLINICO_PRINCIPAIS", "TEM_AVC"))),
  sim_sem_codigo_nas_linhas = MORT_CHAGAS %>%
    filter(CID_CHAGAS_ENCONTRADO == "Sem menção específica de CID Chagas") %>%
    count(TIPO_MENCAO, FORMA_CLINICA),
  denominadores_principais = bind_rows(DEN_8 %>% mutate(tabela = "8"), DEN_12 %>% mutate(tabela = "12"),
                                       DEN_15 %>% mutate(tabela = "15"))
)

# =============================================================================
# 7. EXPORTAR
# =============================================================================

write_xlsx(c(numeros, list(testes = testes_usados), conferencia),
           file.path(pasta_saida, "Tabelas_5_a_15_numeros.xlsx"))

if (exists("tabelas") && requireNamespace("gt", quietly = TRUE)) {
  estilo_gt <- function(tabela_gt) {
    tabela_gt %>%
      gt::tab_source_note(source_note = "Fonte: MS/SVS/DATASUS, 2026.") %>%
      gt::tab_options(
        row.striping.include_table_body = FALSE, column_labels.vlines.style = "none",
        table_body.vlines.style = "none", table.border.top.style = "solid",
        table.border.top.width = gt::px(2), table.border.top.color = "black",
        table.border.bottom.style = "solid", table.border.bottom.width = gt::px(2),
        table.border.bottom.color = "black", column_labels.border.bottom.style = "solid",
        column_labels.border.bottom.width = gt::px(1), column_labels.border.bottom.color = "black",
        table_body.hlines.style = "none", table.font.size = 12, table.font.names = "Times New Roman",
        heading.align = "left", source_notes.font.size = 10) %>%
      gt::cols_align(align = "left", columns = 1) %>%
      gt::cols_align(align = "right", columns = -1)
  }
  iwalk(tabelas, function(tb, nome) {
    g <- estilo_gt(as_gt(tb))
    ok <- tryCatch({ gt::gtsave(g, paste0(nome, ".docx"), path = pasta_saida); TRUE }, error = function(e) FALSE)
    if (!ok) gt::gtsave(g, paste0(nome, ".html"), path = pasta_saida)
  })
}

reset_gtsummary_theme()
message("Pronto. Saídas em: ", normalizePath(pasta_saida))
