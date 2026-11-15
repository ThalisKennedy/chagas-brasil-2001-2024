# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 2 - Análises Descritivas/Apêndice D/APENDICE_D_script_unico_v2.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 2 - Análises Descritivas/Apêndice D"))

# =============================================================================
# APÊNDICE D – ANÁLISES COMPLEMENTARES DA ETAPA DESCRITIVA (SCRIPT ÚNICO) – v2
# -----------------------------------------------------------------------------
# Gera, em uma única execução:
#   Tabela D1      – taxas bruta e padronizada por idade (por 100 mil hab.),
#                    por sexo, região de residência e faixa etária, para os
#                    três desfechos (SINAN, e-SUS Notifica e SIM);
#   Tabelas D2–D12 – as tabelas descritivas do texto principal (Tabelas 5 a
#                    15), estratificadas por região de residência.
#
# v2 (20/10/2026): mesmas regras do script das Tabelas 5 a 15
#   - teste de cada variável escolhido pela função teste_p(): qui-quadrado;
#     Fisher (Monte Carlo, 10.000 réplicas) se alguma frequência esperada < 5;
#     categorias "Não se aplica"/"Não realizado" fora do teste; semente fixada
#     antes de cada teste;
#   - valores-p com três casas decimais;
#   - linhas "Principais ..." com denominador de quem teve ao menos um item
#     (no % e no valor-p);
#   - e-SUS: uso de medicamento específico recalculado (TRAT_BNZ, HIST_BNZ e
#     TRAT_NFX), modo de suspeita agrupado e "Grau não informado" como ignorado;
#   - rótulos iguais aos das tabelas da dissertação;
#   - rodar_D1 <- FALSE pula a Tabela D1 (que exige os arquivos POPS).
#
# Saídas (na pasta_saida): uma tabela por arquivo (.docx; .html se o .docx
# falhar), Tabela_D1_taxas.xlsx e Tabelas_D2_a_D12_numeros.xlsx.
# =============================================================================

library(tidyverse)
library(gtsummary)
library(gt)
library(foreign)   # leitura dos arquivos POPS (.dbf)
library(writexl)

# Números em pt-BR (vírgula decimal, ponto de milhar) e rótulos do gtsummary em
# português ("Valor-p", nome dos testes). O tema é desfeito no fim do script.
theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")


# -----------------------------------------------------------------------------
# 0. CONFIGURAÇÃO – ajuste aqui
# -----------------------------------------------------------------------------

if (!exists("caminho_dados")) {
  caminho_dados <- paste0(file.path(DIR_DADOS, "IBGE/Dados"), "/")
}

# Pasta com os arquivos POPS do DATASUS (pop01.dbf, pop02.dbf, ..., pop24.dbf)
# e com pop_padrao_br2022.rds
pasta_pops <- caminho_dados

# Bases dos desfechos. O script procura cada arquivo na pasta de trabalho do R,
# na pasta de saída e na pasta acima dela; se estiverem em outro lugar,
# escreva aqui o caminho completo.
arq_sinan <- "CHAGAS_AGUDA_FILTRADO_2_2001_2024.rds"
arq_esus  <- "CHAGAS_CRONICA_FINAL_TRATADO.rds"
arq_sim   <- "MORT_CHAGAS_FINAL_2_TRATADO_COMPLETO.rds"

if (!exists("pasta_saida")) {
  pasta_saida <- file.path(DIR_DADOS, "Análises/Parte 2 - Análises Descritivas/Apêndice D")
}

# FALSE pula a Tabela D1 (útil para refazer só as Tabelas D2 a D12)
if (!exists("rodar_D1")) rodar_D1 <- TRUE

# Faixas etárias da Tabela D1 (iguais para os três desfechos).
# Os cortes coincidem com as faixas quinquenais da padronização.
cortes_idade_d1  <- c(0, 20, 40, 60, 75, Inf)
rotulos_idade_d1 <- c("0 a 19 anos", "20 a 39 anos", "40 a 59 anos",
                      "60 a 74 anos", "75 anos ou mais")

anos_serie <- 2001:2024   # SINAN e SIM
ano_esus   <- 2023        # e-SUS Notifica (mesmo filtro do cálculo das taxas no texto)

# Valores-p do teste de Fisher simulados (Monte Carlo): a semente é fixada
# imediatamente antes de cada teste (função teste_p), para que o valor-p não
# dependa da ordem das variáveis nem de outras partes do script.
semente     <- 2026
replicas_mc <- 10000

dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

termos_para_anular <- c("Ignorado/Em branco", "Ignorado", "Grau não informado")
fora_do_teste      <- c("Não se aplica", "Não realizado")
ordem_regioes      <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
rotulos_fq         <- c(paste0(seq(0, 75, 5), "-", seq(4, 79, 5)), "80+")


# -----------------------------------------------------------------------------
# Funções auxiliares gerais
# -----------------------------------------------------------------------------

faixa_quinquenal <- function(idade) {
  as.character(cut(idade, breaks = c(seq(0, 80, 5), Inf), right = FALSE, labels = rotulos_fq))
}

faixa_d1 <- function(idade) {
  as.character(cut(idade, breaks = cortes_idade_d1, right = FALSE, labels = rotulos_idade_d1))
}

localizar <- function(arquivo) {
  candidatos <- unique(c(arquivo,
                         file.path(pasta_saida, arquivo),
                         file.path(dirname(pasta_saida), arquivo),
                         file.path(caminho_dados, arquivo)))
  achado <- candidatos[file.exists(candidatos)][1]
  if (is.na(achado)) {
    stop("Arquivo não encontrado: ", arquivo, "\nProcurado em: ",
         paste(c(getwd(), candidatos[-1]), collapse = " | "), call. = FALSE)
  }
  achado
}

# "Ignorado/Em branco" vira NA (excluído do denominador), preservando a ordem
# das categorias definida no processamento de cada base.
anular_ignorados <- function(df) {
  df %>%
    mutate(
      across(where(is.factor),    ~ factor(.x, levels = setdiff(levels(.x), termos_para_anular))),
      across(where(is.character), ~ if_else(.x %in% termos_para_anular, NA_character_, .x))
    )
}

# Ordena e renomeia as categorias. `niveis` pode ter nomes: c("Rótulo da
# tabela" = "valor na base"). Categorias não previstas são mantidas no fim.
ordenar <- function(x, niveis = NULL) {
  x <- as.character(x)
  if (is.null(niveis)) niveis <- sort(unique(x[!is.na(x)]))
  rotulos <- if (is.null(names(niveis))) niveis else ifelse(names(niveis) == "", niveis, names(niveis))
  x <- unname(ifelse(x %in% niveis, rotulos[match(x, niveis)], x))
  extras <- setdiff(sort(unique(x[!is.na(x)])), rotulos)
  factor(x, levels = unique(c(rotulos, extras)))
}

# Indicador sim/não a partir de um campo-resumo. Fica NA quando o resumo é
# ignorado e quando o registro não teve nenhum item (tem_algum = FALSE): as
# linhas "Principais ..." usam como denominador quem teve ao menos um item,
# tanto no % quanto no valor-p.
detectar <- function(texto, padrao_regex, tem_algum) {
  case_when(
    is.na(tem_algum) | !tem_algum ~ NA,
    is.na(texto)                  ~ NA,
    TRUE ~ str_detect(texto, regex(padrao_regex, ignore_case = TRUE))
  )
}

# Teste de cada variável: qui-quadrado; Fisher se alguma frequência esperada
# < 5 (Monte Carlo nas tabelas maiores que 2x2). Mesma regra das Tabelas 5–15.
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

# Valores-p com três casas (o padrão do gtsummary mostra p >= 0,2 com uma casa)
p_tres_casas <- function(x) style_pvalue(x, digits = 3)

fmt_n   <- function(x) style_number(x, big.mark = ".", decimal.mark = ",")
fmt_pct <- function(x) style_number(x * 100, digits = 1, big.mark = ".", decimal.mark = ",")

# Números de uma tabela como aparecem no apêndice: linha do rótulo (n total,
# n por região e valor-p) e uma linha por categoria ("n (%)"); variáveis
# dicotômicas em uma única linha.
numeros_tabela <- function(dados, estrato, variaveis, rotulos, dicotomicas = character(0)) {
  dados  <- dados %>% filter(!is.na(.data[[estrato]]))
  grupos <- levels(droplevels(factor(dados[[estrato]])))
  map_dfr(variaveis, function(v) {
    d   <- dados %>% filter(!is.na(.data[[v]]))
    tst <- teste_p(dados, v, estrato)
    n_g <- map_chr(grupos, ~ fmt_n(sum(d[[estrato]] == .x)))
    if (v %in% dicotomicas) {
      sim <- d %>% filter(.data[[v]] %in% TRUE)
      cel <- function(sub, tot) paste0(fmt_n(nrow(sub)), " (", fmt_pct(if (tot > 0) nrow(sub) / tot else 0), "%)")
      tibble(variavel = v, tipo = "dicotomica", rotulo = rotulos[[v]],
             n_total = fmt_n(nrow(d)), n_grupos = paste(n_g, collapse = " | "),
             stat_total = cel(sim, nrow(d)),
             stat_grupos = paste(map_chr(grupos, ~ cel(filter(sim, .data[[estrato]] == .x),
                                                        sum(d[[estrato]] == .x))), collapse = " | "),
             p = p_tres_casas(tst$p.value), teste = tst$method)
    } else {
      niveis <- if (is.factor(dados[[v]])) levels(dados[[v]]) else sort(unique(d[[v]]))
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


# =============================================================================
# 1. BASES DOS DESFECHOS
# =============================================================================

CHAGAS_AGUDA <- readRDS(localizar(arq_sinan))       # casos confirmados (SINAN)
MORT_CHAGAS  <- readRDS(localizar(arq_sim))         # óbitos com menção à DC (SIM)

# e-SUS: categorias criadas no script original (DCCRBR23)
CHAGAS_CRONICA <- readRDS(localizar(arq_esus)) %>%
  mutate(
    ANO_DIAG_NUM = suppressWarnings(as.numeric(as.character(ANO_DIAG))),
    ANO_DIAG_CAT = case_when(
      ANO_DIAG_NUM < 2000 ~ "Antes de 2000",
      ANO_DIAG_NUM >= 2000 & ANO_DIAG_NUM <= 2009 ~ "2000 a 2009",
      ANO_DIAG_NUM >= 2010 & ANO_DIAG_NUM <= 2019 ~ "2010 a 2019",
      ANO_DIAG_NUM >= 2020 ~ "2020 em diante",
      TRUE ~ "Ignorado/Em branco"
    ),
    DIF_DIAG_NOTIF_CAT = case_when(
      DIF_DIAG_NOTIF == 0 ~ "Notificação Oportuna (Mesmo ano)",
      DIF_DIAG_NOTIF == 1 ~ "Notificação Recente (1 ano)",
      DIF_DIAG_NOTIF > 1 & DIF_DIAG_NOTIF <= 10 ~ "Notificação Tardia (2 a 10 anos)",
      DIF_DIAG_NOTIF > 10 ~ "Notificação Histórica (Mais de 10 anos)",
      TRUE ~ "Ignorado/Em branco"
    ),
    QTD_EXAMES_CAT = case_when(
      is.na(QTD_EXAMES_ALTERADOS) ~ "Ignorado/Em branco",
      QTD_EXAMES_ALTERADOS == 0 ~ "Nenhum exame alterado",
      QTD_EXAMES_ALTERADOS == 1 ~ "Um alterado",
      QTD_EXAMES_ALTERADOS == 2 ~ "Dois alterados",
      QTD_EXAMES_ALTERADOS == 3 ~ "Três alterados",
      QTD_EXAMES_ALTERADOS >= 4 ~ "Quatro ou mais alterados"
    ),
    QTD_SORO_CAT = case_when(
      is.na(QTD_SOROLOGIAS_REAGENTES) ~ "Ignorado/Em branco",
      QTD_SOROLOGIAS_REAGENTES == 0 ~ "Nenhuma sorologia reagente",
      QTD_SOROLOGIAS_REAGENTES == 1 ~ "Uma reagente",
      QTD_SOROLOGIAS_REAGENTES == 2 ~ "Duas reagentes",
      QTD_SOROLOGIAS_REAGENTES == 3 ~ "Três reagentes",
      QTD_SOROLOGIAS_REAGENTES >= 4 ~ "Quatro ou mais reagentes"
    ),
    # Uso de medicamento específico: mesma regra do script DCCRBR23
    USOU_MEDICAMENTO = case_when(
      TRAT_BNZ_FINAL %in% "Sim" | HIST_BNZ_FINAL %in% "Sim" ~ "Sim, Benznidazol",
      TRAT_NFX_FINAL %in% "Sim" ~ "Sim, Nifurtimox",
      TRAT_BNZ_FINAL %in% "Não" & TRAT_NFX_FINAL %in% "Não" & !HIST_BNZ_FINAL %in% "Sim" ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    ),
    # Modo de suspeita agrupado como nas tabelas
    MO_SUSPEIT_FINAL = case_when(
      MO_SUSPEIT_FINAL %in% c("Rastreamento/Busca Ativa", "Outras Buscas Ativas") ~ "Rastreamento/Busca Ativa",
      MO_SUSPEIT_FINAL %in% c("Banco de Sangue", "Triagem de Doador", "Receptor de Transplante",
                              "Pré-Natal", "Serviço HIV/AIDS", "Outros") ~ "Outros",
      TRUE ~ as.character(MO_SUSPEIT_FINAL)
    )
  )

message("Registros carregados – SINAN: ", nrow(CHAGAS_AGUDA),
        " | e-SUS: ", nrow(CHAGAS_CRONICA), " | SIM: ", nrow(MORT_CHAGAS))

# Formatação visual comum a todas as tabelas do apêndice
estilo_gt <- function(tabela_gt) {
  tabela_gt %>%
    gt::tab_options(
      row.striping.include_table_body   = FALSE,
      column_labels.vlines.style        = "none",
      table_body.vlines.style           = "none",
      table.border.top.style            = "solid",
      table.border.top.width            = gt::px(2),
      table.border.top.color            = "black",
      table.border.bottom.style         = "solid",
      table.border.bottom.width         = gt::px(2),
      table.border.bottom.color         = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      table_body.hlines.style           = "none",
      table.font.size                   = 12,
      table.font.names                  = "Times New Roman",
      heading.align                     = "left",
      source_notes.font.size            = 10
    )
}


if (rodar_D1) {

# =============================================================================
# 2. POPULAÇÃO (DENOMINADORES) – arquivos POPS, por ano, região, sexo e idade
# =============================================================================
# Os arquivos tratados usados no texto principal (POP_IBGE_TRATADA.rds e
# POP_IBGE_REGIÃO_TRATADA.rds) não têm sexo. Por isso a população é refeita a
# partir dos arquivos POPS originais; a seção confere se os totais por ano e
# faixa quinquenal batem com os do texto principal.

arq_pop_cache <- file.path(pasta_saida, "POP_POPS_ANO_REGIAO_SEXO_IDADE.rds")

if (file.exists(arq_pop_cache)) {

  POP_D <- readRDS(arq_pop_cache)

} else {

  arquivos_pops <- list.files(pasta_pops, pattern = "^pop.*\\.dbf$",
                              full.names = TRUE, ignore.case = TRUE)
  if (length(arquivos_pops) == 0) {
    stop("Nenhum arquivo POPS (.dbf) encontrado em: ", pasta_pops, call. = FALSE)
  }
  message("Lendo ", length(arquivos_pops), " arquivos POPS (pode levar alguns minutos)...")

  POP_D <- map_dfr(arquivos_pops, function(arq) {
    x <- foreign::read.dbf(arq, as.is = TRUE)
    names(x) <- toupper(names(x))
    x %>%
      transmute(
        ANO    = as.numeric(ANO),
        REGIAO = case_when(
          substr(as.character(COD_MUN), 1, 1) == "1" ~ "Norte",
          substr(as.character(COD_MUN), 1, 1) == "2" ~ "Nordeste",
          substr(as.character(COD_MUN), 1, 1) == "3" ~ "Sudeste",
          substr(as.character(COD_MUN), 1, 1) == "4" ~ "Sul",
          substr(as.character(COD_MUN), 1, 1) == "5" ~ "Centro-Oeste",
          TRUE ~ NA_character_
        ),
        SEXO = case_when(
          as.character(SEXO) == "1" ~ "Masculino",
          as.character(SEXO) == "2" ~ "Feminino",
          TRUE ~ NA_character_
        ),
        IDADE = pmin(as.numeric(IDADE), 80),   # 80 = 80 anos ou mais
        POP   = as.numeric(POP)
      ) %>%
      group_by(ANO, REGIAO, SEXO, IDADE) %>%
      summarise(POP = sum(POP), .groups = "drop")
  })

  saveRDS(POP_D, arq_pop_cache)
}

POP_D <- POP_D %>%
  mutate(FQ = faixa_quinquenal(IDADE), FAIXA_D1 = faixa_d1(IDADE))

message("Anos disponíveis na população: ", paste(sort(unique(POP_D$ANO)), collapse = ", "))

if (!all(c(anos_serie, ano_esus) %in% POP_D$ANO)) {
  stop("Faltam anos nos arquivos POPS: ",
       paste(setdiff(c(anos_serie, ano_esus), POP_D$ANO), collapse = ", "), call. = FALSE)
}
if (anyNA(POP_D$REGIAO) || anyNA(POP_D$SEXO)) {
  warning("Há registros POPS sem região ou sexo identificados.")
}

# Conferência com a população usada no texto principal
arq_pop_texto <- file.path(caminho_dados, "POP_IBGE_TRATADA.rds")
if (file.exists(arq_pop_texto)) {
  conferencia_pop <- readRDS(arq_pop_texto) %>%
    transmute(ANO = as.numeric(ANO_OBITO), FQ = as.character(FAIXA_QUINQUENAL),
              pop_texto = populacao_residente) %>%
    group_by(ANO, FQ) %>%
    summarise(pop_texto = sum(pop_texto), .groups = "drop") %>%
    left_join(POP_D %>% group_by(ANO, FQ) %>% summarise(pop_pops = sum(POP), .groups = "drop"),
              by = c("ANO", "FQ"))
  message("Conferência POPS x POP_IBGE_TRATADA – maior diferença absoluta: ",
          max(abs(conferencia_pop$pop_texto - conferencia_pop$pop_pops), na.rm = TRUE),
          " habitantes (o esperado é 0).")
}

# População-padrão (a mesma do texto principal)
pop_padrao_br2022 <- readRDS(file.path(caminho_dados, "pop_padrao_br2022.rds"))
padrao <- pop_padrao_br2022 %>%
  transmute(FQ = as.character(FAIXA_QUINQUENAL), peso = peso_pop_padrao)

if (!all(rotulos_fq %in% padrao$FQ)) {
  stop("Os rótulos das faixas quinquenais de pop_padrao_br2022 não correspondem a: ",
       paste(rotulos_fq, collapse = ", "), call. = FALSE)
}


# =============================================================================
# 3. TABELA D1 – TAXAS POR SEXO, REGIÃO E FAIXA ETÁRIA
# =============================================================================
# Taxa bruta: casos (ou óbitos) acumulados / população acumulada no período
#   x 100 mil (para 2001–2024, equivale à taxa média anual).
# Taxa padronizada: método direto, faixas quinquenais, população-padrão de
#   2022; nas linhas de faixa etária, a padronização é feita dentro da faixa.
# Registros sem idade são excluídos (como no texto principal); nas linhas de
# sexo e de região, também os registros sem essa informação.

preparar_casos <- function(df, ano, idade, sexo, regiao, anos) {
  casos <- df %>%
    transmute(
      ANO    = suppressWarnings(as.numeric(as.character({{ ano }}))),
      IDADE  = suppressWarnings(as.numeric(as.character({{ idade }}))),
      SEXO   = as.character({{ sexo }}),
      REGIAO = as.character({{ regiao }})
    )

  message("  Registros na base: ", nrow(casos),
          " | no período analisado: ", sum(casos$ANO %in% anos),
          " | destes, sem idade válida (excluídos das taxas): ",
          sum(casos$ANO %in% anos & (is.na(casos$IDADE) | casos$IDADE < 0)))

  casos %>%
    filter(ANO %in% anos, !is.na(IDADE), IDADE >= 0) %>%
    mutate(
      IDADE    = pmin(floor(IDADE), 80),
      SEXO     = if_else(SEXO %in% c("Masculino", "Feminino"), SEXO, NA_character_),
      REGIAO   = if_else(REGIAO %in% ordem_regioes, REGIAO, NA_character_),
      FQ       = faixa_quinquenal(IDADE),
      FAIXA_D1 = faixa_d1(IDADE)
    )
}

taxas_estrato <- function(casos, pop, var = character(0)) {
  grupos <- c(var, "FQ")
  if (length(var)) casos <- filter(casos, !is.na(.data[[var]]))

  n_casos <- casos %>% count(across(all_of(grupos)), name = "casos")
  pop_ag  <- pop %>% group_by(across(all_of(grupos))) %>%
    summarise(pop = sum(POP), .groups = "drop")

  pop_ag %>%
    left_join(n_casos, by = grupos) %>%
    mutate(casos = replace_na(casos, 0L)) %>%
    left_join(padrao, by = "FQ") %>%
    group_by(across(all_of(var))) %>%
    summarise(
      n                = sum(casos),
      populacao        = sum(pop),
      taxa_bruta       = n / populacao * 1e5,
      taxa_padronizada = sum(casos / pop * peso) / sum(peso) * 1e5,
      .groups = "drop"
    )
}

ordem_categorias <- c("Brasil", "Masculino", "Feminino", ordem_regioes, rotulos_idade_d1)

taxas_desfecho <- function(casos, pop) {
  estratos <- c("Sexo" = "SEXO", "Região de residência" = "REGIAO", "Faixa etária" = "FAIXA_D1")

  total <- taxas_estrato(casos, pop) %>%
    mutate(Grupo = "Total", Categoria = "Brasil")

  por_estrato <- imap_dfr(estratos, function(v, nome) {
    taxas_estrato(casos, pop, v) %>%
      rename(Categoria = all_of(v)) %>%
      mutate(Grupo = nome, Categoria = as.character(Categoria))
  })

  bind_rows(total, por_estrato) %>%
    select(Grupo, Categoria, n, populacao, taxa_bruta, taxa_padronizada) %>%
    arrange(factor(Categoria, levels = ordem_categorias))
}

message("SINAN – casos confirmados de DC aguda:")
casos_sinan <- preparar_casos(CHAGAS_AGUDA, NU_ANO, IDADE_FINAL, SEXO_FINAL,
                              REGIAO_RESIDENCIA, anos_serie)
taxas_sinan <- taxas_desfecho(casos_sinan, filter(POP_D, ANO %in% anos_serie))

message("e-SUS Notifica – casos de DC crônica:")
casos_esus <- preparar_casos(CHAGAS_CRONICA, ANO_NOTIFICACAO, IDADE_FINAL, SEXO_FINAL,
                             REGIAO_RESIDENCIA, ano_esus)
taxas_esus <- taxas_desfecho(casos_esus, filter(POP_D, ANO == ano_esus))

message("SIM – óbitos com menção à DC:")
casos_sim <- preparar_casos(MORT_CHAGAS, ANO_OBITO, IDADE_OBITO, SEXO,
                            REGIAO_RESIDENCIA, anos_serie)
taxas_sim <- taxas_desfecho(casos_sim, filter(POP_D, ANO %in% anos_serie))

tabela_d1 <- list(sinan = taxas_sinan, esus = taxas_esus, sim = taxas_sim) %>%
  imap(function(df, sufixo) {
    df %>%
      select(Grupo, Categoria, n, taxa_bruta, taxa_padronizada) %>%
      rename_with(~ paste0(.x, "_", sufixo), c(n, taxa_bruta, taxa_padronizada))
  }) %>%
  reduce(full_join, by = c("Grupo", "Categoria")) %>%
  arrange(factor(Categoria, levels = ordem_categorias))

print(tabela_d1, n = Inf, width = Inf)

TBL_D1_TAXAS <- tabela_d1 %>%
  gt(rowname_col = "Categoria", groupname_col = "Grupo") %>%
  tab_stubhead(label = md("**Característica**")) %>%
  tab_spanner(label = md(paste0("**DC aguda – SINAN, ", min(anos_serie), "–", max(anos_serie), "**")),
              columns = ends_with("_sinan")) %>%
  tab_spanner(label = md(paste0("**DC crônica – e-SUS Notifica, ", ano_esus, "**")),
              columns = ends_with("_esus")) %>%
  tab_spanner(label = md(paste0("**Óbitos com menção à DC – SIM, ", min(anos_serie), "–", max(anos_serie), "**")),
              columns = ends_with("_sim")) %>%
  cols_label(
    n_sinan = "Casos",  taxa_bruta_sinan = "Bruta", taxa_padronizada_sinan = "Padronizada",
    n_esus  = "Casos",  taxa_bruta_esus  = "Bruta", taxa_padronizada_esus  = "Padronizada",
    n_sim   = "Óbitos", taxa_bruta_sim   = "Bruta", taxa_padronizada_sim   = "Padronizada"
  ) %>%
  fmt_integer(columns = starts_with("n_"), sep_mark = ".") %>%
  fmt_number(columns = starts_with("taxa_"), decimals = 2, dec_mark = ",", sep_mark = ".") %>%
  cols_align(align = "right", columns = c(starts_with("n_"), starts_with("taxa_"))) %>%
  tab_source_note(paste(
    "Taxas por 100 mil habitantes. SINAN e SIM: taxa média anual do período",
    "(casos ou óbitos acumulados / população residente acumulada), que difere",
    "da média das taxas anuais apresentada na seção 5.3.",
    "e-SUS Notifica: notificações com data de notificação em", paste0(ano_esus, ".")
  )) %>%
  tab_source_note(paste(
    "Taxa padronizada: padronização direta por idade, com faixas quinquenais e a",
    "população-padrão brasileira de 2022; nas linhas de faixa etária, a",
    "padronização foi feita dentro de cada faixa."
  )) %>%
  tab_source_note(paste(
    "Foram excluídos os registros sem idade informada; nas linhas de sexo e de",
    "região, também os registros sem essa informação. SINAN: casos confirmados",
    "de DC aguda. SIM: óbitos com menção à DC como causa básica ou associada."
  )) %>%
  tab_source_note("Fonte: MS/SVS/DATASUS; população: estimativas POPS (IBGE/DATASUS), 2026.") %>%
  estilo_gt()

TBL_D1_TAXAS

write_xlsx(
  list(Tabela_D1 = tabela_d1, SINAN = taxas_sinan, eSUS = taxas_esus, SIM = taxas_sim),
  file.path(pasta_saida, "Tabela_D1_taxas.xlsx")
)

}  # fim de if (rodar_D1)


# =============================================================================
# 4. TABELAS D2–D12 – ANÁLISE DESCRITIVA ESTRATIFICADA POR REGIÃO
# =============================================================================
# Mesmas tabelas do texto principal, trocando o estrato (faixa etária no SINAN
# e no e-SUS; tipo de menção no SIM) pela região de residência. O antigo
# estrato passa a ser uma linha da tabela. "Ignorado/Em branco" fica fora do
# denominador; "Não se aplica", só fora do teste.

nota_testes <- paste(
  "Valor-p: teste qui-quadrado de independência; teste exato de Fisher, com",
  "valor-p simulado por Monte Carlo (10.000 réplicas), quando alguma",
  "frequência esperada foi menor que 5. Categorias ignoradas/em branco",
  "foram excluídas do denominador; a categoria 'Não se aplica', só do teste."
)

formatar_gt <- function(tabela_gt) {
  tabela_gt %>%
    gt::tab_source_note(source_note = "Fonte: MS/SVS/DATASUS, 2026.") %>%
    estilo_gt() %>%
    gt::cols_align(align = "left",  columns = 1) %>%
    gt::cols_align(align = "right", columns = -1)
}

preparar_regiao <- function(dados) {
  dados %>%
    filter(REGIAO_RESIDENCIA %in% ordem_regioes) %>%
    mutate(REGIAO_RESIDENCIA = factor(REGIAO_RESIDENCIA, levels = ordem_regioes))
}

# Gera uma tabela estratificada por região de residência (objeto gtsummary).
#   variaveis   : vetor nomeado c(VARIAVEL = "Rótulo")
#   dicotomicas : variáveis lógicas exibidas só com a linha "Sim" (TRUE)
#   fonte_dados : texto da nota (ex.: "Dados do SIM (2001-2024).")
tabela_por_regiao <- function(dados, variaveis, dicotomicas = character(0), fonte_dados) {

  dados <- preparar_regiao(dados)
  tipo  <- if (length(dicotomicas)) setNames(rep(list("dichotomous"), length(dicotomicas)), dicotomicas) else NULL
  valor <- if (length(dicotomicas)) setNames(rep(list(TRUE),          length(dicotomicas)), dicotomicas) else NULL

  tbl_summary(
    data      = dados,
    by        = REGIAO_RESIDENCIA,
    include   = all_of(names(variaveis)),
    label     = as.list(variaveis),
    type      = tipo,
    value     = valor,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits    = list(all_categorical() ~ c(0, 1)),
    missing   = "no"
  ) %>%
    add_n() %>%
    add_p(test = everything() ~ teste_p, pvalue_fun = p_tres_casas) %>%
    remove_footnote_header(columns = p.value) %>%   # os testes já constam da nota abaixo
    add_overall(last = FALSE, col_label = "**Total**") %>%
    modify_header(label ~ "**Variável**") %>%
    modify_spanning_header(all_stat_cols() ~ "**Região de Residência**") %>%
    bold_labels() %>%
    modify_footnote_header(
      footnote = paste(fonte_dados, "n (%).", nota_testes),
      columns  = all_stat_cols()
    )
}


# -----------------------------------------------------------------------------
# 4.1 SINAN – DOENÇA DE CHAGAS AGUDA (2001–2024) – Tabelas D2 a D5
# -----------------------------------------------------------------------------

sinan_regiao <- CHAGAS_AGUDA %>%
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
    FLUXO_ASSISTENCIA    = ordenar(FLUXO_ASSISTENCIA, c("Sem Deslocamento", "Deslocamento Intermunicipal",
                                                        "Deslocamento Interestadual",
                                                        "Deslocamento Inter-regional" = "Deslocamento Interregional")),
    LOCAL_INFECCAO_FINAL = ordenar(LOCAL_INFECCAO_FINAL, c("Domicílio", "Unidade de Hemoterapia", "Laboratório", "Outro")),
    PRESENCA_FINAL       = ordenar(PRESENCA_FINAL, c("Sim", "Não", "Não se aplica" = "Não realizado")),
    TPAUTOCTO_FINAL_2007 = ordenar(TPAUTOCTO_FINAL_2007, c("Autóctone", "Importado" = "Não Autóctone", "Indeterminado")),
    MODO_INFECCAO_FINAL  = ordenar(MODO_INFECCAO_FINAL, c("Oral", "Vetorial", "Acidental", "Vertical/Transplacentária", "Transfusional", "Outra")),
    ASSINTOMA_FINAL      = ordenar(ASSINTOMA_FINAL, c("Sim", "Não")),
    USOU_MEDICAMENTO     = ordenar(USOU_MEDICAMENTO, c("Nenhum medicamento", "Sim, somente específico", "Sim, somente sintomático", "Sim, ambos")),
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
  # Indicadores sim/não: mesmas expressões da Tabela 8
  mutate(
    SINTOMA_FEBRE          = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Febre", TEM_SINTOMA),
    SINTOMA_EDEMA          = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Edema|Inchaço|Face|Pernas", TEM_SINTOMA),
    SINTOMA_DOR            = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Dor|Cefaleia|Cabeca|Corpo", TEM_SINTOMA),
    SINTOMA_ICC            = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "ICC|Congest|Cardia", TEM_SINTOMA),
    SINTOMA_ARRITMIA       = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Arritmia|Palpitat|Batedeira", TEM_SINTOMA),
    SINTOMA_ASTENIA        = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Astenia|Fraqueza|Cansa", TEM_SINTOMA),
    SINTOMA_HEPATOMEGALIA  = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Hepatoespleno|Hepato|Figado", TEM_SINTOMA),
    SINTOMA_ESPLENOMEGALIA = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Espleno|Baco", TEM_SINTOMA),
    SINTOMA_CHAGOMA        = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Chagoma|Inoculacao", TEM_SINTOMA),
    SINTOMA_MENINGO        = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Meningo", TEM_SINTOMA),
    SINTOMA_GANGLIOS       = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Adenomegalia|Ganglio|Adeno", TEM_SINTOMA),
    SINTOMA_DIGESTIVO      = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Anorexia|Hiporexia|Vomito|Diarreia|Nausea", TEM_SINTOMA),
    SINTOMA_ANOREXIA       = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Anorexia", TEM_SINTOMA),
    SINTOMA_HIPOREXIA      = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Hiporexia", TEM_SINTOMA),
    SINTOMA_AUMENTO        = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Aumento", TEM_SINTOMA),
    SINTOMA_POLIADENO      = detectar(RESUMO_SINTOMAS_PRINCIPAIS, "Poliadeno", TEM_SINTOMA),

    SORO_ELISA = detectar(LISTA_SOROLOGIAS_REAGENTES, "ELISA", TEM_SORO),
    SORO_IFI   = detectar(LISTA_SOROLOGIAS_REAGENTES, "IFI", TEM_SORO),
    SORO_HAI   = detectar(LISTA_SOROLOGIAS_REAGENTES, "HAI", TEM_SORO),

    PARASITO_DIRETO   = detectar(LISTA_PARASITO_POSITIVOS, "Direto|Gota|Fresco", TEM_PARASITO),
    PARASITO_STROUT   = detectar(LISTA_PARASITO_POSITIVOS, "Strout|Micro", TEM_PARASITO),
    PARASITO_HEMOCULT = detectar(LISTA_PARASITO_POSITIVOS, "Hemo|Cultura", TEM_PARASITO),
    PARASITO_XENO     = detectar(LISTA_PARASITO_POSITIVOS, "Xenodiag|Xeno", TEM_PARASITO),
    PARASITO_OUTRO    = detectar(LISTA_PARASITO_POSITIVOS, "Outro|PCR", TEM_PARASITO),
    PARASITO_HISTO    = detectar(LISTA_PARASITO_POSITIVOS, "Histopatologia|Histo", TEM_PARASITO),

    MEDIDA_BORRIFACAO = detectar(LISTA_MEDIDAS_TOMADAS, "Borrif|Fiscaliza|Inseticida", TEM_MEDIDA),
    MEDIDA_EDUCACAO   = detectar(LISTA_MEDIDAS_TOMADAS, "Educa|Orienta", TEM_MEDIDA),
    MEDIDA_PESQ_VETOR = detectar(LISTA_MEDIDAS_TOMADAS, "Pesq|Vetor", TEM_MEDIDA),
    MEDIDA_VIGILANCIA = detectar(LISTA_MEDIDAS_TOMADAS, "Vigil[aâ]ncia|Investig|Inquerito|Inquérito|Coleta|Epidemiol", TEM_MEDIDA),
    MEDIDA_BIOSSEG    = detectar(LISTA_MEDIDAS_TOMADAS, "Biosseg", TEM_MEDIDA)
  ) %>%
  # Mesma amostra do texto: os 7.531 casos com idade informada (Tabela 1)
  filter(!is.na(FAIXA_ETARIA_GRUPO))

fonte_sinan <- "Dados do SINAN (2001-2024)."

vars_D2 <- c(FAIXA_ETARIA_GRUPO = "Faixa Etária", SEXO_FINAL = "Sexo", RACACOR_FINAL = "Raça/Cor",
             ESCOLARIDADE_FINAL = "Escolaridade", PERFIL_OCUPACIONAL = "Perfil Ocupacional")
vars_D3 <- c(OPORTUNIDADE_NOTIFICACAO = "Tempo de Notificação", FLUXO_ASSISTENCIA = "Fluxo Assistencial",
             LOCAL_INFECCAO_FINAL = "Local Provável de Infecção",
             PRESENCA_FINAL = "Vestígios de Triatomíneos no Intradomicílio?",
             TPAUTOCTO_FINAL_2007 = "Classificação de Autoctonia (Após 2007)")
vars_D4 <- c(MODO_INFECCAO_FINAL = "Modo Provável de Infecção", ASSINTOMA_FINAL = "Paciente Assintomático?",
             USOU_MEDICAMENTO = "Uso de Medicamento?", DROGA_FINAL = "Medicamento Utilizado",
             CRITERIO_FINAL = "Critério de Confirmação", EVOLUCAO_FINAL = "Evolução do Caso")
vars_D5 <- c(QTD_SINTOMAS_CAT = "Quantidade de Sintomas", SINTOMA_FEBRE = "Febre", SINTOMA_ASTENIA = "Astenia",
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
dic_D5 <- grep("^(SINTOMA|SORO|PARASITO|MEDIDA)_", names(vars_D5), value = TRUE)


# -----------------------------------------------------------------------------
# 4.2 e-SUS NOTIFICA – DOENÇA DE CHAGAS CRÔNICA (2023) – Tabelas D6 a D9
# -----------------------------------------------------------------------------

esus_regiao <- CHAGAS_CRONICA %>%
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
    # Ao menos um item: comorbidade na ficha (perfil) ou só no texto livre (AVC,
    # doença renal, DAC); sorologia reagente; exame alterado
    TEM_COMORB = PERFIL_COMORBIDADE %in% c("Uma Comorbidade", "Multimorbidade (2 ou mais)") |
                 (!is.na(RESUMO_CLINICO_PRINCIPAIS) & RESUMO_CLINICO_PRINCIPAIS != "Sem as Principais Comorbidades"),
    TEM_SORO   = !is.na(QTD_SOROLOGIAS_REAGENTES) & QTD_SOROLOGIAS_REAGENTES > 0,
    TEM_EXAME  = !is.na(QTD_EXAMES_ALTERADOS) & QTD_EXAMES_ALTERADOS > 0
  ) %>%
  # Indicadores sim/não: mesmas expressões da Tabela 12
  mutate(
    HAS         = detectar(RESUMO_CLINICO_PRINCIPAIS, "HAS|HIPERTENSAO", TEM_COMORB),
    DM          = detectar(RESUMO_CLINICO_PRINCIPAIS, "DM|DIABETES", TEM_COMORB),
    CARDIOPATIA = detectar(RESUMO_CLINICO_PRINCIPAIS, "Cardiopatia|DAC|ICC|Coracao|Isquemica", TEM_COMORB),
    AVC         = detectar(RESUMO_CLINICO_PRINCIPAIS, "AVC|AVE|Derrame", TEM_COMORB),
    DRC         = detectar(RESUMO_CLINICO_PRINCIPAIS, "DRC|Renal", TEM_COMORB),
    HIV         = detectar(RESUMO_CLINICO_PRINCIPAIS, "HIV|Aids|Imuno", TEM_COMORB),

    EXAME_ECG     = detectar(LISTA_EXAMES_ALTERADOS, "ECG|Eletro", TEM_EXAME),
    EXAME_ECO     = detectar(LISTA_EXAMES_ALTERADOS, "Eco\\b|Ecocardiograma", TEM_EXAME),
    EXAME_RX      = detectar(LISTA_EXAMES_ALTERADOS, "RX|Torax|Raio", TEM_EXAME),
    EXAME_ESOFAGO = detectar(LISTA_EXAMES_ALTERADOS, "Esofago|EED", TEM_EXAME),
    EXAME_HOLTER  = detectar(LISTA_EXAMES_ALTERADOS, "Holter", TEM_EXAME),
    EXAME_COLON   = detectar(LISTA_EXAMES_ALTERADOS, "Colon|Enema", TEM_EXAME),

    SORO_ELISA  = detectar(LISTA_SOROLOGIAS_REAGENTES, "ELISA", TEM_SORO),
    SORO_IFI    = detectar(LISTA_SOROLOGIAS_REAGENTES, "IFI|Imuno", TEM_SORO),
    SORO_HAI    = detectar(LISTA_SOROLOGIAS_REAGENTES, "HAI|Hemaglutinacao", TEM_SORO),
    SORO_QUIMIO = detectar(LISTA_SOROLOGIAS_REAGENTES, "Quimio", TEM_SORO),
    SORO_PCR    = detectar(LISTA_SOROLOGIAS_REAGENTES, "PCR|Molecular", TEM_SORO)
  )

fonte_esus <- "Dados do e-SUS Notifica (2023)."

vars_D6 <- c(FAIXA_ETARIA_GRUPO = "Faixa Etária", SEXO_FINAL = "Sexo", RACACOR_FINAL = "Raça/Cor",
             ESCOLARIDADE_FINAL = "Escolaridade", PERFIL_OCUPACIONAL = "Perfil Ocupacional",
             COMU_TRAD_FINAL = "Pertence a Comunidade Tradicional?")
vars_D7 <- c(ANO_DIAG_CAT = "Ano do Diagnóstico", DIF_DIAG_NOTIF_CAT = "Tempo entre Diagnóstico e Notificação",
             CLASSIFICACAO_MIGRATORIA = "Perfil Migratório", FLUXO_ASSISTENCIA = "Fluxo Assistencial")
vars_D8 <- c(MO_SUSPEIT_FINAL = "Modo de Suspeita", FORMA_CLINICA = "Forma Clínica",
             GRAVIDADE_CARDIACA = "Classificação da Gravidade Cardíaca",
             FAMILIA_HISTORICO_DIAG = "Histórico Familiar de Chagas?",
             USOU_MEDICAMENTO = "Uso de Medicamento Específico?", REATIVACAO_FINAL = "Houve Reativação da Doença?",
             ST_ENCERRA_FINAL = "Situação de Encerramento")
vars_D9 <- c(PERFIL_COMORBIDADE = "Perfil de Comorbidades", HAS = "Hipertensão Arterial", DM = "Diabetes Mellitus",
             CARDIOPATIA = "Cardiopatia ou Insuficiência Cardíaca", AVC = "Acidente Vascular Cerebral",
             DRC = "Doença Renal", HIV = "Coinfecção com HIV",
             QTD_SORO_CAT = "Quantidade de Sorologias Reagentes", SORO_ELISA = "ELISA Reagente",
             SORO_IFI = "IFI Reagente", SORO_HAI = "HAI Reagente", SORO_QUIMIO = "Quimioluminescência Reagente",
             SORO_PCR = "PCR Positivo", QTD_EXAMES_CAT = "Quantidade de Exames Alterados",
             EXAME_ECG = "Eletrocardiograma Alterado", EXAME_ECO = "Ecocardiograma Alterado",
             EXAME_RX = "Radiografia de Tórax Alterada", EXAME_ESOFAGO = "Radiografia de Esôfago Alterada",
             EXAME_HOLTER = "Holter Alterado", EXAME_COLON = "Radiografia de Cólon Alterada")
dic_D9 <- c("HAS", "DM", "CARDIOPATIA", "AVC", "DRC", "HIV",
            "SORO_ELISA", "SORO_IFI", "SORO_HAI", "SORO_QUIMIO", "SORO_PCR",
            "EXAME_ECG", "EXAME_ECO", "EXAME_RX", "EXAME_ESOFAGO", "EXAME_HOLTER", "EXAME_COLON")


# -----------------------------------------------------------------------------
# 4.3 SIM – ÓBITOS COM MENÇÃO À DOENÇA DE CHAGAS (2001–2024) – Tabelas D10 a D12
# -----------------------------------------------------------------------------

sim_regiao <- MORT_CHAGAS %>%
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
  # Indicadores sim/não: mesmas expressões da Tabela 15
  mutate(
    HAS    = detectar(RESUMO_CLINICO, "HAS|HIPERTENSAO", TEM_COMORB),
    DM     = detectar(RESUMO_CLINICO, "DM|DIABETES", TEM_COMORB),
    DAC    = detectar(RESUMO_CLINICO, "DAC|CORONARIOPATIA|ISQUEMICA", TEM_COMORB),
    DRC    = detectar(RESUMO_CLINICO, "DRC|RENAL", TEM_COMORB),
    AVC    = detectar(RESUMO_CLINICO, "AVC|AVE|DERRAME", TEM_COMORB),
    CANCER = detectar(RESUMO_CLINICO, "CANCER|CÂNCER|NEOPLASIA", TEM_COMORB)
  )

fonte_sim <- "Dados do SIM (2001-2024)."

vars_D10 <- c(TIPO_MENCAO = "Tipo de Menção da DC no Óbito", SEXO = "Sexo", RACACOR = "Raça/Cor",
              FAIXA_ETARIA = "Faixa Etária", ESCOLARIDADE = "Escolaridade", ESTADO_CIVIL = "Estado Civil",
              PERFIL_OCUPACIONAL = "Perfil Ocupacional")
vars_D11 <- c(CLASSIFICACAO_MIGRATORIA = "Perfil Migratório", FLUXO_ASSISTENCIA = "Fluxo Assistencial",
              LOCAL_OCORRENCIA = "Local de Ocorrência do Óbito")
vars_D12 <- c(FASE_DOENCA = "Fase da Doença", FORMA_CLINICA = "Forma Clínica",
              PERFIL_COMORBIDADE = "Perfil de Comorbidades", HAS = "Hipertensão",
              DAC = "Doença Arterial Coronariana", DRC = "Doença Renal", DM = "Diabetes",
              AVC = "Acidente Vascular Cerebral", CANCER = "Neoplasias",
              ASSISTMED = "Teve Assistência Médica no Óbito?", NECROPSIA = "Realizou Necropsia?")
dic_D12 <- c("HAS", "DAC", "DRC", "DM", "AVC", "CANCER")


# -----------------------------------------------------------------------------
# 4.4 Montagem das Tabelas D2 a D12
# -----------------------------------------------------------------------------

especificacao_D <- list(
  Tabela_D2_SINAN = list(dados = "sinan_regiao", vars = vars_D2,  dic = character(0), fonte = fonte_sinan),
  Tabela_D3_SINAN = list(dados = "sinan_regiao", vars = vars_D3,  dic = character(0), fonte = fonte_sinan),
  Tabela_D4_SINAN = list(dados = "sinan_regiao", vars = vars_D4,  dic = character(0), fonte = fonte_sinan),
  Tabela_D5_SINAN = list(dados = "sinan_regiao", vars = vars_D5,  dic = dic_D5,       fonte = fonte_sinan),
  Tabela_D6_ESUS  = list(dados = "esus_regiao",  vars = vars_D6,  dic = character(0), fonte = fonte_esus),
  Tabela_D7_ESUS  = list(dados = "esus_regiao",  vars = vars_D7,  dic = character(0), fonte = fonte_esus),
  Tabela_D8_ESUS  = list(dados = "esus_regiao",  vars = vars_D8,  dic = character(0), fonte = fonte_esus),
  Tabela_D9_ESUS  = list(dados = "esus_regiao",  vars = vars_D9,  dic = dic_D9,       fonte = fonte_esus),
  Tabela_D10_SIM  = list(dados = "sim_regiao",   vars = vars_D10, dic = character(0), fonte = fonte_sim),
  Tabela_D11_SIM  = list(dados = "sim_regiao",   vars = vars_D11, dic = character(0), fonte = fonte_sim),
  Tabela_D12_SIM  = list(dados = "sim_regiao",   vars = vars_D12, dic = dic_D12,      fonte = fonte_sim)
)

# Números (n, % e valor-p já formatados) e teste de cada variável
numeros_D <- map(especificacao_D, function(e) {
  numeros_tabela(preparar_regiao(get(e$dados)), "REGIAO_RESIDENCIA", names(e$vars), e$vars, e$dic)
})

if (!exists("gerar_gt") || isTRUE(gerar_gt)) {
  tabelas_apendice_D <- map(especificacao_D, function(e) {
    tabela_por_regiao(get(e$dados), e$vars, e$dic, e$fonte)
  })
}


# =============================================================================
# 5. EXPORTAR AS TABELAS D2 A D12
# =============================================================================

write_xlsx(numeros_D, file.path(pasta_saida, "Tabelas_D2_a_D12_numeros.xlsx"))

# Word (.docx); se a versão do pacote gt não permitir, salva em .html
salvar_tabela <- function(tabela, nome) {
  ok <- tryCatch({
    gt::gtsave(tabela, filename = paste0(nome, ".docx"), path = pasta_saida)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) gt::gtsave(tabela, filename = paste0(nome, ".html"), path = pasta_saida)
  invisible(ok)
}

if (rodar_D1) salvar_tabela(TBL_D1_TAXAS, "Tabela_D1_taxas")
if (exists("tabelas_apendice_D")) {
  iwalk(tabelas_apendice_D, function(tb, nome) salvar_tabela(formatar_gt(as_gt(tb)), nome))
}

reset_gtsummary_theme()

message("Pronto. Tabelas do Apêndice D salvas em: ", normalizePath(pasta_saida))
