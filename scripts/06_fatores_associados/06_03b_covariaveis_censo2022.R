# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/Censo 2022/01_censo2022_municipios_v2.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Censo 2022"))

library(tidyverse)  

caminho_base <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Bancos/Microdados - Censo 2022/microdados_censo_amostra_2022_csv_20260901_003504")

dir_saida    <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Censo 2022")

arquivo_v1   <- "Censo2022_Agregado_Municipios.csv"

N_MUNICIPIOS   <- 5570
POP_CENSO_2022 <- 203080756

pct_pond <- function(w, cond, elig) {
  elig <- elig %in% TRUE
  cond <- cond %in% TRUE
  den  <- sum(w[elig], na.rm = TRUE)
  if (den == 0) return(NA_real_)
  100 * sum(w[elig & cond], na.rm = TRUE) / den
}

media_pond <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

# ---- 1. DOMICILIO ----------------------------------------------------------------
cols_dom <- c("D0080", "D0111", "D0140", "D0150", "D0210", "D0240", "D0250",
              "D0300", "D0310", "D0360")

processar_domicilios <- function(arquivo) {
  df <- data.table::fread(arquivo, select = cols_dom)     
  stopifnot(all(cols_dom %in% names(df)))
  
  df %>%
    filter(!is.na(D0080), !is.na(D0111)) %>%
    mutate(w_dom = D0111,                # peso do domicílio
           w_pes = D0111 * D0150) %>%    # peso por pessoa: cada morador herda o peso do domicílio
    group_by(codigo_municipio = D0080) %>%
    summarise(
      n_dom_amostra = n(),
      n_dom_est     = sum(w_dom),
      n_pes_dom_est = sum(w_pes, na.rm = TRUE),
      
      # D0360: rendimento domiciliar per capita (R$ nominais; só domicílios particulares) - média por PESSOA
      renda_media_pc    = media_pond(D0360, w_pes),
      # D0240: moradores por cômodo utilizado como dormitório - média por PESSOA
      adensamento_medio = media_pond(D0240, w_pes),
      
      # D0140: 1 = urbana; 2 = rural
      pct_rural = pct_pond(w_dom, D0140 == 2, !is.na(D0140)),
      
      # D0210: 3 = taipa sem revestimento; 5 = madeira aproveitada; 6 = outro material; 7 = sem parede
      pct_parede_inadequada = pct_pond(w_dom, D0210 %in% c(3, 5, 6, 7), !is.na(D0210)),
      pct_parede_taipa      = pct_pond(w_dom, D0210 == 3,               !is.na(D0210)),
      
      # D0250: 4 = fossa rudimentar; 5 = vala; 6 = rio/lago/mar; 7 = outra; 9 = SEM banheiro nem sanitário
      # (2 e 3 = fossa séptica ligada / não ligada à rede: tratadas como adequadas)
      pct_esgoto_inadequado = pct_pond(w_dom, D0250 %in% c(4, 5, 6, 7, 9), !is.na(D0250)),
      
      # D0300: 1 = encanada dentro de casa; 2 = só no terreno; 3 = não chega encanada
      pct_sem_agua_interna  = pct_pond(w_dom, D0300 %in% c(2, 3), !is.na(D0300)),
      
      # D0310: 3 = queimado; 4 = enterrado; 5 = jogado em terreno/via pública; 6 = outro destino
      pct_lixo_inadequado   = pct_pond(w_dom, D0310 %in% c(3, 4, 5, 6), !is.na(D0310)),
      
      # diagnósticos: % de domicílios particulares (D0250 preenchida) sem D0240 / sem D0360.
      # Se D0240 vier vazia quando não há dormitório, a média acima exclui os mais adensados.
      diag_pct_sem_d0240 = pct_pond(w_dom, is.na(D0240), !is.na(D0250)),
      diag_pct_sem_d0360 = pct_pond(w_dom, is.na(D0360), !is.na(D0250)),
      .groups = "drop"
    )
}

# ---- 2. PESSOAS ------------------------------------------------------------------
cols_pes <- c("P0020", "P0080", "P0111", "P0180", "P0210", "P0220", "P0480", "P0490",
              "P0640", "P0890", "P0920", "P0930", "P0950")
UF_VALIDAS <- c(11:17, 21:29, 31:33, 35, 41:43, 50:53)

processar_pessoas <- function(arquivo) {
  df <- data.table::fread(arquivo, select = cols_pes)
  stopifnot(all(cols_pes %in% names(df)))
  
  df %>%
    filter(!is.na(P0080), !is.na(P0111)) %>%
    mutate(
      idade_15mais   = P0180 %in% 4:17,                      # 04 = 15-19 anos ... 17 = 80+ ; 99 = ignorado
      uf_nasc_valida = P0480 == 2 & P0490 %in% UF_VALIDAS,   # outro município do Brasil, UF conhecida
      nasc_outra_uf  = uf_nasc_valida & P0490 != P0020,
      nasc_outra_reg = uf_nasc_valida & (P0490 %/% 10) != (P0020 %/% 10)   # 1=N 2=NE 3=SE 4=S 5=CO
    ) %>%
    group_by(codigo_municipio = P0080) %>%
    summarise(
      n_pes_amostra = n(),
      n_pes_est     = sum(P0111),
      
      # P0640: 1 = sabe ler/escrever; 2 = não (branco < 5 anos) -> restrito a 15+
      pct_analfabetismo_15mais = pct_pond(P0111, P0640 == 2, P0640 %in% 1:2 & idade_15mais),
      
      # P0950: 1 = ocupada; 0 = desocupada (só na força de trabalho). P0920 = 14+; P0930 = força de trabalho
      pct_ocupados_14mais = pct_pond(P0111, P0950 == 1, P0920 == 1),
      pct_desocupacao     = pct_pond(P0111, P0950 == 0, P0930 == 1),
      
      # P0210: 2 = preta; 4 = parda; 5 = indígena; 9 = ignorada (fora do denominador)
      # P0220 == 1: mora em terra indígena, não se declarou indígena em P0210, mas se considera indígena
      pct_negros   = pct_pond(P0111, P0210 %in% c(2, 4),      P0210 %in% 1:5),
      pct_indigena = pct_pond(P0111, P0210 == 5 | P0220 == 1, P0210 %in% 1:5),
      
      # P0480: 1 = neste município; 2 = outro município do Brasil; 3 = outro país
      pct_nasc_fora_municipio = pct_pond(P0111, P0480 %in% 2:3, P0480 %in% 1:3),   
      pct_nasc_outra_uf       = pct_pond(P0111, nasc_outra_uf,  P0480 == 1 | uf_nasc_valida),
      pct_nasc_outra_regiao   = pct_pond(P0111, nasc_outra_reg, P0480 == 1 | uf_nasc_valida),
      
      # P0890: 1 = trabalho só para o consumo do domicílio (preenchida apenas p/ quem não teve outro trabalho)
      pct_subsistencia_14mais = pct_pond(P0111, P0890 == 1, P0920 == 1),
      .groups = "drop"
    )
}

arquivos_dom <- list.files(caminho_base, pattern = "Domicilios_.*\\.csv$", recursive = TRUE, full.names = TRUE)
arquivos_pes <- list.files(caminho_base, pattern = "Pessoas_.*\\.csv$",    recursive = TRUE, full.names = TRUE)
stopifnot(length(arquivos_dom) > 0, length(arquivos_pes) > 0)
cat("Arquivos de domicílios:", length(arquivos_dom), "| de pessoas:", length(arquivos_pes), "\n")

cat("Processando domicílios...\n"); censo_dom <- map_df(arquivos_dom, processar_domicilios)
cat("Processando pessoas...\n");    censo_pes <- map_df(arquivos_pes, processar_pessoas)
cat("Municípios: domicílios =", nrow(censo_dom), "| pessoas =", nrow(censo_pes), "\n")

stopifnot(!anyDuplicated(censo_dom$codigo_municipio), !anyDuplicated(censo_pes$codigo_municipio))

censo <- full_join(censo_dom, censo_pes, by = "codigo_municipio", relationship = "one-to-one")

stopifnot(nrow(censo) == N_MUNICIPIOS, !anyNA(censo$n_dom_est), !anyNA(censo$n_pes_est))

razao_pop <- sum(censo$n_pes_est) / POP_CENSO_2022
cat(sprintf("Soma de P0111 = %s | Censo 2022 = %s | razão = %.4f\n",
            format(round(sum(censo$n_pes_est)), big.mark = ".", decimal.mark = ","),
            format(POP_CENSO_2022,              big.mark = ".", decimal.mark = ","),
            razao_pop))
if (abs(razao_pop - 1) > 0.01) warning("Soma dos pesos difere > 1% do total do Censo: investigar.")

dif_rel <- abs(censo$n_pes_dom_est / censo$n_pes_est - 1)
cat("Diferença relativa domicílios x pessoas: mediana =", signif(median(dif_rel), 3),
    "| máx =", signif(max(dif_rel), 3), "\n")

pcts <- grep("^pct_", names(censo), value = TRUE)
stopifnot(all(vapply(censo[pcts], function(x) all(round(x, 5) >= 0 & round(x, 5) <= 100, na.rm = TRUE), logical(1))))
cat("NA por variável (esperado: 0):\n"); print(colSums(is.na(censo)))
cat("Diagnósticos D0240 / D0360 ausentes (% de domicílios particulares):\n")
print(summary(select(censo, starts_with("diag_"))))

if (file.exists(arquivo_v1)) {
  v1   <- read_csv(arquivo_v1, show_col_types = FALSE)
  comp <- inner_join(v1, censo, by = "codigo_municipio")
  cat("Máx. |v1 - v2| (esperado ~ 0):\n")
  print(c(
    rural           = max(abs(comp$prop_rural            - comp$pct_rural)),
    sem_agua        = max(abs(comp$prop_sem_agua_interna - comp$pct_sem_agua_interna)),
    lixo            = max(abs(comp$prop_lixo_inadequado  - comp$pct_lixo_inadequado)),
    parede          = max(abs(comp$prop_parede_inadequada - comp$pct_parede_inadequada)),
    migrante        = max(abs(comp$prop_migrante         - comp$pct_nasc_fora_municipio)),
    ocupados_vs_pea = max(abs(comp$prop_ocupados         - (100 - comp$pct_desocupacao)))
  ))
} else {
  message("v1 não encontrada em ", getwd(), " - teste de regressão ignorado.")
}

dir.create(dir_saida, showWarnings = FALSE, recursive = TRUE)
saveRDS(censo, file.path(dir_saida, "censo2022_municipios.rds"))
write_csv(censo, file.path(dir_saida, "censo2022_municipios.csv"))
print(summary(censo))
cat("\nSalvo em:", dir_saida, "\n")