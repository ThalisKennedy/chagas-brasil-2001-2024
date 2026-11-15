# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SINAN/CHAGBR 2001-2024/CHAGBR 2001-2024 - Parte 5.5 - MORAN E LM TESTE - v2 - GLOBAL.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SINAN/CHAGBR 2001-2024"))

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, sf, spdep, spatialreg, flextable, officer, dplyr)

dir_dados <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo")
dir_shapefile <- file.path(DIR_DADOS, "Análises/Parte 4 - Análise Espacial/Shapes") 
dir_exportacao <- file.path(DIR_DADOS, "SINAN/CHAGBR 2001-2024")

setwd(dir_dados)
df <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Aguda_global.csv", show_col_types = FALSE)

setwd(dir_shapefile)
mapa_br <- st_read("BR_Municipios_2024.shp", quiet = TRUE) 

df <- df %>% mutate(ibge_csv_str = as.character(codigo_municipio)) 
mapa_br <- mapa_br %>% mutate(CD_MUN = as.character(CD_MUN))

if (nchar(df$ibge_csv_str[1]) == 6) {
  mapa_br <- mapa_br %>% mutate(chave_ibge = substr(CD_MUN, 1, 6))
} else {
  mapa_br <- mapa_br %>% mutate(chave_ibge = CD_MUN)
}

mapa_df <- mapa_br %>%
  inner_join(df, by = c("chave_ibge" = "ibge_csv_str")) %>%
  filter(!is.na(residuos_ols_aguda))

cat("Dimensões do mapa após o join:", nrow(mapa_df), "municípios (Aguda).\n")

# ------------------------------------------------------------------------------
# 1. CRIAÇÃO DAS MATRIZES DE PESOS ESPACIAIS (QUEEN E KNN)
# ------------------------------------------------------------------------------
sf_use_s2(FALSE) 

# Matriz 1: Queen
vizinhos_queen <- poly2nb(mapa_df, queen = TRUE)
matriz_W_queen <- nb2listw(vizinhos_queen, style = "W", zero.policy = TRUE)

ilhas_idx <- which(card(vizinhos_queen) == 0)
cat("\nTotal de municípios sem vizinhos (Queen):", length(ilhas_idx), "\n")
if(length(ilhas_idx) > 0) {
  cat("Municípios isolados:", paste(mapa_df$NM_MUN[ilhas_idx], collapse = ", "), "\n")
}

# Matriz 2: KNN (K = 5)
coords <- st_centroid(st_geometry(mapa_df))
vizinhos_knn <- knn2nb(knearneigh(coords, k = 5))
matriz_W_knn <- nb2listw(vizinhos_knn, style = "W")

# ------------------------------------------------------------------------------
# 2. MODELO OLS BASE
# ------------------------------------------------------------------------------
# Regressão contendo apenas as 19 Variáveis validadas do modelo OLS final
modelo_aguda_espacial <- lm(
  aguda_taxa_padronizada_suavizada ~ 
    # Distais (4)
    amazonia_legal_flag + densidade_familias_assentadas_km2 + 
    pct_indigena_2022 + perc_media_agro_19_21 +
    # Intermediárias (9)
    mapbiomas_vegsec_media_historica_perc + mapbiomas_desmatamento_media_anual_perc + 
    cobertura_aps + ripsa_tx_enfermeiros_media + sisab_isf_global + 
    razao_consultas_aps_media + adensamento_medio + pct_rural + 
    sisagua_cobertura_media +
    # Proximais (6)
    pct_nasc_fora_municipio + pct_nasc_outra_regiao + pct_subsistencia_14mais + 
    pct_area_acai + pct_area_cana + pct_parede_taipa,
  data = mapa_df,
  na.action = na.exclude
)

# ------------------------------------------------------------------------------
# 3. TESTE DE I DE MORAN
# ------------------------------------------------------------------------------
# Moran com matriz Queen
moran_queen <- lm.morantest(modelo_aguda_espacial, listw = matriz_W_queen, zero.policy = TRUE)
# Moran com matriz KNN
moran_knn <- lm.morantest(modelo_aguda_espacial, listw = matriz_W_knn)

cat("\n=== I de Moran Global (Queen) ===\n")
print(moran_queen)
cat("\n=== I de Moran Global (KNN-5) ===\n")
print(moran_knn)

# ------------------------------------------------------------------------------
# 4. TESTES DO MULTIPLICADOR DE LAGRANGE (LM TESTS)
# ------------------------------------------------------------------------------
lm_testes_queen <- suppressWarnings(lm.LMtests(modelo_aguda_espacial, listw = matriz_W_queen, test = "all", zero.policy = TRUE))
lm_testes_knn <- suppressWarnings(lm.LMtests(modelo_aguda_espacial, listw = matriz_W_knn, test = "all"))

# ------------------------------------------------------------------------------
# 5. EXPORTAÇÃO DOS RESULTADOS (CSV)
# ------------------------------------------------------------------------------
res_diagnostico <- data.frame(
  Matriz = c(rep("Queen", 6), rep("KNN_5", 6)),
  Teste = rep(c("Moran_I", "LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"), 2),
  Estatistica = c(
    moran_queen$estimate[1], sapply(lm_testes_queen, function(x) x$statistic),
    moran_knn$estimate[1], sapply(lm_testes_knn, function(x) x$statistic)
  ),
  p_valor = c(
    moran_queen$p.value, sapply(lm_testes_queen, function(x) x$p.value),
    moran_knn$p.value, sapply(lm_testes_knn, function(x) x$p.value)
  )
)

setwd(dir_exportacao)
write_csv(res_diagnostico, "Diagnostico_Espacial_Aguda_Completo_V2_global.csv")
cat("\nResultados com Moran, LM Tests e comparação Queen/KNN exportados em 'Diagnostico_Espacial_Aguda_Completo_V2_global.csv'.\n")

# ------------------------------------------------------------------------------
# 6. GERAÇÃO E EXPORTAÇÃO DA TABELA ABNT (DIAGNÓSTICO ESPACIAL)
# ------------------------------------------------------------------------------
tabela_diag <- res_diagnostico %>%
  mutate(
    Sig = case_when(
      p_valor < 0.001 ~ "***",
      p_valor < 0.01  ~ "**",
      p_valor < 0.05  ~ "*",
      TRUE            ~ "—"
    ),
    `Valor-p` = ifelse(p_valor < 0.001, "<0,001", sprintf("%.3f", p_valor)),
    Estatística = sprintf("%.4f", Estatistica)
  ) %>%
  mutate(across(c(`Valor-p`, Estatística), ~gsub("\\.", ",", .))) %>%
  select(Matriz, Teste, Estatística, `Valor-p`, Sig)

set_flextable_defaults(font.family = "Times New Roman", font.size = 11)

tabela_abnt_espacial <- flextable(tabela_diag) %>%
  merge_v(j = "Matriz") %>%
  valign(j = "Matriz", valign = "top") %>%
  border_remove() %>%
  hline_top(border = fp_border(color = "black", width = 1.5), part = "header") %>%
  hline_bottom(border = fp_border(color = "black", width = 1), part = "header") %>%
  hline_bottom(border = fp_border(color = "black", width = 1.5), part = "body") %>%
  align(j = 1:2, align = "left", part = "all") %>%
  align(j = 3:5, align = "center", part = "all") %>%
  autofit() %>%
  set_caption(
    caption = as_paragraph(as_chunk("Tabela 5 – Diagnósticos de dependência espacial global (I de Moran e Testes LM) para o modelo de Chagas Aguda (Taxa Global).", props = fp_text(bold = FALSE, font.family = "Times New Roman")))
  ) %>%
  add_footer_lines(c(
    "Nota: *** p < 0,001; ** p < 0,01; * p < 0,05.",
    "Fonte: Elaborado pelo autor (2026)."
  )) %>%
  fontsize(size = 10, part = "footer") %>%
  font(fontname = "Times New Roman", part = "all")

setwd(dir_exportacao)
doc <- read_docx() %>%
  body_add_flextable(value = tabela_abnt_espacial, align = "center") %>%
  print(target = "Tabela_Diagnostico_Espacial_Aguda_ABNT_global.docx")

cat("\nTabela de Diagnóstico Espacial em formato ABNT gerada e exportada para 'Tabela_Diagnostico_Espacial_Aguda_ABNT_global.docx'.\n")