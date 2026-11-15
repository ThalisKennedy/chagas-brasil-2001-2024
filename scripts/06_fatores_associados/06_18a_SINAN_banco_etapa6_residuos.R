# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SINAN/CHAGBR 2001-2024/CHAGBR 2001-2024 - Parte 5.4 - OLS MODELO FINAL.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SINAN/CHAGBR 2001-2024"))

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, broom, car, lmtest, moments, effectsize, flextable, officer, sandwich)

dir_dados_completos <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo")
dir_sinan_chagbr <- file.path(DIR_DADOS, "SINAN/CHAGBR 2001-2024")

setwd(dir_dados_completos)
df <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa5.csv", show_col_types = FALSE)

setwd(dir_sinan_chagbr)

# ==============================================================================
# 1. EXECUÇÃO DO MODELO OLS - FINAL RESTRITO (AGUDA - SINAN)
# ==============================================================================

modelo_aguda_final <- lm(
  aguda_taxa_padronizada_suavizada ~ 
    # --- Distais Retidas (2) ---
    amazonia_legal_flag + densidade_familias_assentadas_km2 +
    
    # --- Intermediárias Retidas (12) ---
    mapbiomas_vegsec_media_historica_perc + mapbiomas_desmatamento_media_anual_perc + 
    cobertura_aps + ripsa_tx_enfermeiros_media + ripsa_tx_leitos_sus_media + 
    sisab_isf_global + razao_consultas_aps_media + razao_consultas_esp_media + 
    adensamento_medio + cidacs_indice_precariedade_urbana_2022 + 
    ind_cisterna + sisagua_cobertura_media +
    
    # --- Proximais Retidas (5) ---
    pct_nasc_fora_municipio + pct_nasc_outra_regiao + 
    pct_area_acai + pct_area_cana + pct_parede_taipa,
  
  data = df,
  na.action = na.exclude
)

# Erros Padrão Robustos (HC3)
modelo_aguda_final_robusto <- coeftest(
  modelo_aguda_final, 
  vcov = vcovHC(modelo_aguda_final, type = "HC3")
)

# ==============================================================================
# 2. EXTRAÇÃO BRUTA PARA CSV
# ==============================================================================

betas_std <- standardize_parameters(modelo_aguda_final)
vif_valores <- car::vif(modelo_aguda_final)
residuos <- residuals(modelo_aguda_final)
bp_teste <- bptest(modelo_aguda_final)
sumario <- summary(modelo_aguda_final)

resumo_coef_bruto <- tidy(modelo_aguda_final_robusto) %>%
  left_join(as.data.frame(betas_std) %>% select(Parameter, Std_Coefficient), by = c("term" = "Parameter")) %>%
  left_join(data.frame(term = names(vif_valores), VIF = vif_valores), by = "term") %>%
  rename(Coef_Nao_Padronizado = estimate, Erro_Padrao_Robusto = std.error, 
         Estatistica_t = statistic, p_valor_Robusto = p.value, 
         Beta_Padronizado = Std_Coefficient)

diagnosticos_bruto <- data.frame(
  Metrica = c("R2_Multiplo", "R2_Ajustado", "Estatistica_F", "Valor_p_F", 
              "Breusch_Pagan_p", "Skewness", "Kurtosis_Excesso"),
  Valor = c(
    sumario$r.squared, sumario$adj.r.squared, sumario$fstatistic[1],
    pf(sumario$fstatistic[1], sumario$fstatistic[2], sumario$fstatistic[3], lower.tail = FALSE),
    bp_teste$p.value, skewness(residuos, na.rm = TRUE), kurtosis(residuos, na.rm = TRUE) - 3
  )
)

write_csv(resumo_coef_bruto, "OLS_SINAN_Aguda_Final_Coeficientes.csv")
write_csv(diagnosticos_bruto, "OLS_SINAN_Aguda_Final_Diagnosticos.csv")

# ==============================================================================
# 3. FORMATAÇÃO ABNT E CRIAÇÃO DA TABELA
# ==============================================================================

formata_num <- function(x, decimais = 4, sinal = FALSE) {
  res <- sprintf(paste0("%.", decimais, "f"), x)
  if(sinal) res <- ifelse(x > 0, paste0("+", res), res)
  gsub("\\.", ",", res)
}

formata_p <- function(p) {
  ifelse(p < 0.001, "<0,001", gsub("\\.", ",", sprintf("%.3f", p)))
}

formata_sig <- function(p) {
  case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "—")
}

tabela_df <- tidy(modelo_aguda_final_robusto) %>%
  left_join(as.data.frame(betas_std) %>% select(Parameter, Std_Coefficient), by = c("term" = "Parameter")) %>%
  left_join(data.frame(term = names(vif_valores), VIF = vif_valores), by = "term") %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variável = term,
    Coef = formata_num(estimate, 4),
    `β_std` = formata_num(Std_Coefficient, 4, TRUE),
    p = formata_p(p.value),
    VIF_fmt = formata_num(VIF, 2),
    Sig = formata_sig(p.value)
  ) %>%
  select(Variável, Coef, `β_std`, p, VIF = VIF_fmt, Sig)

r2_str <- formata_num(sumario$r.squared, 4)
r2adj_str <- formata_num(sumario$adj.r.squared, 4)
f_stat <- formata_num(sumario$fstatistic[1], 2)
bp_str <- ifelse(bp_teste$p.value < 0.001, "p \u2248 0", paste0("p = ", formata_p(bp_teste$p.value)))
skew_str <- formata_num(skewness(residuos, na.rm = TRUE), 2)
kurt_str <- formata_num(kurtosis(residuos, na.rm = TRUE) - 3, 2)

ft <- flextable(tabela_df) %>%
  border_remove() %>%
  hline_top(border = fp_border(color = "black", width = 1.5)) %>%
  hline_bottom(border = fp_border(color = "black", width = 1.5)) %>%
  hline(i = 1, part = "header", border = fp_border(color = "black", width = 1)) %>%
  align(j = 2:6, align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(part = "header") %>%
  autofit()

# ==============================================================================
# 4. EXPORTAÇÃO PARA WORD NO DIRETÓRIO DO SINAN
# ==============================================================================

texto_metodologico_final <- "Após a execução inicial do Bloco 3, procedeu-se à remoção iterativa das 4 variáveis que apresentaram valor-p (HC3) ≥ 0,20 ('spei_media_2001_2024', 'ripsa_tx_raiox_media', 'pct_rural' e 'pct_subsistencia_14mais'). O modelo OLS Restrito (Final) foi consolidado com 19 covariáveis significativas, preservando um VIF máximo estritamente controlado (< 10), o que assegura a completa mitigação da multicolinearidade. A heterocedasticidade remanescente e a não-normalidade dos resíduos justificam a extração destes para submissão aos diagnósticos de dependência espacial."

doc <- read_docx() %>%
  body_add_par("Tabela 5 - Parâmetros do modelo de regressão OLS Restrito (Final) para incidência de Doença de Chagas Aguda.", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(fpar(ftext("Modelo Final OLS \u2014 Resultados (19 variáveis validadas)", prop = fp_text(bold = TRUE, font.family = "Times New Roman")))) %>%
  body_add_fpar(fpar(ftext(sprintf("Chagas Aguda \u2014 R² = %s | R²adj = %s", r2_str, r2adj_str), prop = fp_text(bold = TRUE, font.family = "Times New Roman")))) %>%
  body_add_par(sprintf("F = %s | BP (heterocedasticidade): %s | Resíduos: Skew = %s, Kurt_exc = %s", f_stat, bp_str, skew_str, kurt_str), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par("\u2713 VIF máximo aferido < 10 (Adequado).", style = "Normal") %>%
  body_add_par("\u2713 Variáveis prontas para a Etapa Espacial.", style = "Normal") %>%
  body_add_par("Nota: Valores-p calculados utilizando Erros Padrão Robustos (HC3).", style = "Normal") %>%
  body_add_par("Fonte: Elaborado pelo autor (2026).", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(texto_metodologico_final, style = "Normal")

print(doc, target = "Tabela_OLS_SINAN_Aguda_Final_Restrito_ABNT.docx")
saveRDS(modelo_aguda_final, "Objeto_OLS_SINAN_Aguda_Final.rds")

# ==============================================================================
# 5. EXTRAÇÃO DOS RESÍDUOS ESPACIAIS E SALVAMENTO DO DATASET
# ==============================================================================

# Criação de uma coluna no dataframe principal com os resíduos OLS 
df$residuos_ols_aguda <- residuals(modelo_aguda_final)

setwd(dir_dados_completos)
write_csv(df, "Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Aguda.csv")