# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SIM/DOBR - CHAGAS/DOBR - CHAGAS - PARTE 5.1 - OLS - v2 - GLOBAL.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SIM/DOBR - CHAGAS"))

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, broom, car, lmtest, moments, effectsize, flextable, officer)

dir_dados_completos <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo")
dir_sim_dobr <- file.path(DIR_DADOS, "SIM/DOBR - CHAGAS")

setwd(dir_dados_completos)

df <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Mortalidade_global.csv", show_col_types = FALSE)

setwd(dir_sim_dobr)

# ==============================================================================
# 1. EXECUÇÃO DO MODELO OLS - BLOCO 1 (Nível Distal)
# ==============================================================================

modelo_mortalidade_distal <- lm(
  mort_taxa_padronizada_suavizada ~ pct_negros + amazonia_legal_flag + 
    pct_analfabetismo_15mais + densidade_familias_assentadas_km2 + 
    pct_quilombola_2022 + ivm_global_2022 + pct_indigena_2022 + 
    perc_media_agro_19_21,
  data = df
)

# ==============================================================================
# 2. EXTRAÇÃO BRUTA PARA CSV
# ==============================================================================

betas_std <- standardize_parameters(modelo_mortalidade_distal)
vif_valores <- car::vif(modelo_mortalidade_distal)
residuos <- residuals(modelo_mortalidade_distal)
bp_teste <- bptest(modelo_mortalidade_distal)
sumario <- summary(modelo_mortalidade_distal)

f_p_val <- pf(sumario$fstatistic[1], sumario$fstatistic[2], sumario$fstatistic[3], lower.tail = FALSE)

resumo_coef_bruto <- tidy(modelo_mortalidade_distal) %>%
  left_join(as.data.frame(betas_std) %>% select(Parameter, Std_Coefficient), by = c("term" = "Parameter")) %>%
  left_join(data.frame(term = names(vif_valores), VIF = vif_valores), by = "term") %>%
  rename(Coef_Nao_Padronizado = estimate, Erro_Padrao = std.error, Estatistica_t = statistic, 
         p_valor = p.value, Beta_Padronizado = Std_Coefficient)

diagnosticos_bruto <- data.frame(
  Metrica = c("R2_Multiplo", "R2_Ajustado", "Estatistica_F", "Valor_p_F", 
              "Breusch_Pagan_p", "Skewness", "Kurtosis_Excesso"),
  Valor = c(
    sumario$r.squared, sumario$adj.r.squared, sumario$fstatistic[1],
    f_p_val, bp_teste$p.value, skewness(residuos), kurtosis(residuos) - 3
  )
)

write_csv(resumo_coef_bruto, "OLS_SIM_Mortalidade_Bloco1_Coeficientes_v3_global.csv")
write_csv(diagnosticos_bruto, "OLS_SIM_Mortalidade_Bloco1_Diagnosticos_v3_global.csv")

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

tabela_df <- tidy(modelo_mortalidade_distal) %>%
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
f_p_str <- ifelse(f_p_val < 0.001, "p < 0,001", paste0("p = ", formata_p(f_p_val)))
bp_str <- ifelse(bp_teste$p.value < 0.001, "p < 0,001", paste0("p = ", formata_p(bp_teste$p.value)))
skew_str <- formata_num(skewness(residuos), 2)
kurt_str <- formata_num(kurtosis(residuos) - 3, 2)

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
# 4. EXPORTAÇÃO PARA WORD NO DIRETÓRIO DO SIM
# ==============================================================================

doc <- read_docx() %>%
  
  body_add_par("Tabela 1 - Parâmetros do modelo de regressão OLS para mortalidade por Doença de Chagas (Bloco 1 - Nível Distal - Taxa Global).", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_fpar(fpar(ftext("Bloco 1 \u2014 Resultados (Nível Distal, 8 variáveis)", prop = fp_text(bold = TRUE, font.family = "Times New Roman")))) %>%
  body_add_fpar(fpar(ftext(sprintf("Mortalidade \u2014 R² = %s | R²adj = %s", r2_str, r2adj_str), prop = fp_text(bold = TRUE, font.family = "Times New Roman")))) %>%
  body_add_par(sprintf("F = %s (%s) | BP (heterocedasticidade): %s | Resíduos: Skew = %s, Kurt_exc = %s", f_stat, f_p_str, bp_str, skew_str, kurt_str), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_flextable(ft) %>%
  
  body_add_par("", style = "Normal") %>%
  body_add_par("\u2713 Nenhuma variável com VIF \u2265 10. 8/8 retidas.", style = "Normal") %>%
  body_add_par("Fonte: Elaborado pelo autor (2026).", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("As variáveis do Nível Distal (Bloco 1) foram introduzidas simultaneamente no modelo múltiplo. Para evitar o sobreajuste (overfitting) e respeitar o princípio da parcimônia, adotou-se um critério de retenção estatística intra-bloco: apenas as variáveis que apresentaram valor-p < 0,20 foram mantidas como fatores de ajuste para a entrada do Bloco 2 (Nível Intermediário). Esta filtragem metodológica visou preservar os graus de liberdade e mitigar a inflação da variância.", style = "Normal")

print(doc, target = "Tabela_OLS_SIM_Mortalidade_ABNT_v3_global.docx")
saveRDS(modelo_mortalidade_distal, "Objeto_OLS_SIM_Mortalidade_Bloco1_v3_global.rds")