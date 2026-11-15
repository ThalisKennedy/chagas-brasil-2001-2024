# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: E-SUS/DCCRBR23/DCCRBR23 - PARTE 5.3 - PROXIMAL - GLOBAL.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "E-SUS/DCCRBR23"))

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, broom, car, lmtest, moments, effectsize, flextable, officer, sandwich)

dir_dados_completos <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo")
dir_esus_dccr <- file.path(DIR_DADOS, "E-SUS/DCCRBR23")

setwd(dir_dados_completos)
df <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Cronica_global.csv", show_col_types = FALSE)

setwd(dir_esus_dccr)

# ==============================================================================
# 1. EXECUÇÃO DO MODELO OLS - BLOCO 3 INICIAL (CRÔNICA)
# (12 Retidas dos Blocos 1 e 2 + 6 Proximais = 18 variáveis)
# ==============================================================================

modelo_cronica_bloco3_inicial <- lm(
  cronica_taxa_padronizada_suavizada ~ 
    # --- Retidas do Bloco 1 (4 Variáveis Distais com p < 0,20 no B2) ---
    amazonia_legal_flag + pct_quilombola_2022 + 
    pct_indigena_2022 + densidade_familias_assentadas_km2 +
    
    # --- Retidas do Bloco 2 (8 Variáveis Intermediárias com p < 0,20 no B2) ---
    spei_media_2001_2024 + mapbiomas_fogo_media_historica_perc + 
    mapbiomas_vegsec_media_historica_perc + mapbiomas_desmatamento_media_anual_perc + 
    sisab_isf_global + razao_consultas_esp_media + 
    adensamento_medio + pct_esgoto_inadequado +
    
    # --- Variáveis do Bloco 3 (6 Nível Proximal) ---
    pct_nasc_fora_municipio + 
    pct_nasc_outra_regiao + 
    pct_subsistencia_14mais + 
    pct_area_acai + 
    pct_area_cana + 
    pct_parede_taipa,
  
  data = df
)

modelo_cronica_b3_robusto <- coeftest(
  modelo_cronica_bloco3_inicial, 
  vcov = vcovHC(modelo_cronica_bloco3_inicial, type = "HC3")
)

# ==============================================================================
# 2. EXTRAÇÃO BRUTA PARA CSV
# ==============================================================================

betas_std <- standardize_parameters(modelo_cronica_bloco3_inicial)
vif_valores <- car::vif(modelo_cronica_bloco3_inicial)
residuos <- residuals(modelo_cronica_bloco3_inicial)
bp_teste <- bptest(modelo_cronica_bloco3_inicial)
sumario <- summary(modelo_cronica_bloco3_inicial)

f_p_val <- pf(sumario$fstatistic[1], sumario$fstatistic[2], sumario$fstatistic[3], lower.tail = FALSE)

resumo_coef_bruto <- tidy(modelo_cronica_b3_robusto) %>%
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
    f_p_val, bp_teste$p.value, skewness(residuos, na.rm = TRUE), kurtosis(residuos, na.rm = TRUE) - 3
  )
)

write_csv(resumo_coef_bruto, "OLS_ESUS_Cronica_Bloco3_Inicial_Coeficientes_v3_global.csv")
write_csv(diagnosticos_bruto, "OLS_ESUS_Cronica_Bloco3_Inicial_Diagnosticos_v3_global.csv")

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

tabela_df <- tidy(modelo_cronica_b3_robusto) %>%
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
# 4. EXPORTAÇÃO PARA WORD NO DIRETÓRIO DO E-SUS
# ==============================================================================

texto_metodologico_b3 <- "As 6 variáveis do Nível Proximal (Bloco 3) foram introduzidas simultaneamente às 12 variáveis de ajuste retidas dos Blocos 1 e 2. Conforme o protocolo metodológico estabelecido, proceder-se-á com a avaliação da inflação da variância (VIF < 10) e a exclusão manual iterativa das variáveis que apresentarem valor-p HC3 ≥ 0,20 para derivar o Modelo Hierárquico Final para a notificação de Doença de Chagas Crônica."

doc <- read_docx() %>%
  
  body_add_par("Tabela 5 - Parâmetros do modelo de regressão OLS inicial para notificação de Doença de Chagas Crônica (Bloco 3 - Nível Proximal - Taxa Global).", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_fpar(fpar(ftext("Bloco 3 \u2014 Resultados Iniciais (18 variáveis simultâneas)", prop = fp_text(bold = TRUE, font.family = "Times New Roman")))) %>%
  body_add_fpar(fpar(ftext(sprintf("Chagas Crônica \u2014 R² = %s | R²adj = %s", r2_str, r2adj_str), prop = fp_text(bold = TRUE, font.family = "Times New Roman")))) %>%
  body_add_par(sprintf("F = %s (%s) | BP (heterocedasticidade): %s | Resíduos: Skew = %s, Kurt_exc = %s", f_stat, f_p_str, bp_str, skew_str, kurt_str), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_flextable(ft) %>%
  
  body_add_par("", style = "Normal") %>%
  body_add_par("Nota: Valores-p calculados utilizando Erros Padrão Robustos (HC3) devido à presença de heterocedasticidade.", style = "Normal") %>%
  body_add_par("Fonte: Elaborado pelo autor (2026).", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par(texto_metodologico_b3, style = "Normal")

print(doc, target = "Tabela_OLS_ESUS_Cronica_Bloco3_Inicial_ABNT_v3_global.docx")
saveRDS(modelo_cronica_bloco3_inicial, "Objeto_OLS_ESUS_Cronica_Bloco3_Inicial_v3_global.rds")
saveRDS(modelo_cronica_b3_robusto, "Objeto_OLS_ESUS_Cronica_Bloco3_HC3_v3_global.rds")