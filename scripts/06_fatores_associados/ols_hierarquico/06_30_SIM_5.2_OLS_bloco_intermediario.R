# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SIM/DOBR - CHAGAS/DOBR - CHAGAS - PARTE 5.2 - INTERMEDIARIO - GLOBAL.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SIM/DOBR - CHAGAS"))

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, broom, car, lmtest, moments, effectsize, flextable, officer, sandwich)

dir_dados_completos <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo")
dir_sim_dobr <- file.path(DIR_DADOS, "SIM/DOBR - CHAGAS")

setwd(dir_dados_completos)
# Carregamento da base corrigida com a taxa bayesiana global
df <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Mortalidade_global.csv", show_col_types = FALSE)

setwd(dir_sim_dobr)

# ==============================================================================
# 1. EXECUÇÃO DO MODELO OLS - BLOCO 2 (Distais Retidas + Intermediárias)
# ==============================================================================
modelo_mortalidade_bloco2_inicial <- lm(
  mort_taxa_padronizada_suavizada ~ 
    # Variáveis retidas do Bloco 1 (Todas as 8 Distais mantidas p < 0,20)
    pct_negros + amazonia_legal_flag + pct_analfabetismo_15mais + 
    densidade_familias_assentadas_km2 + pct_quilombola_2022 + 
    ivm_global_2022 + pct_indigena_2022 + perc_media_agro_19_21 +
    
    # Nível Intermediário - Ambiente e Vetor (4)
    spei_media_2001_2024 + mapbiomas_fogo_media_historica_perc + 
    mapbiomas_vegsec_media_historica_perc + mapbiomas_desmatamento_media_anual_perc +
    
    # Nível Intermediário - Capacidade de Saúde (11)
    cobertura_aps + tx_equipamentos_sus_100k_2023 + tx_horas_medicos_aps_sus_10k_2023 + 
    ripsa_tx_enfermeiros_media + ripsa_tx_leitos_sus_media + ripsa_tx_raiox_media + 
    ripsa_tx_usg_media + sisab_isf_global + siops_pc_total_media + 
    razao_consultas_aps_media + razao_consultas_esp_media +
    
    # Nível Intermediário - Saneamento e Urbanismo (7)
    adensamento_medio + pct_rural + pct_esgoto_inadequado + 
    cidacs_indice_precariedade_urbana_2022 + cidacs_perc_arborizacao_densa_2022 + 
    ind_cisterna + sisagua_cobertura_media,
  
  data = df
)

# Aplicação de Erros Padrão Robustos (HC3) devido à heterocedasticidade detectada no Bloco 1
modelo_mortalidade_b2_robusto <- coeftest(
  modelo_mortalidade_bloco2_inicial, 
  vcov = vcovHC(modelo_mortalidade_bloco2_inicial, type = "HC3")
)

# ==============================================================================
# 2. EXTRAÇÃO BRUTA PARA CSV
# ==============================================================================

betas_std <- standardize_parameters(modelo_mortalidade_bloco2_inicial)
vif_valores <- car::vif(modelo_mortalidade_bloco2_inicial)
residuos <- residuals(modelo_mortalidade_bloco2_inicial)
bp_teste <- bptest(modelo_mortalidade_bloco2_inicial)
sumario <- summary(modelo_mortalidade_bloco2_inicial)

# Cálculo dinâmico do p-valor do modelo (Estatística F)
f_p_val <- pf(sumario$fstatistic[1], sumario$fstatistic[2], sumario$fstatistic[3], lower.tail = FALSE)

resumo_coef_bruto <- tidy(modelo_mortalidade_b2_robusto) %>%
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
    f_p_val, bp_teste$p.value, skewness(residuos), kurtosis(residuos) - 3
  )
)

write_csv(resumo_coef_bruto, "OLS_SIM_Mortalidade_Bloco2_Inicial_Coeficientes_v3_global.csv")
write_csv(diagnosticos_bruto, "OLS_SIM_Mortalidade_Bloco2_Inicial_Diagnosticos_v3_global.csv")

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

tabela_df <- tidy(modelo_mortalidade_b2_robusto) %>%
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
  
  body_add_par("Tabela 2 - Parâmetros do modelo de regressão OLS inicial para mortalidade por Doença de Chagas (Bloco 2 - Nível Intermediário - Taxa Global).", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_fpar(fpar(ftext("Bloco 2 \u2014 Resultados Iniciais (30 variáveis simultâneas)", prop = fp_text(bold = TRUE, font.family = "Times New Roman")))) %>%
  body_add_fpar(fpar(ftext(sprintf("Mortalidade \u2014 R² = %s | R²adj = %s", r2_str, r2adj_str), prop = fp_text(bold = TRUE, font.family = "Times New Roman")))) %>%
  body_add_par(sprintf("F = %s (%s) | BP (heterocedasticidade): %s | Resíduos: Skew = %s, Kurt_exc = %s", f_stat, f_p_str, bp_str, skew_str, kurt_str), style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_flextable(ft) %>%
  
  body_add_par("", style = "Normal") %>%
  body_add_par("Nota: Valores-p calculados utilizando Erros Padrão Robustos (HC3) devido à presença de heterocedasticidade.", style = "Normal") %>%
  body_add_par("Fonte: Elaborado pelo autor (2026).", style = "Normal") %>%
  body_add_par("", style = "Normal") %>%
  
  body_add_par("As variáveis do Nível Intermediário foram introduzidas simultaneamente às variáveis retidas do Bloco 1. Conforme a regra metodológica, proceder-se-á agora com a exclusão manual iterativa das variáveis que apresentarem VIF \u2265 10, seguida da exclusão daquelas com valor-p \u2265 0,20, a fim de derivar o modelo final do Bloco 2 que avançará para o Nível Proximal.", style = "Normal")

print(doc, target = "Tabela_OLS_SIM_Mortalidade_Bloco2_Inicial_ABNT_v3_global.docx")
saveRDS(modelo_mortalidade_bloco2_inicial, "Objeto_OLS_SIM_Mortalidade_Bloco2_Inicial_v3_global.rds")
saveRDS(modelo_mortalidade_b2_robusto, "Objeto_OLS_SIM_Mortalidade_Bloco2_HC3_v3_global.rds")