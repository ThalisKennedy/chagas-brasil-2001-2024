# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/1 - Completo/Exclusão e Correlação.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo"))

library(tidyverse)
library(corrplot)

df <- read_csv("Dataset_Fatores_Associado_Chagas_Completo_2.csv", show_col_types = FALSE)

# 2. Definição estrita dos Desfechos (Y) que DEVEM permanecer
y_finais <- c(
  "mort_taxa_padronizada_suavizada", 
  "aguda_taxa_padronizada_suavizada",
  "cronica_taxa_padronizada_suavizada"
)

# 3. Identificação de variáveis que NÃO entrarão no modelo
# 3.1. Chaves, populacionais e amostrais
col_estrutura <- c(
  "n_dom_amostra", "n_dom_est", "n_pes_dom_est", "n_pes_amostra", "n_pes_est",
  "diag_pct_sem_d0240", "diag_pct_sem_d0360", "pop_media_periodo_ibge",
  paste0("pop_total_", 2001:2024)
)

# 3.2. Desfechos intermediários (tudo que começa com mort_, aguda_, cronica_ mas não é o Y final)
todas_mort_aguda_cronica <- grep("^(mort_|aguda_|cronica_)", names(df), value = TRUE)
col_desfechos_remover <- setdiff(todas_mort_aguda_cronica, y_finais)

# 3.3. Variáveis já tratadas/excluídas em etapas anteriores
col_ja_excluidas <- c(
  "acai_area_media_2015_2023", "cana_area_media_2001_2023", 
  "pib_total_2022", 
  "pop_indigena_2022", "pop_quilombola_2022"
)

# 4. Exclusões Baseadas em Correlação (|rho| > 0.70) e Teoria Epidemiológica
col_redundante_teoria <- c(
  # Socioeconômico: Mantém Bolsa Família (pobreza extrema) e Analfabetismo
  "renda_media_pc",          # Excluída (Redundante com BF/Analfabetismo)
  "pib_per_capita_2022",     # Excluída (Redundante e sofre distorção ecológica)
  "perc_media_pub_19_21",    # Excluída (Redundante com BF)
  "pct_ocupados_14mais",     # Excluída (Redundante com Analfabetismo e BF)
  "bf_valor_medio_real_2004_2024", # Excluída (Mantemos a taxa de famílias no BF)
  
  # Clima: Mantém Temperatura Média
  "temp_max_2001_2023",      # Excluída (rho > 0.95 com temp média)
  "temp_min_2001_2023",      # Excluída (rho > 0.95 com temp média)
  
  # Ambiente/MapBiomas: Mantém Antropização
  "mapbiomas_degradacao_media_historica_perc", # Excluída
  "mapbiomas_borda_media_historica_perc",      # Excluída
  
  # Saúde/Estrutura: Mantém histórico (RIPSA)
  "tx_leitos_sus_100k_2023", # Excluída (Desfecho crônico exige exposição histórica)
  "tx_hospitais_sus_100k",   # Excluída (Redundante com leitos)
  
  # SISAB: Mantém Indicador Sintético Global (ISF)
  "sisab_prenatal_6consultas",   # Excluída
  "sisab_prenatal_sifilis_hiv",  # Excluída
  "sisab_prenatal_odonto",       # Excluída
  "sisab_citopatologico",        # Excluída
  "sisab_vacina_polio_penta",    # Excluída
  "sisab_diabetes_hemo_glicada", # Excluída
  
  # Habitação / Vulnerabilidade: Mantém Parede de Taipa (nicho do vetor)
  "pct_parede_inadequada",   # Excluída (muito inespecífico vs Taipa)
  "pct_sem_agua_interna",    # Excluída (Altamente colinear com BF e analfabetismo)
  
  # Demografia/Geografia duplicada
  "pct_indigena",            # Excluída (Substituída pela de 2022 se disponível, senão remover)
  "pct_nasc_outra_uf",       # Excluída (Mantém pct_nasc_outra_regiao, rho=0.8)
  "cod_bioma",               # Excluída (Mantém nome_bioma)
  "adapta_cidades_flag"      # Excluída (Decisão metodológica via DAG: nó folha)
)

todas_exclusoes <- unique(c(
  col_estrutura, 
  col_desfechos_remover, 
  col_ja_excluidas, 
  col_redundante_teoria
))

df_reduzido <- df %>% 
  select(
    codigo_municipio,
    all_of(y_finais),
    everything(),
    -any_of(todas_exclusoes) 
  )

cat("Resumo da Limpeza Metodológica:\n")
cat("- Colunas originais:", ncol(df), "\n")
cat("- Colunas no banco reduzido:", ncol(df_reduzido), "\n")
cat("- Preditores candidatos finais:", ncol(df_reduzido) - 4, "\n\n")

write_csv(df_reduzido, "Dataset_Fatores_Associados_Chagas_Reduzido.csv")
cat("Arquivo salvo com sucesso: 'Dataset_Fatores_Associados_Chagas_Reduzido.csv'\n\n")

# 8. Heatmap de Correlação Final (Prova Metodológica)
# Verifica se as exposições sobreviventes estão livres de multicolinearidade severa
candidatas_finais <- setdiff(names(df_reduzido), c("codigo_municipio", y_finais))
num_vars <- df_reduzido %>% 
  select(all_of(candidatas_finais)) %>% 
  select(where(is.numeric))

M_final <- cor(num_vars, method = "spearman", use = "pairwise.complete.obs")

png("heatmap_correlacao_candidatas_final.png", width = 1600, height = 1600, res = 150)
corrplot(M_final, order = "hclust", tl.cex = 0.6, tl.col = "black", type = "upper")
dev.off()

cat("Heatmap de validação salvo em 'heatmap_correlacao_candidatas_final.png'.\n")
cat("Revise a imagem para garantir que não restam 'manchas escuras' (rho > 0.70) fora da diagonal principal.\n")

# ==============================================================================
# 9. Geração da Tabela de Correlação
# ==============================================================================

tabela_correlacao <- as.data.frame(as.table(M_final)) %>%
  rename(Variavel_1 = Var1, Variavel_2 = Var2, rho = Freq) %>%
  filter(as.character(Variavel_1) < as.character(Variavel_2)) %>%
  arrange(desc(abs(rho))) %>%
  mutate(rho = round(rho, 3))

cat("\nTop 10 maiores correlações sobreviventes no banco reduzido:\n")
print(head(tabela_correlacao, 10))

write_csv(tabela_correlacao, "Tabela_Correlacao_Candidatas_Final.csv")
cat("\nTabela tabular completa salva em: 'Tabela_Correlacao_Candidatas_Final.csv'\n")