# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/1 - Completo/Recodificação.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo"))

library(dplyr)
library(readr)

dados <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa2.csv")

# 3. Recodificar as variáveis contínuas para binárias
# Valores > 0 indicam que "tem o recurso" (1)
# Valores == 0 indicam que "não tem o recurso" (0)
# NAs originais serão mantidos como NA automaticamente pelo ifelse
dados_recodificados <- dados %>%
  mutate(
    ind_upa = ifelse(tx_upas_100k > 0, 1, 0),
    ind_leito_uti = ifelse(tx_leitos_uti_100k > 0, 1, 0),
    ind_cisterna = ifelse(tx_cisternas_10k_2004_2023 > 0, 1, 0),
    ind_samu = ifelse(capacidade_regional_samu > 0, 1, 0)
  ) %>%
  mutate(
    ind_upa = factor(ind_upa, levels = c(0, 1), labels = c("Não", "Sim")),
    ind_leito_uti = factor(ind_leito_uti, levels = c(0, 1), labels = c("Não", "Sim")),
    ind_cisterna = factor(ind_cisterna, levels = c(0, 1), labels = c("Não", "Sim")),
    ind_samu = factor(ind_samu, levels = c(0, 1), labels = c("Não", "Sim"))
  )

table(dados_recodificados$ind_upa, useNA = "always")
table(dados_recodificados$ind_leito_uti, useNA = "always")
table(dados_recodificados$ind_cisterna, useNA = "always")
table(dados_recodificados$ind_samu, useNA = "always")

write_csv(dados_recodificados, "Dataset_Fatores_Associados_Chagas_Final_Etapa3.csv")

cat("Recodificação concluída e novo arquivo 'Dataset_Fatores_Associados_Chagas_Final_Etapa3.csv' salvo com sucesso.\n")