# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/1 - Completo/Exclusão 4.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo"))

library(dplyr)
library(readr)

dados_etapa4 <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa4.csv")

vars_excluir_etapa5 <- c(
  "ind_upa",
  "ind_leito_uti",
  "ind_samu"
)

dados_limpos_etapa5 <- dados_etapa4 %>%
  select(-any_of(vars_excluir_etapa5))

cat("Número de colunas na Etapa 4:", ncol(dados_etapa4), "\n")
cat("Número de colunas na Etapa 5 após limpeza:", ncol(dados_limpos_etapa5), "\n")

write_csv(dados_limpos_etapa5, "Dataset_Fatores_Associados_Chagas_Final_Etapa5.csv")

cat("Limpeza concluída! O banco de dados 'Dataset_Fatores_Associados_Chagas_Final_Etapa5.csv' está pronto e salvo no diretório.\n")