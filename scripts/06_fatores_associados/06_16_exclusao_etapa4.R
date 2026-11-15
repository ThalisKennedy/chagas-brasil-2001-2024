# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/1 - Completo/Exclusão 3.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo"))

library(dplyr)
library(readr)

dados_etapa3 <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa3.csv")

vars_excluir <- c(
  # Lista do arquivo .md
  "ivm_economia_2022",
  "precipitacao_media_anual_2001_2024",
  "umidade_media_2001_2023",
  "mapbiomas_antropizacao_media_historica_perc",
  "siops_pc_ab_media",
  "siops_pc_vigilancia_media",
  "cobertura_acs",
  "tx_atend_cardio_10k",
  "tx_intern_cardio_10k",
  
  # Lista extra solicitada
  "pct_desocupacao",
  "adesao_brasil_saudavel",
  "maismedicos_media_10k",
  
  # Versões contínuas originais das variáveis que foram recodificadas
  "tx_upas_100k",
  "tx_leitos_uti_100k",
  "tx_cisternas_10k_2004_2023",
  "capacidade_regional_samu"
)

dados_limpos <- dados_etapa3 %>%
  select(-any_of(vars_excluir))

cat("Número de colunas antes da limpeza:", ncol(dados_etapa3), "\n")
cat("Número de colunas após a limpeza:", ncol(dados_limpos), "\n")

write_csv(dados_limpos, "Dataset_Fatores_Associados_Chagas_Final_Etapa4.csv")

cat("Limpeza concluída e novo arquivo 'Dataset_Fatores_Associados_Chagas_Final_Etapa4.csv' salvo com sucesso.\n")