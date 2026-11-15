# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/1 - Completo/Exclusão 2.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo"))

library(tidyverse)

setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo"))

df_etapa1 <- read_csv("Dataset_Fatores_Associados_Chagas_Reduzido.csv", show_col_types = FALSE)

exclusoes_etapa2 <- c(
  "bf_tx_familias_1k_2004_2024", # Redundante com pct_analfabetismo_15mais e pct_negros
  "pct_lixo_inadequado",         # Redundante com pct_rural e pct_subsistencia_14mais
  "temp_media_2001_2023",        # Redundante com pct_negros
  "nome_bioma"                   # Removida por instabilidade metodológica (categoria n=9)
)

df_etapa2 <- df_etapa1 %>%
  select(-any_of(exclusoes_etapa2))

cat("Resumo da Etapa 2 de Exclusões (Correlações e Bioma):\n")
cat("- Colunas no início da etapa:", ncol(df_etapa1), "\n")
colunas_removidas <- intersect(names(df_etapa1), exclusoes_etapa2)
cat("- Quantidade de variáveis removidas:", length(colunas_removidas), "\n")
cat("- Colunas no banco final:", ncol(df_etapa2), "\n\n")

write_csv(df_etapa2, "Dataset_Fatores_Associados_Chagas_Final_Etapa2.csv")
cat("Ficheiro guardado com sucesso: 'Dataset_Fatores_Associados_Chagas_Final_Etapa2.csv'\n")