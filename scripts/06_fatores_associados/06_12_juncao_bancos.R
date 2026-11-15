# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/1 - Completo/Junção - Bancos Agregados.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo"))

library(tidyverse)

raiz <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados")

dir_saida <- file.path(raiz, "Análises")
if(!dir.exists(dir_saida)) {
  dir.create(dir_saida, recursive = TRUE)
  cat("Pasta 'Análises' criada com sucesso.\n")
}

arq_censo     <- file.path(raiz, "Censo 2022", "censo2022_municipios.csv")
arq_desfecho  <- file.path(raiz, "Desfechos", "Desfechos_Chagas_Agregado_Municipios.csv")
arq_sidra     <- file.path(raiz, "SIDRA", "SIDRA_Agregado_Municipios_FINAL_CORRIGIDO_2.csv")
arq_idems     <- file.path(raiz, "IDE-MS", "IDEMS_Agregado_Municipios_FINAL_CORRIGIDO.csv")
arq_cidacs    <- file.path(raiz, "CIDACS", "CIDACS_Agregado_Municipios_FINAL_v13.csv")
arq_mapbiomas <- file.path(raiz, "MapBiomas", "MapBiomas_Agregado_Municipios_FINAL.csv")
arq_portalsus <- file.path(raiz, "Portal de Dados Abertos do SUS", "Portal_Dados_Abertos_SUS_CHAGAS_FINAL.csv")

ler_matriz <- function(caminho) {
  read_csv(caminho, col_types = cols(codigo_municipio = "c", .default = "?"), show_col_types = FALSE)
}

cat("\nImportando as 7 matrizes para a memória...\n")
df_censo     <- ler_matriz(arq_censo) %>% mutate(codigo_municipio = as.character(codigo_municipio))
df_desfecho  <- ler_matriz(arq_desfecho) %>% mutate(codigo_municipio = as.character(codigo_municipio))
df_sidra     <- ler_matriz(arq_sidra) %>% mutate(codigo_municipio = as.character(codigo_municipio))
df_idems     <- ler_matriz(arq_idems) %>% mutate(codigo_municipio = as.character(codigo_municipio))
df_cidacs    <- ler_matriz(arq_cidacs) %>% mutate(codigo_municipio = as.character(codigo_municipio))
df_mapbiomas <- ler_matriz(arq_mapbiomas) %>% mutate(codigo_municipio = as.character(codigo_municipio))
df_portalsus <- ler_matriz(arq_portalsus) %>% mutate(codigo_ibge6 = as.character(codigo_ibge6))

cat("Executando o Master Join (Censo como Gabarito)...\n")

df_master <- df_censo %>%
  # 1. Acopla as Variáveis Dependentes (Y)
  left_join(df_desfecho,  by = "codigo_municipio") %>%
  
  # 2. Acopla os Blocos de Covariáveis Independentes (X) - 7 Dígitos
  left_join(df_sidra,     by = "codigo_municipio") %>%
  left_join(df_idems,     by = "codigo_municipio") %>%
  left_join(df_cidacs,    by = "codigo_municipio") %>%
  left_join(df_mapbiomas, by = "codigo_municipio") %>%
  
  # 3. Acopla o Portal SUS (Criando temporariamente a chave de 6 dígitos)
  mutate(codigo_ibge6 = str_sub(codigo_municipio, 1, 6)) %>%
  left_join(df_portalsus, by = "codigo_ibge6") %>%
  select(-codigo_ibge6) 

df_master <- df_master %>% 
  select(-matches("nome_municipio\\.|NM_MUN\\.|municipio\\."))

arquivo_final <- file.path(dir_saida, "Dataset_Fatores_Associado_Chagas_Completo.csv")
write_csv(df_master, arquivo_final)

cat("Total de Municípios (Linhas):", nrow(df_master), "\n")
cat("Total de Variáveis (Colunas):", ncol(df_master), "\n")
cat("Arquivo salvo em:\n", arquivo_final, "\n")
cat("=========================================================\n")
glimpse(df_master)