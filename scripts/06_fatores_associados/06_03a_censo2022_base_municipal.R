# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/Censo 2022/Censo 2022 - v1.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Censo 2022"))

library(tidyverse)
library(data.table)

caminho_base <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/Bancos/Microdados - Censo 2022/microdados_censo_amostra_2022_csv_20260901_003504") 

arquivos_dom <- list.files(path = caminho_base, pattern = "Domicilios_.*\\.csv", recursive = TRUE, full.names = TRUE)
arquivos_pes <- list.files(path = caminho_base, pattern = "Pessoas_.*\\.csv", recursive = TRUE, full.names = TRUE)

cat("Arquivos de Domicílios encontrados:", length(arquivos_dom), "\n")
cat("Arquivos de Pessoas encontrados:", length(arquivos_pes), "\n")

processar_domicilios <- function(arquivo) {
  df <- fread(arquivo)
  
  df_agg <- df %>%
    filter(!is.na(D0080) & !is.na(D0111)) %>%
    group_by(codigo_municipio = D0080) %>%
    summarise(
      # D0360: Rendimento domiciliar per capita
      renda_media_pc = weighted.mean(D0360, w = D0111, na.rm = TRUE),
      
      # D0240: Adensamento - Número de moradores por cômodo-dormitório
      adensamento_medio = weighted.mean(D0240, w = D0111, na.rm = TRUE),
      
      # D0140: Proporção de Domicílios Rurais (Situação do domicílio == 2)
      prop_rural = sum(D0111[D0140 == 2], na.rm = TRUE) / sum(D0111, na.rm = TRUE),
      
      # D0210: Proporção Paredes Inadequadas (3=Taipa sem revestimento, 5=Madeira aproveitada, 6=Outro, 7=Sem parede)
      prop_parede_inadequada = sum(D0111[D0210 %in% c(3, 5, 6, 7)], na.rm = TRUE) / sum(D0111[!is.na(D0210)], na.rm = TRUE),
      
      # D0250: Proporção Esgoto Inadequado (4=Fossa rudimentar, 5=Vala, 6=Rio/Lago, 7=Outra forma)
      prop_esgoto_inadequado = sum(D0111[D0250 %in% c(4, 5, 6, 7)], na.rm = TRUE) / sum(D0111[!is.na(D0250)], na.rm = TRUE),
      
      # D0300: Proporção Sem Água Canalizada Interna (2=Só no terreno, 3=Não chega encanada)
      prop_sem_agua_interna = sum(D0111[D0300 %in% c(2, 3)], na.rm = TRUE) / sum(D0111[!is.na(D0300)], na.rm = TRUE),
      
      # D0310: Proporção Lixo Inadequado (3=Queimado, 4=Enterrado, 5=Jogado terreno/via, 6=Outro)
      prop_lixo_inadequado = sum(D0111[D0310 %in% c(3, 4, 5, 6)], na.rm = TRUE) / sum(D0111[!is.na(D0310)], na.rm = TRUE),
      
      .groups = "drop"
    )
  return(df_agg)
}

processar_pessoas <- function(arquivo) {
  df <- fread(arquivo)
  
  df_agg <- df %>%
    filter(!is.na(P0080) & !is.na(P0111)) %>%
    group_by(codigo_municipio = P0080) %>%
    summarise(
      # P0640: Proporção Analfabetismo (2=Não sabe ler/escrever)
      prop_analfabetismo = sum(P0111[P0640 == 2], na.rm = TRUE) / sum(P0111[!is.na(P0640)], na.rm = TRUE),
      
      # P0950: Proporção Ocupados (1=Ocupada)
      prop_ocupados = sum(P0111[P0950 == 1], na.rm = TRUE) / sum(P0111[!is.na(P0950)], na.rm = TRUE),
      
      # P0210: Proporção Negros (Pretos + Pardos) (2=Preta, 4=Parda)
      prop_negros = sum(P0111[P0210 %in% c(2, 4)], na.rm = TRUE) / sum(P0111[!is.na(P0210)], na.rm = TRUE),
      
      # P0210: Proporção Indígena (5=Indígena) - Calculado separado caso queira avaliar etnia específica
      prop_indigena = sum(P0111[P0210 == 5], na.rm = TRUE) / sum(P0111[!is.na(P0210)], na.rm = TRUE),
      
      # P0480: Proporção Não-Natural (Migrante) (2=Outro município, 3=Outro país)
      prop_migrante = sum(P0111[P0480 %in% c(2, 3)], na.rm = TRUE) / sum(P0111[!is.na(P0480)], na.rm = TRUE),
      
      # P0890: Proporção Trabalho de Subsistência (Roça, pesca, caça) (1=Sim)
      prop_subsistencia = sum(P0111[P0890 == 1], na.rm = TRUE) / sum(P0111[!is.na(P0890)], na.rm = TRUE),
      
      .groups = "drop"
    )
  return(df_agg)
}

cat("\nProcessando Domicílios...\n")
censo_br_dom <- map_df(arquivos_dom, processar_domicilios)

cat("Processando Pessoas...\n")
censo_br_pes <- map_df(arquivos_pes, processar_pessoas)

censo_brasil_final <- censo_br_dom %>%
  full_join(censo_br_pes, by = "codigo_municipio") %>%
  mutate(across(starts_with("prop_"), ~ .x * 100))

print(summary(censo_brasil_final))

write.csv(censo_brasil_final, "Censo2022_Agregado_Municipios.csv", row.names = FALSE)

cat("\nBanco consolidado salvo como 'Censo2022_Agregado_Municipios.csv'.\n")