# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: IBGE/IBGE_POP_2001_2024/IBGE_POP_2001_2024_REGIAO.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "IBGE/IBGE_POP_2001_2024"))

library(tidyverse)
library(writexl)
library(lubridate)
library(gtsummary)
library(flextable)
library(forcats)
library(haven)
library(stringr)
library(readr)
library(gt)
library(foreign) 
library(readxl)

caminho_pasta <- file.path(DIR_DADOS, "IBGE/Dados")

arquivos_dbf <- list.files(path = caminho_pasta, 
                           pattern = "\\.dbf$", 
                           ignore.case = TRUE, 
                           full.names = TRUE)

ler_e_preparar_pop <- function(caminho_arquivo) {
  df <- read.dbf(caminho_arquivo, as.is = TRUE)
  nome_base <- basename(caminho_arquivo)
  digitos_ano <- str_extract(nome_base, "\\d+")
  ano_completo <- as.numeric(digitos_ano) + 2000
  
  df <- df %>%
    mutate(ANO_OBITO = ano_completo) 
  
  return(df)
}

POP_IBGE_BRUTA <- map_df(arquivos_dbf, ler_e_preparar_pop)

POP_IBGE_TRATADA <- POP_IBGE_BRUTA %>%
  mutate(IDADE_NUM = as.numeric(IDADE)) %>%
  filter(!is.na(IDADE_NUM)) %>% 
  mutate(
    
    COD_REGIAO = str_sub(COD_MUN, 1, 1),
    REGIAO = case_when(
      COD_REGIAO == "1" ~ "Norte",
      COD_REGIAO == "2" ~ "Nordeste",
      COD_REGIAO == "3" ~ "Sudeste",
      COD_REGIAO == "4" ~ "Sul",
      COD_REGIAO == "5" ~ "Centro-Oeste",
      TRUE ~ "Ignorado"
    ),
    
    FAIXA_QUINQUENAL = cut(IDADE_NUM, 
                           breaks = c(seq(0, 80, by = 5), Inf),
                           labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                      "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", 
                                      "75-79", "80+"),
                           right = FALSE)
  ) %>%
  
  group_by(REGIAO, ANO_OBITO, FAIXA_QUINQUENAL) %>%
  summarise(populacao_residente = sum(POP, na.rm = TRUE), .groups = "drop") %>%
  filter(REGIAO != "Ignorado")

glimpse(POP_IBGE_TRATADA)
head(POP_IBGE_TRATADA, 15)

censo_2022_oficial <- read_excel("Censo 2022 - Pirâmide etária - Brasil.xlsx")

pop_padrao_br2022 <- censo_2022_oficial %>%
  mutate(populacao_residente = `População feminina(pessoas)` + `População masculina(pessoas)`) %>%
  mutate(
    FAIXA_QUINQUENAL = case_when(
      `Grupo de idade` == "0 a 4 anos" ~ "0-4",
      `Grupo de idade` == "5 a 9 anos" ~ "5-9",
      `Grupo de idade` == "10 a 14 anos" ~ "10-14",
      `Grupo de idade` == "15 a 19 anos" ~ "15-19",
      `Grupo de idade` == "20 a 24 anos" ~ "20-24",
      `Grupo de idade` == "25 a 29 anos" ~ "25-29",
      `Grupo de idade` == "30 a 34 anos" ~ "30-34",
      `Grupo de idade` == "35 a 39 anos" ~ "35-39",
      `Grupo de idade` == "40 a 44 anos" ~ "40-44",
      `Grupo de idade` == "45 a 49 anos" ~ "45-49",
      `Grupo de idade` == "50 a 54 anos" ~ "50-54",
      `Grupo de idade` == "55 a 59 anos" ~ "55-59",
      `Grupo de idade` == "60 a 64 anos" ~ "60-64",
      `Grupo de idade` == "65 a 69 anos" ~ "65-69",
      `Grupo de idade` == "70 a 74 anos" ~ "70-74",
      `Grupo de idade` == "75 a 79 anos" ~ "75-79",
      `Grupo de idade` %in% c("80 a 84 anos", "85 a 89 anos", "90 a 94 anos", "95 a 99 anos", "100 anos ou mais") ~ "80+",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(FAIXA_QUINQUENAL) %>%
  summarise(pop_absoluta = sum(populacao_residente), .groups = 'drop') %>%
  mutate(
    FAIXA_QUINQUENAL = factor(FAIXA_QUINQUENAL, 
                              levels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                         "25-29", "30-34", "35-39", "40-44", "45-49", 
                                         "50-54", "55-59", "60-64", "65-69", "70-74", 
                                         "75-79", "80+"))
  ) %>%
  arrange(FAIXA_QUINQUENAL) %>%
  mutate(FAIXA_QUINQUENAL = as.character(FAIXA_QUINQUENAL)) %>%

  mutate(peso_pop_padrao = (pop_absoluta / sum(pop_absoluta)) * 100000) %>%
  select(FAIXA_QUINQUENAL, peso_pop_padrao)

# --- Exportação feita no console na execução original (comandos registrados
# no .Rhistory do projeto) ---
saveRDS(POP_IBGE_TRATADA, "POP_IBGE_REGIÃO_TRATADA.rds")
