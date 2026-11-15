# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SINAN/CHAGBR 2001-2024/CHAGBR 2001-2024 - FILTRADO - COMPLETO.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SINAN/CHAGBR 2001-2024"))

library(tidyverse)
library(writexl)
library(lubridate)
library(gtsummary)
library(flextable)
library(officer)
library(forcats)
library(haven)
library(stringr)
library(readr)
library(gt)
library(prais)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(scales)

df_2001_2006 <- readRDS("CHAGAS_AGUDA_FINAL_4_2001_2006_TRATADO.rds")
df_2007_2024 <- readRDS("CHAGAS_AGUDA_FINAL_4_2007_2024_TRATADO.rds")

col_comuns <- intersect(names(df_2001_2006), names(df_2007_2024))
col_exclusivas_2001 <- setdiff(names(df_2001_2006), names(df_2007_2024))
col_exclusivas_2007 <- setdiff(names(df_2007_2024), names(df_2001_2006))

df_2001_2006 <- df_2001_2006 %>%
  rename_with(~ paste0(., "_2001"), all_of(col_exclusivas_2001))

df_2007_2024 <- df_2007_2024 %>%
  rename_with(~ paste0(., "_2007"), all_of(col_exclusivas_2007))

CHAGAS_AGUDA_2001_2024 <- bind_rows(df_2001_2006, df_2007_2024)

glimpse(CHAGAS_AGUDA_2001_2024)

CHAGAS_AGUDA_2_2001_2024 <- CHAGAS_AGUDA_2001_2024 %>% 
  mutate(
    QTD_SINTOMAS_CAT = case_when(
      is.na(QTD_SINTOMAS) ~ "Ignorado/Em branco",
      QTD_SINTOMAS == 0 ~ "Nenhum sintoma",
      QTD_SINTOMAS == 1 ~ "Um sintoma",
      QTD_SINTOMAS == 2 ~ "Dois sintomas",
      QTD_SINTOMAS == 3 ~ "Três sintomas",
      QTD_SINTOMAS >= 4 ~ "Quatro ou mais sintomas"
    ),
    
    QTD_SINTOMAS_CAT = factor(QTD_SINTOMAS_CAT, levels = c(
      "Nenhum sintoma", "Um sintoma", "Dois sintomas", 
      "Três sintomas", "Quatro ou mais sintomas", "Ignorado/Em branco"
    )),
    
    QTD_SORO_CAT = case_when(
      is.na(QTD_SOROLOGIAS_REAGENTES) ~ "Ignorado/Em branco",
      QTD_SOROLOGIAS_REAGENTES == 0 ~ "Nenhuma sorologia reagente",
      QTD_SOROLOGIAS_REAGENTES == 1 ~ "Uma reagente",
      QTD_SOROLOGIAS_REAGENTES == 2 ~ "Duas reagentes",
      QTD_SOROLOGIAS_REAGENTES == 3 ~ "Três reagentes",
      QTD_SOROLOGIAS_REAGENTES >= 4 ~ "Quatro ou mais reagentes"
    ),
    
    QTD_SORO_CAT = factor(QTD_SORO_CAT, levels = c(
      "Nenhuma sorologia reagente", "Uma reagente", "Duas reagentes", 
      "Três reagentes", "Quatro ou mais reagentes", "Ignorado/Em branco"
    )),
    
    QTD_PARASITO_CAT = case_when(
      is.na(QTD_PARASITO_POSITIVOS) ~ "Ignorado/Em branco",
      QTD_PARASITO_POSITIVOS == 0 ~ "Nenhum exame positivo",
      QTD_PARASITO_POSITIVOS == 1 ~ "Um positivo",
      QTD_PARASITO_POSITIVOS == 2 ~ "Dois positivos",
      QTD_PARASITO_POSITIVOS >= 3 ~ "Três ou mais positivos"
    ),
    
    QTD_PARASITO_CAT = factor(QTD_PARASITO_CAT, levels = c(
      "Nenhum exame positivo", "Um positivo", "Dois positivos", 
      "Três ou mais positivos", "Ignorado/Em branco"
    )),
    
    QTD_CARDIACO_CAT_2001 = case_when(
      is.na(QTD_CARDIACO_ALTERADOS_2001) ~ "Ignorado/Em branco",
      QTD_CARDIACO_ALTERADOS_2001 == 0 ~ "Nenhuma alteração cardíaca",
      QTD_CARDIACO_ALTERADOS_2001 == 1 ~ "Uma alteração",
      QTD_CARDIACO_ALTERADOS_2001 >= 2 ~ "Duas alterações"
    ),
    
    QTD_CARDIACO_CAT_2001 = factor(QTD_CARDIACO_CAT_2001, levels = c(
      "Nenhuma alteração cardíaca", "Uma alteração", "Duas alterações", 
      "Ignorado/Em branco"
    )),
    
    QTD_MEDIDAS_CAT = case_when(
      is.na(QTD_MEDIDAS_TOMADAS) ~ "Ignorado/Em branco",
      QTD_MEDIDAS_TOMADAS == 0 ~ "Nenhuma medida tomada",
      QTD_MEDIDAS_TOMADAS == 1 ~ "Uma medida",
      QTD_MEDIDAS_TOMADAS == 2 ~ "Duas medidas",
      QTD_MEDIDAS_TOMADAS >= 3 ~ "Três ou mais medidas"
    ),
    
    QTD_MEDIDAS_CAT = factor(QTD_MEDIDAS_CAT, levels = c(
      "Nenhuma medida tomada", "Uma medida", "Duas medidas", 
      "Três ou mais medidas", "Ignorado/Em branco"
    ))
  )

print(table(CHAGAS_AGUDA_2_2001_2024$QTD_CARDIACO_CAT_2001, useNA = "ifany"))

print(table(CHAGAS_AGUDA_2_2001_2024$QTD_PARASITO_CAT, useNA = "ifany"))

print(table(CHAGAS_AGUDA_2_2001_2024$QTD_SINTOMAS_CAT, useNA = "ifany"))

print(table(CHAGAS_AGUDA_2_2001_2024$QTD_MEDIDAS_CAT, useNA = "ifany"))

print(table(CHAGAS_AGUDA_2_2001_2024$QTD_SORO_CAT, useNA = "ifany"))

CHAGAS_AGUDA_FILTRADO_2001_2024 <- CHAGAS_AGUDA_2_2001_2024 %>%
  filter(IS_CONFIRMADO == "1")

print(table(CHAGAS_AGUDA_FILTRADO_2001_2024$FAIXA_ETARIA, useNA = "ifany"))

print(table(CHAGAS_AGUDA_FILTRADO_2001_2024$NU_ANO, useNA = "ifany"))

CHAGAS_AGUDA_FILTRADO_2001_2024 <- CHAGAS_AGUDA_FILTRADO_2001_2024 %>% 
  mutate(
    NU_ANO = as.numeric(NU_ANO),
    
    FAIXA_ETARIA_GRUPO = case_when(
      IDADE_FINAL >= 0 & IDADE_FINAL <= 19  ~ "0 a 19 anos",
      IDADE_FINAL >= 20 & IDADE_FINAL <= 59  ~ "20 a 59 anos",
      IDADE_FINAL >= 60 ~ "60 anos ou mais",
      TRUE ~ NA_character_
    ))

print(table(CHAGAS_AGUDA_FILTRADO_2001_2024$FAIXA_ETARIA_GRUPO, useNA = "ifany"))

table(CHAGAS_AGUDA_FILTRADO_2001_2024$USOU_MEDICAMENTO, useNA = "always")

class(CHAGAS_AGUDA_FILTRADO_2001_2024$USOU_MEDICAMENTO)

# --- Exportação feita no console na execução original ---
saveRDS(CHAGAS_AGUDA_FILTRADO_2001_2024, "CHAGAS_AGUDA_FILTRADO_2_2001_2024.rds")
