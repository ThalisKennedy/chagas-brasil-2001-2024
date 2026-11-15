# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SINAN/CHAGBR 2001-2024/CHAGBR 2001-2024 - Parte 4.R
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
library(sf)
library(geobr)
library(spdep)

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

caminho_dados <- paste0(file.path(DIR_DADOS, "IBGE/IBGE_POP_2001_2024"), "/")

pop_padrao_br2022 <- readRDS(paste0(caminho_dados, "pop_padrao_br2022.rds"))

pop_ibge <- readRDS(paste0(caminho_dados, "POP_IBGE_MUNICIPIO_TRATADA.rds")) %>% 
  mutate(COD_MUN = as.character(substr(COD_MUN, 1, 6)))

casos_agudos <- CHAGAS_AGUDA_FILTRADO_2001_2024 %>%
  mutate(CD_MN_RESI = as.character(substr(CD_MN_RESI, 1, 6)))

casos_municipio <- casos_agudos %>%
  filter(!is.na(IDADE_FINAL)) %>% 
  mutate(
    FAIXA_QUINQUENAL = cut(IDADE_FINAL, 
                           breaks = c(seq(0, 80, by = 5), Inf),
                           labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                      "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", 
                                      "75-79", "80+"),
                           right = FALSE)
  ) %>%
  group_by(CD_MN_RESI, FAIXA_QUINQUENAL) %>%
  summarise(total_casos = n(), .groups = "drop")

pop_municipio_agrupada <- pop_ibge %>%
  group_by(COD_MUN, FAIXA_QUINQUENAL) %>%
  summarise(pop_media_periodo = mean(populacao_residente, na.rm = TRUE), .groups = "drop")

dados_combinados <- pop_municipio_agrupada %>%
  left_join(casos_municipio, by = c("COD_MUN" = "CD_MN_RESI", "FAIXA_QUINQUENAL" = "FAIXA_QUINQUENAL")) %>%
  mutate(total_casos = replace_na(total_casos, 0)) %>%
  left_join(pop_padrao_br2022, by = "FAIXA_QUINQUENAL")

taxas_municipais <- dados_combinados %>%
  mutate(
    media_casos_anuais = total_casos / 24,
    
    taxa_especifica = if_else(pop_media_periodo == 0, 0, media_casos_anuais / pop_media_periodo),
    
    casos_esperados_padrao = taxa_especifica * peso_pop_padrao 
  ) %>%
  group_by(COD_MUN) %>%
  summarise(
    total_casos_mun = sum(total_casos),     
    pop_media_mun = sum(pop_media_periodo), 
    
    taxa_bruta = ((total_casos_mun / 24) / pop_media_mun) * 100000,
    
    taxa_padronizada = sum(casos_esperados_padrao, na.rm = TRUE),
    
    .groups = "drop"
  )

malha_brasil_bruta <- st_read(file.path(DIR_DADOS, "IBGE/IBGE_POP_2001_2024/BR_Municipios_2022/BR_Municipios_2022.shp"))

malha_brasil <- malha_brasil_bruta %>%
  mutate(COD_MUN = as.character(substr(CD_MUN, 1, 6)))

mapa_taxas <- malha_brasil %>%
  left_join(taxas_municipais, by = "COD_MUN") %>%
  mutate(
    total_casos_mun = replace_na(total_casos_mun, 0),
    pop_media_mun = replace_na(pop_media_mun, 0),
    taxa_bruta = replace_na(taxa_bruta, 0),
    taxa_padronizada = replace_na(taxa_padronizada, 0)
  )

mapa_valido <- mapa_taxas %>% 
  filter(!st_is_empty(geometry) & pop_media_mun > 0)

nrow(mapa_valido)

vizinhos <- poly2nb(mapa_valido, queen = TRUE)

mapa_valido <- mapa_valido %>%
  mutate(
    casos_padronizados_esperados = (taxa_padronizada * pop_media_mun) / 100000
  )

res_bayes_local <- EBlocal(
  ri = mapa_valido$casos_padronizados_esperados,  
  ni = mapa_valido$pop_media_mun, 
  nb = vizinhos, 
  zero.policy = TRUE
)

mapa_valido$taxa_padronizada_suavizada <- res_bayes_local$est * 100000

# --- Exportação feita no console na execução original (comandos registrados
# no .Rhistory do projeto) ---
dados_tabulares_finais <- sf::st_drop_geometry(mapa_valido)
readr::write_csv(dados_tabulares_finais, "TAXAS_CHAGAS_AGUDA_SUAVIZADAS.csv")
writexl::write_xlsx(dados_tabulares_finais, "TAXAS_CHAGAS_AGUDA_SUAVIZADAS.xlsx")
sf::st_write(mapa_valido, dsn = "TAXAS_CHAGAS_AGUDA_SUAVIZADAS.gpkg", driver = "GPKG", delete_dsn = TRUE)
