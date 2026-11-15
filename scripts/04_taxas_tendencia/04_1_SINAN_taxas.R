# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SINAN/CHAGBR 2001-2024/CHAGBR 2001-2024 - Parte 3.1_2.R
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

caminho_dados <- paste0(file.path(DIR_DADOS, "IBGE/IBGE_POP_2001_2024"), "/")

pop_padrao_br2022 <- readRDS(paste0(caminho_dados, "pop_padrao_br2022.rds"))

POP_IBGE_TRATADA <- readRDS(paste0(caminho_dados, "POP_IBGE_TRATADA.rds"))

POP_IBGE_REGIAO_TRATADA <- readRDS(paste0(caminho_dados, "POP_IBGE_REGIÃO_TRATADA.rds"))

pop_brasil_agregada <- POP_IBGE_TRATADA %>%
  rename(NU_ANO = ANO_OBITO)

pop_brasil_agregada <- pop_brasil_agregada %>%
  group_by(NU_ANO, FAIXA_QUINQUENAL) %>%
  summarise(populacao_residente = sum(populacao_residente, na.rm = TRUE), .groups = "drop")

aguda_idade_ampla <- CHAGAS_AGUDA_FILTRADO_2001_2024 %>%
  filter(!is.na(IDADE_FINAL)) %>%
  mutate(
    FAIXA_QUINQUENAL = cut(IDADE_FINAL, 
                           breaks = c(seq(0, 80, by = 5), Inf),
                           labels = pop_padrao_br2022$FAIXA_QUINQUENAL,
                           right = FALSE),
    ESTRATIFICACAO = case_when(
      FAIXA_QUINQUENAL %in% c("0-4", "5-9", "10-14", "15-19") ~ "0 a 19 anos",
      FAIXA_QUINQUENAL %in% c("20-24", "25-29", "30-34", "35-39", 
                              "40-44", "45-49", "50-54", "55-59") ~ "20 a 59 anos",
      TRUE ~ "60 anos ou mais"
    )
  ) %>%
  group_by(NU_ANO, FAIXA_QUINQUENAL, ESTRATIFICACAO) %>%
  summarise(aguda = n(), .groups = 'drop')

aguda_total_idade <- aguda_idade_ampla %>%
  group_by(NU_ANO, FAIXA_QUINQUENAL) %>%
  summarise(aguda = sum(aguda), .groups = 'drop') %>%
  mutate(ESTRATIFICACAO = "Total")

df_aguda_faixa <- bind_rows(aguda_idade_ampla, aguda_total_idade) %>%
  mutate(ESTRATIFICACAO = factor(ESTRATIFICACAO, 
                                 levels = c("Total", "0 a 19 anos", "20 a 59 anos", "60 anos ou mais")))

df_base_pop_faixa <- pop_brasil_agregada %>%
  left_join(pop_padrao_br2022, by = "FAIXA_QUINQUENAL") %>%
  mutate(
    ESTRATIFICACAO = case_when(
      FAIXA_QUINQUENAL %in% c("0-4", "5-9", "10-14", "15-19") ~ "0 a 19 anos",
      FAIXA_QUINQUENAL %in% c("20-24", "25-29", "30-34", "35-39", 
                              "40-44", "45-49", "50-54", "55-59") ~ "20 a 59 anos",
      TRUE ~ "60 anos ou mais"
    )
  )

df_base_pop_total <- df_base_pop_faixa %>%
  mutate(ESTRATIFICACAO = "Total")

df_pop_completa <- bind_rows(df_base_pop_faixa, df_base_pop_total) %>%
  mutate(ESTRATIFICACAO = factor(ESTRATIFICACAO, 
                                 levels = c("Total", "0 a 19 anos", "20 a 59 anos", "60 anos ou mais")))

df_taxas_faixa <- df_pop_completa %>%
  left_join(df_aguda_faixa, by = c("NU_ANO", "FAIXA_QUINQUENAL", "ESTRATIFICACAO")) %>%
  replace_na(list(aguda = 0))

df_taxas_padr_faixa <- df_taxas_faixa %>%
  mutate(taxa_especifica = (aguda / populacao_residente) * 100000) %>%
  mutate(aguda_esperados = (taxa_especifica * peso_pop_padrao) / 100000) %>%
  group_by(NU_ANO, ESTRATIFICACAO) %>%
  summarise(
    casos_totais = sum(aguda, na.rm = TRUE),
    pop_total = sum(populacao_residente, na.rm = TRUE),
    
    taxa_bruta = (casos_totais / pop_total) * 100000,
    se_bruta = (sqrt(casos_totais) / pop_total) * 100000,
    
    taxa_padronizada = (sum(aguda_esperados, na.rm = TRUE) / sum(peso_pop_padrao, na.rm = TRUE)) * 100000,
    
    se_padronizada = sqrt(sum(
      (peso_pop_padrao / sum(peso_pop_padrao, na.rm = TRUE))^2 * if_else(populacao_residente == 0, 0, aguda / (populacao_residente^2)), 
      na.rm = TRUE
    )) * 100000,
    
    .groups = 'drop'
  )

df_total <- df_taxas_padr_faixa %>% 
  filter(ESTRATIFICACAO == "Total")

df_faixas <- df_taxas_padr_faixa %>% 
  filter(ESTRATIFICACAO != "Total")

limite_maximo_total <- max(c(df_total$taxa_padronizada, df_total$taxa_bruta), na.rm = TRUE)
limite_maximo_faixas <- max(c(df_faixas$taxa_padronizada, df_faixas$taxa_bruta), na.rm = TRUE)

tipos_linha_faixas <- c("0 a 19 anos" = "solid", "20 a 59 anos" = "dotted", 
                        "60 anos ou mais" = "solid")

grafico_total_bruto <- ggplot(df_total, aes(x = NU_ANO, y = taxa_bruta)) +
  geom_line(aes(color = "Taxa de Incidência Bruta"), linewidth = 1, alpha = 0.7) +
  geom_point(aes(color = "Taxa de Incidência Bruta"), size = 3) +
  geom_text_repel(aes(label = format(round(taxa_bruta, 1), nsmall = 1, decimal.mark = ",")),                  color = "black",
                  size = 3.0, fontface = "bold", show.legend = FALSE, direction = "y", 
                  box.padding = 0.4, point.padding = 0.2, min.segment.length = 0.5) +  
  scale_y_continuous(limits = c(0, limite_maximo_total * 1.05),
                     expand = expansion(mult = c(0, 0.05)),
                     labels = function(x) format(x, decimal.mark = ",", scientific = FALSE)) +
  scale_x_continuous(breaks = seq(2001, 2024, by = 1)) +
  scale_color_manual(name = NULL, values = c("Taxa de Incidência Bruta" = "#0072B2")) + 
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.title = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 10, face = "bold"))

grafico_total_padronizado <- ggplot(df_total, aes(x = NU_ANO, y = taxa_padronizada)) +
  geom_line(aes(color = "Taxa de Incidência Padronizada por Idade"), linewidth = 1, alpha = 0.7) +
  geom_point(aes(color = "Taxa de Incidência Padronizada por Idade"), size = 3) +
  geom_text_repel(aes(label = format(round(taxa_padronizada, 1), nsmall = 1, decimal.mark = ",")),                  color = "black",
                  size = 3.0, fontface = "bold", show.legend = FALSE, direction = "y", 
                  box.padding = 0.4, point.padding = 0.2, min.segment.length = 0.5) +
  scale_y_continuous(limits = c(0, limite_maximo_total * 1.05),
                     expand = expansion(mult = c(0, 0.05)),
                     labels = function(x) format(x, decimal.mark = ",", scientific = FALSE)) +
  scale_x_continuous(breaks = seq(2001, 2024, by = 1)) +
  scale_color_manual(name = NULL, values = c("Taxa de Incidência Padronizada por Idade" = "#0072B2")) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.title = element_text(size = 10, face = "bold"),
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(), 
        axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"),
        legend.position = "bottom", 
        legend.text = element_text(size = 10, face = "bold"))
        
grafico_faixa_bruto <- ggplot(df_faixas, aes(x = NU_ANO, y = taxa_bruta, 
                                             color = ESTRATIFICACAO, shape = ESTRATIFICACAO,
                                             linetype = ESTRATIFICACAO)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(values = c("0 a 19 anos" = "#D55E00", "20 a 59 anos" = "#0072B2", "60 anos ou mais" = "#009E73")) +
  scale_linetype_manual(values = tipos_linha_faixas) +
  scale_y_continuous(limits = c(0, limite_maximo_faixas * 1.05), 
                     expand = expansion(mult = c(0, 0.05)),
                     labels = function(x) format(x, decimal.mark = ",", scientific = FALSE)) +
  scale_x_continuous(breaks = seq(2001, 2024, by = 1)) +
  labs(title = NULL, x = NULL, y = NULL,
       color = "Faixa Etária:", shape = "Faixa Etária:", linetype = "Faixa Etária:") +
  theme_minimal() +
  theme(text = element_text(family = "sans"), legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"))

grafico_faixa_padronizado <- ggplot(df_faixas, aes(x = NU_ANO, y = taxa_padronizada, 
                                                   color = ESTRATIFICACAO, shape = ESTRATIFICACAO,
                                                   linetype = ESTRATIFICACAO)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(values = c("0 a 19 anos" = "#D55E00", "20 a 59 anos" = "#0072B2", "60 anos ou mais" = "#009E73")) +
  scale_linetype_manual(values = tipos_linha_faixas) +
  scale_y_continuous(limits = c(0, limite_maximo_faixas * 1.05), 
                     expand = expansion(mult = c(0, 0.05)),
                     labels = function(x) format(x, decimal.mark = ",", scientific = FALSE)) +
  scale_x_continuous(breaks = seq(2001, 2024, by = 1)) +
  labs(title = NULL, x = NULL, y = NULL,
       color = "Faixa Etária:", shape = "Faixa Etária:", linetype = "Faixa Etária:") +
  theme_minimal() +
  theme(text = element_text(family = "sans"), legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
        axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"))

print(grafico_total_bruto)
print(grafico_total_padronizado)
print(grafico_faixa_bruto)
print(grafico_faixa_padronizado)

aguda_regioes <- CHAGAS_AGUDA_FILTRADO_2001_2024 %>%
  
  filter(!is.na(IDADE_FINAL), !is.na(REGIAO_RESIDENCIA), REGIAO_RESIDENCIA != "Ignorado/Em branco") %>%
  
  mutate(
    FAIXA_QUINQUENAL = cut(IDADE_FINAL, 
                           breaks = c(seq(0, 80, by = 5), Inf),
                           labels = pop_padrao_br2022$FAIXA_QUINQUENAL,
                           right = FALSE)
  ) %>%
  group_by(NU_ANO, FAIXA_QUINQUENAL, REGIAO_RESIDENCIA) %>%
  summarise(aguda = n(), .groups = 'drop')

aguda_brasil <- CHAGAS_AGUDA_FILTRADO_2001_2024 %>%
  filter(!is.na(IDADE_FINAL)) %>%
  mutate(
    FAIXA_QUINQUENAL = cut(IDADE_FINAL, 
                           breaks = c(seq(0, 80, by = 5), Inf),
                           labels = pop_padrao_br2022$FAIXA_QUINQUENAL,
                           right = FALSE)
  ) %>%
  group_by(NU_ANO, FAIXA_QUINQUENAL) %>%
  summarise(aguda = n(), .groups = 'drop') %>%
  mutate(REGIAO_RESIDENCIA = "Brasil")

df_aguda_geo <- bind_rows(aguda_regioes, aguda_brasil)

pop_brasil <- POP_IBGE_TRATADA %>% 
  rename(NU_ANO = ANO_OBITO)

pop_brasil <- pop_brasil %>% 
  group_by(NU_ANO, FAIXA_QUINQUENAL) %>%
  summarise(populacao_residente = sum(populacao_residente, na.rm=TRUE), .groups = 'drop') %>%
  mutate(REGIAO_RESIDENCIA = "Brasil")

pop_regioes <- POP_IBGE_REGIAO_TRATADA %>%
  rename(NU_ANO = ANO_OBITO)

pop_regioes <- pop_regioes %>%
  rename_with(~"REGIAO_RESIDENCIA", any_of(c("REGIAO", "Regiao", "regiao"))) 

pop_completa_geo <- bind_rows(pop_regioes, pop_brasil)

df_taxas_geo <- pop_completa_geo %>%
  left_join(pop_padrao_br2022, by = "FAIXA_QUINQUENAL") %>%
  left_join(df_aguda_geo, by = c("NU_ANO", "FAIXA_QUINQUENAL", "REGIAO_RESIDENCIA")) %>%
  replace_na(list(aguda = 0))

df_taxas_padronizadas_geo <- df_taxas_geo %>%
  mutate(taxa_especifica = (aguda / populacao_residente) * 100000) %>%
  mutate(aguda_esperados = (taxa_especifica * peso_pop_padrao) / 100000) %>%
  group_by(NU_ANO, REGIAO_RESIDENCIA) %>%
  summarise(
    casos_totais = sum(aguda, na.rm = TRUE),
    pop_total = sum(populacao_residente, na.rm = TRUE),
    
    taxa_bruta = (casos_totais / pop_total) * 100000,
    se_bruta = (sqrt(casos_totais) / pop_total) * 100000,
    
    taxa_padronizada = (sum(aguda_esperados, na.rm = TRUE) / sum(peso_pop_padrao, na.rm = TRUE)) * 100000,
    
    se_padronizada = sqrt(sum(
      (peso_pop_padrao / sum(peso_pop_padrao, na.rm = TRUE))^2 * if_else(populacao_residente == 0, 0, aguda / (populacao_residente^2)), 
      na.rm = TRUE
    )) * 100000,
    
    .groups = 'drop'
  ) %>%
  mutate(REGIAO_RESIDENCIA = factor(REGIAO_RESIDENCIA, 
                                    levels = c("Brasil", "Norte", "Nordeste", 
                                               "Sudeste", "Sul", "Centro-Oeste")))

limite_maximo_geo <- max(c(df_taxas_padronizadas_geo$taxa_padronizada, 
                           df_taxas_padronizadas_geo$taxa_bruta), na.rm = TRUE)

cores_geo <- c("Brasil" = "#000000", "Norte" = "#1b9e77", "Nordeste" = "#d95f02", 
               "Sudeste" = "#7570b3", "Sul" = "#e7298a", "Centro-Oeste" = "#00A0B0")

shapes_geo <- c("Brasil" = 16, "Norte" = 15, "Nordeste" = 17, 
                "Sudeste" = 18, "Sul" = 8, "Centro-Oeste" = 4)

tipos_linha_geo <- c("Brasil" = "solid", "Norte" = "dotted", "Nordeste" = "dotted", 
                     "Sudeste" = "dotted", "Sul" = "solid", "Centro-Oeste" = "solid")

grafico_padronizado_geo <- ggplot(df_taxas_padronizadas_geo, 
                                  aes(x = NU_ANO, y = taxa_padronizada, 
                                      color = REGIAO_RESIDENCIA, shape = REGIAO_RESIDENCIA,
                                      linetype = REGIAO_RESIDENCIA)) + 
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(values = cores_geo) +
  scale_shape_manual(values = shapes_geo) +
  scale_linetype_manual(values = tipos_linha_geo) +
  scale_y_continuous(limits = c(0, limite_maximo_geo * 1.05), 
                     expand = expansion(mult = c(0, 0.05)),
                     labels = function(x) format(x, decimal.mark = ",", scientific = FALSE))+
  scale_x_continuous(breaks = seq(2001, 2024, by = 1)) +
  labs(title = NULL, subtitle = NULL, x = NULL, y = NULL,
       color = "Localidade:", shape = "Localidade:", linetype = "Localidade:") +
  theme_minimal() +
  theme(text = element_text(family = "sans"), legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), 
        axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"),
        legend.key.width = unit(2, "cm"))

grafico_bruto_geo <- ggplot(df_taxas_padronizadas_geo, 
                            aes(x = NU_ANO, y = taxa_bruta, 
                                color = REGIAO_RESIDENCIA, shape = REGIAO_RESIDENCIA,
                                linetype = REGIAO_RESIDENCIA)) + 
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(size = 3) +
  scale_color_manual(values = cores_geo) +
  scale_shape_manual(values = shapes_geo) +
  scale_linetype_manual(values = tipos_linha_geo) + 
  scale_y_continuous(limits = c(0, limite_maximo_geo * 1.05), 
                     expand = expansion(mult = c(0, 0.05)),
                     labels = function(x) format(x, decimal.mark = ",", scientific = FALSE)) +
  scale_x_continuous(breaks = seq(2001, 2024, by = 1)) +
  labs(title = NULL, subtitle = NULL, x = NULL, y = NULL,
       color = "Localidade:", shape = "Localidade:", linetype = "Localidade:") +
  theme_minimal() +
  theme(text = element_text(family = "sans"), legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black"),
        legend.key.width = unit(2, "cm"))

print(grafico_padronizado_geo)
print(grafico_bruto_geo)

length(pop_padrao_br2022$FAIXA_QUINQUENAL)
length(c(seq(0,80,5), Inf)) - 1

print(table(CHAGAS_AGUDA_FILTRADO_2001_2024$NU_ANO, useNA = "ifany"))

df_joinpoint_faixa_aguda <- df_taxas_padr_faixa %>%
  select(NU_ANO, ESTRATIFICACAO, taxa_bruta, se_bruta, taxa_padronizada, se_padronizada) %>%
  pivot_wider(
    names_from = ESTRATIFICACAO,
    values_from = c(taxa_bruta, se_bruta, taxa_padronizada, se_padronizada)
  ) %>%
  rename(
    ANO = NU_ANO,
    TAXA_BRUTA_TOTAL_BRASIL = taxa_bruta_Total,
    SE_BRUTA_TOTAL_BRASIL   = se_bruta_Total,
    TAXA_PAD_TOTAL_BRASIL   = taxa_padronizada_Total,
    SE_PAD_TOTAL_BRASIL     = se_padronizada_Total,
    
    TAXA_BRUTA_0_19    = `taxa_bruta_0 a 19 anos`,
    SE_BRUTA_0_19      = `se_bruta_0 a 19 anos`,
    TAXA_PAD_0_19      = `taxa_padronizada_0 a 19 anos`,
    SE_PAD_0_19        = `se_padronizada_0 a 19 anos`,
    
    TAXA_BRUTA_20_59   = `taxa_bruta_20 a 59 anos`,
    SE_BRUTA_20_59     = `se_bruta_20 a 59 anos`,
    TAXA_PAD_20_59     = `taxa_padronizada_20 a 59 anos`,
    SE_PAD_20_59       = `se_padronizada_20 a 59 anos`,
    
    TAXA_BRUTA_60_MAIS = `taxa_bruta_60 anos ou mais`,
    SE_BRUTA_60_MAIS   = `se_bruta_60 anos ou mais`,
    TAXA_PAD_60_MAIS   = `taxa_padronizada_60 anos ou mais`,
    SE_PAD_60_MAIS     = `se_padronizada_60 anos ou mais`
  )


df_joinpoint_regioes_aguda <- df_taxas_padronizadas_geo %>%
  filter(REGIAO_RESIDENCIA != "Brasil") %>%
  select(NU_ANO, REGIAO_RESIDENCIA, taxa_bruta, se_bruta, taxa_padronizada, se_padronizada) %>%
  pivot_wider(
    names_from = REGIAO_RESIDENCIA,
    values_from = c(taxa_bruta, se_bruta, taxa_padronizada, se_padronizada)
  ) %>%
  rename(
    ANO = NU_ANO,
    TAXA_BRUTA_NORTE     = taxa_bruta_Norte,
    SE_BRUTA_NORTE       = se_bruta_Norte,
    TAXA_PAD_NORTE       = taxa_padronizada_Norte,
    SE_PAD_NORTE         = se_padronizada_Norte,
    
    TAXA_BRUTA_NORDESTE  = taxa_bruta_Nordeste,
    SE_BRUTA_NORDESTE    = se_bruta_Nordeste,
    TAXA_PAD_NORDESTE    = taxa_padronizada_Nordeste,
    SE_PAD_NORDESTE      = se_padronizada_Nordeste,
    
    TAXA_BRUTA_SUDESTE   = taxa_bruta_Sudeste,
    SE_BRUTA_SUDESTE     = se_bruta_Sudeste,
    TAXA_PAD_SUDESTE     = taxa_padronizada_Sudeste,
    SE_PAD_SUDESTE       = se_padronizada_Sudeste,
    
    TAXA_BRUTA_SUL       = taxa_bruta_Sul,
    SE_BRUTA_SUL         = se_bruta_Sul,
    TAXA_PAD_SUL         = taxa_padronizada_Sul,
    SE_PAD_SUL           = se_padronizada_Sul,
    
    TAXA_BRUTA_CENTRO_OESTE = `taxa_bruta_Centro-Oeste`,
    SE_BRUTA_CENTRO_OESTE   = `se_bruta_Centro-Oeste`,
    TAXA_PAD_CENTRO_OESTE   = `taxa_padronizada_Centro-Oeste`,
    SE_PAD_CENTRO_OESTE     = `se_padronizada_Centro-Oeste`
  )

df_joinpoint_aguda_final <- df_joinpoint_faixa_aguda %>%
  left_join(df_joinpoint_regioes_aguda, by = "ANO")

# --- Exportação feita no console na execução original (reconstruída: nomes dos
# arquivos e objetos conferidos com as saídas usadas na dissertação) ---
# Figuras 4 e 5 e Figura D1 (parâmetros do ggsave do script Parte 3_2):
ggplot2::ggsave(filename = "Fig1_Taxa_Aguda_Bruta_Brasil.png", plot = grafico_total_bruto, width = 25, height = 15,
                units = "cm", dpi = 600, bg = "white")
ggplot2::ggsave(filename = "Fig2_Taxa_Aguda_Padronizada_Brasil.png", plot = grafico_total_padronizado, width = 25, height = 15,
                units = "cm", dpi = 600, bg = "white")
ggplot2::ggsave(filename = "Fig3_Taxas_Aguda_Brutas_Faixas.png", plot = grafico_faixa_bruto, width = 25, height = 15,
                units = "cm", dpi = 600, bg = "white")
ggplot2::ggsave(filename = "Fig4_Taxas_Aguda_Padronizadas_Faixas.png", plot = grafico_faixa_padronizado, width = 25, height = 15,
                units = "cm", dpi = 600, bg = "white")
ggplot2::ggsave(filename = "Fig5_Taxas_Aguda_Brutas_Regioes.png", plot = grafico_bruto_geo, width = 25, height = 15,
                units = "cm", dpi = 600, bg = "white")
ggplot2::ggsave(filename = "Fig6_Taxas_Aguda_Padronizadas_Regioes.png", plot = grafico_padronizado_geo, width = 25, height = 15,
                units = "cm", dpi = 600, bg = "white")
# Entrada do Joinpoint (taxas padronizadas e erros-padrão):
dir.create(file.path(DIR_DADOS, "Análises/Parte 3 - Taxas e Joinpoint/Joinpoint"), recursive = TRUE, showWarnings = FALSE)
jp <- dplyr::select(df_joinpoint_aguda_final, ANO, dplyr::matches("^(TAXA|SE)_PAD_"))
# Anos sem casos numa região: o Joinpoint (modelo log-linear) não aceita taxa zero.
# Na execução original, o zero virou metade da menor taxa positiva da série e o
# erro-padrão, essa taxa vezes raiz de 2 (o equivalente a meio caso).
for (v in grep("^TAXA_PAD_", names(jp), value = TRUE)) {
  z <- which(jp[[v]] == 0)
  if (length(z)) {
    meia <- min(jp[[v]][jp[[v]] > 0], na.rm = TRUE) / 2
    jp[[v]][z] <- meia
    jp[[sub("^TAXA_", "SE_", v)]][z] <- meia * sqrt(2)
  }
}
jp <- dplyr::mutate(jp, dplyr::across(-ANO, ~ round(.x, 10)))
writexl::write_xlsx(jp, file.path(DIR_DADOS, "Análises/Parte 3 - Taxas e Joinpoint/Joinpoint", "Taxas_Joinpoint_Chagas_AGUDA_SE_2001_2024.xlsx"))
