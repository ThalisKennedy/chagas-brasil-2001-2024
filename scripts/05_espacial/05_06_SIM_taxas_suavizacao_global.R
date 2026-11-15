# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SIM/DOBR - CHAGAS/DOBR - CHAGAS - PARTE 4 - GLOBAL.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SIM/DOBR - CHAGAS"))

# Base de entrada (na execução original, importada pelo menu do RStudio):
if (!exists("MORT_CHAGAS_PRE_PROCESSAMENTO_2")) MORT_CHAGAS_PRE_PROCESSAMENTO_2 <- haven::read_dta(file.path(DIR_DADOS, "SIM/MORT_CHAGAS_PRE_PROCESSAMENTO_2.dta"))

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, writexl, lubridate, gtsummary, flextable, officer, 
               forcats, haven, stringr, readr, gt, prais, dplyr, tidyr, 
               ggplot2, ggrepel, sf, geobr, spdep)

names(MORT_CHAGAS_PRE_PROCESSAMENTO_2) <- toupper(names(MORT_CHAGAS_PRE_PROCESSAMENTO_2))

vars_para_manter <- c("DTOBITO", "DTNASC", "IDADE", "SEXO", "RACACOR", "ESTCIV", "NATURAL", "CODMUNNATU",
                      "ESC",  "OCUP", "CODMUNRES", "CODMUNOCOR", "ASSISTMED", "EXAME", "CIRURGIA", "LOCOCOR", 
                      "NECROPSIA", "LINHAA", "LINHAB", "LINHAC", "LINHAD", "LINHAII", "CAUSABAS", "ATESTADO", 
                      "TPPOS", "CHAGAS_MENCAO", "TIPO_MENCAO", "FONTEINV")

MORT_CHAGAS_LIMPO <- MORT_CHAGAS_PRE_PROCESSAMENTO_2 %>% select(any_of(vars_para_manter))

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_LIMPO %>%
  mutate(
    DATA_OBITO = dmy(DTOBITO), 
    DATA_NASC  = dmy(DTNASC),
    idade_fracionada = time_length(interval(DATA_NASC, DATA_OBITO), "years"),
    IDADE_OBITO = floor(idade_fracionada),
    ANO_OBITO = year(DATA_OBITO)
  ) %>%
  select(-IDADE, -idade_fracionada, -DTOBITO, -DTNASC)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    SEXO_TEMP = str_trim(as.character(SEXO)),
    SEXO = case_when(
      SEXO_TEMP == "1" ~ "Masculino",
      SEXO_TEMP == "2" ~ "Feminino",
      TRUE ~ "Ignorado/Em branco"
    ),
    SEXO = factor(SEXO, levels = c("Masculino", "Feminino", "Ignorado/Em branco")),
    
    RACA_TEMP = as.numeric(RACACOR),
    RACA_TEMP = replace_na(RACA_TEMP, 9),
    RACACOR = factor(RACA_TEMP,
                     levels = c(1, 2, 3, 4, 5, 9),
                     labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorado/Em branco"))
  ) %>%
  select(-SEXO_TEMP, -RACA_TEMP)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    ESTCIV_TEMP = as.numeric(ESTCIV),
    ESTCIV_TEMP = replace_na(ESTCIV_TEMP, 9),
    ESTCIV_TEMP = if_else(ESTCIV_TEMP == 0, 9, ESTCIV_TEMP),
    ESTADO_CIVIL = factor(ESTCIV_TEMP,
                          levels = c(1, 2, 3, 4, 5, 9),
                          labels = c("Solteiro", "Casado", "Viúvo", "Separado/Divorciado", "União Estável", "Ignorado/Em branco"))
  ) %>%
  select(-ESTCIV_TEMP, -ESTCIV)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    FAIXA_ETARIA = cut(IDADE_OBITO, 
                       breaks = c(0, 46, 60, 75, Inf),
                       labels = c("0 a 45 anos", "46 a 59 anos", "60 a 74 anos", "75 anos ou mais"),
                       right = FALSE),
    FAIXA_ETARIA = as.character(FAIXA_ETARIA),
    FAIXA_ETARIA = replace_na(FAIXA_ETARIA, "Ignorado/Em branco"),
    FAIXA_ETARIA = factor(FAIXA_ETARIA, levels = c("0 a 45 anos", "46 a 59 anos", "60 a 74 anos", "75 anos ou mais", "Ignorado/Em branco"))
  )

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    CODMUNRES = substr(as.character(CODMUNRES), 1, 6),
    CODMUNOCOR = substr(as.character(CODMUNOCOR), 1, 6)
  ) %>%
  select(-CHAGAS_MENCAO, -NATURAL, -CODMUNNATU) 

MORT_CHAGAS_FINAL_TRATADO <- MORT_CHAGAS_TRATADO


caminho_dados <- paste0(file.path(DIR_DADOS, "IBGE/IBGE_POP_2001_2024"), "/")

pop_padrao_br2022 <- readRDS(paste0(caminho_dados, "pop_padrao_br2022.rds"))

pop_ibge <- readRDS(paste0(caminho_dados, "POP_IBGE_MUNICIPIO_TRATADA.rds")) %>% 
  mutate(COD_MUN = as.character(substr(COD_MUN, 1, 6)))

obitos <- MORT_CHAGAS_FINAL_TRATADO %>%
  mutate(CODMUNRES = as.character(CODMUNRES))

obitos_municipio <- obitos %>%
  filter(!is.na(IDADE_OBITO)) %>% 
  mutate(
    FAIXA_QUINQUENAL = cut(IDADE_OBITO, 
                           breaks = c(seq(0, 80, by = 5), Inf),
                           labels = c("0-4", "5-9", "10-14", "15-19", "20-24", 
                                      "25-29", "30-34", "35-39", "40-44", "45-49", 
                                      "50-54", "55-59", "60-64", "65-69", "70-74", 
                                      "75-79", "80+"),
                           right = FALSE)
  ) %>%
  group_by(CODMUNRES, FAIXA_QUINQUENAL) %>%
  summarise(total_obitos = n(), .groups = "drop")

pop_municipio_agrupada <- pop_ibge %>%
  group_by(COD_MUN, FAIXA_QUINQUENAL) %>%
  summarise(pop_media_periodo = mean(populacao_residente, na.rm = TRUE), .groups = "drop")

dados_combinados <- pop_municipio_agrupada %>%
  left_join(obitos_municipio, by = c("COD_MUN" = "CODMUNRES", "FAIXA_QUINQUENAL" = "FAIXA_QUINQUENAL")) %>%
  mutate(total_obitos = replace_na(total_obitos, 0)) %>%
  left_join(pop_padrao_br2022, by = "FAIXA_QUINQUENAL")

taxas_municipais <- dados_combinados %>%
  mutate(
    media_obitos_anuais = total_obitos / 24, 
    taxa_especifica = if_else(pop_media_periodo == 0, 0, media_obitos_anuais / pop_media_periodo),
    obitos_esperados_padrao = taxa_especifica * peso_pop_padrao 
  ) %>%
  group_by(COD_MUN) %>%
  summarise(
    total_obitos_mun = sum(total_obitos),     
    pop_media_mun = sum(pop_media_periodo), 
    taxa_bruta = ((total_obitos_mun / 24) / pop_media_mun) * 100000,
    taxa_padronizada = sum(obitos_esperados_padrao, na.rm = TRUE),
    .groups = "drop"
  )

dir_shapefile <- file.path(DIR_DADOS, "Análises/Parte 4 - Análise Espacial/Shapes") 
setwd(dir_shapefile)

malha_brasil <- st_read("BR_Municipios_2024.shp", quiet = TRUE) %>%
  mutate(COD_MUN = as.character(substr(CD_MUN, 1, 6)))

mapa_taxas <- malha_brasil %>%
  left_join(taxas_municipais, by = "COD_MUN") %>%
  mutate(
    total_obitos_mun = replace_na(total_obitos_mun, 0),
    pop_media_mun = replace_na(pop_media_mun, 0),
    taxa_bruta = replace_na(taxa_bruta, 0),
    taxa_padronizada = replace_na(taxa_padronizada, 0)
  )

mapa_valido <- mapa_taxas %>% filter(!st_is_empty(st_geometry(mapa_taxas)) & pop_media_mun > 0)

mapa_valido <- mapa_valido %>%
  mutate(
    obitos_padronizados_esperados = (taxa_padronizada * pop_media_mun) / 100000
  )

res_bayes_global <- EBest(
  n = mapa_valido$obitos_padronizados_esperados,  
  x = mapa_valido$pop_media_mun                   
)

mapa_valido$taxa_padronizada_suavizada_global <- res_bayes_global$est * 100000

taxas_finais <- mapa_valido %>%
  st_drop_geometry() %>%
  select(codigo_municipio = COD_MUN, taxa_padronizada_suavizada_global) %>%
  mutate(codigo_municipio = as.numeric(codigo_municipio))

dir_etapa6 <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo")
setwd(dir_etapa6)

df_etapa6 <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Mortalidade.csv", show_col_types = FALSE)

df_etapa6_atualizado <- df_etapa6 %>%
  mutate(chave_6_digitos = as.numeric(substr(as.character(codigo_municipio), 1, 6))) %>%
  left_join(taxas_finais %>% rename(chave_6_digitos = codigo_municipio), by = "chave_6_digitos") %>%
  select(-chave_6_digitos) %>%
  select(-mort_taxa_padronizada_suavizada) %>%
  rename(mort_taxa_padronizada_suavizada = taxa_padronizada_suavizada_global)

write_csv(df_etapa6_atualizado, "Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Mortalidade_global.csv")