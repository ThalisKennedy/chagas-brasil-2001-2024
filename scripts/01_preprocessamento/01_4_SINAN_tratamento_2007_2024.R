# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SINAN/CHAGBR 2007-2024/CHAGBR 2007-2024.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SINAN/CHAGBR 2007-2024"))

# Base de entrada (na execução original, importada pelo menu do RStudio):
if (!exists("CHAGBR_2007_2024_STATA")) CHAGBR_2007_2024_STATA <- haven::read_dta(file.path(DIR_DADOS, "SINAN/CHAGBR 2007-2024 - STATA.dta"))

library(tidyverse)
library(writexl)
library(lubridate)
library(gtsummary)
library(flextable)
library(forcats)
library(haven)
library(stringr)
library(readr)

names(CHAGBR_2007_2024_STATA) <- toupper(names(CHAGBR_2007_2024_STATA))

CHAGAS_AGUDA_2007_2024_LIMPO <- CHAGBR_2007_2024_STATA %>% 
  
  select(
    
    DT_NOTIFIC, NU_ANO, SG_UF_NOT, ID_MUNICIP, DT_SIN_PRI, ANO_NASC, NU_IDADE_N,
    
    CS_SEXO, CS_RACA, CS_ESCOL_N, SG_UF, ID_MN_RESI, NDUPLIC_N, ID_OCUPA_N, PRESENCA, HISTORIA, 
    
    MANIPULA, ORAL, ASSINTOMA, EDEMA, MENINGOE, POLIADENO, FEBRE, HEPATOME, SINAIS_ICC, ARRITMIAS, ASTENIA, 
    
    ESPLENOM, CHAGOMA, OUTRO_SIN, OUTRO_ESP, EXAME, MICRO_HEMA, OUTRO, XENODIAG, HEMOCULT, ELI_IGM_S1, ELI_IGG_S1, 
    
    ELI_IGM_S2, ELI_IGG_S2, HEM_IGM_S1, HEM_IGG_S1, HEM_IGM_S2, HEM_IGG_S2, IMU_IGM_S1, TIT_IGM_S1, IMU_IGM_S2, TIT_IGM_S2,
    
    IMU_IGG_S1, TIT_IGG_S1, IMU_IGG_S2, TIT_IGG_S2, RES_HIST, ESPECIFICO, SINTOMATIC, DROGA, CON_TRIAT, BIOSSEG, FISCALIZA,
    
    MED_OUTRO, OUTRO_DES, CLASSI_FIN, CRITERIO, EVOLUCAO, CON_PROVAV, CON_LOCAL, CON_OUTRA, TPAUTOCTO, DOENCA_TRA
  )

glimpse(CHAGAS_AGUDA_2007_2024_LIMPO)

vars_auditoria_aguda <- c(
  "DT_NOTIFIC", "NU_ANO", "SG_UF_NOT", "ID_MUNICIP", "DT_SIN_PRI", 
  "ANO_NASC", "NU_IDADE_N", "CS_SEXO", "CS_RACA", "CS_ESCOL_N", 
  "SG_UF", "ID_MN_RESI", "NDUPLIC_N", "ID_OCUPA_N", "PRESENCA", 
  "HISTORIA", "MANIPULA", "ORAL", "ASSINTOMA", "EDEMA", "MENINGOE", 
  "POLIADENO", "FEBRE", "HEPATOME", "SINAIS_ICC", "ARRITMIAS", "ASTENIA", 
  "ESPLENOM", "CHAGOMA", "OUTRO_SIN", "OUTRO_ESP", "EXAME", "MICRO_HEMA", 
  "OUTRO", "XENODIAG", "HEMOCULT", "ELI_IGM_S1", "ELI_IGG_S1", "ELI_IGM_S2", 
  "ELI_IGG_S2", "HEM_IGM_S1", "HEM_IGG_S1", "HEM_IGM_S2", "HEM_IGG_S2", 
  "IMU_IGM_S1", "TIT_IGM_S1", "IMU_IGM_S2", "TIT_IGM_S2", "IMU_IGG_S1", 
  "TIT_IGG_S1", "IMU_IGG_S2", "TIT_IGG_S2", "RES_HIST", "ESPECIFICO", 
  "SINTOMATIC", "DROGA", "CON_TRIAT", "BIOSSEG", "FISCALIZA", "MED_OUTRO", 
  "OUTRO_DES", "CLASSI_FIN", "CRITERIO", "EVOLUCAO", "CON_PROVAV", 
  "CON_LOCAL", "CON_OUTRA", "TPAUTOCTO", "DOENCA_TRA"
)

TABELA_QUALIDADE_AGUDA <- CHAGAS_AGUDA_2007_2024_LIMPO %>%
  select(any_of(vars_auditoria_aguda)) %>%
  
  summarise(across(everything(), list(
    
    PCT_EM_BRANCO = ~ mean(is.na(.) | as.character(.) == "" | 
                             as.character(.) == "null", na.rm = TRUE) * 100,
    
    PCT_IGNORADO_CODIGO = ~ mean(as.character(.) %in% c("9", "99", "0", "00", "Ignorado", "I"), 
                                 na.rm = TRUE) * 100
  ))) %>%
  
  pivot_longer(everything(), names_to = "Indicador", values_to = "Valor") %>%
  
  separate(Indicador, into = c("Variavel", "Metrica"), sep = "_PCT_", extra = "merge") %>%
  
  mutate(
    Metrica = paste0("PCT_", Metrica),
    Valor = round(Valor, 2)
  ) %>%
  
  arrange(desc(Valor))

print(head(TABELA_QUALIDADE_AGUDA, 25))

nrow(CHAGAS_AGUDA_2007_2024_LIMPO)

vetor_incompletude <- c("9", "99", "0", "00", "Ignorado", "I", "", "NA", "null")

TABELA_QUALIDADE_AGUDA_2007_TOTAL <- CHAGAS_AGUDA_2007_2024_LIMPO %>%
  
  select(any_of(vars_auditoria_aguda)) %>%
  
  summarise(across(everything(), 
                   ~ mean(is.na(.) | as.character(.) %in% vetor_incompletude, na.rm = TRUE) * 100
  )) %>%
  
  pivot_longer(everything(), names_to = "Variavel", values_to = "PCT_INCOMPLETUDE") %>%
  
  mutate(
    PCT_INCOMPLETUDE = round(PCT_INCOMPLETUDE, 2),
    
    CLASSIFICACAO = case_when(
      PCT_INCOMPLETUDE < 10 ~ "Excelente (<10%)",
      PCT_INCOMPLETUDE < 20 ~ "Bom (10-20%)",
      PCT_INCOMPLETUDE < 50 ~ "Ruim (20-50%)",
      TRUE ~ "Crítico (>50%)"
    )
  ) %>%
  
  arrange(desc(PCT_INCOMPLETUDE))

print(head(TABELA_QUALIDADE_AGUDA_2007_TOTAL, 30))

CHAGAS_AGUDA_2007_2024_LIMPO <- CHAGAS_AGUDA_2007_2024_LIMPO %>% 
  
  filter(as.character(NDUPLIC_N) != "2" | is.na(NDUPLIC_N))

nrow(CHAGAS_AGUDA_2007_2024_LIMPO)

ncol(CHAGAS_AGUDA_2007_2024_LIMPO)

CHAGAS_AGUDA_2007_2024_LIMPO <- CHAGAS_AGUDA_2007_2024_LIMPO %>% 
  
  select(-NDUPLIC_N, -DOENCA_TRA)

ncol(CHAGAS_AGUDA_2007_2024_LIMPO)

CHAGAS_AGUDA_1_2007_2024_TRATADO <- CHAGAS_AGUDA_2007_2024_LIMPO %>%
  mutate(
    DT_NOTIFIC = ymd(DT_NOTIFIC),
    DT_SIN_PRI = ymd(DT_SIN_PRI),
    
    DIAS_ATE_NOTIFICACAO = as.numeric(DT_NOTIFIC - DT_SIN_PRI)
  ) %>%
  
  filter(DIAS_ATE_NOTIFICACAO >= 0 | is.na(DIAS_ATE_NOTIFICACAO))

CHAGAS_AGUDA_1_2007_2024_TRATADO <- CHAGAS_AGUDA_1_2007_2024_TRATADO %>%
  mutate(
    OPORTUNIDADE_NOTIFICACAO = case_when(
      is.na(DIAS_ATE_NOTIFICACAO) ~ "Ignorado/Em branco",
      DIAS_ATE_NOTIFICACAO <= 7 ~ "Oportuna (≤7 dias)",
      DIAS_ATE_NOTIFICACAO > 7 ~ "Tardia (>7 dias)"
    ),
    OPORTUNIDADE_NOTIFICACAO = factor(
      OPORTUNIDADE_NOTIFICACAO,
      levels = c("Oportuna (≤7 dias)", "Tardia (>7 dias)", "Ignorado/Em branco")
    )
  )

summary(CHAGAS_AGUDA_1_2007_2024_TRATADO$DIAS_ATE_NOTIFICACAO)

summary(CHAGAS_AGUDA_1_2007_2024_TRATADO$OPORTUNIDADE_NOTIFICACAO)

CHAGAS_AGUDA_1_2007_2024_TRATADO %>%
  select(DT_SIN_PRI, DT_NOTIFIC, DIAS_ATE_NOTIFICACAO) %>%
  head(10)

CHAGAS_AGUDA_1_2007_2024_TRATADO <- CHAGAS_AGUDA_1_2007_2024_TRATADO %>%
  mutate(
    IDADE_REAL = case_when(
      NU_IDADE_N >= 4000 ~ NU_IDADE_N - 4000,        
      NU_IDADE_N >= 1000 & NU_IDADE_N < 4000 ~ 0,    
      TRUE ~ NA_real_                                
    ),
    
    ANO_NASC_NUM = as.numeric(ANO_NASC),
    
    ANO_NOTIFIC_NUM = as.numeric(NU_ANO)
  ) %>%
  
  mutate(
    IDADE_CALCULADA = ANO_NOTIFIC_NUM - ANO_NASC_NUM,
    
    IDADE_FINAL = case_when(
      
      IDADE_REAL < 0 | IDADE_REAL > 120 ~ NA_real_,
      
      !is.na(IDADE_CALCULADA) & abs(IDADE_REAL - IDADE_CALCULADA) > 2 ~ NA_real_,
      
      TRUE ~ IDADE_REAL
    )
  ) %>%

    select(-IDADE_REAL, -IDADE_CALCULADA, -ANO_NASC_NUM, -ANO_NOTIFIC_NUM, -NU_IDADE_N)

print(table(CHAGAS_AGUDA_1_2007_2024_TRATADO$IDADE_FINAL))

CHAGAS_AGUDA_1_2007_2024_TRATADO <- CHAGAS_AGUDA_1_2007_2024_TRATADO %>%
  mutate(
    FAIXA_ETARIA = case_when(
      is.na(IDADE_FINAL) ~ "Ignorado/Em branco",
      
      IDADE_FINAL >= 0 & IDADE_FINAL <= 19  ~ "0 a 19 anos",
      IDADE_FINAL >= 20 & IDADE_FINAL <= 39 ~ "20 a 39 anos",
      IDADE_FINAL >= 40 & IDADE_FINAL <= 59 ~ "40 a 59 anos",
      IDADE_FINAL >= 60 ~ "60 anos ou mais",
      
      TRUE ~ "Ignorado/Em branco"
    )
  )

table(CHAGAS_AGUDA_1_2007_2024_TRATADO$FAIXA_ETARIA, useNA = "ifany")

head(CHAGAS_AGUDA_1_2007_2024_TRATADO%>% select(IDADE_FINAL, FAIXA_ETARIA), 15)

tabela_nomes_local <- tibble(
  CODIGO = c(11, 12, 13, 14, 15, 16, 17, 
             21, 22, 23, 24, 25, 26, 27, 28, 29, 
             31, 32, 33, 35, 
             41, 42, 43, 
             50, 51, 52, 53),
  
  NOME_ESTADO = c("Rondonia", "Acre", "Amazonas", "Roraima", "Para", "Amapa", "Tocantins",
                  "Maranhao", "Piaui", "Ceara", "Rio Grande do Norte", "Paraiba", "Pernambuco", "Alagoas", "Sergipe", "Bahia",
                  "Minas Gerais", "Espirito Santo", "Rio de Janeiro", "Sao Paulo",
                  "Parana", "Santa Catarina", "Rio Grande do Sul",
                  "Mato Grosso do Sul", "Mato Grosso", "Goias", "Distrito Federal"),
  
  REGIAO = c(rep("Norte", 7), 
             rep("Nordeste", 9), 
             rep("Sudeste", 4), 
             rep("Sul", 3), 
             rep("Centro-Oeste", 4))
)

CHAGAS_AGUDA_2_2007_2024_TRATADO <- CHAGAS_AGUDA_1_2007_2024_TRATADO %>%
  mutate(
    
    COD_UF_NOT_NUM = as.numeric(SG_UF_NOT),
    COD_UF_RES_NUM = as.numeric(SG_UF),
    
    CD_MN_RESI = str_remove(as.character(ID_MN_RESI), "\\.0$")
  ) %>%
  
  left_join(tabela_nomes_local, by = c("COD_UF_NOT_NUM" = "CODIGO")) %>%
  rename(NOME_UF_NOT = NOME_ESTADO, REGIAO_NOTIFICACAO = REGIAO) %>%
  
  left_join(tabela_nomes_local, by = c("COD_UF_RES_NUM" = "CODIGO")) %>%
  rename(NOME_UF_RES = NOME_ESTADO, REGIAO_RESIDENCIA = REGIAO) %>%
  
  mutate(

    SG_UF_NOT = replace_na(NOME_UF_NOT, "Ignorado/Em branco"),
    SG_UF = replace_na(NOME_UF_RES, "Ignorado/Em branco"),
    
    REGIAO_NOTIFICACAO = replace_na(REGIAO_NOTIFICACAO, "Ignorado/Em branco"),
    REGIAO_RESIDENCIA = replace_na(REGIAO_RESIDENCIA, "Ignorado/Em branco")
  ) %>%
  
  select(-COD_UF_NOT_NUM, -COD_UF_RES_NUM, -NOME_UF_NOT, -NOME_UF_RES)

head(CHAGAS_AGUDA_2_2007_2024_TRATADO %>% 
       select(SG_UF_NOT, REGIAO_NOTIFICACAO, SG_UF, CD_MN_RESI, REGIAO_RESIDENCIA))

print(table(CHAGAS_AGUDA_2_2007_2024_TRATADO$REGIAO_RESIDENCIA, useNA = "ifany"))

CHAGAS_AGUDA_2_2007_2024_TRATADO <- CHAGAS_AGUDA_2_2007_2024_TRATADO %>%
  mutate(
    
    COD_MUN_NOT_F = str_pad(as.character(ID_MUNICIP), width = 6, pad = "0", side = "left"),
    COD_MUN_RES_F = str_pad(as.character(CD_MN_RESI), width = 6, pad = "0", side = "left"),
    
    COD_UF_NOT = substr(COD_MUN_NOT_F, 1, 2),
    COD_UF_RES = substr(COD_MUN_RES_F, 1, 2),
    
    COD_REG_NOT = substr(COD_MUN_NOT_F, 1, 1),
    COD_REG_RES = substr(COD_MUN_RES_F, 1, 1),
    
    FLUXO_ASSISTENCIA = case_when(
      
      is.na(CD_MN_RESI) | is.na(ID_MUNICIP) ~ "Ignorado/Em branco",
      
      COD_MUN_RES_F %in% c("000000", "0000NA") | COD_MUN_NOT_F %in% c("000000", "0000NA") ~ "Ignorado/Em branco",
      
      COD_MUN_RES_F == COD_MUN_NOT_F ~ "Sem Deslocamento",
      
      (COD_MUN_RES_F != COD_MUN_NOT_F) & (COD_UF_RES == COD_UF_NOT) ~ "Deslocamento Intermunicipal",
      
      (COD_UF_RES != COD_UF_NOT) & (COD_REG_RES == COD_REG_NOT) ~ "Deslocamento Interestadual",
      
      COD_REG_RES != COD_REG_NOT ~ "Deslocamento Interregional",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    FLUXO_ASSISTENCIA = factor(FLUXO_ASSISTENCIA, 
                               levels = c("Sem Deslocamento",
                                          "Deslocamento Intermunicipal",
                                          "Deslocamento Interestadual",
                                          "Deslocamento Interregional",
                                          "Ignorado/Em branco"))
  ) %>%  
  
  select(-COD_MUN_RES_F, -COD_MUN_NOT_F, -COD_UF_RES, -COD_UF_NOT, -COD_REG_RES, -COD_REG_NOT, -ID_MUNICIP, -ID_MN_RESI)

print(table(CHAGAS_AGUDA_2_2007_2024_TRATADO$FLUXO_ASSISTENCIA, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_2_2007_2024_TRATADO$FLUXO_ASSISTENCIA)) * 100, 2))

CHAGAS_AGUDA_2_2007_2024_TRATADO <- CHAGAS_AGUDA_2_2007_2024_TRATADO %>%
  mutate(
    
    SEXO_FINAL = case_when(
      
      str_trim(toupper(as.character(CS_SEXO))) == "M" ~ "Masculino",
      
      str_trim(toupper(as.character(CS_SEXO))) == "F" ~ "Feminino",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    SEXO_FINAL = factor(SEXO_FINAL, levels = c("Masculino", "Feminino", "Ignorado/Em branco"))
  ) %>%  
  
  select(-CS_SEXO)

print(table(CHAGAS_AGUDA_2_2007_2024_TRATADO$SEXO_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_2_2007_2024_TRATADO$SEXO_FINAL)) * 100, 2))

CHAGAS_AGUDA_2_2007_2024_TRATADO <- CHAGAS_AGUDA_2_2007_2024_TRATADO %>%
  mutate(
    RACACOR_FINAL = case_when(
      as.character(CS_RACA) == "1" ~ "Branca",
      as.character(CS_RACA) == "2" ~ "Preta",
      as.character(CS_RACA) == "3" ~ "Amarela",
      as.character(CS_RACA) == "4" ~ "Parda",
      as.character(CS_RACA) == "5" ~ "Indígena",
      
      as.character(CS_RACA) == "9" ~ "Ignorado/Em branco",
      is.na(CS_RACA) | as.character(CS_RACA) == "" ~ "Ignorado/Em branco",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    RACACOR_FINAL = factor(RACACOR_FINAL, 
                     levels = c("Branca", 
                                "Preta", 
                                "Amarela", 
                                "Parda", 
                                "Indígena", 
                                "Ignorado/Em branco"))
  ) %>%  
  
  select(-CS_RACA)

print(table(CHAGAS_AGUDA_2_2007_2024_TRATADO$RACACOR_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_2_2007_2024_TRATADO$RACACOR_FINAL)) * 100, 2))

CHAGAS_AGUDA_2_2007_2024_TRATADO <- CHAGAS_AGUDA_2_2007_2024_TRATADO %>% 
  mutate(
    
    ESCOL_NUM = as.numeric(as.character(CS_ESCOL_N)),
    
    ESCOLARIDADE_FINAL = case_when(
      
      IDADE_FINAL < 5 ~ "Não se aplica",
      
      ESCOL_NUM == 0 ~ "Nenhuma",
      
      ESCOL_NUM %in% c(1, 2, 3) ~ "1 a 7 anos",
      
      ESCOL_NUM %in% c(4, 5, 6) ~ "8 a 11 anos",
      
      ESCOL_NUM %in% c(7, 8) ~ "12 anos ou mais",
      
      ESCOL_NUM == 10 ~ "Não se aplica",
      
      ESCOL_NUM == 9 ~ "Ignorado/Em branco",
      
      is.na(ESCOL_NUM) ~ "Ignorado/Em branco",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    ESCOLARIDADE_FINAL = factor(ESCOLARIDADE_FINAL,
                                levels = c("Nenhuma", 
                                           "1 a 7 anos", 
                                           "8 a 11 anos", 
                                           "12 anos ou mais", 
                                           "Não se aplica",
                                           "Ignorado/Em branco"))
  ) %>%
  
  select(-ESCOL_NUM, -CS_ESCOL_N)

print(table(CHAGAS_AGUDA_2_2007_2024_TRATADO$ESCOLARIDADE_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_2_2007_2024_TRATADO$ESCOLARIDADE_FINAL)) * 100, 2))

CHAGAS_AGUDA_3_2007_2024_TRATADO <- CHAGAS_AGUDA_2_2007_2024_TRATADO %>%
  mutate(
    
    OCUP_TEMP = as.character(ID_OCUPA_N),
    
    OCUP_TEMP = str_remove_all(OCUP_TEMP, "[^0-9]"), 
    
    OCUP_TEMP = str_pad(OCUP_TEMP, width = 6, pad = "0", side = "left"), 
    
    PRIMEIRO_DIGITO = substr(OCUP_TEMP, 1, 1),
    
    GRANDE_GRUPO_OCUP = case_when(
      
      is.na(ID_OCUPA_N) | OCUP_TEMP %in% c("000000", "999999", "998999", "999990") ~ "Ignorado/Em branco",
      
      OCUP_TEMP %in% c("999991", "999992", "999994") ~ "Inativos/Sem Ocupação", 
      
      OCUP_TEMP == "999993" ~ "Aposentado/Pensionista",
      
      PRIMEIRO_DIGITO == "0" ~ "Membros das Forças Armadas/Polícia/Bombeiros",
      PRIMEIRO_DIGITO == "1" ~ "Membros Superiores/Dirigentes/Poder Público",
      PRIMEIRO_DIGITO == "2" ~ "Profissionais das Ciências e Artes",
      PRIMEIRO_DIGITO == "3" ~ "Técnicos de Nível Médio",
      PRIMEIRO_DIGITO == "4" ~ "Serviços Administrativos",
      PRIMEIRO_DIGITO == "5" ~ "Trabalhadores de Serviços/Comércio",
      PRIMEIRO_DIGITO == "6" ~ "Trabalhadores Agropecuários/Florestais/Pesca",
      PRIMEIRO_DIGITO == "7" ~ "Trabalhadores da Produção Industrial",
      PRIMEIRO_DIGITO == "8" ~ "Operadores de Instalações/Máquinas",
      PRIMEIRO_DIGITO == "9" ~ "Trabalhadores de Reparação/Manutenção",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    PERFIL_OCUPACIONAL = case_when(
      
      GRANDE_GRUPO_OCUP == "Trabalhadores Agropecuários/Florestais/Pesca" ~ "Agropecuário/Rural",
      
      GRANDE_GRUPO_OCUP %in% c("Membros Superiores/Dirigentes/Poder Público", 
                               "Profissionais das Ciências e Artes", 
                               "Técnicos de Nível Médio") ~ "Não Manual/Intelectual/Técnico",
      
      GRANDE_GRUPO_OCUP %in% c("Serviços Administrativos", 
                               "Trabalhadores de Serviços/Comércio", 
                               "Trabalhadores da Produção Industrial", 
                               "Operadores de Instalações/Máquinas", 
                               "Trabalhadores de Reparação/Manutenção",
                               "Membros das Forças Armadas/Polícia/Bombeiros") ~ "Manual Urbano/Serviços/Industrial",
      
      GRANDE_GRUPO_OCUP == "Aposentado/Pensionista" ~ "Aposentado/Pensionista",
      
      GRANDE_GRUPO_OCUP == "Inativos/Sem Ocupação" ~ "Inativos/Sem Ocupação",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    PERFIL_OCUPACIONAL = factor(PERFIL_OCUPACIONAL,
                                levels = c("Agropecuário/Rural",
                                           "Manual Urbano/Serviços/Industrial",
                                           "Não Manual/Intelectual/Técnico",
                                           "Aposentado/Pensionista",
                                           "Inativos/Sem Ocupação",
                                           "Ignorado/Em branco"))
  ) %>%
  
  select(-OCUP_TEMP, -PRIMEIRO_DIGITO, -ID_OCUPA_N)

count(CHAGAS_AGUDA_3_2007_2024_TRATADO, GRANDE_GRUPO_OCUP) %>%
  mutate(Percentual = round(n/sum(n)*100, 2)) %>%
  print()

count(CHAGAS_AGUDA_3_2007_2024_TRATADO, PERFIL_OCUPACIONAL) %>%
  mutate(Percentual = round(n/sum(n)*100, 2)) %>%
  print()

CHAGAS_AGUDA_4_2007_2024_TRATADO <- CHAGAS_AGUDA_3_2007_2024_TRATADO %>% 
  mutate(
    
    PRESENCA_FINAL = case_when(
      as.character(PRESENCA) == "1" ~ "Sim",
      as.character(PRESENCA) == "2" ~ "Não",
      as.character(PRESENCA) == "3" ~ "Não realizado",
      as.character(PRESENCA) == "9" ~ "Ignorado/Em branco",
      is.na(PRESENCA) ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    PRESENCA_FINAL = factor(PRESENCA_FINAL, 
                            levels = c("Sim", 
                                       "Não", 
                                       "Não realizado", 
                                       "Ignorado/Em branco"))
  )

print(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$PRESENCA_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$PRESENCA_FINAL))*100, 2))

CHAGAS_AGUDA_4_2007_2024_TRATADO <- CHAGAS_AGUDA_4_2007_2024_TRATADO %>% 
  mutate(
    HISTORIA_FINAL = case_when(
      as.character(HISTORIA) == "1" ~ "Sim",
      as.character(HISTORIA) == "2" ~ "Não",
      as.character(HISTORIA) == "9" ~ "Ignorado/Em branco",
      is.na(HISTORIA) ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    HISTORIA_FINAL = factor(HISTORIA_FINAL, 
                           levels = c("Sim", "Não", "Ignorado/Em branco"))
  )

print(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$HISTORIA_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$HISTORIA_FINAL))*100, 2))

CHAGAS_AGUDA_4_2007_2024_TRATADO <- CHAGAS_AGUDA_4_2007_2024_TRATADO %>% 
  mutate(
    MANIPULA_FINAL = case_when(
      as.character(MANIPULA) == "1" ~ "Sim",
      as.character(MANIPULA) == "2" ~ "Não",
      as.character(MANIPULA) == "3" ~ "Não se aplica",
      as.character(MANIPULA) == "9" ~ "Ignorado/Em branco",
      is.na(MANIPULA) ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    MANIPULA_FINAL = factor(MANIPULA_FINAL, 
                           levels = c("Sim", 
                                      "Não", 
                                      "Não se aplica", 
                                      "Ignorado/Em branco"))
  )

print(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$MANIPULA_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$MANIPULA_FINAL))*100, 2))

CHAGAS_AGUDA_4_2007_2024_TRATADO <- CHAGAS_AGUDA_4_2007_2024_TRATADO %>% 
  mutate(
    
    ORAL_FINAL = case_when(
      as.character(ORAL) == "1" ~ "Sim",
      as.character(ORAL) == "2" ~ "Não",
      as.character(ORAL) == "9" ~ "Ignorado/Em branco",
      is.na(ORAL) ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    ORAL_FINAL = factor(ORAL_FINAL, 
                       levels = c("Sim", 
                                  "Não", 
                                  "Ignorado/Em branco"))
  )
print(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$ORAL_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$ORAL_FINAL))*100, 2))

CHAGAS_AGUDA_4_2007_2024_TRATADO <- CHAGAS_AGUDA_4_2007_2024_TRATADO %>% 
  mutate(
    ASSINTOMA_FINAL = case_when(
      as.character(ASSINTOMA) == "1" ~ "Sim",
      as.character(ASSINTOMA) == "2" ~ "Não",
      as.character(ASSINTOMA) == "9" ~ "Ignorado/Em branco",
      is.na(ASSINTOMA) ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    ASSINTOMA_FINAL = factor(ASSINTOMA_FINAL, 
                            levels = c("Sim", "Não", "Ignorado/Em branco"))
  )

print(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$ASSINTOMA_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_4_2007_2024_TRATADO$ASSINTOMA_FINAL))*100, 2))

limpar_sintoma_estrito <- function(coluna) {
  case_when(
    as.character(coluna) == "1" ~ 1,      
    as.character(coluna) == "2" ~ 0,      
    TRUE ~ NA_real_                       
  )
}

status_parasito <- function(coluna) {
  case_when(
    as.character(coluna) == "1" ~ "Positivo/Alterado",
    as.character(coluna) == "2" ~ "Negativo/Normal",
    as.character(coluna) == "3" ~ "Não Realizado",
    TRUE ~ "Ignorado/Em branco"
  )
}

status_soro <- function(coluna) {
  case_when(
    as.character(coluna) == "1" ~ "Reagente",
    as.character(coluna) == "2" ~ "Não Reagente",
    as.character(coluna) == "3" ~ "Inconclusivo",
    as.character(coluna) == "4" ~ "Não Realizado",
    TRUE ~ "Ignorado/Em branco"
  )
}

is_positivo <- function(coluna) {
  if_else(as.character(coluna) == "1", 1, 0, missing = 0)
}

is_realizado_soro <- function(col) {
  as.character(col) %in% c("1", "2", "3")
}

is_realizado_parasito <- function(col) {
  as.character(col) %in% c("1", "2")
}

is_inconclusivo <- function(col) {
  as.character(col) == "3"
}

CHAGAS_AGUDA_5_2007_2024_TRATADO <- CHAGAS_AGUDA_4_2007_2024_TRATADO %>%
  mutate(
    
    TEM_FEBRE      = limpar_sintoma_estrito(FEBRE),
    TEM_EDEMA      = limpar_sintoma_estrito(EDEMA),
    TEM_SINAIS_ICC = limpar_sintoma_estrito(SINAIS_ICC),
    TEM_ARRITMIAS  = limpar_sintoma_estrito(ARRITMIAS),
    TEM_ASTENIA    = limpar_sintoma_estrito(ASTENIA),
    TEM_HEPATOME   = limpar_sintoma_estrito(HEPATOME),
    TEM_ESPLENOM   = limpar_sintoma_estrito(ESPLENOM),
    TEM_CHAGOMA    = limpar_sintoma_estrito(CHAGOMA),
    TEM_MENINGOE   = limpar_sintoma_estrito(MENINGOE),
    TEM_POLIADENO  = limpar_sintoma_estrito(POLIADENO)

  ) %>%
  mutate(
    
    QTD_CAMPOS_SINTOMAS_VALIDOS = rowSums(!is.na(select(., 
                                                        TEM_FEBRE, TEM_EDEMA, TEM_SINAIS_ICC, TEM_ARRITMIAS, 
                                                        TEM_ASTENIA,TEM_HEPATOME, TEM_ESPLENOM, TEM_CHAGOMA, 
                                                        TEM_MENINGOE, TEM_POLIADENO))),
    
    QTD_SINTOMAS = rowSums(select(., 
                                  TEM_FEBRE, TEM_EDEMA, TEM_SINAIS_ICC, TEM_ARRITMIAS, TEM_ASTENIA,
                                  TEM_HEPATOME, TEM_ESPLENOM, TEM_CHAGOMA, TEM_MENINGOE, TEM_POLIADENO), 
                           na.rm = TRUE),
    
    QTD_SINTOMAS = if_else(QTD_CAMPOS_SINTOMAS_VALIDOS == 0, NA_real_, QTD_SINTOMAS),
    
    PERFIL_SINTOMAS = case_when(
      is.na(QTD_SINTOMAS) ~ "Ignorado/Em branco",
      (TEM_SINAIS_ICC == 1 | TEM_ARRITMIAS == 1 | TEM_MENINGOE == 1) ~ "Sintomático Grave (Cardíaco/Neuro)",
      QTD_SINTOMAS > 0 ~ "Sintomático Leve/Moderado",
      QTD_SINTOMAS == 0 ~ "Assintomático",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    RESUMO_SINTOMAS_PRINCIPAIS = paste0(
      if_else(TEM_FEBRE == 1, "Febre ", "", missing=""),
      if_else(TEM_EDEMA == 1, "Edema ", "", missing=""),
      if_else(TEM_SINAIS_ICC == 1, "ICC ", "", missing=""),
      if_else(TEM_ARRITMIAS == 1, "Arritmia ", "", missing=""),
      if_else(TEM_MENINGOE == 1, "Meningo ", "", missing=""),
      if_else(TEM_CHAGOMA == 1, "Chagoma ", "", missing=""),
      if_else(TEM_POLIADENO == 1, "Poliadeno ", "", missing=""),
      if_else(TEM_ASTENIA == 1,   "Astenia ", "", missing=""),
      if_else(TEM_HEPATOME == 1,  "Hepatomegalia ", "", missing=""),
      if_else(TEM_ESPLENOM == 1,  "Esplenomegalia ", "", missing="")
    ),
    
    RESUMO_SINTOMAS_PRINCIPAIS = str_trim(RESUMO_SINTOMAS_PRINCIPAIS),
    
    RESUMO_SINTOMAS_PRINCIPAIS = case_when(
      is.na(QTD_SINTOMAS) ~ "Ignorado/Em branco",
      RESUMO_SINTOMAS_PRINCIPAIS == "" & QTD_SINTOMAS == 0 ~ "Sem Sintomas Principais",
      TRUE ~ RESUMO_SINTOMAS_PRINCIPAIS
    ),
    
    STATUS_EXAME_DIR = status_parasito(EXAME),
    STATUS_STROUT    = status_parasito(MICRO_HEMA),
    STATUS_XENO      = status_parasito(XENODIAG),
    STATUS_HEMOCULT  = status_parasito(HEMOCULT),
    STATUS_OUTRO     = status_parasito(OUTRO),
    
    STATUS_HISTO     = if_else(as.character(RES_HIST) == "1", "Positivo", 
                               if_else(as.character(RES_HIST) == "2", "Negativo", "Ignorado/Em branco", 
                                       missing="Ignorado/Em branco"), 
                               missing="Ignorado/Em branco"),
    
    FEZ_ELISA_IGM = is_realizado_soro(ELI_IGM_S1) | is_realizado_soro(ELI_IGM_S2),
    FEZ_IFI_IGM   = is_realizado_soro(IMU_IGM_S1) | is_realizado_soro(IMU_IGM_S2),
    FEZ_HAI_IGM   = is_realizado_soro(HEM_IGM_S1) | is_realizado_soro(HEM_IGM_S2),
    
    FEZ_ELISA_IGG = is_realizado_soro(ELI_IGG_S1) | is_realizado_soro(ELI_IGG_S2),
    FEZ_IFI_IGG   = is_realizado_soro(IMU_IGG_S1) | is_realizado_soro(IMU_IGG_S2),
    FEZ_HAI_IGG   = is_realizado_soro(HEM_IGG_S1) | is_realizado_soro(HEM_IGG_S2),
    
    IS_POS_ELISA_IGM = if_else(as.character(ELI_IGM_S1) == "1" | as.character(ELI_IGM_S2) == "1", 1, 0, missing=0),
    IS_POS_IFI_IGM   = if_else(as.character(IMU_IGM_S1) == "1" | as.character(IMU_IGM_S2) == "1", 1, 0, missing=0),
    IS_POS_HAI_IGM   = if_else(as.character(HEM_IGM_S1) == "1" | as.character(HEM_IGM_S2) == "1", 1, 0, missing=0),
    
    IS_POS_ELISA_IGG = if_else(as.character(ELI_IGG_S1) == "1" | as.character(ELI_IGG_S2) == "1", 1, 0, missing=0),
    IS_POS_IFI_IGG   = if_else(as.character(IMU_IGG_S1) == "1" | as.character(IMU_IGG_S2) == "1", 1, 0, missing=0),
    IS_POS_HAI_IGG   = if_else(as.character(HEM_IGG_S1) == "1" | as.character(HEM_IGG_S2) == "1", 1, 0, missing=0),
    
    TEM_INC_ELISA_IGM = (is_inconclusivo(ELI_IGM_S1) | is_inconclusivo(ELI_IGM_S2)) & IS_POS_ELISA_IGM == 0,
    TEM_INC_IFI_IGM   = (is_inconclusivo(IMU_IGM_S1) | is_inconclusivo(IMU_IGM_S2)) & IS_POS_IFI_IGM == 0,
    TEM_INC_HAI_IGM   = (is_inconclusivo(HEM_IGM_S1) | is_inconclusivo(HEM_IGM_S2)) & IS_POS_HAI_IGM == 0,
    
    FEZ_DIRETO   = is_realizado_parasito(EXAME),
    FEZ_STROUT   = is_realizado_parasito(MICRO_HEMA),
    FEZ_XENO     = is_realizado_parasito(XENODIAG),
    FEZ_HEMOCULT = is_realizado_parasito(HEMOCULT),
    FEZ_OUTRO    = is_realizado_parasito(OUTRO),
    
    FEZ_HISTO    = as.character(RES_HIST) %in% c("1", "2"),
    
    IS_POS_DIRETO    = is_positivo(EXAME),
    IS_POS_STROUT    = is_positivo(MICRO_HEMA),
    IS_POS_XENO      = is_positivo(XENODIAG),
    IS_POS_HEMOCULT  = is_positivo(HEMOCULT),
    IS_POS_OUTRO     = is_positivo(OUTRO),        
    IS_POS_HISTO     = if_else(as.character(RES_HIST) == "1", 1, 0, missing=0),
    
    QTD_SOROLOGIAS_REALIZADAS = FEZ_ELISA_IGM + FEZ_IFI_IGM + FEZ_HAI_IGM + 
                                FEZ_ELISA_IGG + FEZ_IFI_IGG + FEZ_HAI_IGG,
    
    QTD_SOROLOGIAS_REAGENTES_TEMP = IS_POS_ELISA_IGM + IS_POS_IFI_IGM + IS_POS_HAI_IGM +
                                    IS_POS_ELISA_IGG + IS_POS_IFI_IGG + IS_POS_HAI_IGG,
    
    QTD_SOROLOGIAS_REAGENTES = if_else(QTD_SOROLOGIAS_REALIZADAS == 0, NA_real_, QTD_SOROLOGIAS_REAGENTES_TEMP),
    
    LISTA_SOROLOGIAS_REAGENTES = paste0(
      if_else(IS_POS_ELISA_IGM == 1, "ELISA-IgM ", "", missing=""),
      if_else(IS_POS_IFI_IGM == 1,   "IFI-IgM ",   "", missing=""),
      if_else(IS_POS_HAI_IGM == 1,   "HAI-IgM ",   "", missing=""),
      if_else(IS_POS_ELISA_IGG == 1, "ELISA-IgG ", "", missing=""),
      if_else(IS_POS_IFI_IGG == 1,   "IFI-IgG ",   "", missing=""),
      if_else(IS_POS_HAI_IGG == 1,   "HAI-IgG ",   "", missing="")
    ),
    
    LISTA_SOROLOGIAS_REAGENTES = str_trim(LISTA_SOROLOGIAS_REAGENTES),
    
    LISTA_SOROLOGIAS_REAGENTES = case_when(
      LISTA_SOROLOGIAS_REAGENTES != "" ~ LISTA_SOROLOGIAS_REAGENTES,
      
      (TEM_INC_ELISA_IGM | TEM_INC_IFI_IGM | TEM_INC_HAI_IGM) ~ "Inconclusivo",
      
      QTD_SOROLOGIAS_REALIZADAS > 0 ~ "Não Reagente",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    QTD_PARASITO_REALIZADOS = FEZ_DIRETO + FEZ_STROUT + FEZ_XENO + FEZ_HEMOCULT + FEZ_OUTRO + FEZ_HISTO,
    
    QTD_PARASITO_POSITIVOS = if_else(QTD_PARASITO_REALIZADOS == 0, NA_real_, 
                                     IS_POS_DIRETO + IS_POS_STROUT + IS_POS_XENO + 
                                       IS_POS_HEMOCULT + IS_POS_OUTRO + IS_POS_HISTO),
    
    LISTA_PARASITO_POSITIVOS = paste0(
      if_else(IS_POS_DIRETO == 1,   "Direto/Fresco ", "", missing=""),
      if_else(IS_POS_STROUT == 1,   "Strout/Micro ",  "", missing=""),
      if_else(IS_POS_XENO == 1,     "Xenodiag ",      "", missing=""),
      if_else(IS_POS_HEMOCULT == 1, "Hemocultura ",   "", missing=""),
      if_else(IS_POS_OUTRO == 1,    "Outro-Parasito ", "", missing=""),
      if_else(IS_POS_HISTO == 1,    "Histopatologia ", "", missing="")
    ),
    
    LISTA_PARASITO_POSITIVOS = str_trim(LISTA_PARASITO_POSITIVOS),
    
    LISTA_PARASITO_POSITIVOS = case_when(
      LISTA_PARASITO_POSITIVOS != "" ~ LISTA_PARASITO_POSITIVOS,
      
      QTD_PARASITO_REALIZADOS > 0 ~ "Negativo",
      
      TRUE ~ "Ignorado/Em branco"
    )
  )

tab_soro <- table(CHAGAS_AGUDA_5_2007_2024_TRATADO$LISTA_SOROLOGIAS_REAGENTES)

tab_soro_prop <- round(prop.table(tab_soro) * 100, 2)

df_conf_soro <- data.frame(
  Combinacao = names(tab_soro),
  Frequencia = as.numeric(tab_soro),
  Percentual = as.numeric(tab_soro_prop)
) %>%
  arrange(desc(Frequencia))

print(head(df_conf_soro, 20))

print(table(CHAGAS_AGUDA_5_2007_2024_TRATADO$QTD_SOROLOGIAS_REAGENTES, useNA = "always"))

tab_par <- table(CHAGAS_AGUDA_5_2007_2024_TRATADO$LISTA_PARASITO_POSITIVOS)

tab_par_prop <- round(prop.table(tab_par) * 100, 2)

df_conf_par <- data.frame(
  Combinacao = names(tab_par),
  Frequencia = as.numeric(tab_par),
  Percentual = as.numeric(tab_par_prop)
) %>%
  arrange(desc(Frequencia))

print(head(df_conf_par, 20))

tab_sint <- table(CHAGAS_AGUDA_5_2007_2024_TRATADO$RESUMO_SINTOMAS_PRINCIPAIS)

tab_sint_prop <- round(prop.table(tab_sint) * 100, 2)

df_conf_sint <- data.frame(
  Sintomas = names(tab_sint),
  Frequencia = as.numeric(tab_sint),
  Percentual = as.numeric(tab_sint_prop)
) %>%
  arrange(desc(Frequencia))

print(head(df_conf_sint, 20))

print(table(CHAGAS_AGUDA_5_2007_2024_TRATADO$PERFIL_SINTOMAS, useNA = "always"))

CHAGAS_AGUDA_5_2007_2024_TRATADO <- CHAGAS_AGUDA_5_2007_2024_TRATADO %>%
  
  select(
    -c(FEBRE, EDEMA, SINAIS_ICC, ARRITMIAS, ASTENIA, 
       HEPATOME, ESPLENOM, CHAGOMA, MENINGOE, POLIADENO,
       OUTRO_SIN, OUTRO_ESP),
    
    -c(PRESENCA, HISTORIA, MANIPULA, ORAL, ASSINTOMA),
    
    -c(EXAME, MICRO_HEMA, XENODIAG, HEMOCULT, OUTRO, RES_HIST),
    
    -matches("^ELI_|^IMU_|^HEM_|^TIT_"), 
    
    -c(QTD_CAMPOS_SINTOMAS_VALIDOS, QTD_SOROLOGIAS_REAGENTES_TEMP)
  )

print(head(names(CHAGAS_AGUDA_5_2007_2024_TRATADO), 20))

CHAGAS_AGUDA_6_2007_2024_TRATADO <- CHAGAS_AGUDA_5_2007_2024_TRATADO %>%
  mutate(
    
    RECEBEU_ESPECIFICO = case_when(
      as.character(ESPECIFICO) == "1" ~ 1,  
      as.character(ESPECIFICO) == "2" ~ 0,  
      TRUE ~ NA_real_                       
    ),
    
    ESPECIFICO_FINAL = case_when(
      as.character(ESPECIFICO) == "1" ~ "Realizado",
      as.character(ESPECIFICO) == "2" ~ "Não Realizado",
      as.character(ESPECIFICO) == "9" ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    RECEBEU_SINTOMATICO = case_when(
      as.character(SINTOMATIC) == "1" ~ 1,  
      as.character(SINTOMATIC) == "2" ~ 0,
      TRUE ~ NA_real_
    ),
    
    SINTOMATICO_FINAL = case_when(
      as.character(SINTOMATIC) == "1" ~ "Realizado",
      as.character(SINTOMATIC) == "2" ~ "Não Realizado",
      as.character(SINTOMATIC) == "9" ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    )
  )

print(table(CHAGAS_AGUDA_6_2007_2024_TRATADO$ESPECIFICO_FINAL, useNA = "always"))

print(table(CHAGAS_AGUDA_6_2007_2024_TRATADO$SINTOMATICO_FINAL, useNA = "always"))

CHAGAS_AGUDA_6_2007_2024_TRATADO <- CHAGAS_AGUDA_6_2007_2024_TRATADO %>%
  select(
    
    -c(ESPECIFICO, SINTOMATIC)
  
  )

CHAGAS_AGUDA_6_2007_2024_TRATADO <- CHAGAS_AGUDA_6_2007_2024_TRATADO %>%
  mutate(
    
    DROGA_FINAL = case_when(
      
      RECEBEU_ESPECIFICO == 0 ~ "Não se aplica",
      
      is.na(ESPECIFICO_FINAL) ~ "Ignorado/Em branco",
      
      as.character(DROGA) == "1" ~ "Benznidazol",
      as.character(DROGA) == "2" ~ "Outra",
      
      TRUE ~ "Ignorado/Em branco" 
    )
  ) %>%
  
  select(-c(DROGA))

print(table(CHAGAS_AGUDA_6_2007_2024_TRATADO$DROGA_FINAL, useNA = "always"))

status_medida <- function(coluna) {
  case_when(
    as.character(coluna) == "1" ~ "Sim",
    as.character(coluna) == "2" ~ "Não",
    as.character(coluna) == "3" ~ "Não se aplica",
    as.character(coluna) == "9" ~ "Ignorado/Em branco",
    TRUE ~ "Ignorado/Em branco"
  )
}

tem_medida <- function(coluna) {
  if_else(as.character(coluna) == "1", 1, 0, missing = 0)
}

is_preenchido_medida <- function(coluna) {
  as.character(coluna) %in% c("1", "2", "3")
}

CHAGAS_AGUDA_6_2007_2024_TRATADO <- CHAGAS_AGUDA_6_2007_2024_TRATADO  %>%
  mutate(
    
    STATUS_CON_TRIAT = status_medida(CON_TRIAT),  
    STATUS_BIOSSEG   = status_medida(BIOSSEG),    
    STATUS_FISCALIZA = status_medida(FISCALIZA),  
    STATUS_MED_OUTRO = status_medida(MED_OUTRO),  
    
    TEM_CON_TRIAT = tem_medida(CON_TRIAT),
    TEM_BIOSSEG   = tem_medida(BIOSSEG),
    TEM_FISCALIZA = tem_medida(FISCALIZA),
    TEM_MED_OUTRO = tem_medida(MED_OUTRO),
    
    PREENCHEU_CON_TRIAT = is_preenchido_medida(CON_TRIAT),
    PREENCHEU_BIOSSEG   = is_preenchido_medida(BIOSSEG),
    PREENCHEU_FISCALIZA = is_preenchido_medida(FISCALIZA),
    PREENCHEU_MED_OUTRO = is_preenchido_medida(MED_OUTRO),
    
    TUDO_NAO_SE_APLICA = (STATUS_CON_TRIAT == "3" & 
                            STATUS_BIOSSEG   == "3" & 
                            STATUS_FISCALIZA == "3" & 
                            STATUS_MED_OUTRO == "3"),
    
    TUDO_PREENCHIDO = (PREENCHEU_CON_TRIAT & PREENCHEU_BIOSSEG & 
                         PREENCHEU_FISCALIZA & PREENCHEU_MED_OUTRO),
    
    OUTRO_DES_TEMP = str_to_upper(str_trim(as.character(OUTRO_DES))),
    
    CATEGORIA_OUTRO_DES = case_when(
      TEM_MED_OUTRO == 0 & (is.na(OUTRO_DES_TEMP) | OUTRO_DES_TEMP == "") ~ "Não se aplica",
      str_detect(OUTRO_DES_TEMP, "INVESTIG") ~ "Investigação Epidemiológica",
      str_detect(OUTRO_DES_TEMP, "EDUC|PALESTRA|CONSCIENT") ~ "Educação em Saúde",
      str_detect(OUTRO_DES_TEMP, "ORIENT") ~ "Orientações Gerais",
      str_detect(OUTRO_DES_TEMP, "VIGILANCIA|VIGEPI|MONITOR") ~ "Ações de Vigilância",
      str_detect(OUTRO_DES_TEMP, "INQUERITO|COLETA|SORO|HEMO") ~ "Inquérito/Coleta",
      str_detect(OUTRO_DES_TEMP, "BORRIF|FUMACE|PULVER") ~ "Controle Químico",
      str_detect(OUTRO_DES_TEMP, "MOSQUET|REDE|TELAGEM") ~ "Barreira Física",
      TEM_MED_OUTRO == 1 & (is.na(OUTRO_DES_TEMP) | OUTRO_DES_TEMP == "") ~ "Outra",
      TEM_MED_OUTRO == 1 ~ "Outra", 
      TRUE ~ "Ignorado/Em branco"
    ),
    
    QTD_MEDIDAS_TOMADAS = TEM_CON_TRIAT + TEM_BIOSSEG + TEM_FISCALIZA + TEM_MED_OUTRO,
    
    HOUVE_INTERVENCAO_GERAL = case_when(
      QTD_MEDIDAS_TOMADAS > 0 ~ "Sim",
      
      TUDO_NAO_SE_APLICA ~ "Não se aplica",
      
      TUDO_PREENCHIDO ~ "Não",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    LISTA_MEDIDAS_TOMADAS = paste0(
      if_else(TEM_CON_TRIAT == 1, "Pesq.Vetor ", "", missing=""),
      if_else(TEM_BIOSSEG   == 1, "Biossegurança ", "", missing=""),
      if_else(TEM_FISCALIZA == 1, "Fiscalização/Borrif ", "", missing=""),
      if_else(TEM_MED_OUTRO == 1, paste0("Outra(", CATEGORIA_OUTRO_DES, ") "), "", missing="")
    ),
    
    LISTA_MEDIDAS_TOMADAS = str_trim(LISTA_MEDIDAS_TOMADAS),
    
    LISTA_MEDIDAS_TOMADAS = case_when(
      LISTA_MEDIDAS_TOMADAS != "" ~ LISTA_MEDIDAS_TOMADAS,
      
      TUDO_NAO_SE_APLICA ~ "Não se aplica",
      
      TUDO_PREENCHIDO ~ "Nenhuma Medida Adotada",
      
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%
  
  select(-c(CON_TRIAT, BIOSSEG, FISCALIZA, MED_OUTRO, OUTRO_DES, OUTRO_DES_TEMP))

print(table(CHAGAS_AGUDA_6_2007_2024_TRATADO$HOUVE_INTERVENCAO_GERAL, useNA = "always"))

print(head(sort(table(CHAGAS_AGUDA_6_2007_2024_TRATADO$CATEGORIA_OUTRO_DES), decreasing = TRUE), 15))

print(head(sort(table(CHAGAS_AGUDA_6_2007_2024_TRATADO$LISTA_MEDIDAS_TOMADAS), decreasing = TRUE), 20))

CHAGAS_AGUDA_7_2007_2024_TRATADO <- CHAGAS_AGUDA_6_2007_2024_TRATADO %>%
  mutate(
    
    CLASSIFICACAO_FINAL = case_when(
      as.character(CLASSI_FIN) == "1" ~ "Confirmado",
      as.character(CLASSI_FIN) == "2" ~ "Descartado",
      as.character(CLASSI_FIN) == "8" ~ "Transferência/Não Encerrado",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    IS_CONFIRMADO = if_else(as.character(CLASSI_FIN) == "1", 1, 0, missing = 0),
    
    CRITERIO_FINAL = case_when(
      IS_CONFIRMADO == 0 ~ "Não se aplica", 
      
      as.character(CRITERIO) == "1" ~ "Laboratorial",
      as.character(CRITERIO) == "2" ~ "Clínico-Epidemiológico",
      as.character(CRITERIO) == "3" ~ "Clínico",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    EVOLUCAO_FINAL = case_when(
      as.character(EVOLUCAO) == "1" ~ "Cura",
      as.character(EVOLUCAO) == "2" ~ "Óbito por Chagas",
      as.character(EVOLUCAO) == "3" ~ "Óbito por outras causas",
      as.character(EVOLUCAO) == "9" ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    IS_OBITO_CHAGAS = if_else(as.character(EVOLUCAO) == "2", 1, 0, missing = 0)
  ) %>%
  
  select(-c(CLASSI_FIN, CRITERIO, EVOLUCAO))

print(table(CHAGAS_AGUDA_7_2007_2024_TRATADO$CLASSIFICACAO_FINAL, useNA = "always"))

print(table(CHAGAS_AGUDA_7_2007_2024_TRATADO$CRITERIO_FINAL, useNA = "always"))

print(table(CHAGAS_AGUDA_7_2007_2024_TRATADO$EVOLUCAO_FINAL, useNA = "always"))

CHAGAS_AGUDA_7_2007_2024_TRATADO <- CHAGAS_AGUDA_7_2007_2024_TRATADO %>%
  mutate(
    
    TPAUTOCTO_FINAL = case_when(
      IS_CONFIRMADO == 1 & TPAUTOCTO == 1 ~ "Autóctone",
      IS_CONFIRMADO == 1 & TPAUTOCTO == 2 ~ "Não Autóctone",
      IS_CONFIRMADO == 1 & TPAUTOCTO == 3 ~ "Indeterminado",
      
      IS_CONFIRMADO == 1 & (is.na(TPAUTOCTO) | TPAUTOCTO == 9) ~ "Ignorado/Em branco",
      
      IS_CONFIRMADO != 1 ~ "Não se aplica",
      
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%
  
  select(-TPAUTOCTO)

print(table(CHAGAS_AGUDA_7_2007_2024_TRATADO$TPAUTOCTO_FINAL, useNA = "always"))

print(table(CHAGAS_AGUDA_7_2007_2024_TRATADO$CLASSIFICACAO_FINAL, 
            CHAGAS_AGUDA_7_2007_2024_TRATADO$TPAUTOCTO_FINAL))

CHAGAS_AGUDA_FINAL_2007_2024_TRATADO <- CHAGAS_AGUDA_7_2007_2024_TRATADO %>%
  mutate(
      USOU_MEDICAMENTO = case_when(
        ESPECIFICO_FINAL == "Realizado" & SINTOMATICO_FINAL == "Realizado" ~ "Sim, ambos",
        
        ESPECIFICO_FINAL == "Realizado" ~ "Sim, somente específico",
        
        SINTOMATICO_FINAL == "Realizado" ~ "Sim, somente sintomático",
        
        ESPECIFICO_FINAL == "Não Realizado" & SINTOMATICO_FINAL == "Não Realizado" ~ "Nenhum medicamento",
        
        TRUE ~ "Ignorado/Em branco"
      ),
      
      USOU_MEDICAMENTO = factor(USOU_MEDICAMENTO, levels = c(
        "Sim, ambos",
        "Sim, somente específico",
        "Sim, somente sintomático",
        "Nenhum medicamento",
        "Ignorado/Em branco"
    )))

print(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$USOU_MEDICAMENTO, useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$FAIXA_ETARIA))*100, 2))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$ESCOLARIDADE_FINAL))*100, 2))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$SINTOMATICO_FINAL))*100, 2))

print(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$CRITERIO_FINAL[
  CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$CRITERIO_FINAL[
  CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$IS_CONFIRMADO == 1]))*100, 2))

print(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$FAIXA_ETARIA[
  CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$TPAUTOCTO_FINAL[
  CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$IS_CONFIRMADO == 1]))*100, 2))

print(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$REGIAO_RESIDENCIA[
  CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$REGIAO_RESIDENCIA[
  CHAGAS_AGUDA_FINAL_2007_2024_TRATADO$IS_CONFIRMADO == 1]))*100, 2))

CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO <- CHAGAS_AGUDA_FINAL_2007_2024_TRATADO %>%
  mutate(
    
    LOCAL_INFECCAO_FINAL = case_when(
      as.character(CON_LOCAL) == "1" ~ "Unidade de Hemoterapia",
      as.character(CON_LOCAL) == "2" ~ "Domicílio",
      as.character(CON_LOCAL) == "3" ~ "Laboratório",
      as.character(CON_LOCAL) == "4" ~ "Outro",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    CON_OUTRA_CLEAN = str_to_upper(str_trim(as.character(CON_OUTRA))),
    CON_OUTRA_CLEAN = iconv(CON_OUTRA_CLEAN, from = "UTF-8", to = "ASCII//TRANSLIT"),
    
    MECANISMO_TEMP = case_when(
      as.character(CON_PROVAV) == "1" ~ "Transfusional",
      as.character(CON_PROVAV) == "2" ~ "Vetorial",
      as.character(CON_PROVAV) == "3" ~ "Vertical/Transplacentária",
      as.character(CON_PROVAV) == "4" ~ "Acidental",
      as.character(CON_PROVAV) == "5" ~ "Oral",
      TRUE ~ "Verificar Texto"
    ),
    
    MODO_INFECCAO_FINAL = case_when(
      MECANISMO_TEMP != "Verificar Texto" ~ MECANISMO_TEMP,
      
      str_detect(CON_OUTRA_CLEAN, "ORAL|INGEST|ALIMENT|COMIDA|BEBIDA|SUCO|ACAI|BACABA|CANA|GARAPA") ~ "Oral",
      
      str_detect(CON_OUTRA_CLEAN, "VETOR|BARBEIRO|PICADA|INSETO|TRIATOM|FEZES") ~ "Vetorial",
      
      str_detect(CON_OUTRA_CLEAN, "ACIDENT|PERFURO|CORTANTE|CONTAMINACAO.*LABORAT") ~ "Acidental",
      
      str_detect(CON_OUTRA_CLEAN, "TRANSF|HEMODERIV|RECEBEU SANGUE") ~ "Transfusional",
      
      str_detect(CON_OUTRA_CLEAN, "MAE|MATERNA|CONGENIT|PARTO|GRAVID") ~ "Vertical/Transplacentária",
      
      as.character(CON_PROVAV) == "6" ~ "Outra",
      
      TRUE ~ "Ignorado/Em branco"
      
    ),
    
    MODO_INFECCAO_FINAL = factor(MODO_INFECCAO_FINAL, levels = c(
      "Vetorial", "Oral", "Vertical/Transplacentária", "Acidental", 
      "Transfusional", "Outra", "Ignorado/Em branco"
    ))
  ) %>%
  
  select(-CON_OUTRA_CLEAN, -MECANISMO_TEMP, -CON_PROVAV, -CON_LOCAL, -CON_OUTRA)

print(table(CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO$LOCAL_INFECCAO_FINAL[
  CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO$LOCAL_INFECCAO_FINAL[
  CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO$IS_CONFIRMADO == 1]))*100, 2))

print(table(CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO$MODO_INFECCAO_FINAL[
  CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO$MODO_INFECCAO_FINAL[
  CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO$IS_CONFIRMADO == 1]))*100, 2))

# --- Exportação feita no console na execução original ---
saveRDS(CHAGAS_AGUDA_FINAL_2_2007_2024_TRATADO, "CHAGAS_AGUDA_FINAL_4_2007_2024_TRATADO.rds")
