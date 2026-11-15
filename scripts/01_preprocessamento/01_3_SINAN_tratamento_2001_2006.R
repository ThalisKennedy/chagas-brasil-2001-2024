# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SINAN/CHAGBR 2001-2006/CHAGBR 2001-2006.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SINAN/CHAGBR 2001-2006"))

# Base de entrada (na execução original, importada pelo menu do RStudio):
if (!exists("CHAGBR_2001_2006_STATA")) CHAGBR_2001_2006_STATA <- haven::read_dta(file.path(DIR_DADOS, "SINAN/CHAGBR 2001-2006 - STATA.dta"))

library(tidyverse)
library(writexl)
library(lubridate)
library(gtsummary)
library(flextable)
library(forcats)
library(haven)
library(stringr)
library(readr)

names(CHAGBR_2001_2006_STATA) <- toupper(names(CHAGBR_2001_2006_STATA))

CHAGAS_AGUDA_2001_2006_LIMPO <- CHAGBR_2001_2006_STATA %>%
  rename(
    NU_IDADE_N = NU_IDADE,
    CS_ESCOL_N = CS_ESCOLAR,
    NDUPLIC_N  = NDUPLIC,
    ID_OCUPA_N = OCUPACAO,
    SINAIS_ICC = SINAIS,
    HEMOCULT   = HEMO,
    SINTOMATIC = INESPECIF,
    CON_TRIAT  = MED_CONTR,
    BIOSSEG    = IMPLANTA,
    CLASSI_FIN = CON_CLASSI,
    CRITERIO   = CON_CRITER,
    CON_LOCAL  = CON_PROVAV, 
    CON_PROVAV = CON_INFECC, 
    CON_OUTRA =  CON_INF_OU    
  )

CHAGAS_AGUDA_2001_2006_LIMPO <- CHAGAS_AGUDA_2001_2006_LIMPO %>% 
  
  select(
    
    DT_NOTIFIC, NU_ANO, SG_UF_NOT, ID_MUNICIP, DT_SIN_PRI, NU_IDADE_N,
    
    CS_SEXO, CS_RACA, CS_ESCOL_N, SG_UF, ID_MN_RESI, NDUPLIC_N, ID_OCUPA_N, PRESENCA, HISTORIA, 
    
    MANIPULA, ASSINTOMA, EDEMA, MENINGOE, FEBRE, HEPATOME, SINAIS_ICC, ARRITMIAS, ASTENIA, 
    
    ESPLENOM, CHAGOMA, EXAME, XENODIAG, HEMOCULT, ELISA, IMUNO, ESPECIFICO, SINTOMATIC, DROGA, CON_TRIAT, BIOSSEG, 
    
    FISCALIZA, MED_OUTRO, CLASSI_FIN, CRITERIO, CON_EVOLUC, SUSPEITOS, CONFIRMAD, CEFALEIA, ANOREXIA,
    
    HIPOREXIA, HEPAESPLE, GANGLIOS, AUMENTO, ECG, RAIOX,
    
    CON_LOCAL, CON_PROVAV, CON_OUTRA
  )

glimpse(CHAGAS_AGUDA_2001_2006_LIMPO)


vars_auditoria_aguda_2001 <- c(
  "DT_NOTIFIC", "NU_ANO", "SG_UF_NOT", "ID_MUNICIP", "DT_SIN_PRI", "NU_IDADE_N", 
  "CS_SEXO", "CS_RACA", "CS_ESCOL_N", "SG_UF", "ID_MN_RESI", "NDUPLIC_N", "ID_OCUPA_N", "PRESENCA", "HISTORIA", 
  "MANIPULA", "ASSINTOMA", "EDEMA", "MENINGOE", "FEBRE", "HEPATOME", "SINAIS_ICC", "ARRITMIAS", "ASTENIA", 
  "ESPLENOM", "CHAGOMA", "EXAME", "XENODIAG", "HEMOCULT", "ELISA", "IMUNO", "ESPECIFICO", "SINTOMATIC", "DROGA", 
  "CON_TRIAT", "BIOSSEG", "FISCALIZA", "MED_OUTRO", "CLASSI_FIN", "CRITERIO", "CON_EVOLUC", "SUSPEITOS", 
  "CONFIRMAD", "CEFALEIA", "ANOREXIA", "HIPOREXIA", "HEPAESPLE", "GANGLIOS", "AUMENTO", "ECG", "RAIOX",
  "CON_LOCAL", "CON_PROVAV", "CON_OUTRA"
)

TABELA_QUALIDADE_AGUDA_2001 <- CHAGAS_AGUDA_2001_2006_LIMPO %>%
  select(any_of(vars_auditoria_aguda_2001)) %>%
  
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

print(head(TABELA_QUALIDADE_AGUDA_2001, 25))

nrow(CHAGAS_AGUDA_2001_2006_LIMPO)

vetor_incompletude <- c("9", "99", "0", "00", "Ignorado", "I", "", "NA", "null")

TABELA_QUALIDADE_AGUDA_2001_TOTAL <- CHAGAS_AGUDA_2001_2006_LIMPO %>%
  
  select(any_of(vars_auditoria_aguda_2001)) %>%
  
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

print(head(TABELA_QUALIDADE_AGUDA_2001_TOTAL, 30))

CHAGAS_AGUDA_2001_2006_LIMPO <- CHAGAS_AGUDA_2001_2006_LIMPO %>% 
  
  filter(as.character(NDUPLIC_N) != "2" | is.na(NDUPLIC_N))

nrow(CHAGAS_AGUDA_2001_2006_LIMPO)

ncol(CHAGAS_AGUDA_2001_2006_LIMPO)

CHAGAS_AGUDA_2001_2006_LIMPO <- CHAGAS_AGUDA_2001_2006_LIMPO %>% 
  
  select(-NDUPLIC_N)

ncol(CHAGAS_AGUDA_2001_2006_LIMPO)

CHAGAS_AGUDA_2001_2006_TRATADO <- CHAGAS_AGUDA_2001_2006_LIMPO %>%
  mutate(
    DT_NOTIFIC = ymd(DT_NOTIFIC),
    DT_SIN_PRI = ymd(DT_SIN_PRI),
    
    DIAS_ATE_NOTIFICACAO = as.numeric(DT_NOTIFIC - DT_SIN_PRI)
    
  ) %>%
  
  filter(DIAS_ATE_NOTIFICACAO >= 0 | is.na(DIAS_ATE_NOTIFICACAO))

CHAGAS_AGUDA_2001_2006_TRATADO <- CHAGAS_AGUDA_2001_2006_TRATADO %>%
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

summary(CHAGAS_AGUDA_2001_2006_TRATADO$DIAS_ATE_NOTIFICACAO)

summary(CHAGAS_AGUDA_2001_2006_TRATADO$OPORTUNIDADE_NOTIFICACAO)

CHAGAS_AGUDA_2001_2006_TRATADO %>%
  select(DT_SIN_PRI, DT_NOTIFIC, DIAS_ATE_NOTIFICACAO) %>%
  head(10)

CHAGAS_AGUDA_2001_2006_TRATADO <- CHAGAS_AGUDA_2001_2006_TRATADO %>%
  mutate(
    
    TIPO_IDADE = str_sub(NU_IDADE_N, 1, 1),
    
    VALOR_IDADE = as.numeric(str_sub(NU_IDADE_N, 2, 4)),
    
    IDADE_FINAL = case_when(
      
      TIPO_IDADE == "A" ~ VALOR_IDADE,
      
      TIPO_IDADE %in% c("M", "D") ~ 0,
      
      TIPO_IDADE %in% c("1", "2", "3") & VALOR_IDADE < 1000 ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    IDADE_FINAL = ifelse(IDADE_FINAL < 0 | IDADE_FINAL > 120, NA_real_, IDADE_FINAL),
    
    FAIXA_ETARIA = case_when(
      is.na(IDADE_FINAL) ~ "Ignorado/Em branco",
      IDADE_FINAL >= 0 & IDADE_FINAL <= 19  ~ "0 a 19 anos",
      IDADE_FINAL >= 20 & IDADE_FINAL <= 39 ~ "20 a 39 anos",
      IDADE_FINAL >= 40 & IDADE_FINAL <= 59 ~ "40 a 59 anos",
      IDADE_FINAL >= 60 ~ "60 anos ou mais",
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%
  
  select(-TIPO_IDADE, -VALOR_IDADE, -NU_IDADE_N)

print(table(CHAGAS_AGUDA_2001_2006_TRATADO$IDADE_FINAL))

print(table(CHAGAS_AGUDA_2001_2006_TRATADO$FAIXA_ETARIA, useNA = "ifany"))

head(CHAGAS_AGUDA_2001_2006_TRATADO %>% select(IDADE_FINAL, FAIXA_ETARIA), 15)

tabela_nomes_local <- tibble(
  SIGLA = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", 
            "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", 
            "MG", "ES", "RJ", "SP", 
            "PR", "SC", "RS", 
            "MS", "MT", "GO", "DF"),
  
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

CHAGAS_AGUDA_1_2001_2006_TRATADO <- CHAGAS_AGUDA_2001_2006_TRATADO %>%
  mutate(
    
    CD_MN_RESI = str_remove(as.character(ID_MN_RESI), "\\.0$"),
    
    SG_UF_NOT_CLEAN = str_trim(toupper(SG_UF_NOT)),
    SG_UF_RES_CLEAN = str_trim(toupper(SG_UF))
  ) %>%
  
  left_join(tabela_nomes_local, by = c("SG_UF_NOT_CLEAN" = "SIGLA")) %>%
  rename(NOME_UF_NOT = NOME_ESTADO, REGIAO_NOTIFICACAO = REGIAO) %>%
  
  left_join(tabela_nomes_local, by = c("SG_UF_RES_CLEAN" = "SIGLA")) %>%
  rename(NOME_UF_RES = NOME_ESTADO, REGIAO_RESIDENCIA = REGIAO) %>%
  
  mutate(
    
    SG_UF_NOT = replace_na(NOME_UF_NOT, "Ignorado/Em branco"),
    SG_UF = replace_na(NOME_UF_RES, "Ignorado/Em branco"),
    
    REGIAO_NOTIFICACAO = replace_na(REGIAO_NOTIFICACAO, "Ignorado/Em branco"),
    REGIAO_RESIDENCIA = replace_na(REGIAO_RESIDENCIA, "Ignorado/Em branco")
  ) %>%
  
  select(-SG_UF_NOT_CLEAN, -SG_UF_RES_CLEAN, -NOME_UF_NOT, -NOME_UF_RES)

head(CHAGAS_AGUDA_1_2001_2006_TRATADO %>% 
       select(SG_UF_NOT, REGIAO_NOTIFICACAO, SG_UF, CD_MN_RESI, REGIAO_RESIDENCIA))

print(table(CHAGAS_AGUDA_1_2001_2006_TRATADO$REGIAO_RESIDENCIA, useNA = "ifany"))

CHAGAS_AGUDA_1_2001_2006_TRATADO <- CHAGAS_AGUDA_1_2001_2006_TRATADO %>%
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

print(table(CHAGAS_AGUDA_1_2001_2006_TRATADO$FLUXO_ASSISTENCIA, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_1_2001_2006_TRATADO$FLUXO_ASSISTENCIA)) * 100, 2))

CHAGAS_AGUDA_2_2001_2006_TRATADO <- CHAGAS_AGUDA_1_2001_2006_TRATADO %>%
  mutate(
    
    SEXO_FINAL = case_when(
      
      str_trim(toupper(as.character(CS_SEXO))) == "M" ~ "Masculino",
      
      str_trim(toupper(as.character(CS_SEXO))) == "F" ~ "Feminino",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    SEXO_FINAL = factor(SEXO_FINAL, levels = c("Masculino", "Feminino", "Ignorado/Em branco"))
  ) %>%  
  
  select(-CS_SEXO)

print(table(CHAGAS_AGUDA_2_2001_2006_TRATADO$SEXO_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_2_2001_2006_TRATADO$SEXO_FINAL)) * 100, 2))

CHAGAS_AGUDA_2_2001_2006_TRATADO <- CHAGAS_AGUDA_2_2001_2006_TRATADO %>%
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

print(table(CHAGAS_AGUDA_2_2001_2006_TRATADO$RACACOR_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_2_2001_2006_TRATADO$RACACOR_FINAL)) * 100, 2))

CHAGAS_AGUDA_2_2001_2006_TRATADO <- CHAGAS_AGUDA_2_2001_2006_TRATADO %>% 
  mutate(

    ESCOL_NUM = as.numeric(as.character(CS_ESCOL_N)),
    
    ESCOLARIDADE_FINAL = case_when(
      
      IDADE_FINAL < 5 | ESCOL_NUM == 6 ~ "Não se aplica",
      
      ESCOL_NUM == 1 ~ "Nenhuma",
      
      ESCOL_NUM %in% c(2, 3) ~ "1 a 7 anos",
      
      ESCOL_NUM == 4 ~ "8 a 11 anos",
      
      ESCOL_NUM == 5 ~ "12 anos ou mais",
      
      ESCOL_NUM == 9 | is.na(ESCOL_NUM) ~ "Ignorado/Em branco",
      
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

print(table(CHAGAS_AGUDA_2_2001_2006_TRATADO$ESCOLARIDADE_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_2_2001_2006_TRATADO$ESCOLARIDADE_FINAL)) * 100, 2))

CHAGAS_AGUDA_2_2001_2006_TRATADO <- CHAGAS_AGUDA_2_2001_2006_TRATADO %>% 
  mutate(

    OCUP_TEMP = as.character(as.numeric(ID_OCUPA_N)), 
    OCUP_TEMP = str_remove(OCUP_TEMP, "\\.0$"),
    OCUP_TEMP = str_replace(OCUP_TEMP, "NA", ""),
    
    OCUP_TEMP = if_else(OCUP_TEMP == "" | is.na(OCUP_TEMP), "999", str_pad(
      OCUP_TEMP, width = 3, pad = "0", side = "left")),
    
    PRIMEIRO_DIGITO = substr(OCUP_TEMP, 1, 1),
    
    GRANDE_GRUPO_OCUP = case_when(
      
      OCUP_TEMP %in% c("991", "992", "994", "995", "996", "997", "998") ~ "Inativos/Sem Ocupação",
      
      OCUP_TEMP == "993" ~ "Aposentado/Pensionista",
      
      OCUP_TEMP %in% c("999", "000") ~ "Ignorado/Em branco",
      
      PRIMEIRO_DIGITO == "0" ~ "Profissionais das Ciências e Artes",
      
      PRIMEIRO_DIGITO == "1" ~ "Membros Superiores/Dirigentes/Poder Público",
      
      PRIMEIRO_DIGITO == "2" ~ "Serviços Administrativos",
      
      PRIMEIRO_DIGITO %in% c("3", "4", "5") ~ "Trabalhadores de Serviços/Comércio",
      
      PRIMEIRO_DIGITO == "6" ~ "Trabalhadores Agropecuários/Florestais/Pesca",
      
      PRIMEIRO_DIGITO == "7" ~ "Trabalhadores da Produção Industrial",
      
      PRIMEIRO_DIGITO == "8" ~ "Operadores de Instalações/Máquinas",
      
      PRIMEIRO_DIGITO == "9" ~ "Trabalhadores de Reparação/Manutenção",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    PERFIL_OCUPACIONAL = case_when(
      GRANDE_GRUPO_OCUP == "Trabalhadores Agropecuários/Florestais/Pesca" ~ "Agropecuário/Rural",
      
      GRANDE_GRUPO_OCUP %in% c("Membros Superiores/Dirigentes/Poder Público", 
                               "Profissionais das Ciências e Artes") ~ "Não Manual/Intelectual/Técnico",
      
      GRANDE_GRUPO_OCUP %in% c("Serviços Administrativos", 
                               "Trabalhadores de Serviços/Comércio", 
                               "Trabalhadores da Produção Industrial", 
                               "Operadores de Instalações/Máquinas", 
                               "Trabalhadores de Reparação/Manutenção") ~ "Manual Urbano/Serviços/Industrial",
      
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

count(CHAGAS_AGUDA_2_2001_2006_TRATADO, GRANDE_GRUPO_OCUP) %>%
  mutate(Percentual = round(n/sum(n)*100, 2)) %>%
  print()

count(CHAGAS_AGUDA_2_2001_2006_TRATADO, PERFIL_OCUPACIONAL) %>%
  mutate(Percentual = round(n/sum(n)*100, 2)) %>%
  print()

CHAGAS_AGUDA_3_2001_2006_TRATADO <- CHAGAS_AGUDA_2_2001_2006_TRATADO %>% 
  mutate(
    
    PRESENCA_FINAL = case_when(
      as.character(PRESENCA) == "1" ~ "Sim",
      as.character(PRESENCA) == "2" ~ "Não",
      as.character(PRESENCA) == "9" ~ "Ignorado/Em branco",
      is.na(PRESENCA) ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    PRESENCA_FINAL = factor(PRESENCA_FINAL, 
                            levels = c("Sim", 
                                       "Não", 
                                       "Ignorado/Em branco"))
  )

print(table(CHAGAS_AGUDA_3_2001_2006_TRATADO$PRESENCA_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_3_2001_2006_TRATADO$PRESENCA_FINAL))*100, 2))

CHAGAS_AGUDA_3_2001_2006_TRATADO <- CHAGAS_AGUDA_3_2001_2006_TRATADO %>% 
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

print(table(CHAGAS_AGUDA_3_2001_2006_TRATADO$HISTORIA_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_3_2001_2006_TRATADO$HISTORIA_FINAL))*100, 2))

CHAGAS_AGUDA_3_2001_2006_TRATADO <- CHAGAS_AGUDA_3_2001_2006_TRATADO %>% 
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

print(table(CHAGAS_AGUDA_3_2001_2006_TRATADO$MANIPULA_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_3_2001_2006_TRATADO$MANIPULA_FINAL))*100, 2))

CHAGAS_AGUDA_3_2001_2006_TRATADO <- CHAGAS_AGUDA_3_2001_2006_TRATADO %>% 
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

print(table(CHAGAS_AGUDA_3_2001_2006_TRATADO$ASSINTOMA_FINAL, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_AGUDA_3_2001_2006_TRATADO$ASSINTOMA_FINAL))*100, 2))

limpar_sintoma_estrito <- function(coluna) {
  case_when(
    as.character(coluna) == "1" ~ 1,      
    as.character(coluna) == "2" ~ 0,      
    TRUE ~ NA_real_                        
  )
}

status_antigo <- function(coluna) {
  case_when(
    as.character(coluna) == "1" ~ "Positivo/Alterado",
    as.character(coluna) == "2" ~ "Negativo/Normal",
    as.character(coluna) == "3" ~ "Não Realizado",
    TRUE ~ "Ignorado/Em branco"
  )
}

is_positivo_antigo <- function(coluna) {
  if_else(as.character(coluna) == "1", 1, 0, missing = 0)
}

is_realizado_antigo <- function(col) {
  as.character(col) %in% c("1", "2")
}

CHAGAS_AGUDA_4_2001_2006_TRATADO <- CHAGAS_AGUDA_3_2001_2006_TRATADO %>% 
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
    
    TEM_CEFALEIA   = limpar_sintoma_estrito(CEFALEIA),
    TEM_ANOREXIA   = limpar_sintoma_estrito(ANOREXIA),
    TEM_HIPOREXIA  = limpar_sintoma_estrito(HIPOREXIA),
    TEM_HEPAESPLE  = limpar_sintoma_estrito(HEPAESPLE), 
    TEM_GANGLIOS   = limpar_sintoma_estrito(GANGLIOS),  
    TEM_AUMENTO    = limpar_sintoma_estrito(AUMENTO),   
    
    STATUS_EXAME_DIR = status_antigo(EXAME),
    STATUS_XENO      = status_antigo(XENODIAG),
    STATUS_HEMOCULT  = status_antigo(HEMOCULT),
    
    STATUS_ELISA     = status_antigo(ELISA),
    STATUS_IFI       = status_antigo(IMUNO),
    
    STATUS_ECG       = status_antigo(ECG),    
    STATUS_RAIOX     = status_antigo(RAIOX),  
    
    FEZ_DIRETO   = is_realizado_antigo(EXAME),
    FEZ_XENO     = is_realizado_antigo(XENODIAG),
    FEZ_HEMOCULT = is_realizado_antigo(HEMOCULT),
    
    FEZ_ELISA    = is_realizado_antigo(ELISA),
    FEZ_IFI      = is_realizado_antigo(IMUNO),
    
    FEZ_ECG      = is_realizado_antigo(ECG),
    FEZ_RAIOX    = is_realizado_antigo(RAIOX),
    
    IS_POS_DIRETO    = is_positivo_antigo(EXAME),
    IS_POS_XENO      = is_positivo_antigo(XENODIAG),
    IS_POS_HEMOCULT  = is_positivo_antigo(HEMOCULT),
    
    IS_POS_ELISA     = is_positivo_antigo(ELISA),
    IS_POS_IFI       = is_positivo_antigo(IMUNO),
    
    IS_ALT_ECG       = is_positivo_antigo(ECG),
    IS_ALT_RAIOX     = is_positivo_antigo(RAIOX)
    
  ) %>%
  mutate(
    
    QTD_CAMPOS_SINTOMAS_VALIDOS = rowSums(!is.na(select(., 
                                                        TEM_FEBRE, TEM_EDEMA, TEM_SINAIS_ICC, TEM_ARRITMIAS, 
                                                        TEM_ASTENIA, TEM_HEPATOME, TEM_ESPLENOM, TEM_CHAGOMA, 
                                                        TEM_MENINGOE, TEM_CEFALEIA, TEM_ANOREXIA, TEM_HIPOREXIA, 
                                                        TEM_HEPAESPLE, TEM_GANGLIOS, TEM_AUMENTO))),
    
    QTD_SINTOMAS = rowSums(select(., 
                                  TEM_FEBRE, TEM_EDEMA, TEM_SINAIS_ICC, TEM_ARRITMIAS, TEM_ASTENIA,
                                  TEM_HEPATOME, TEM_ESPLENOM, TEM_CHAGOMA, TEM_MENINGOE, TEM_CEFALEIA, 
                                  TEM_ANOREXIA, TEM_HIPOREXIA, TEM_HEPAESPLE, TEM_GANGLIOS, TEM_AUMENTO), 
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
      if_else(TEM_ASTENIA == 1, "Astenia ", "", missing=""),
      if_else(TEM_HEPATOME == 1, "Hepatomegalia ", "", missing=""),
      if_else(TEM_ESPLENOM == 1, "Esplenomegalia ", "", missing=""),
      if_else(TEM_CEFALEIA == 1, "Cefaleia ", "", missing=""),
      if_else(TEM_ANOREXIA == 1, "Anorexia ", "", missing=""),
      if_else(TEM_HIPOREXIA == 1, "Hiporexia ", "", missing=""),
      if_else(TEM_HEPAESPLE == 1, "Hepatoespleno ", "", missing=""),
      if_else(TEM_GANGLIOS == 1, "Adenomegalia ", "", missing=""),
      if_else(TEM_AUMENTO == 1, "Aumento-Vol ", "", missing="")
    ),
    
    RESUMO_SINTOMAS_PRINCIPAIS = str_trim(RESUMO_SINTOMAS_PRINCIPAIS),
    
    RESUMO_SINTOMAS_PRINCIPAIS = case_when(
      is.na(QTD_SINTOMAS) ~ "Ignorado/Em branco",
      RESUMO_SINTOMAS_PRINCIPAIS == "" & QTD_SINTOMAS == 0 ~ "Sem Sintomas Principais",
      TRUE ~ RESUMO_SINTOMAS_PRINCIPAIS
    ),
    
    QTD_SOROLOGIAS_REALIZADAS = FEZ_ELISA + FEZ_IFI,
    
    QTD_SOROLOGIAS_REAGENTES_TEMP = IS_POS_ELISA + IS_POS_IFI,
    
    QTD_SOROLOGIAS_REAGENTES = if_else(QTD_SOROLOGIAS_REALIZADAS == 0, NA_real_, 
                                       QTD_SOROLOGIAS_REAGENTES_TEMP),
    
    
    LISTA_SOROLOGIAS_REAGENTES = paste0(
      if_else(IS_POS_ELISA == 1, "ELISA ", "", missing=""),
      if_else(IS_POS_IFI == 1,   "IFI ",   "", missing="")
    ),
    
    LISTA_SOROLOGIAS_REAGENTES = str_trim(LISTA_SOROLOGIAS_REAGENTES),
    
    LISTA_SOROLOGIAS_REAGENTES = case_when(
      LISTA_SOROLOGIAS_REAGENTES != "" ~ LISTA_SOROLOGIAS_REAGENTES,
      
      QTD_SOROLOGIAS_REALIZADAS > 0 ~ "Não Reagente",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    QTD_PARASITO_REALIZADOS = FEZ_DIRETO + FEZ_XENO + FEZ_HEMOCULT,
    
    QTD_PARASITO_POSITIVOS = if_else(QTD_PARASITO_REALIZADOS == 0, NA_real_, 
                                     IS_POS_DIRETO + IS_POS_XENO + IS_POS_HEMOCULT),
    
    LISTA_PARASITO_POSITIVOS = paste0(
      if_else(IS_POS_DIRETO == 1,   "Direto/Fresco ", "", missing=""),
      if_else(IS_POS_XENO == 1,     "Xenodiag ",      "", missing=""),
      if_else(IS_POS_HEMOCULT == 1, "Hemocultura ",   "", missing="")
    ),
    
    LISTA_PARASITO_POSITIVOS = str_trim(LISTA_PARASITO_POSITIVOS),
    
    LISTA_PARASITO_POSITIVOS = case_when(
      LISTA_PARASITO_POSITIVOS != "" ~ LISTA_PARASITO_POSITIVOS,
      
      QTD_PARASITO_REALIZADOS > 0 ~ "Negativo",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    QTD_CARDIACO_REALIZADOS = FEZ_ECG + FEZ_RAIOX,
    
    QTD_CARDIACO_ALTERADOS = if_else(QTD_CARDIACO_REALIZADOS == 0, NA_real_, IS_ALT_ECG + IS_ALT_RAIOX),
    
    LISTA_CARDIACO_ALTERADOS = paste0(
      if_else(IS_ALT_ECG == 1,   "ECG ", "", missing=""),
      if_else(IS_ALT_RAIOX == 1, "RX ",  "", missing="")
    ),
    
    LISTA_CARDIACO_ALTERADOS = str_trim(LISTA_CARDIACO_ALTERADOS),
    
    LISTA_CARDIACO_ALTERADOS = case_when(
      LISTA_CARDIACO_ALTERADOS != "" ~ LISTA_CARDIACO_ALTERADOS,
      
      QTD_CARDIACO_REALIZADOS > 0 ~ "Normal",
      
      TRUE ~ "Ignorado/Em branco"
  ))

tab_soro <- table(CHAGAS_AGUDA_4_2001_2006_TRATADO$LISTA_SOROLOGIAS_REAGENTES)

tab_soro_prop <- round(prop.table(tab_soro) * 100, 2)

df_conf_soro <- data.frame(
  Combinacao = names(tab_soro),
  Frequencia = as.numeric(tab_soro),
  Percentual = as.numeric(tab_soro_prop)
) %>%
  arrange(desc(Frequencia))

print(head(df_conf_soro, 20))

print(table(CHAGAS_AGUDA_4_2001_2006_TRATADO$QTD_SOROLOGIAS_REAGENTES, useNA = "always"))

tab_par <- table(CHAGAS_AGUDA_4_2001_2006_TRATADO$LISTA_PARASITO_POSITIVOS)

tab_par_prop <- round(prop.table(tab_par) * 100, 2)

df_conf_par <- data.frame(
  Combinacao = names(tab_par),
  Frequencia = as.numeric(tab_par),
  Percentual = as.numeric(tab_par_prop)
) %>%
  arrange(desc(Frequencia))

print(head(df_conf_par, 20))

tab_cardio <- table(CHAGAS_AGUDA_4_2001_2006_TRATADO$LISTA_CARDIACO_ALTERADOS)

tab_cardio_prop <- round(prop.table(tab_cardio) * 100, 2)

df_conf_cardio <- data.frame(
  Combinacao = names(tab_cardio),
  Frequencia = as.numeric(tab_cardio),
  Percentual = as.numeric(tab_cardio_prop)
) %>%
  arrange(desc(Frequencia))

print(head(df_conf_cardio, 20))

print(table(CHAGAS_AGUDA_4_2001_2006_TRATADO$QTD_CARDIACO_ALTERADOS, useNA = "always"))

tab_sint <- table(CHAGAS_AGUDA_4_2001_2006_TRATADO$RESUMO_SINTOMAS_PRINCIPAIS)

tab_sint_prop <- round(prop.table(tab_sint) * 100, 2)

df_conf_sint <- data.frame(
  Sintomas = names(tab_sint),
  Frequencia = as.numeric(tab_sint),
  Percentual = as.numeric(tab_sint_prop)
) %>%
  arrange(desc(Frequencia))

print(head(df_conf_sint, 20))

print(table(CHAGAS_AGUDA_4_2001_2006_TRATADO$PERFIL_SINTOMAS, useNA = "always"))

CHAGAS_AGUDA_4_2001_2006_TRATADO <- CHAGAS_AGUDA_4_2001_2006_TRATADO %>%
  
  select(
    -c(FEBRE, EDEMA, SINAIS_ICC, ARRITMIAS, ASTENIA, 
       HEPATOME, ESPLENOM, CHAGOMA, MENINGOE),
    
    -c(CEFALEIA, ANOREXIA, HIPOREXIA, HEPAESPLE, GANGLIOS, AUMENTO),
    
    -c(PRESENCA, HISTORIA, MANIPULA, ASSINTOMA),
    
    -c(EXAME, XENODIAG, HEMOCULT, ELISA, IMUNO),
    
    -c(ECG, RAIOX),
    
    -c(QTD_CAMPOS_SINTOMAS_VALIDOS, QTD_SOROLOGIAS_REAGENTES_TEMP)
  )

print(head(names(CHAGAS_AGUDA_4_2001_2006_TRATADO), 30))

CHAGAS_AGUDA_5_2001_2006_TRATADO <- CHAGAS_AGUDA_4_2001_2006_TRATADO %>%
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

print(table(CHAGAS_AGUDA_5_2001_2006_TRATADO$ESPECIFICO_FINAL, useNA = "always"))

print(table(CHAGAS_AGUDA_5_2001_2006_TRATADO$SINTOMATICO_FINAL, useNA = "always"))

CHAGAS_AGUDA_5_2001_2006_TRATADO <- CHAGAS_AGUDA_5_2001_2006_TRATADO %>%
  select(
    
    -c(ESPECIFICO, SINTOMATIC)
    
  )

CHAGAS_AGUDA_5_2001_2006_TRATADO <- CHAGAS_AGUDA_5_2001_2006_TRATADO %>%
  mutate(
    
    DROGA_TEMP = str_to_upper(as.character(DROGA)),
    
    DROGA_FINAL = case_when(
      
      RECEBEU_ESPECIFICO == 0 ~ "Não se aplica",
      
      is.na(RECEBEU_ESPECIFICO) ~ "Ignorado/Em branco",
      
      str_detect(DROGA_TEMP, "BENZ|ROCH|ROX|RUCH|RADANIL|LOCH|BEUZ") ~ "Benznidazol",
      
      str_detect(DROGA_TEMP, "NIFU|NIFE|LAMPIT") ~ "Outra",
      
      !is.na(DROGA_TEMP) & DROGA_TEMP != "" & 
        !DROGA_TEMP %in% c("2", "9", "IGNORADO", "NAO", "N+O") ~ "Outra",
            
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%
  
  select(-c(DROGA, DROGA_TEMP))

print(table(CHAGAS_AGUDA_5_2001_2006_TRATADO$DROGA_FINAL, useNA = "always"))

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

CHAGAS_AGUDA_5_2001_2006_TRATADO <- CHAGAS_AGUDA_5_2001_2006_TRATADO  %>%
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
      if_else(TEM_MED_OUTRO == 1, "Outra(Não Espec.) ", "", missing="")
      ),
    
    LISTA_MEDIDAS_TOMADAS = str_trim(LISTA_MEDIDAS_TOMADAS),
    
    LISTA_MEDIDAS_TOMADAS = case_when(
      LISTA_MEDIDAS_TOMADAS != "" ~ LISTA_MEDIDAS_TOMADAS,
      
      TUDO_NAO_SE_APLICA ~ "Não se aplica",
      
      TUDO_PREENCHIDO ~ "Nenhuma Medida Adotada",
      
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%

    select(-c(CON_TRIAT, BIOSSEG, FISCALIZA, MED_OUTRO))

print(table(CHAGAS_AGUDA_5_2001_2006_TRATADO$HOUVE_INTERVENCAO_GERAL, useNA = "always"))

print(head(sort(table(CHAGAS_AGUDA_5_2001_2006_TRATADO$LISTA_MEDIDAS_TOMADAS), decreasing = TRUE), 15))

CHAGAS_AGUDA_6_2001_2006_TRATADO <- CHAGAS_AGUDA_5_2001_2006_TRATADO %>%
  select(
    -c(SUSPEITOS, CONFIRMAD)
  )

print(names(CHAGAS_AGUDA_6_2001_2006_TRATADO))

CHAGAS_AGUDA_6_2001_2006_TRATADO <- CHAGAS_AGUDA_6_2001_2006_TRATADO %>%
  mutate(
    
    CLASSIFICACAO_FINAL = case_when(
      as.character(CLASSI_FIN) == "1" ~ "Confirmado",
      as.character(CLASSI_FIN) == "2" ~ "Descartado",
      
      as.character(CLASSI_FIN) == "3" ~ "Indeterminado",
      as.character(CLASSI_FIN) == "8" ~ "Transferência/Não Encerrado",
      
      as.character(CLASSI_FIN) == "0" ~ "Transferência/Não Encerrado",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    IS_CONFIRMADO = if_else(as.character(CLASSI_FIN) == "1", 1, 0, missing = 0),
    
    CRITERIO_FINAL = case_when(
      IS_CONFIRMADO == 0 ~ "Não se aplica",
      
      as.character(CRITERIO) == "1" ~ "Laboratorial",
      as.character(CRITERIO) == "2" ~ "Clínico-Epidemiológico",
      as.character(CRITERIO) == "3" ~ "Clínico",
      
      as.character(CRITERIO) %in% c("4", "5") ~ "Outros Critérios",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    EVOLUCAO_FINAL = case_when(
      as.character(CON_EVOLUC) == "1" ~ "Cura",
      as.character(CON_EVOLUC) == "2" ~ "Óbito por Chagas",
      as.character(CON_EVOLUC) == "3" ~ "Óbito por outras causas",
            
      as.character(CON_EVOLUC) == "4" ~ "Óbito em investigação",
      
      as.character(CON_EVOLUC) %in% c("0", "9") ~ "Ignorado/Em branco",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    IS_OBITO_CHAGAS = if_else(as.character(CON_EVOLUC) == "2", 1, 0, missing = 0)
  ) %>%
  
  select(-c(CLASSI_FIN, CRITERIO, CON_EVOLUC))

print(table(CHAGAS_AGUDA_6_2001_2006_TRATADO$CLASSIFICACAO_FINAL, useNA = "always"))

print(table(CHAGAS_AGUDA_6_2001_2006_TRATADO$EVOLUCAO_FINAL, useNA = "always"))

CHAGAS_AGUDA_FINAL_2001_2006_TRATADO <- CHAGAS_AGUDA_6_2001_2006_TRATADO %>%
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

print(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$USOU_MEDICAMENTO, useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$FAIXA_ETARIA))*100, 2))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$ESCOLARIDADE_FINAL))*100, 2))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$SINTOMATICO_FINAL))*100, 2))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$CLASSIFICACAO_FINAL))*100, 2))

print(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$CLASSIFICACAO_FINAL, useNA = "always"))

print(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$CRITERIO_FINAL[
  CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$CRITERIO_FINAL[
  CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$IS_CONFIRMADO == 1]))*100, 2))

print(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$FAIXA_ETARIA[
  CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$REGIAO_RESIDENCIA[
  CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$REGIAO_RESIDENCIA[
  CHAGAS_AGUDA_FINAL_2001_2006_TRATADO$IS_CONFIRMADO == 1]))*100, 2))

CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO <- CHAGAS_AGUDA_FINAL_2001_2006_TRATADO %>%
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

print(table(CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO$LOCAL_INFECCAO_FINAL[
  CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO$LOCAL_INFECCAO_FINAL[
  CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO$IS_CONFIRMADO == 1]))*100, 2))

print(table(CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO$MODO_INFECCAO_FINAL[
  CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO$IS_CONFIRMADO == 1], useNA = "always"))

print(round(prop.table(table(CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO$MODO_INFECCAO_FINAL[
  CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO$IS_CONFIRMADO == 1]))*100, 2))

# --- Exportação feita no console na execução original ---
saveRDS(CHAGAS_AGUDA_FINAL_2_2001_2006_TRATADO, "CHAGAS_AGUDA_FINAL_4_2001_2006_TRATADO.rds")
