# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SIM/DOBR - CHAGAS/DOBR - CHAGAS - PARTE 4.1.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SIM/DOBR - CHAGAS"))

# Base de entrada (na execução original, importada pelo menu do RStudio):
if (!exists("MORT_CHAGAS_PRE_PROCESSAMENTO_2")) MORT_CHAGAS_PRE_PROCESSAMENTO_2 <- haven::read_dta(file.path(DIR_DADOS, "SIM/MORT_CHAGAS_PRE_PROCESSAMENTO_2.dta"))

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
library(sf)
library(geobr)
library(spdep)

names(MORT_CHAGAS_PRE_PROCESSAMENTO_2) <- toupper(names(MORT_CHAGAS_PRE_PROCESSAMENTO_2))

vars_para_manter <- c("DTOBITO", "DTNASC", "IDADE", "SEXO", "RACACOR", "ESTCIV", "NATURAL", "CODMUNNATU",
                       "ESC",  "OCUP", "CODMUNRES", "CODMUNOCOR", "ASSISTMED", "EXAME", "CIRURGIA", "LOCOCOR", 
                       "NECROPSIA", "LINHAA", "LINHAB", "LINHAC", "LINHAD", "LINHAII", "CAUSABAS", "ATESTADO", 
                       "TPPOS", "CHAGAS_MENCAO", "TIPO_MENCAO", "FONTEINV")

MORT_CHAGAS_LIMPO <- MORT_CHAGAS_PRE_PROCESSAMENTO_2 %>% select(any_of(vars_para_manter))

vars_auditoria <- c("DTOBITO", "DTNASC", "IDADE", "SEXO", "RACACOR", "ESTCIV", "NATURAL", "CODMUNNATU",
                    "ESC",  "OCUP", "CODMUNRES", "CODMUNOCOR", "ASSISTMED", "EXAME", "CIRURGIA", "LOCOCOR",
                    "NECROPSIA", "LINHAA", "LINHAB", "LINHAC", "LINHAD", "LINHAII", "CAUSABAS", 
                    "ATESTADO", "TPPOS", "FONTEINV")

TABELA_QUALIDADE <- MORT_CHAGAS_LIMPO %>%
  summarise(across(all_of(vars_auditoria), list(
    
    PCT_EM_BRANCO = ~ mean(is.na(.) | as.character(.) == "") * 100,
    
    PCT_IGNORADO_CODIGO = ~ mean(as.character(.) %in% c("9", "99", "0", "Ignorado"), na.rm = TRUE) * 100
  ))) %>%
  pivot_longer(everything(), names_to = "Indicador", values_to = "Valor") %>%
  separate(Indicador, into = c("Variavel", "Tipo_Erro", "Metrica"), sep = "_", extra = "merge")

print(TABELA_QUALIDADE)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_LIMPO %>%
  mutate(
    DATA_OBITO = dmy(DTOBITO), 
    
    DATA_NASC  = dmy(DTNASC),
    
    idade_fracionada = time_length(interval(DATA_NASC, DATA_OBITO), "years"),
    
    IDADE_OBITO = floor(idade_fracionada)
    
  ) 

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    ANO_OBITO = year(DATA_OBITO)
    
  ) 

MORT_CHAGAS_TRATADO %>%
  select(DTOBITO, DTNASC, DATA_OBITO, DATA_NASC, IDADE_OBITO, ANO_OBITO) %>%
  head()

erros_conversao <- MORT_CHAGAS_TRATADO %>%
  filter(is.na(DATA_OBITO) & !is.na(DTOBITO)) %>%
  select(DTOBITO, DATA_OBITO)

count(erros_conversao)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  select(-IDADE, -idade_fracionada, -DTOBITO, -DTNASC)

glimpse(MORT_CHAGAS_TRATADO)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    SEXO_TEMP = str_trim(as.character(SEXO)),
    
    SEXO = case_when(
      SEXO_TEMP == "1" ~ "Masculino",
      SEXO_TEMP == "2" ~ "Feminino",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    SEXO = factor(SEXO, levels = c("Masculino", "Feminino", "Ignorado/Em branco"))
  )
    
MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    RACA_TEMP = as.numeric(RACACOR),
    RACA_TEMP = replace_na(RACA_TEMP, 9),
    RACACOR = factor(RACA_TEMP,
                     levels = c(1, 2, 3, 4, 5, 9),
                     labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorado/Em branco"))
  ) %>%
  select(-SEXO_TEMP, -RACA_TEMP)

table(MORT_CHAGAS_TRATADO$SEXO, useNA = "always")
table(MORT_CHAGAS_TRATADO$RACACOR, useNA = "always")

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    ESTCIV_TEMP = as.numeric(ESTCIV),
    ESTCIV_TEMP = replace_na(ESTCIV_TEMP, 9),
    ESTCIV_TEMP = if_else(ESTCIV_TEMP == 0, 9, ESTCIV_TEMP),
    ESTADO_CIVIL = factor(ESTCIV_TEMP,
                          levels = c(1, 2, 3, 4, 5, 9),
                          labels = c("Solteiro", 
                                     "Casado", 
                                     "Viúvo", 
                                     "Separado/Divorciado", 
                                     "União Estável", 
                                     "Ignorado/Em branco"))
  ) %>%
  select(-ESTCIV_TEMP)

table(MORT_CHAGAS_TRATADO$ESTADO_CIVIL, useNA = "always")

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    FAIXA_ETARIA = cut(IDADE_OBITO, 
                       breaks = c(0, 46, 60, 75, Inf),
                       labels = c("0 a 45 anos", 
                                  "46 a 59 anos", 
                                  "60 a 74 anos", 
                                  "75 anos ou mais"),
                       right = FALSE)
  )

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    FAIXA_ETARIA = as.character(FAIXA_ETARIA),
    FAIXA_ETARIA = replace_na(FAIXA_ETARIA, "Ignorado/Em branco"),
    FAIXA_ETARIA = factor(FAIXA_ETARIA, 
                          levels = c("0 a 45 anos", 
                                     "46 a 59 anos", 
                                     "60 a 74 anos", 
                                     "75 anos ou mais", 
                                     "Ignorado/Em branco"))
  )

table(MORT_CHAGAS_TRATADO$FAIXA_ETARIA, useNA = "always")

round(prop.table(table(MORT_CHAGAS_TRATADO$SEXO)) * 100, 2)
round(prop.table(table(MORT_CHAGAS_TRATADO$FAIXA_ETARIA)) * 100, 2)
round(prop.table(table(MORT_CHAGAS_TRATADO$RACACOR)) * 100, 2)

summary(MORT_CHAGAS_TRATADO$IDADE_OBITO)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  select(-ESTCIV)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    CODMUNRES = substr(as.character(CODMUNRES), 1, 6),
    
    CODMUNOCOR = substr(as.character(CODMUNOCOR), 1, 6)

    )

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    COD_UF_TEMP = case_when(
      !is.na(CODMUNNATU) & 
        nchar(as.character(CODMUNNATU)) >= 2 & 
        !substr(as.character(CODMUNNATU), 1, 2) %in% c("00", "99") ~ substr(as.character(CODMUNNATU), 1, 2),
      
      !is.na(NATURAL) & 
        substr(as.character(NATURAL), 1, 1) == "8" & 
        nchar(as.character(NATURAL)) >= 3 ~ substr(as.character(NATURAL), 2, 3),
      
      !is.na(NATURAL) & 
        nchar(as.character(NATURAL)) >= 2 &
        !substr(as.character(NATURAL), 1, 2) %in% c("00", "80", "99") ~ substr(as.character(NATURAL), 1, 2),
      
      TRUE ~ NA_character_
    ),
    
    UF_NASCIMENTO = case_when(
      COD_UF_TEMP == "11" ~ "Rondônia",
      COD_UF_TEMP == "12" ~ "Acre",
      COD_UF_TEMP == "13" ~ "Amazonas",
      COD_UF_TEMP == "14" ~ "Roraima",
      COD_UF_TEMP == "15" ~ "Pará",
      COD_UF_TEMP == "16" ~ "Amapá",
      COD_UF_TEMP == "17" ~ "Tocantins",
      COD_UF_TEMP == "21" ~ "Maranhão",
      COD_UF_TEMP == "22" ~ "Piauí",
      COD_UF_TEMP == "23" ~ "Ceará",
      COD_UF_TEMP == "24" ~ "Rio Grande do Norte",
      COD_UF_TEMP == "25" ~ "Paraíba",
      COD_UF_TEMP == "26" ~ "Pernambuco",
      COD_UF_TEMP == "27" ~ "Alagoas",
      COD_UF_TEMP == "28" ~ "Sergipe",
      COD_UF_TEMP == "29" ~ "Bahia",
      COD_UF_TEMP == "31" ~ "Minas Gerais",
      COD_UF_TEMP == "32" ~ "Espírito Santo",
      COD_UF_TEMP == "33" ~ "Rio de Janeiro",
      COD_UF_TEMP == "35" ~ "São Paulo",
      COD_UF_TEMP == "41" ~ "Paraná",
      COD_UF_TEMP == "42" ~ "Santa Catarina",
      COD_UF_TEMP == "43" ~ "Rio Grande do Sul",
      COD_UF_TEMP == "50" ~ "Mato Grosso do Sul",
      COD_UF_TEMP == "51" ~ "Mato Grosso",
      COD_UF_TEMP == "52" ~ "Goiás",
      COD_UF_TEMP == "53" ~ "Distrito Federal",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    REGIAO_NASCIMENTO = case_when(
      substr(COD_UF_TEMP, 1, 1) == "1" ~ "Norte",
      substr(COD_UF_TEMP, 1, 1) == "2" ~ "Nordeste",
      substr(COD_UF_TEMP, 1, 1) == "3" ~ "Sudeste",
      substr(COD_UF_TEMP, 1, 1) == "4" ~ "Sul",
      substr(COD_UF_TEMP, 1, 1) == "5" ~ "Centro-Oeste",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    REGIAO_NASCIMENTO = factor(REGIAO_NASCIMENTO, 
                               levels = c("Sudeste", "Centro-Oeste", "Nordeste",  "Sul", "Norte", "Ignorado/Em branco"))
  ) %>%
  
  select(-COD_UF_TEMP)

table(MORT_CHAGAS_TRATADO$REGIAO_NASCIMENTO, useNA = "always")
head(sort(table(MORT_CHAGAS_TRATADO$UF_NASCIMENTO), decreasing = TRUE), 10)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    COD_UF_RESID = substr(as.character(CODMUNRES), 1, 2),
    
    UF_RESIDENCIA = case_when(
      COD_UF_RESID == "11" ~ "Rondônia",
      COD_UF_RESID == "12" ~ "Acre",
      COD_UF_RESID == "13" ~ "Amazonas",
      COD_UF_RESID == "14" ~ "Roraima",
      COD_UF_RESID == "15" ~ "Pará",
      COD_UF_RESID == "16" ~ "Amapá",
      COD_UF_RESID == "17" ~ "Tocantins",
      COD_UF_RESID == "21" ~ "Maranhão",
      COD_UF_RESID == "22" ~ "Piauí",
      COD_UF_RESID == "23" ~ "Ceará",
      COD_UF_RESID == "24" ~ "Rio Grande do Norte",
      COD_UF_RESID == "25" ~ "Paraíba",
      COD_UF_RESID == "26" ~ "Pernambuco",
      COD_UF_RESID == "27" ~ "Alagoas",
      COD_UF_RESID == "28" ~ "Sergipe",
      COD_UF_RESID == "29" ~ "Bahia",
      COD_UF_RESID == "31" ~ "Minas Gerais",
      COD_UF_RESID == "32" ~ "Espírito Santo",
      COD_UF_RESID == "33" ~ "Rio de Janeiro",
      COD_UF_RESID == "35" ~ "São Paulo",
      COD_UF_RESID == "41" ~ "Paraná",
      COD_UF_RESID == "42" ~ "Santa Catarina",
      COD_UF_RESID == "43" ~ "Rio Grande do Sul",
      COD_UF_RESID == "50" ~ "Mato Grosso do Sul",
      COD_UF_RESID == "51" ~ "Mato Grosso",
      COD_UF_RESID == "52" ~ "Goiás",
      COD_UF_RESID == "53" ~ "Distrito Federal",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    REGIAO_RESIDENCIA = case_when(
      substr(COD_UF_RESID, 1, 1) == "1" ~ "Norte",
      substr(COD_UF_RESID, 1, 1) == "2" ~ "Nordeste",
      substr(COD_UF_RESID, 1, 1) == "3" ~ "Sudeste",
      substr(COD_UF_RESID, 1, 1) == "4" ~ "Sul",
      substr(COD_UF_RESID, 1, 1) == "5" ~ "Centro-Oeste",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    COD_UF_NASC_TEMP = case_when(
      !is.na(CODMUNNATU) & 
        nchar(as.character(CODMUNNATU)) >= 2 & 
        !substr(as.character(CODMUNNATU), 1, 2) %in% c("00", "99") ~ substr(as.character(CODMUNNATU), 1, 2),
      
      !is.na(NATURAL) & 
        substr(as.character(NATURAL), 1, 1) == "8" & 
        nchar(as.character(NATURAL)) >= 3 ~ substr(as.character(NATURAL), 2, 3),
      
      !is.na(NATURAL) & 
        nchar(as.character(NATURAL)) >= 2 &
        !substr(as.character(NATURAL), 1, 2) %in% c("00", "80", "99") ~ substr(as.character(NATURAL), 1, 2),
      
      TRUE ~ NA_character_
      ),
    
    MIGRANTE = case_when(
      is.na(COD_UF_NASC_TEMP) | is.na(COD_UF_RESID) ~ "Ignorado/Em branco",
      COD_UF_NASC_TEMP != COD_UF_RESID ~ "Sim",
      COD_UF_NASC_TEMP == COD_UF_RESID ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%
  select(-COD_UF_RESID, -COD_UF_NASC_TEMP)

table(MORT_CHAGAS_TRATADO$REGIAO_RESIDENCIA)

table(MORT_CHAGAS_TRATADO$REGIAO_NASCIMENTO)

table(MORT_CHAGAS_TRATADO$MIGRANTE)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  select(-NATURAL, -CODMUNNATU)

tabela_verificacao <- table(MORT_CHAGAS_TRATADO$CHAGAS_MENCAO, useNA = "always")
print(tabela_verificacao)
print(round(prop.table(tabela_verificacao) * 100, 2))

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>% select(-CHAGAS_MENCAO)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    TIPO_MENCAO = zap_labels(TIPO_MENCAO),
    
    TIPO_MENCAO = factor(TIPO_MENCAO,
                         levels = c(1, 2),
                         labels = c("Causa Básica", "Causa Associada"))
  )

table(MORT_CHAGAS_TRATADO$TIPO_MENCAO)

round(prop.table(table(MORT_CHAGAS_TRATADO$TIPO_MENCAO)) * 100, 2)

MORT_CHAGAS_ASSOCIADA <- MORT_CHAGAS_TRATADO %>%
  filter(TIPO_MENCAO == "Causa Associada")

RANKING_CAUSABAS_ASSOCIADA <- MORT_CHAGAS_ASSOCIADA %>%
  mutate(CAUSABAS_LIMPA = str_remove_all(str_to_upper(CAUSABAS), "[^A-Z0-9]")) %>%
  
  count(CAUSABAS_LIMPA, sort = TRUE) %>%
  
  mutate(
    PERCENTUAL = round((n / sum(n)) * 100, 2)) %>%
  head(20)

print(RANKING_CAUSABAS_ASSOCIADA)

MORT_CHAGAS_ASSOCIADA <- MORT_CHAGAS_ASSOCIADA %>%
  mutate(
      GRUPO_CAUSABAS = case_when(
        str_detect(CAUSABAS, "^I2[0-5]") ~ "Doenças Isquêmicas do Coração (I20-I25)",
        
        str_detect(CAUSABAS, "^I6") ~ "Doenças Cerebrovasculares (I60-I69)",

        str_detect(CAUSABAS, "^J09|^J1[0-8]|^J2[0-2]|^B342|^U07") ~ "Infecções Respiratórias Agudas e COVID-19 (J09-J18, U07)",
        
        str_detect(CAUSABAS, "^C|^D[0-4]") ~ "Neoplasias (C00-D48)",
        
        str_detect(CAUSABAS, "^I1[0-5]") ~ "Doenças Hipertensivas (I10-I15)",
        
        TRUE ~ "Outras Causas"
    )
  )

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    DIAGNOSTICO_COMPLETO = paste(
      as.character(CAUSABAS),
      as.character(LINHAA),
      as.character(LINHAB),
      as.character(LINHAC),
      as.character(LINHAD),
      as.character(LINHAII),
      as.character(ATESTADO),
      sep = " "
    ),
    
    DIAGNOSTICO_COMPLETO = str_to_upper(DIAGNOSTICO_COMPLETO),
   
    DIAGNOSTICO_LIMPO = str_remove_all(DIAGNOSTICO_COMPLETO, "[^A-Z0-9]"),
    
    FASE_DOENCA = case_when(
      str_detect(DIAGNOSTICO_LIMPO, "B570") ~ "Aguda", 
      str_detect(DIAGNOSTICO_LIMPO, "B571") ~ "Aguda",
      TRUE ~ "Crônica"
    ),
    
    FORMA_CLINICA = case_when(
      (str_detect(DIAGNOSTICO_LIMPO, "B570|B572|I412|I981") & 
         str_detect(DIAGNOSTICO_LIMPO, "B573|K231|K931")) | 
        str_detect(DIAGNOSTICO_LIMPO, "B575") ~ "Mista (Cardíaca + Digestiva)",
      
      str_detect(DIAGNOSTICO_LIMPO, "B573|K231|K931") ~ "Digestiva",
      
      str_detect(DIAGNOSTICO_LIMPO, "B574") ~ "Nervosa",
      
      str_detect(DIAGNOSTICO_LIMPO, "B570|B572|I412|I981") ~ "Cardíaca",
      
      str_detect(DIAGNOSTICO_LIMPO, "B571") ~ "Não Especificada/Indeterminada",
      
      TRUE ~ "Não Especificada/Indeterminada"
    )
  ) %>%
  
  select(-DIAGNOSTICO_COMPLETO, -DIAGNOSTICO_LIMPO) %>%
  
  mutate(
    FASE_DOENCA = factor(FASE_DOENCA, levels = c("Aguda", "Crônica")),
    
    FORMA_CLINICA = factor(FORMA_CLINICA, 
                           levels = c("Cardíaca", 
                                      "Digestiva", 
                                      "Mista (Cardíaca + Digestiva)",
                                      "Nervosa",
                                      "Não Especificada/Indeterminada"))
  )

table(MORT_CHAGAS_TRATADO$FORMA_CLINICA, useNA = "always")

table(MORT_CHAGAS_TRATADO$TIPO_MENCAO, MORT_CHAGAS_TRATADO$FORMA_CLINICA)

table(MORT_CHAGAS_TRATADO$FASE_DOENCA, useNA = "always")

round(prop.table(table(MORT_CHAGAS_TRATADO$FASE_DOENCA)) * 100, 3)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    DIAG_TEMP = paste(as.character(CAUSABAS), as.character(LINHAA), as.character(LINHAB), 
                      as.character(LINHAC), as.character(LINHAD), as.character(LINHAII), 
                      sep = " "),
    DIAG_TEMP = str_to_upper(DIAG_TEMP),
    DIAG_TEMP = str_remove_all(DIAG_TEMP, "[^A-Z0-9]"), 

    TEM_HIPERTENSAO = if_else(str_detect(DIAG_TEMP, "I10|I11|I12|I13|I14|I15"), 1, 0),
    TEM_DIABETES    = if_else(str_detect(DIAG_TEMP, "E10|E11|E12|E13|E14"), 1, 0),
    TEM_ISQUEMICA   = if_else(str_detect(DIAG_TEMP, "I20|I21|I22|I23|I24|I25"), 1, 0),
    TEM_AVC         = if_else(str_detect(DIAG_TEMP, "I60|I61|I62|I63|I64|I69"), 1, 0),
    TEM_NEOPLASIA   = if_else(str_detect(DIAG_TEMP, "C[0-9]"), 1, 0),
    TEM_RENAL       = if_else(str_detect(DIAG_TEMP, "N17|N18|N19"), 1, 0),

    QTD_COMORBIDADES = TEM_HIPERTENSAO + TEM_DIABETES + TEM_ISQUEMICA + TEM_AVC + TEM_NEOPLASIA + TEM_RENAL,
    
    PERFIL_COMORBIDADE = case_when(
      QTD_COMORBIDADES == 0 ~ "Nenhuma Identificada",
      QTD_COMORBIDADES == 1 ~ "Uma Comorbidade",
      QTD_COMORBIDADES >= 2 ~ "Multimorbidade (2 ou mais)",
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%
  
  mutate(
    RESUMO_CLINICO = paste0(
      if_else(TEM_HIPERTENSAO == 1, "HAS ", ""),
      if_else(TEM_DIABETES == 1, "DM ", ""),
      if_else(TEM_ISQUEMICA == 1, "DAC ", ""),
      if_else(TEM_AVC == 1, "AVC ", ""),
      if_else(TEM_NEOPLASIA == 1, "Câncer ", ""),
      if_else(TEM_RENAL == 1, "DRC ", "")
    ),
    RESUMO_CLINICO = str_trim(RESUMO_CLINICO), 
    RESUMO_CLINICO = if_else(RESUMO_CLINICO == "", "Sem Comorbidades Principais", RESUMO_CLINICO)
  )

table(MORT_CHAGAS_TRATADO$PERFIL_COMORBIDADE)
round(prop.table(table(MORT_CHAGAS_TRATADO$PERFIL_COMORBIDADE))*100, 2)

head(sort(table(MORT_CHAGAS_TRATADO$RESUMO_CLINICO), decreasing = TRUE), 10)

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    DIAG_VARREDURA = paste(
      as.character(CAUSABAS), 
      if("LINHAA" %in% names(.)) as.character(LINHAA) else "",
      if("LINHAB" %in% names(.)) as.character(LINHAB) else "",
      if("LINHAC" %in% names(.)) as.character(LINHAC) else "",
      if("LINHAD" %in% names(.)) as.character(LINHAD) else "",
      if("LINHAII" %in% names(.)) as.character(LINHAII) else "",
      sep = " "
    ),
    
    DIAG_VARREDURA = str_remove_all(str_to_upper(DIAG_VARREDURA), "[^A-Z0-9]"),
    
    CID_CHAGAS_ENCONTRADO = case_when(
      (str_detect(DIAG_VARREDURA, "B570|B572|I412|I981") & 
         str_detect(DIAG_VARREDURA, "B573|K231|K931")) | 
        str_detect(DIAG_VARREDURA, "B575") ~ "Forma Mista (Cardíaca + Digestiva)",
      
      str_detect(DIAG_VARREDURA, "K231") ~ "K23.1 - Megaesôfago",
      str_detect(DIAG_VARREDURA, "K931") ~ "K93.1 - Megacólon",
      str_detect(DIAG_VARREDURA, "B573") ~ "B57.3 - Forma Digestiva",
      
      str_detect(DIAG_VARREDURA, "B570") ~ "B57.0 - Aguda Cardíaca",
      str_detect(DIAG_VARREDURA, "I412") ~ "I41.2 - Miocardite Chagásica",
      str_detect(DIAG_VARREDURA, "I981") ~ "I98.1 - Transtornos Cardiovasc.",
      str_detect(DIAG_VARREDURA, "B572") ~ "B57.2 - Crônica Cardíaca",
      
      str_detect(DIAG_VARREDURA, "B574") ~ "B57.4 - Forma Nervosa",
      
      str_detect(DIAG_VARREDURA, "B571") ~ "B57.1 - Aguda s/ comprometimento",
      str_detect(DIAG_VARREDURA, "B57")  ~ "B57 - Chagas não especificado",
      
      TRUE ~ "Sem menção específica de CID Chagas"
    )
  ) %>%
  select(-DIAG_VARREDURA)

count(MORT_CHAGAS_TRATADO, CID_CHAGAS_ENCONTRADO, sort = TRUE) %>%
  mutate(Percentual = round(n / sum(n) * 100, 2))

limpar_texto_uf <- function(texto) {
  texto <- str_to_upper(as.character(texto)) 
  texto <- iconv(texto, to = "ASCII//TRANSLIT")
  texto <- str_replace_all(texto, "[^A-Z]", "") 
  return(texto)
}

MORT_CHAGAS_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    COD_UF_RESID_NUM = substr(as.character(CODMUNRES), 1, 2),
    COD_REGIAO_RESID_NUM = substr(as.character(CODMUNRES), 1, 1),
    
    UF_NASC_TEMP = limpar_texto_uf(UF_NASCIMENTO),
    
    COD_UF_NASC_NUM = case_when(
      UF_NASC_TEMP %in% c("RONDONIA", "RO") ~ "11",
      UF_NASC_TEMP %in% c("ACRE", "AC") ~ "12",
      UF_NASC_TEMP %in% c("AMAZONAS", "AM") ~ "13",
      UF_NASC_TEMP %in% c("RORAIMA", "RR") ~ "14",
      UF_NASC_TEMP %in% c("PARA", "PA") ~ "15",
      UF_NASC_TEMP %in% c("AMAPA", "AP") ~ "16",
      UF_NASC_TEMP %in% c("TOCANTINS", "TO") ~ "17",
      UF_NASC_TEMP %in% c("MARANHAO", "MA") ~ "21",
      UF_NASC_TEMP %in% c("PIAUI", "PI") ~ "22",
      UF_NASC_TEMP %in% c("CEARA", "CE") ~ "23",
      UF_NASC_TEMP %in% c("RIOGRANDEDONORTE", "RN") ~ "24",
      UF_NASC_TEMP %in% c("PARAIBA", "PB") ~ "25",
      UF_NASC_TEMP %in% c("PERNAMBUCO", "PE") ~ "26",
      UF_NASC_TEMP %in% c("ALAGOAS", "AL") ~ "27",
      UF_NASC_TEMP %in% c("SERGIPE", "SE") ~ "28",
      UF_NASC_TEMP %in% c("BAHIA", "BA") ~ "29",
      UF_NASC_TEMP %in% c("MINASGERAIS", "MG") ~ "31",
      UF_NASC_TEMP %in% c("ESPIRITOSANTO", "ES") ~ "32",
      UF_NASC_TEMP %in% c("RIODEJANEIRO", "RJ") ~ "33",
      UF_NASC_TEMP %in% c("SAOPAULO", "SP") ~ "35",
      UF_NASC_TEMP %in% c("PARANA", "PR") ~ "41",
      UF_NASC_TEMP %in% c("SANTACATARINA", "SC") ~ "42",
      UF_NASC_TEMP %in% c("RIOGRANDEDOSUL", "RS") ~ "43",
      UF_NASC_TEMP %in% c("MATOGROSSODOSUL", "MS") ~ "50",
      UF_NASC_TEMP %in% c("MATOGROSSO", "MT") ~ "51",
      UF_NASC_TEMP %in% c("GOIAS", "GO") ~ "52",
      UF_NASC_TEMP %in% c("DISTRITOFEDERAL", "DF") ~ "53",
      TRUE ~ NA_character_ 
    ),
    
    COD_REGIAO_NASC_NUM = substr(COD_UF_NASC_NUM, 1, 1),
    
    CLASSIFICACAO_MIGRATORIA = case_when(
      is.na(COD_UF_NASC_NUM) | is.na(COD_UF_RESID_NUM) ~ "Ignorado/Em branco",
      
      COD_REGIAO_NASC_NUM != COD_REGIAO_RESID_NUM ~ "Migrante Interregional",
      
      COD_UF_NASC_NUM != COD_UF_RESID_NUM ~ "Migrante Interestadual",
      
      COD_UF_NASC_NUM == COD_UF_RESID_NUM ~ "Não Migrante",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    CLASSIFICACAO_MIGRATORIA = factor(CLASSIFICACAO_MIGRATORIA,
                                      levels = c("Não Migrante",
                                                 "Migrante Interestadual",
                                                 "Migrante Interregional",
                                                 "Ignorado/Em branco"))
  ) %>%
  
  select(-COD_UF_RESID_NUM, -COD_REGIAO_RESID_NUM, 
         -COD_UF_NASC_NUM, -COD_REGIAO_NASC_NUM, -UF_NASC_TEMP)

table(MORT_CHAGAS_TRATADO$CLASSIFICACAO_MIGRATORIA, useNA = "ifany")

round(prop.table(table(MORT_CHAGAS_TRATADO$CLASSIFICACAO_MIGRATORIA)) * 100, 2)

MORT_CHAGAS_2_TRATADO <- MORT_CHAGAS_TRATADO %>%
  mutate(
    ESCOLARIDADE = case_when(
      IDADE_OBITO < 5 ~ "Não se aplica",
      as.character(ESC) == "1" ~ "Nenhuma",
      as.character(ESC) %in% c("2", "3") ~ "1 a 7 anos",
      as.character(ESC) == "4" ~ "8 a 11 anos",
      as.character(ESC) == "5" ~ "12 anos ou mais",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    ESCOLARIDADE = factor(ESCOLARIDADE, 
                          levels = c("Nenhuma", "1 a 7 anos", 
                                     "8 a 11 anos", "12 anos ou mais", 
                                     "Não se aplica", "Ignorado/Em branco")),
    
  ) %>%
  
    select(
  
    everything(),
    
    -ESC, -DIAG_TEMP, -MIGRANTE, -ATESTADO, -LINHAA, -LINHAB, -LINHAC, -LINHAD, -LINHAII, -CAUSABAS
  )

print(table(MORT_CHAGAS_2_TRATADO$ESCOLARIDADE))

MORT_CHAGAS_2_TRATADO <- MORT_CHAGAS_2_TRATADO %>%
  mutate(
    TPPOS_LIMPO = str_trim(as.character(TPPOS)),
    
    STATUS_INVESTIGACAO_2006 = case_when(
      TPPOS_LIMPO == "S" ~ "Sim, foi Investigado",
      TPPOS_LIMPO == "N" ~ "Não Investigado",
      TRUE ~ "Ignorado/Em branco"
    ),
    
      STATUS_INVESTIGACAO_2006 = factor(STATUS_INVESTIGACAO_2006, 
                                 levels = c("Sim, foi Investigado", 
                                            "Não Investigado", 
                                            "Ignorado/Em branco"))
  ) %>%
  
  select(-TPPOS, -TPPOS_LIMPO)

table(MORT_CHAGAS_2_TRATADO$FASE_DOENCA, MORT_CHAGAS_2_TRATADO$STATUS_INVESTIGACAO_2006)


prop.table(table(MORT_CHAGAS_2_TRATADO$FASE_DOENCA, MORT_CHAGAS_2_TRATADO$STATUS_INVESTIGACAO_2006), margin = 1) %>%
  round(4) * 100

MORT_CHAGAS_2_TRATADO <- MORT_CHAGAS_2_TRATADO %>%
  mutate(
    ANO_OBITO = year(DATA_OBITO))

table(MORT_CHAGAS_2_TRATADO$ANO_OBITO, useNA = "always")

glimpse(MORT_CHAGAS_2_TRATADO %>% select(ASSISTMED, EXAME, CIRURGIA, NECROPSIA))

MORT_CHAGAS_2_TRATADO <- MORT_CHAGAS_2_TRATADO %>%
  mutate(across(c(
    ASSISTMED, EXAME, CIRURGIA, NECROPSIA), ~ case_when(
    as.character(.) == "1" ~ "Sim",
    as.character(.) == "2" ~ "Não",
    as.character(.) == "9" ~ "Ignorado/Em branco",
    as.character(.) == "Ignorado" ~ "Ignorado/Em branco",
    is.na(.) | as.character(.) == "" ~ "Ignorado/Em branco", 
    TRUE ~ "Ignorado/Em branco"
  ))) %>%
  mutate(across(c(
    ASSISTMED, EXAME, CIRURGIA, NECROPSIA), 
                ~ factor(., levels = c("Sim", "Não", "Ignorado/Em branco"))))

verificar_qualidade <- function(coluna) {
  tabela <- table(coluna, useNA = "always")
  prop <- round(prop.table(tabela) * 100, 1)
  return(prop)
}

print(verificar_qualidade(MORT_CHAGAS_2_TRATADO$ASSISTMED))

print(verificar_qualidade(MORT_CHAGAS_2_TRATADO$EXAME))

print(verificar_qualidade(MORT_CHAGAS_2_TRATADO$CIRURGIA))

print(verificar_qualidade(MORT_CHAGAS_2_TRATADO$NECROPSIA))

MORT_CHAGAS_2_TRATADO <- MORT_CHAGAS_2_TRATADO %>%
  mutate(
    LOCOCOR_TEMP = as.character(LOCOCOR),
    
    LOCAL_OCORRENCIA = case_when(
      LOCOCOR_TEMP == "1" ~ "Hospital",
      LOCOCOR_TEMP == "2" ~ "Outros Estabelecimento de Saúde",
      LOCOCOR_TEMP == "3" ~ "Domicílio",
      LOCOCOR_TEMP == "4" ~ "Via Pública",
      LOCOCOR_TEMP == "5" ~ "Outros",
      
      LOCOCOR_TEMP %in% c("9", "0") | is.na(LOCOCOR_TEMP) ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    LOCAL_OCORRENCIA = factor(LOCAL_OCORRENCIA, 
                              levels = c("Hospital", 
                                         "Domicílio", 
                                         "Outros Estabelecimento de Saúde", 
                                         "Via Pública", 
                                         "Outros", 
                                         "Ignorado/Em branco"))
  )

print(count(MORT_CHAGAS_2_TRATADO, LOCAL_OCORRENCIA) %>% 
        mutate(Percent = round(n/sum(n)*100, 2)))

MORT_CHAGAS_2_TRATADO <- MORT_CHAGAS_2_TRATADO %>%
  mutate(
    COD_MUN_RES_F = str_pad(as.character(CODMUNRES), width = 6, pad = "0", side = "left"),
    COD_MUN_OCOR_F = str_pad(as.character(CODMUNOCOR), width = 6, pad = "0", side = "left"),
    
    COD_UF_RES = substr(COD_MUN_RES_F, 1, 2),
    COD_UF_OCOR = substr(COD_MUN_OCOR_F, 1, 2),
    
    COD_REG_RES = substr(COD_MUN_RES_F, 1, 1),
    COD_REG_OCOR = substr(COD_MUN_OCOR_F, 1, 1),
    
    FLUXO_ASSISTENCIA = case_when(
      is.na(CODMUNRES) | is.na(CODMUNOCOR) ~ "Ignorado/Em branco",
      COD_MUN_RES_F == "000000" | COD_MUN_OCOR_F == "000000" ~ "Ignorado/Em branco",
      
      COD_MUN_RES_F == COD_MUN_OCOR_F ~ "Sem Deslocamento",
      
      (COD_MUN_RES_F != COD_MUN_OCOR_F) & (COD_UF_RES == COD_UF_OCOR) ~ "Deslocamento Intermunicipal",
      
      (COD_UF_RES != COD_UF_OCOR) & (COD_REG_RES == COD_REG_OCOR) ~ "Deslocamento Interestadual",
      
      COD_REG_RES != COD_REG_OCOR ~ "Deslocamento Interregional",
      
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%

  mutate(FLUXO_ASSISTENCIA = factor(FLUXO_ASSISTENCIA, 
                                    levels = c("Sem Deslocamento",
                                               "Deslocamento Intermunicipal",
                                               "Deslocamento Interestadual",
                                               "Deslocamento Interregional",
                                               "Ignorado/Em branco")))

print(count(MORT_CHAGAS_2_TRATADO, FLUXO_ASSISTENCIA) %>% 
        mutate(Percent = round(n/sum(n)*100, 2)))

MORT_CHAGAS_3_TRATADO <- MORT_CHAGAS_2_TRATADO %>%
  select(
    -CODMUNOCOR,
    -LOCOCOR,
    -COD_MUN_RES_F, 
    -COD_MUN_OCOR_F, 
    -COD_UF_RES, 
    -COD_UF_OCOR, 
    -COD_REG_RES, 
    -COD_REG_OCOR,
    -LOCOCOR_TEMP
  )

glimpse(MORT_CHAGAS_3_TRATADO)

MORT_CHAGAS_3_TRATADO <- MORT_CHAGAS_3_TRATADO %>%
  mutate(
    OCUP_TEMP = str_remove_all(as.character(OCUP), "[^0-9]"),
    
    PRIMEIRO_DIGITO = substr(OCUP_TEMP, 1, 1),
    
    GRANDE_GRUPO_OCUP = case_when(
      is.na(OCUP) | OCUP_TEMP %in% c("000000", "999999", "99999", "0", "") ~ "Ignorado/Em branco",
      
      OCUP_TEMP %in% c("999991", "999992", "999994") ~ "Inativos/Sem Ocupação",
      OCUP_TEMP == "999993" ~ "Aposentado/Pensionista",
      
      ANO_OBITO <= 2005 & PRIMEIRO_DIGITO %in% c("0", "1") ~ "Profissionais das Ciências e Artes",
      ANO_OBITO <= 2005 & PRIMEIRO_DIGITO == "2" ~ "Membros Superiores/Dirigentes",
      ANO_OBITO <= 2005 & PRIMEIRO_DIGITO == "3" ~ "Serviços Administrativos",
      ANO_OBITO <= 2005 & PRIMEIRO_DIGITO == "4" ~ "Trabalhadores de Serviços/Comércio",
      ANO_OBITO <= 2005 & PRIMEIRO_DIGITO == "5" ~ "Trabalhadores de Serviços/Comércio",
      ANO_OBITO <= 2005 & PRIMEIRO_DIGITO == "6" ~ "Trabalhadores Agropecuários/Florestais/Pesca",
      ANO_OBITO <= 2005 & PRIMEIRO_DIGITO %in% c("7", "8", "9") ~ "Trabalhadores da Produção Industrial",
      
      ANO_OBITO >= 2006 & PRIMEIRO_DIGITO == "0" ~ "Membros das Forças Armadas/Polícia/Bombeiros",
      ANO_OBITO >= 2006 & PRIMEIRO_DIGITO == "1" ~ "Membros Superiores/Dirigentes/Poder Público",
      ANO_OBITO >= 2006 & PRIMEIRO_DIGITO == "2" ~ "Profissionais das Ciências e Artes",
      ANO_OBITO >= 2006 & PRIMEIRO_DIGITO == "3" ~ "Técnicos de Nível Médio",
      ANO_OBITO >= 2006 & PRIMEIRO_DIGITO == "4" ~ "Serviços Administrativos",
      ANO_OBITO >= 2006 & PRIMEIRO_DIGITO == "5" ~ "Trabalhadores de Serviços/Comércio",
      ANO_OBITO >= 2006 & PRIMEIRO_DIGITO == "6" ~ "Trabalhadores Agropecuários/Florestais/Pesca", 
      ANO_OBITO >= 2006 & PRIMEIRO_DIGITO %in% c("7", "8", "9") ~ "Trabalhadores da Produção Industrial",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    PERFIL_OCUPACIONAL = case_when(
      GRANDE_GRUPO_OCUP == "Trabalhadores Agropecuários/Florestais/Pesca" ~ "Agropecuário/Rural",
      
      GRANDE_GRUPO_OCUP %in% c("Membros Superiores/Dirigentes/Poder Público", 
                               "Membros Superiores/Dirigentes",
                               "Profissionais das Ciências e Artes", 
                               "Técnicos de Nível Médio") ~ "Não Manual/Intelectual/Técnico",
      
      GRANDE_GRUPO_OCUP %in% c("Serviços Administrativos", 
                               "Trabalhadores de Serviços/Comércio", 
                               "Trabalhadores da Produção Industrial", 
                               "Membros das Forças Armadas/Polícia/Bombeiros") ~ "Manual Urbano/Serviços/Industrial",
      
      GRANDE_GRUPO_OCUP == "Aposentado/Pensionista" ~ "Aposentado/Pensionista",
      GRANDE_GRUPO_OCUP == "Inativos/Sem Ocupação" ~ "Inativos/Sem Ocupação",
      
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%
  
  select(-OCUP_TEMP, -PRIMEIRO_DIGITO) %>%
  
  mutate(
    PERFIL_OCUPACIONAL = factor(PERFIL_OCUPACIONAL,
                                levels = c("Aposentado/Pensionista",
                                           "Manual Urbano/Serviços/Industrial",
                                           "Agropecuário/Rural",
                                           "Não Manual/Intelectual/Técnico",
                                           "Inativos/Sem Ocupação",
                                           "Ignorado/Em branco"))
  )

count(MORT_CHAGAS_3_TRATADO, GRANDE_GRUPO_OCUP) %>%
  mutate(Percentual = round(n/sum(n)*100, 2)) %>%
  print()

count(MORT_CHAGAS_3_TRATADO, PERFIL_OCUPACIONAL) %>%
  mutate(Percentual = round(n/sum(n)*100, 2)) %>%
  print()

MORT_CHAGAS_FINAL_TRATADO <- MORT_CHAGAS_3_TRATADO %>%
  select(-OCUP)

MORT_CHAGAS_FINAL_TRATADO <- MORT_CHAGAS_FINAL_TRATADO %>%
  mutate(
    FONTE_INVESTIGACAO_2006 = case_when(
      
      ANO_OBITO < 2006 ~ "Não se aplica",
      
      FONTEINV == "1" ~ "Comitê de Morte Materna ou Infantil",
      FONTEINV == "2" ~ "Visita domiciliar ou Entrevista familiar",
      FONTEINV == "3" ~ "Estabelecimento de Saúde ou Prontuário",
      FONTEINV %in% c("5", "6") ~ "Órgãos Médico-Legais (SVO ou IML)",
      FONTEINV %in% c("4", "7", "8") ~ "Outras fontes de investigação",
      FONTEINV == "9" ~ "Ignorado/Em branco",
      
      STATUS_INVESTIGACAO_2006 == "Sim, foi Investigado" &
        (is.na(FONTEINV) | FONTEINV == "") ~
        "Fonte não informada",
      
      STATUS_INVESTIGACAO_2006 == "Não Investigado" ~
        "Não se aplica",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    FONTE_INVESTIGACAO_2006 = factor(
      FONTE_INVESTIGACAO_2006,
      levels = c(
        "Estabelecimento de Saúde ou Prontuário",
        "Órgãos Médico-Legais (SVO ou IML)",
        "Comitê de Morte Materna ou Infantil",
        "Visita domiciliar ou Entrevista familiar",
        "Outras fontes de investigação",
        "Fonte não informada",
        "Não se aplica",
        "Ignorado/Em branco"
      )
    )
  )

table(MORT_CHAGAS_FINAL_TRATADO$STATUS_INVESTIGACAO_2006,
      MORT_CHAGAS_FINAL_TRATADO$FONTE_INVESTIGACAO_2006)

round(prop.table(table(MORT_CHAGAS_FINAL_TRATADO$TIPO_MENCAO, MORT_CHAGAS_FINAL_TRATADO$FONTE_INVESTIGACAO_2006))*100, 2)

table(MORT_CHAGAS_FINAL_TRATADO$TIPO_MENCAO, MORT_CHAGAS_FINAL_TRATADO$FONTE_INVESTIGACAO_2006)

table(MORT_CHAGAS_TRATADO$TIPO_MENCAO, MORT_CHAGAS_TRATADO$CID_CHAGAS_ENCONTRADO)

table(MORT_CHAGAS_TRATADO$FASE_DOENCA, MORT_CHAGAS_TRATADO$TPPOS)

table(MORT_CHAGAS_TRATADO$FASE_DOENCA)

round(prop.table(table(MORT_CHAGAS_TRATADO$FASE_DOENCA, MORT_CHAGAS_TRATADO$TPPOS))*100, 2)

table(MORT_CHAGAS_ASSOCIADA$GRUPO_CAUSABAS)

round(prop.table(table(MORT_CHAGAS_ASSOCIADA$GRUPO_CAUSABAS))*100, 2)

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
  
  malha_brasil_bruta <- st_read(file.path(DIR_DADOS, "IBGE/IBGE_POP_2001_2024/BR_Municipios_2022/BR_Municipios_2022.shp"))
  
  malha_brasil <- malha_brasil_bruta %>%
    mutate(COD_MUN = as.character(substr(CD_MUN, 1, 6)))
  
  mapa_taxas <- malha_brasil %>%
    left_join(taxas_municipais, by = "COD_MUN") %>%
    mutate(
      total_obitos_mun = replace_na(total_obitos_mun, 0),
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
      obitos_padronizados_esperados = (taxa_padronizada * pop_media_mun) / 100000
      )
  
  res_bayes_local <- EBlocal(
    ri = mapa_valido$obitos_padronizados_esperados,  
    ni = mapa_valido$pop_media_mun, 
    nb = vizinhos, 
    zero.policy = TRUE
  )
  
  mapa_valido$taxa_padronizada_suavizada <- res_bayes_local$est * 100000