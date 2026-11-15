# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: E-SUS/DCCRBR23/DCCRBR23 - PARTE 4 - GLOBAL.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "E-SUS/DCCRBR23"))

# Base de entrada (na execução original, importada pelo menu do RStudio):
if (!exists("DCCRSTATA")) DCCRSTATA <- haven::read_dta(file.path(DIR_DADOS, "E-SUS/DCCRBR23STATA.dta"))

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

names(DCCRSTATA) <- toupper(names(DCCRSTATA))

CHAGAS_CRONICA_LIMPO <- DCCRSTATA %>%
  
  select(
    DT_NOTIFIC, ANO_DIAG, SG_UF_NOT, ID_MUNICIP, CD_MUNICIP, AC_NOT,     
    
    NU_IDADE_N, ANO_NASC, CS_SEXO, CS_RACA, CS_ESCOL_N, ID_OCUPA_N, COMU_TRAD, CD_COMUNIN, COMUNINF, ANO_NASC,
    
    SG_UF, ID_MN_RESI, CD_MN_RESI, UF_NASC, MUN_NASC, CD_MUN_NAS,
    
    FORMA, MO_SUSPEIT, HIST_EPID, REATIVACAO,
    
    COMORBID, COMORBID__, ESP_COMORB, CARDIOPAT, HIPERTEN, DIABETES, NEOPLASIA, 
    HIV, LEISHMANIA, HEPATITE, OUT_COMORB, OUTRO_QUAL,   
    
    EIE_IGG, IFI_IGG, HAI_IGG, QUIMIO_IGG, PCR, EXAME_DESC, ELETROCARD,
    ECOCARDIO, RX_TORAX, RX_ESOFAGO, RX_COLON, OUTRO_EXAM, HOSP_ESP,     
    
    TRAT_BNZ, HIST_BNZ, TRAT_NFX,     
    
    BUSCAATIVA, CONF_FAMIL, EXAM_FAMIL, DIAG_FAMIL,
    
    ST_ENCERRA,DT_ENCERRA
  )

glimpse(CHAGAS_CRONICA_LIMPO)

vars_auditoria_completa <- c(
  "SG_UF_NOT", "ID_MUNICIP", "CD_MUNICIP", "SG_UF", "AC_NOT", 
  "DT_NOTIFIC", "ANO_DIAG",
  
  "CS_SEXO", "ID_MN_RESI", "CD_MN_RESI", "ID_OCUP_N", "CS_RACA", 
  "COMU_TRAD", "CS_ESCOL_N", "NU_IDADE_N", "ANO_NASC", 
  "CD_COMUNIN", "COMUNINF", "UF_NASC", "MUN_NASC", "CD_MUN_NASC",
  
  "FORMA", "MO_SUSPEIT", "REATIVAÇÃO", "HIST_EPID", "BUSCA_ATIVA",
  
  "CORMOBID", "COMORBID_", "ESP_COMORB", "OUT_COMORB", "OUTRO_QUAL",
  "HIV", "HIPERTEN", "HEPATITE", "DIABETES", "CARDIOPAT", "NEOPLASIA", "LEISHMANIA",
  
  "EIE_IGG", "IFI_IGG", "PCR", "QUIMIO_IGG", "HAI_IGG", "EXAME_DESC",
  "ELETROCARD", "RX_TORAX", "OUTRO_EXAM", "ECOCARDIO", "RX_COLON", "RX_ESOFAGO",
  "HOSP_ESP",
  
  "TRAT_BNZ", "HIST_BNZ", "TRAT_NFX",
  "CONF_FAMIL", "EXAM_FAMIL", "DIAG_FAMIL",
  
  "ST_ENCERRA", "DT_ENCERRA"
)

TABELA_QUALIDADE_CHAGAS_CRONICA <- CHAGAS_CRONICA_LIMPO %>%
  select(any_of(vars_auditoria_completa)) %>% 
  
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

print(head(TABELA_QUALIDADE_CHAGAS_CRONICA, 20))

CHAGAS_CRONICA_LIMPO <- CHAGAS_CRONICA_LIMPO %>% 
  select(-CD_COMUNIN, -COMUNINF, -DT_ENCERRA)

CHAGAS_CRONICA_1_TABULADO <- CHAGAS_CRONICA_LIMPO %>%
  mutate(
    ANO_NOTIFICACAO = year(DT_NOTIFIC),
    
    DIF_DIAG_NOTIF = ANO_NOTIFICACAO - as.numeric(ANO_DIAG)
  )

summary(CHAGAS_CRONICA_1_TABULADO$DIF_DIAG_NOTIF)

identificar_regiao_por_nome <- function(nome_uf) {
  case_when(
    
    str_detect(nome_uf, regex("Acre|Amazon|Amap|Rond|Roraima|Tocant", ignore_case = T)) ~ "Norte",
    str_detect(nome_uf, regex("^Par[áa]$|^Par[áa] ", ignore_case = T)) ~ "Norte",
    
    str_detect(nome_uf, regex("Alago|Bahia|Cear|Maranh|Piau|Pernamb|Sergip|Rio Grande do N", ignore_case = T)) ~ "Nordeste",
    str_detect(nome_uf, regex("Paraib|Paraíb", ignore_case = T)) ~ "Nordeste",
    
    str_detect(nome_uf, regex("Esp.rito|Minas|Rio de Jan|Sao Paulo|São Paulo", ignore_case = T)) ~ "Sudeste",
    
    str_detect(nome_uf, regex("Paran|Santa Cat|Rio Grande do S", ignore_case = T)) ~ "Sul",
    
    str_detect(nome_uf, regex("Distrito|Goi|Mato Grosso", ignore_case = T)) ~ "Centro-Oeste",
    
    TRUE ~ "Ignorado/Em branco"
  )
}

CHAGAS_CRONICA_1_TABULADO <- CHAGAS_CRONICA_1_TABULADO %>%
  mutate(
    
    SG_UF_NOT = str_trim(SG_UF_NOT), 
    
    REGIAO_NOTIFICACAO = identificar_regiao_por_nome(SG_UF_NOT),
    
    SG_UF = str_trim(SG_UF),
    
    REGIAO_RESIDENCIA = identificar_regiao_por_nome(SG_UF),
    
    UF_NASC = str_trim(UF_NASC),
    
    REGIAO_NASCIMENTO = identificar_regiao_por_nome(UF_NASC),
  ) 

glimpse(CHAGAS_CRONICA_1_TABULADO %>% 
          select(SG_UF_NOT, REGIAO_NOTIFICACAO, 
                 SG_UF, REGIAO_RESIDENCIA))

limpar_texto_uf <- function(texto) {
  texto <- str_to_upper(as.character(texto)) 
  texto <- stringi::stri_trans_general(texto, "Latin-ASCII") 
  texto <- str_replace_all(texto, "[^A-Z]", "") 
  return(texto)
}

CHAGAS_CRONICA_1_TABULADO <- CHAGAS_CRONICA_1_TABULADO %>%
  mutate(
    UF_RESID_TEMP = limpar_texto_uf(SG_UF),
    UF_NASC_TEMP  = limpar_texto_uf(UF_NASC),
    
    COD_UF_RESID_NUM = case_when(
      UF_RESID_TEMP %in% c("RONDONIA", "RO") ~ "11",
      UF_RESID_TEMP %in% c("ACRE", "AC") ~ "12",
      UF_RESID_TEMP %in% c("AMAZONAS", "AM") ~ "13",
      UF_RESID_TEMP %in% c("RORAIMA", "RR") ~ "14",
      UF_RESID_TEMP %in% c("PARA", "PA") ~ "15",
      UF_RESID_TEMP %in% c("AMAPA", "AP") ~ "16",
      UF_RESID_TEMP %in% c("TOCANTINS", "TO") ~ "17",
      UF_RESID_TEMP %in% c("MARANHAO", "MA") ~ "21",
      UF_RESID_TEMP %in% c("PIAUI", "PI") ~ "22",
      UF_RESID_TEMP %in% c("CEARA", "CE") ~ "23",
      UF_RESID_TEMP %in% c("RIOGRANDEDONORTE", "RN") ~ "24",
      UF_RESID_TEMP %in% c("PARAIBA", "PB") ~ "25",
      UF_RESID_TEMP %in% c("PERNAMBUCO", "PE") ~ "26",
      UF_RESID_TEMP %in% c("ALAGOAS", "AL") ~ "27",
      UF_RESID_TEMP %in% c("SERGIPE", "SE") ~ "28",
      UF_RESID_TEMP %in% c("BAHIA", "BA") ~ "29",
      UF_RESID_TEMP %in% c("MINASGERAIS", "MG") ~ "31",
      UF_RESID_TEMP %in% c("ESPIRITOSANTO", "ES") ~ "32",
      UF_RESID_TEMP %in% c("RIODEJANEIRO", "RJ") ~ "33",
      UF_RESID_TEMP %in% c("SAOPAULO", "SP", "SPAULO") ~ "35",
      UF_RESID_TEMP %in% c("PARANA", "PR") ~ "41",
      UF_RESID_TEMP %in% c("SANTACATARINA", "SC") ~ "42",
      UF_RESID_TEMP %in% c("RIOGRANDEDOSUL", "RS") ~ "43",
      UF_RESID_TEMP %in% c("MATOGROSSODOSUL", "MS") ~ "50",
      UF_RESID_TEMP %in% c("MATOGROSSO", "MT") ~ "51",
      UF_RESID_TEMP %in% c("GOIAS", "GO") ~ "52",
      UF_RESID_TEMP %in% c("DISTRITOFEDERAL", "DF") ~ "53",
      TRUE ~ NA_character_ 
    ),
    
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
      UF_NASC_TEMP %in% c("SAOPAULO", "SP", "SPAULO") ~ "35",
      UF_NASC_TEMP %in% c("PARANA", "PR") ~ "41",
      UF_NASC_TEMP %in% c("SANTACATARINA", "SC") ~ "42",
      UF_NASC_TEMP %in% c("RIOGRANDEDOSUL", "RS") ~ "43",
      UF_NASC_TEMP %in% c("MATOGROSSODOSUL", "MS") ~ "50",
      UF_NASC_TEMP %in% c("MATOGROSSO", "MT") ~ "51",
      UF_NASC_TEMP %in% c("GOIAS", "GO") ~ "52",
      UF_NASC_TEMP %in% c("DISTRITOFEDERAL", "DF") ~ "53",
      TRUE ~ NA_character_ 
    ),
    
    COD_REGIAO_RESID_NUM = substr(COD_UF_RESID_NUM, 1, 1),
    COD_REGIAO_NASC_NUM  = substr(COD_UF_NASC_NUM, 1, 1),
    
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
  
  select(-UF_RESID_TEMP, -UF_NASC_TEMP, 
         -COD_UF_RESID_NUM, -COD_UF_NASC_NUM, 
         -COD_REGIAO_RESID_NUM, -COD_REGIAO_NASC_NUM)

table(CHAGAS_CRONICA_1_TABULADO$CLASSIFICACAO_MIGRATORIA, useNA = "ifany")

round(prop.table(table(CHAGAS_CRONICA_1_TABULADO$CLASSIFICACAO_MIGRATORIA)) * 100, 2)

CHAGAS_CRONICA_1_TABULADO <- CHAGAS_CRONICA_1_TABULADO %>%
  mutate(
    
    COD_MUN_RES_F = str_pad(as.character(CD_MN_RESI), width = 6, pad = "0", side = "left"),
    COD_MUN_NOT_F = str_pad(as.character(CD_MUNICIP), width = 6, pad = "0", side = "left"),
    
    COD_UF_RES = substr(COD_MUN_RES_F, 1, 2),
    COD_UF_NOT = substr(COD_MUN_NOT_F, 1, 2),
    
    COD_REG_RES = substr(COD_MUN_RES_F, 1, 1),
    COD_REG_NOT = substr(COD_MUN_NOT_F, 1, 1),
    
    FLUXO_ASSISTENCIA = case_when(
      is.na(CD_MN_RESI) | is.na(CD_MUNICIP) ~ "Ignorado/Em branco",
      COD_MUN_RES_F == "000000" | COD_MUN_NOT_F == "000000" ~ "Ignorado/Em branco",
      
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
  
  select(-COD_MUN_RES_F, -COD_MUN_NOT_F, 
         -COD_UF_RES, -COD_UF_NOT, 
         -COD_REG_RES, -COD_REG_NOT,
         -ID_MN_RESI, -MUN_NASC, -CD_MUN_NAS,
         -ID_MUNICIP, -CD_MUNICIP)

print(table(CHAGAS_CRONICA_1_TABULADO$FLUXO_ASSISTENCIA, useNA = "ifany"))

print(round(prop.table(table(CHAGAS_CRONICA_1_TABULADO$FLUXO_ASSISTENCIA)) * 100, 2))

CHAGAS_CRONICA_2_TABULADO <- CHAGAS_CRONICA_1_TABULADO %>%
  mutate(
    
    NU_IDADE_RAW = as.numeric(NU_IDADE_N),
    
    IDADE_DECODIFICADA = case_when(
      is.na(NU_IDADE_RAW) ~ NA_real_,
      NU_IDADE_RAW >= 4000 & NU_IDADE_RAW < 5000 ~ NU_IDADE_RAW - 4000,
      NU_IDADE_RAW >= 5000 ~ NU_IDADE_RAW - 5000 + 100,
      NU_IDADE_RAW < 4000 & NU_IDADE_RAW > 0 ~ 0,
      NU_IDADE_RAW <= 130 ~ NU_IDADE_RAW,
      TRUE ~ NA_real_
    ),
    
    IDADE_CALCULADA = as.numeric(ANO_NOTIFICACAO) - as.numeric(ANO_NASC),
    
    IDADE_FINAL = case_when(
      !is.na(IDADE_DECODIFICADA) & IDADE_DECODIFICADA >= 0 & IDADE_DECODIFICADA <= 120 ~ IDADE_DECODIFICADA,
      !is.na(IDADE_CALCULADA) & IDADE_CALCULADA >= 0 & IDADE_CALCULADA <= 120 ~ IDADE_CALCULADA,
      TRUE ~ NA_real_
    ),
    
    FAIXA_ETARIA = cut(IDADE_FINAL, 
                       breaks = c(0, 46, 60, 75, Inf),
                       labels = c("0 a 45 anos", "46 a 59 anos", "60 a 74 anos", "75 anos ou mais"),
                       right = FALSE),
    FAIXA_ETARIA = factor(replace_na(as.character(FAIXA_ETARIA), "Ignorado/Em branco"),
                          levels = c("0 a 45 anos", "46 a 59 anos", "60 a 74 anos", "75 anos ou mais", "Ignorado/Em branco")),
    
    ESCOLARIDADE_FINAL = case_when(
      
      IDADE_FINAL < 5 ~ "Não se aplica",
      
      str_detect(CS_ESCOL_N, regex("Nenhuma", ignore_case = TRUE)) ~ "Nenhuma",
      str_detect(CS_ESCOL_N, regex("EF Incompleto", ignore_case = TRUE)) ~ "1 a 7 anos",
      str_detect(CS_ESCOL_N, regex("EF Completo|EM Incompleto|EM Completo", ignore_case = TRUE)) ~ "8 a 11 anos",
      str_detect(CS_ESCOL_N, regex("Superior", ignore_case = TRUE)) ~ "12 anos ou mais",
      
      str_detect(CS_ESCOL_N, regex("Nao se aplica|Não se aplica", ignore_case = TRUE)) ~ "Não se aplica",
      
      str_detect(CS_ESCOL_N, regex("Ignorado", ignore_case = TRUE)) ~ "Ignorado/Em branco",
      is.na(CS_ESCOL_N) ~ "Ignorado/Em branco",
      TRUE ~ "Ignorado/Em branco" 
    ),
    
    ESCOLARIDADE_FINAL = factor(ESCOLARIDADE_FINAL,
                                levels = c("Nenhuma", "1 a 7 anos", 
                                           "8 a 11 anos", "12 anos ou mais", 
                                           "Não se aplica", "Ignorado/Em branco")),
    
    RACACOR_FINAL = case_when(
      str_detect(CS_RACA, regex("Branca", ignore_case = TRUE)) ~ "Branca",
      str_detect(CS_RACA, regex("Preta", ignore_case = TRUE)) ~ "Preta",
      str_detect(CS_RACA, regex("Amarela", ignore_case = TRUE)) ~ "Amarela",
      str_detect(CS_RACA, regex("Parda", ignore_case = TRUE)) ~ "Parda",
      str_detect(CS_RACA, regex("Indigena|Indígena", ignore_case = TRUE)) ~ "Indígena",
      TRUE ~ "Ignorado/Em branco"
    ),
    RACACOR_FINAL = factor(RACACOR_FINAL, 
                           levels = c("Branca", "Parda", "Preta", "Amarela", "Indígena", "Ignorado/Em branco")),
    
    SEXO_FINAL = case_when(
      str_detect(CS_SEXO, regex("Masculino", ignore_case = TRUE)) ~ "Masculino",
      str_detect(CS_SEXO, regex("Feminino", ignore_case = TRUE)) ~ "Feminino",
      TRUE ~ "Ignorado/Em branco"
    ),
    SEXO_FINAL = factor(SEXO_FINAL, levels = c("Masculino", "Feminino", "Ignorado/Em branco"))
  )

summary(CHAGAS_CRONICA_2_TABULADO$IDADE_FINAL)
print(head(CHAGAS_CRONICA_2_TABULADO %>% select(NU_IDADE_N, IDADE_FINAL), 10))

print(table(CHAGAS_CRONICA_2_TABULADO$CS_ESCOL_N, CHAGAS_CRONICA_2_TABULADO$ESCOLARIDADE_FINAL))

print(table(CHAGAS_CRONICA_2_TABULADO$SEXO_FINAL))

print(table(CHAGAS_CRONICA_2_TABULADO$RACACOR_FINAL))

CHAGAS_CRONICA_2_TABULADO <- CHAGAS_CRONICA_2_TABULADO %>%
  
  select(-CS_SEXO, -CS_RACA, -CS_ESCOL_N, -NU_IDADE_N,
         -NU_IDADE_RAW, -IDADE_DECODIFICADA, -IDADE_CALCULADA)

limpar_binario_estrito <- function(coluna) {
  case_when(
    str_detect(coluna, regex("Nao realizado|Não realizado|Ignorado", ignore_case = TRUE)) ~ NA_real_,
    
    str_detect(coluna, regex("Sim|Reagente|Alterado|Positivo|Detectavel", ignore_case = TRUE)) ~ 1,
    
    str_detect(coluna, regex("Nao|Não|Normal|Negativo|Indetectavel", ignore_case = TRUE)) ~ 0,
    
    TRUE ~ NA_real_
  )
}

limpar_acesso_estrito <- function(coluna) {
  case_when(
    str_detect(coluna, regex("Normal|Alterado|Reagente|Positivo|Negativo|Detectavel|Sim", ignore_case = TRUE)) ~ 1,
    
    str_detect(coluna, regex("Nao realizado|Não realizado", ignore_case = TRUE)) ~ 0,
    
    TRUE ~ NA_real_
  )
}

CHAGAS_CRONICA_3_TABULADO <- CHAGAS_CRONICA_2_TABULADO %>%
  mutate(
    TEM_HIPERTENSAO = limpar_binario_estrito(HIPERTEN),
    TEM_DIABETES    = limpar_binario_estrito(DIABETES),
    TEM_NEOPLASIA   = limpar_binario_estrito(NEOPLASIA),
    TEM_HIV         = limpar_binario_estrito(HIV),
    TEM_LEISHMANIA  = limpar_binario_estrito(LEISHMANIA),
    TEM_HEPATITE    = limpar_binario_estrito(HEPATITE),
    TEM_CARDIOPATIA_PREVIA = limpar_binario_estrito(CARDIOPAT),
    TEM_OUTRAS_COMORBIDADES = limpar_binario_estrito(OUT_COMORB),
    
    TEXTO_COMORB = str_to_upper(paste(replace_na(ESP_COMORB,""), replace_na(OUTRO_QUAL,""), sep = " ")),
    STATUS_TEXTO_VALIDO = if_else(str_trim(TEXTO_COMORB) == "", 0, 1),
    
    TEM_RENAL = case_when(
      str_detect(TEXTO_COMORB, "RENAL|IRC|DIALISE|RIM") ~ 1, 
      STATUS_TEXTO_VALIDO == 1 ~ 0, 
      TRUE ~ NA_real_),
    
    TEM_AVC   = case_when(
      str_detect(TEXTO_COMORB, "AVC|AVE|DERRAME") ~ 1, 
      STATUS_TEXTO_VALIDO == 1 ~ 0, 
      TRUE ~ NA_real_),
    
    TEM_ISQUEMICA = case_when(
      str_detect(TEXTO_COMORB, "INFARTO|IAM|ANGINA|SAF|STENT") ~ 1, 
      STATUS_TEXTO_VALIDO == 1 ~ 0, TRUE 
      ~ NA_real_)
  )

CHAGAS_CRONICA_3_TABULADO <- CHAGAS_CRONICA_3_TABULADO %>%
  mutate(
    FEZ_ELETROCARD = limpar_acesso_estrito(ELETROCARD), 
    ELETROCARD_ALTERADO = limpar_binario_estrito(ELETROCARD),
    
    FEZ_ECOCARDIO  = limpar_acesso_estrito(ECOCARDIO),  
    ECOCARDIO_ALTERADO  = limpar_binario_estrito(ECOCARDIO),
    
    FEZ_RX_TORAX   = limpar_acesso_estrito(RX_TORAX),   
    RX_TORAX_ALTERADO   = limpar_binario_estrito(RX_TORAX),
    
    FEZ_RX_ESOFAGO = limpar_acesso_estrito(RX_ESOFAGO), 
    RX_ESOFAGO_ALTERADO = limpar_binario_estrito(RX_ESOFAGO),
    
    FEZ_RX_COLON   = limpar_acesso_estrito(RX_COLON),   
    RX_COLON_ALTERADO   = limpar_binario_estrito(RX_COLON),
    
    TEXTO_EXAME = str_to_upper(paste(replace_na(EXAME_DESC,""), replace_na(OUTRO_EXAM,""), sep = " ")),
    
    FEZ_HOLTER = if_else(str_detect(TEXTO_EXAME, "HOLTER"), 1, 0, missing=NA_real_),
    HOLTER_ALTERADO = case_when(
      FEZ_HOLTER == 1 & str_detect(TEXTO_EXAME, "ALTERADO|ARRITMIA|BLOQUEIO") ~ 1, 
      FEZ_HOLTER == 1 ~ 0, 
      TRUE ~ NA_real_),
    
    FEZ_EDA = if_else(str_detect(TEXTO_EXAME, "ENDOSCOPIA|EDA|DIGESTIVA"), 1, 0, missing=NA_real_),
    EDA_ALTERADA = case_when(
      FEZ_EDA == 1 & str_detect(TEXTO_EXAME, "MEGA|ACALASIA|DILATA") ~ 1, 
      FEZ_EDA == 1 ~ 0, 
      TRUE ~ NA_real_
    ),
    
    FEZ_EIE = limpar_acesso_estrito(EIE_IGG),       
    EIE_REAGENTE = limpar_binario_estrito(EIE_IGG),
    
    FEZ_IFI = limpar_acesso_estrito(IFI_IGG),       
    IFI_REAGENTE = limpar_binario_estrito(IFI_IGG),
    
    FEZ_HAI = limpar_acesso_estrito(HAI_IGG),       
    HAI_REAGENTE = limpar_binario_estrito(HAI_IGG),
    
    FEZ_QUIMIO = limpar_acesso_estrito(QUIMIO_IGG), 
    QUIMIO_REAGENTE = limpar_binario_estrito(QUIMIO_IGG),
    
    FEZ_PCR = limpar_acesso_estrito(PCR),           
    PCR_DETECTAVEL = limpar_binario_estrito(PCR),
  )

CHAGAS_CRONICA_4_TABULADO <- CHAGAS_CRONICA_3_TABULADO %>%
  mutate(
    
    N_NAS_COMORB = rowSums(is.na(select(., 
                                        TEM_HIPERTENSAO, TEM_DIABETES, TEM_NEOPLASIA, TEM_HIV, 
                                        TEM_LEISHMANIA, TEM_HEPATITE, TEM_CARDIOPATIA_PREVIA, TEM_OUTRAS_COMORBIDADES))),
    
    ALL_NA_COMORB = if_else(N_NAS_COMORB == 8, 1, 0),
    
    QTD_COMORBIDADES = if_else(
      ALL_NA_COMORB == 1, 
      NA_real_, 
      rowSums(select(., 
                     TEM_HIPERTENSAO, TEM_DIABETES, TEM_NEOPLASIA, TEM_HIV, 
                     TEM_LEISHMANIA, TEM_HEPATITE, TEM_CARDIOPATIA_PREVIA, TEM_OUTRAS_COMORBIDADES), 
              na.rm = TRUE)
    ),
    
    PERFIL_COMORBIDADE = case_when(
      is.na(QTD_COMORBIDADES) ~ "Ignorado/Em branco",
      QTD_COMORBIDADES == 0 ~ "Nenhuma Identificada",
      QTD_COMORBIDADES == 1 ~ "Uma Comorbidade",
      QTD_COMORBIDADES >= 2 ~ "Multimorbidade (2 ou mais)"
    ),
    
    RESUMO_CLINICO_PRINCIPAIS = paste0(
      if_else(TEM_HIPERTENSAO == 1, "HAS ", "", missing=""),
      if_else(TEM_DIABETES == 1, "DM ", "", missing=""),
      if_else(TEM_CARDIOPATIA_PREVIA == 1, "Cardiopatia ", "", missing=""),
      if_else(TEM_RENAL == 1, "DRC ", "", missing=""),
      if_else(TEM_AVC == 1, "AVC ", "", missing=""),
      if_else(TEM_ISQUEMICA == 1, "DAC ", "", missing=""),
      if_else(TEM_HIV == 1, "HIV ", "", missing=""),
      if_else(TEM_HEPATITE == 1, "Hepatite ", "", missing="")
    ),
    RESUMO_CLINICO_PRINCIPAIS = str_trim(RESUMO_CLINICO_PRINCIPAIS),
    RESUMO_CLINICO_PRINCIPAIS = case_when(
      ALL_NA_COMORB == 1 ~ "Ignorado/Em branco",
      RESUMO_CLINICO_PRINCIPAIS == "" ~ "Sem as Principais Comorbidades",
      TRUE ~ RESUMO_CLINICO_PRINCIPAIS
    ),
    
    N_NAS_EXAMES = rowSums(is.na(select(., 
                                        ELETROCARD_ALTERADO, ECOCARDIO_ALTERADO, RX_TORAX_ALTERADO, 
                                        RX_ESOFAGO_ALTERADO, RX_COLON_ALTERADO, HOLTER_ALTERADO, EDA_ALTERADA
                                        
    ))),
    
    ALL_NA_EXAMES = if_else(N_NAS_EXAMES == 7, 1, 0),
    
    QTD_EXAMES_ALTERADOS = if_else(
      ALL_NA_EXAMES == 1, 
      NA_real_, 
      rowSums(select(., 
                     ELETROCARD_ALTERADO, ECOCARDIO_ALTERADO, RX_TORAX_ALTERADO, 
                     RX_ESOFAGO_ALTERADO, RX_COLON_ALTERADO, HOLTER_ALTERADO, EDA_ALTERADA), 
              na.rm = TRUE)
    ),
    
    LISTA_EXAMES_ALTERADOS = paste0(
      if_else(ELETROCARD_ALTERADO == 1, "ECG ", "", missing=""),
      if_else(ECOCARDIO_ALTERADO == 1, "Eco ", "", missing=""),
      if_else(RX_TORAX_ALTERADO == 1, "RX Torax ", "", missing=""),
      if_else(RX_ESOFAGO_ALTERADO == 1, "Esofago ", "", missing=""),
      if_else(RX_COLON_ALTERADO == 1, "Colon ", "", missing=""),
      if_else(HOLTER_ALTERADO == 1, "Holter ", "", missing=""),
      if_else(EDA_ALTERADA == 1, "EDA ", "", missing="")
    ),
    
    LISTA_EXAMES_ALTERADOS = str_trim(LISTA_EXAMES_ALTERADOS),
    
    LISTA_EXAMES_ALTERADOS = case_when(
      ALL_NA_EXAMES == 1 ~ "Ignorado/Em branco",
      LISTA_EXAMES_ALTERADOS == "" ~ "Nenhum Alterado",
      TRUE ~ LISTA_EXAMES_ALTERADOS
    ),
    
    N_NAS_SORO = rowSums(is.na(select(., 
                                      EIE_REAGENTE, IFI_REAGENTE, HAI_REAGENTE, 
                                      QUIMIO_REAGENTE, PCR_DETECTAVEL)
    )),
    
    ALL_NA_SORO = if_else(N_NAS_SORO == 5, 1, 0),
    
    QTD_SOROLOGIAS_REAGENTES = if_else(
      ALL_NA_SORO == 1, 
      NA_real_, rowSums(select(., 
                               EIE_REAGENTE, IFI_REAGENTE, HAI_REAGENTE, 
                               QUIMIO_REAGENTE, PCR_DETECTAVEL),
                        na.rm = TRUE)
    ),
    
    LISTA_SOROLOGIAS_REAGENTES = paste0(
      if_else(EIE_REAGENTE == 1, "ELISA ", "", missing=""),
      if_else(IFI_REAGENTE == 1, "IFI ", "", missing=""),
      if_else(HAI_REAGENTE == 1, "HAI ", "", missing=""),
      if_else(QUIMIO_REAGENTE == 1, "Quimio ", "", missing=""),
      if_else(PCR_DETECTAVEL == 1, "PCR ", "", missing="")
    ),
    
    LISTA_SOROLOGIAS_REAGENTES = str_trim(LISTA_SOROLOGIAS_REAGENTES),
    
    LISTA_SOROLOGIAS_REAGENTES = case_when(
      ALL_NA_SORO == 1 ~ "Ignorado/Em branco",
      LISTA_SOROLOGIAS_REAGENTES == "" ~ "Nenhuma Reagente",
      TRUE ~ LISTA_SOROLOGIAS_REAGENTES
    )
  ) 

print(table(CHAGAS_CRONICA_4_TABULADO$TEM_HIPERTENSAO, useNA = "always"))

head(unique(CHAGAS_CRONICA_4_TABULADO$RESUMO_CLINICO_PRINCIPAIS), 5)

print(table(CHAGAS_CRONICA_4_TABULADO$LISTA_SOROLOGIAS_REAGENTES, useNA = "always"))

head(unique(CHAGAS_CRONICA_4_TABULADO$LISTA_SOROLOGIAS_REAGENTES), 5)

print(table(CHAGAS_CRONICA_4_TABULADO$LISTA_SOROLOGIAS_REAGENTES, useNA = "always"))

CHAGAS_CRONICA_4_TABULADO <- CHAGAS_CRONICA_4_TABULADO %>%
  select(
    -c(
      HIPERTEN, 
      DIABETES, 
      NEOPLASIA, 
      HIV, 
      LEISHMANIA, 
      HEPATITE, 
      CARDIOPAT, 
      OUT_COMORB,
      COMORBID, 
      COMORBID__, 
      ESP_COMORB, 
      OUTRO_QUAL,
      
      ELETROCARD, 
      ECOCARDIO, 
      RX_TORAX, 
      RX_ESOFAGO, 
      RX_COLON,
      EXAME_DESC, 
      OUTRO_EXAM, 
      
      EIE_IGG, 
      IFI_IGG, 
      HAI_IGG, 
      QUIMIO_IGG, 
      PCR,
      
      TEXTO_COMORB, 
      STATUS_TEXTO_VALIDO,
      TEXTO_EXAME,
      N_NAS_COMORB, 
      ALL_NA_COMORB,
      N_NAS_EXAMES, 
      ALL_NA_EXAMES,
      N_NAS_SORO, 
      ALL_NA_SORO
    )
  )

ncol(CHAGAS_CRONICA_4_TABULADO)

print(names(CHAGAS_CRONICA_4_TABULADO))

CHAGAS_CRONICA_5_TABULADO <- CHAGAS_CRONICA_4_TABULADO %>%
  mutate(
    AC_NOT_FINAL = case_when(
      str_detect(AC_NOT, regex("Sim", ignore_case = TRUE)) ~ "Sim",
      
      str_detect(AC_NOT, regex("Nao|Não", ignore_case = TRUE)) ~ "Não",
      
      is.na(AC_NOT) | AC_NOT == "" | AC_NOT == "9" ~ "Ignorado/Em branco",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    AC_NOT_FINAL = factor(AC_NOT_FINAL, levels = c("Sim", "Não", "Ignorado/Em branco"))
  )

tabela_AC_NOT <- CHAGAS_CRONICA_5_TABULADO %>%
  count(AC_NOT_FINAL) %>%
  mutate(
    Porcentagem = n / sum(n) * 100,
    Porcentagem_Formatada = sprintf("%.1f%%", Porcentagem)
  )

CHAGAS_CRONICA_5_TABULADO <- CHAGAS_CRONICA_5_TABULADO %>%
  mutate(
    COMU_TRAD_FINAL = case_when(
      str_detect(COMU_TRAD, regex("Sim", ignore_case = TRUE)) ~ "Sim",
      
      str_detect(COMU_TRAD, regex("Nao|Não", ignore_case = TRUE)) ~ "Não",
      
      is.na(COMU_TRAD) | COMU_TRAD == "" | COMU_TRAD == "9" ~ "Ignorado/Em branco",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    COMU_TRAD_FINAL = factor(COMU_TRAD_FINAL, levels = c("Sim", "Não", "Ignorado/Em branco"))
  )

tabela_COMU_TRAD <- CHAGAS_CRONICA_5_TABULADO %>%
  count(COMU_TRAD_FINAL) %>%
  mutate(
    Porcentagem = n / sum(n) * 100,
    Porcentagem_Formatada = sprintf("%.2f%%", Porcentagem) 
  )

print(tabela_COMU_TRAD)

CHAGAS_CRONICA_5_TABULADO <- CHAGAS_CRONICA_5_TABULADO %>%
  mutate(
    
    REATIVACAO_FINAL = case_when(
      str_detect(REATIVACAO, regex("Sim", ignore_case = TRUE)) ~ "Sim",
      str_detect(REATIVACAO, regex("Nao|Não", ignore_case = TRUE)) ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    ),
    REATIVACAO_FINAL = factor(REATIVACAO_FINAL, levels = c("Sim", "Não", "Ignorado/Em branco")),
    
    HIST_BNZ_FINAL = case_when(
      str_detect(HIST_BNZ, regex("Sim", ignore_case = TRUE)) ~ "Sim",
      str_detect(HIST_BNZ, regex("Nao|Não", ignore_case = TRUE)) ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    ),
    HIST_BNZ_FINAL = factor(HIST_BNZ_FINAL, levels = c("Sim", "Não", "Ignorado/Em branco")),
    
    BUSCAATIVA_FINAL = case_when(
      str_detect(BUSCAATIVA, regex("Sim", ignore_case = TRUE)) ~ "Sim",
      str_detect(BUSCAATIVA, regex("Nao|Não", ignore_case = TRUE)) ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    ),
    BUSCAATIVA_FINAL = factor(BUSCAATIVA_FINAL, levels = c("Sim", "Não", "Ignorado/Em branco"))
  )

print(table(CHAGAS_CRONICA_5_TABULADO$REATIVACAO_FINAL, useNA = "ifany"))

print(table(CHAGAS_CRONICA_5_TABULADO$HIST_BNZ_FINAL, useNA = "ifany"))

print(table(CHAGAS_CRONICA_5_TABULADO$BUSCAATIVA_FINAL, useNA = "ifany"))

CHAGAS_CRONICA_6_TABULADO <- CHAGAS_CRONICA_5_TABULADO %>%
  mutate(
    
    TRAT_BNZ_FINAL = case_when(
      str_detect(TRAT_BNZ, regex("Sim", ignore_case = TRUE)) ~ "Sim",
      str_detect(TRAT_BNZ, regex("Nao|Não", ignore_case = TRUE)) ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    TRAT_BNZ_FINAL = factor(TRAT_BNZ_FINAL, levels = c("Sim", "Não", "Ignorado/Em branco")),
    
    TRAT_NFX_FINAL = case_when(
      str_detect(TRAT_NFX, regex("Sim", ignore_case = TRUE)) ~ "Sim",
      str_detect(TRAT_NFX, regex("Nao|Não", ignore_case = TRUE)) ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    TRAT_NFX_FINAL = factor(TRAT_NFX_FINAL, levels = c("Sim", "Não", "Ignorado/Em branco")),
    
    USOU_MEDICAMENTO = case_when(
      TRAT_BNZ_FINAL %in% "Sim" | HIST_BNZ_FINAL %in% "Sim" ~ "Sim, Benznidazol",
      TRAT_NFX_FINAL %in% "Sim" ~ "Sim, Nifurtimox",
      TRAT_BNZ_FINAL == "Não" & TRAT_NFX_FINAL == "Não" & HIST_BNZ_FINAL != "Sim" ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    USOU_MEDICAMENTO = factor(USOU_MEDICAMENTO, levels = c("Sim, Benznidazol", "Sim, Nifurtimox", "Não", "Ignorado/Em branco"))
  )

print(table(CHAGAS_CRONICA_6_TABULADO$TRAT_BNZ_FINAL, CHAGAS_CRONICA_6_TABULADO$TRAT_NFX_FINAL, dnn = c("BNZ", "NFX")))

tabela_USO_MED <- CHAGAS_CRONICA_6_TABULADO %>%
  count(USOU_MEDICAMENTO) %>%
  mutate(
    Porcentagem = n / sum(n) * 100,
    Porcentagem_Formatada = sprintf("%.1f%%", Porcentagem)
  )

print(tabela_USO_MED)

CHAGAS_CRONICA_6_TABULADO <- CHAGAS_CRONICA_6_TABULADO %>%
  select(
    
    -AC_NOT,        
    -COMU_TRAD,     
    -REATIVACAO,    
    -HIST_BNZ,      
    -BUSCAATIVA,    
    -TRAT_BNZ,      
    -TRAT_NFX,      
  )

glimpse(CHAGAS_CRONICA_6_TABULADO)

CHAGAS_CRONICA_6_TABULADO <- CHAGAS_CRONICA_6_TABULADO %>% 
  mutate(
    
    TEMP_N_HISTORICO = parse_number(DIAG_FAMIL), 
    TEMP_N_EXAMES    = parse_number(EXAM_FAMIL), 
    TEMP_N_POSITIVOS = parse_number(CONF_FAMIL), 
    
    FAMILIA_HISTORICO = case_when(
      BUSCAATIVA_FINAL != "Sim" ~ NA_character_,
      TEMP_N_HISTORICO > 0 ~ "Sim",             
      TEMP_N_HISTORICO == 0 ~ "Não",            
      TRUE ~ "Ignorado/Em branco"               
    ),
    
    FAMILIA_EXAME = case_when(
      BUSCAATIVA_FINAL != "Sim" ~ NA_character_,
      TEMP_N_EXAMES > 0 ~ "Sim",
      TEMP_N_EXAMES == 0 ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    FAMILIA_POSITIVO = case_when(
      BUSCAATIVA_FINAL != "Sim" ~ NA_character_,
      TEMP_N_POSITIVOS > 0 ~ "Sim",
      TEMP_N_POSITIVOS == 0 ~ "Não",
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%
  
  mutate(
    FAMILIA_HISTORICO = factor(FAMILIA_HISTORICO, levels = c("Sim", "Não", "Ignorado/Em branco")),
    FAMILIA_EXAME     = factor(FAMILIA_EXAME, levels = c("Sim", "Não", "Ignorado/Em branco")),
    FAMILIA_POSITIVO  = factor(FAMILIA_POSITIVO, levels = c("Sim", "Não", "Ignorado/Em branco"))
  ) %>%
  
  select(
    -TEMP_N_HISTORICO, 
    -TEMP_N_EXAMES, 
    -TEMP_N_POSITIVOS,
    
    -DIAG_FAMIL,
    -EXAM_FAMIL,
    -CONF_FAMIL
  )

print(table(CHAGAS_CRONICA_6_TABULADO$FAMILIA_HISTORICO))

print(table(CHAGAS_CRONICA_6_TABULADO$FAMILIA_EXAME))

print(table(CHAGAS_CRONICA_6_TABULADO$FAMILIA_POSITIVO))

CHAGAS_CRONICA_7_TABULADO <- CHAGAS_CRONICA_6_TABULADO %>%
  mutate(
    MO_SUSPEIT_FINAL = case_when(
      
      str_detect(MO_SUSPEIT, "Rastreamento/Busca ativa") ~ "Rastreamento/Busca Ativa",
      
      str_detect(MO_SUSPEIT, "Demanda espontanea.*Hospital") ~ "Demanda Espontânea: Hospital/Especializado",
      str_detect(MO_SUSPEIT, "Demanda espontanea.*UBS") ~ "Demanda Espontânea: UBS",
      
      str_detect(MO_SUSPEIT, "Banco de sangue") ~ "Outros",
      str_detect(MO_SUSPEIT, "Triagem para doador") ~ "Outros",
      str_detect(MO_SUSPEIT, "Receptor em transplante") ~ "Outros",
      
      str_detect(MO_SUSPEIT, "Pre-Natal") ~ "Outros",
      str_detect(MO_SUSPEIT, "Servicos de assistencia em HIV") ~ "Outros",
      str_detect(MO_SUSPEIT, "Comite investigacao") ~ "Comitê de Óbito",
      
      str_detect(MO_SUSPEIT, "Busca ativa") ~ "Rastreamento/Busca Ativa",
      
      str_detect(MO_SUSPEIT, "Outros") ~ "Outros",
      
      TRUE ~ "Ignorado/Em branco"
    )
  ) %>%
  
  mutate(
    MO_SUSPEIT_FINAL = fct_infreq(MO_SUSPEIT_FINAL)
  ) %>%
  
  select(-MO_SUSPEIT)

print(table(CHAGAS_CRONICA_7_TABULADO$MO_SUSPEIT_FINAL))

CHAGAS_CRONICA_7_TABULADO <- CHAGAS_CRONICA_7_TABULADO %>%
  mutate(
    HOSP_ESP_FINAL = case_when(
      
      str_detect(MO_SUSPEIT_FINAL, regex("Obito|Óbito", ignore_case = TRUE)) ~ "Não se aplica",
      
      str_detect(HOSP_ESP, regex("Sim", ignore_case = TRUE)) ~ "Sim",
      str_detect(HOSP_ESP, regex("Nao|Não", ignore_case = TRUE)) ~ "Não",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    HOSP_ESP_FINAL = factor(HOSP_ESP_FINAL, levels = c("Sim", "Não", "Ignorado/Em branco", "Não se aplica")),
  )

print(table(CHAGAS_CRONICA_7_TABULADO$HOSP_ESP_FINAL))

CHAGAS_CRONICA_7_TABULADO <- CHAGAS_CRONICA_7_TABULADO %>%
  mutate(
    
    TEMP_N_HIST_EPID = parse_number(HIST_EPID),
    
    FAMILIA_HISTORICO_DIAG = case_when(
      TEMP_N_HIST_EPID > 0 ~ "Sim",          
      TEMP_N_HIST_EPID == 0 ~ "Não",         
      
      
      is.na(TEMP_N_HIST_EPID) | 
        str_detect(HIST_EPID, regex("Sem informa|Ignorado", ignore_case = TRUE)) ~ "Ignorado/Em branco",
      
      TRUE ~ "Ignorado/Em branco"
    ),
    
    FAMILIA_HISTORICO_DIAG = factor(FAMILIA_HISTORICO_DIAG, levels = c("Sim", "Não", "Ignorado/Em branco"))
  ) %>%
  
  select(-HIST_EPID, -TEMP_N_HIST_EPID, -HIST_EPID)

print(table(CHAGAS_CRONICA_7_TABULADO$FAMILIA_HISTORICO_DIAG))

CHAGAS_CRONICA_7_TABULADO <- CHAGAS_CRONICA_7_TABULADO %>%
  mutate(
    
    CODIGO_CBO = parse_number(ID_OCUPA_N),
    
    PERFIL_OCUPACIONAL = case_when(
      
      CODIGO_CBO >= 6000 & CODIGO_CBO < 7000 ~ "Agropecuário/Rural",
      
      (CODIGO_CBO >= 5000 & CODIGO_CBO < 6000) |
        (CODIGO_CBO >= 7000 & CODIGO_CBO < 10000) ~ "Manual Urbano/Serviços/Industrial",
      
      CODIGO_CBO >= 1000 & CODIGO_CBO < 5000 ~ "Não Manual/Intelectual/Técnico",
      
      CODIGO_CBO >= 100 & CODIGO_CBO < 400 ~ "Membros das Forças Armadas/Polícia",
      
      is.na(CODIGO_CBO) | CODIGO_CBO == 0 | CODIGO_CBO == 9999 ~ "Ignorado/Em branco",
      
      TRUE ~ "Outros/Mal Definidos" 
    ),
    
    PERFIL_OCUPACIONAL = factor(PERFIL_OCUPACIONAL, levels = c(
      "Agropecuário/Rural",
      "Manual Urbano/Serviços/Industrial",
      "Não Manual/Intelectual/Técnico",
      "Membros das Forças Armadas/Polícia",
      "Outros/Mal Definidos",
      "Ignorado/Em branco"
    )),
    
    FORMA_CLINICA = case_when(
      
      str_detect(FORMA, regex("Cardiaca", ignore_case = TRUE)) & 
        !str_detect(FORMA, regex("Cardiodigestiva|Mista", ignore_case = TRUE)) ~ "Cardíaca",
      
      str_detect(FORMA, regex("Cardiodigestiva|Mista", ignore_case = TRUE)) ~ "Mista (Cardíaca + Digestiva)",
      
      str_detect(FORMA, regex("Digestiva", ignore_case = TRUE)) & 
        !str_detect(FORMA, regex("Cardiodigestiva", ignore_case = TRUE)) ~ "Digestiva",
      
      str_detect(FORMA, regex("Nervosa", ignore_case = TRUE)) ~ "Nervosa",
      
      str_detect(FORMA, regex("Indeterminada|investiga", ignore_case = TRUE)) ~ "Não Especificada/Indeterminada",
      
      is.na(FORMA) | FORMA == "" ~ "Ignorado/Em branco",
      
      TRUE ~ "Ignorado/Em branco" 
    ),
    
    FORMA_CLINICA = factor(FORMA_CLINICA, levels = c(
      "Cardíaca", 
      "Digestiva", 
      "Mista (Cardíaca + Digestiva)", 
      "Nervosa",
      "Não Especificada/Indeterminada",
      "Ignorado/Em branco"             
    )),
    
    GRAVIDADE_CARDIACA = case_when(
      str_detect(FORMA, regex("Cardiaca leve/moderada", ignore_case = TRUE)) ~ "Leve/Moderada",
      str_detect(FORMA, regex("Cardiaca avancada", ignore_case = TRUE)) ~ "Avançada",
      TRUE ~ NA_character_ 
    ),
    
    GRAVIDADE_CARDIACA = factor(GRAVIDADE_CARDIACA, levels = c(
      "Leve/Moderada", 
      "Avançada"
    ))
  ) %>%
  
  select(-ID_OCUPA_N, -FORMA, -CODIGO_CBO)

print(table(CHAGAS_CRONICA_7_TABULADO$FORMA_CLINICA, useNA = "ifany"))

print(table(CHAGAS_CRONICA_7_TABULADO$GRAVIDADE_CARDIACA, useNA = "ifany"))

print(table(CHAGAS_CRONICA_7_TABULADO$FORMA_CLINICA, CHAGAS_CRONICA_7_TABULADO$GRAVIDADE_CARDIACA, useNA = "ifany"))

print(table(CHAGAS_CRONICA_7_TABULADO$PERFIL_OCUPACIONAL, useNA = "ifany"))

CHAGAS_CRONICA_FINAL <- CHAGAS_CRONICA_7_TABULADO %>%
  mutate(
    
    ST_ENCERRA_FINAL = case_when(
      
      str_detect(ST_ENCERRA, regex("Obito por d. Chagas", ignore_case = TRUE)) ~ "Óbito por Doença de Chagas",
      
      str_detect(ST_ENCERRA, regex("Obito por outras causas", ignore_case = TRUE)) ~ "Óbito por outras causas",
      
      str_detect(ST_ENCERRA, regex("Permanece em acompanhamento", ignore_case = TRUE)) ~ "Em acompanhamento",
      
      str_detect(ST_ENCERRA, regex("Abandono", ignore_case = TRUE)) ~ "Abandono",
      
      str_detect(ST_ENCERRA, regex("Em aberto", ignore_case = TRUE)) ~ "Em aberto",
      
      is.na(ST_ENCERRA) | ST_ENCERRA == "" ~ "Ignorado/Em branco",
      
      TRUE ~ "Outros"
    ),
    
    ST_ENCERRA_FINAL = factor(ST_ENCERRA_FINAL, levels = c(
      "Em acompanhamento",
      "Óbito por Doença de Chagas",
      "Óbito por outras causas",
      "Em aberto",
      "Abandono",
      "Ignorado/Em branco"
    ))
    
  ) %>%
  
  select(-ST_ENCERRA)

print(table(CHAGAS_CRONICA_FINAL$ST_ENCERRA_FINAL, useNA = "ifany"))

CHAGAS_CRONICA_2_FINAL <- CHAGAS_CRONICA_FINAL %>%
  mutate(
    ANO_DIAG_CAT = case_when(
      ANO_DIAG < 2000 ~ "Antes de 2000",
      ANO_DIAG >= 2000 & ANO_DIAG <= 2009 ~ "2000 a 2009",
      ANO_DIAG >= 2010 & ANO_DIAG <= 2019 ~ "2010 a 2019",
      ANO_DIAG >= 2020 ~ "2020 em diante",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    ANO_DIAG_CAT = factor(ANO_DIAG_CAT, levels = c(
      "Antes de 2000", "2000 a 2009", "2010 a 2019", "2020 em diante", "Ignorado/Em branco"
    )),
    
    DIF_DIAG_NOTIF_CAT = case_when(
      DIF_DIAG_NOTIF == 0 ~ "Notificação Oportuna (Mesmo ano)",
      DIF_DIAG_NOTIF == 1 ~ "Notificação Recente (1 ano)",
      DIF_DIAG_NOTIF > 1 & DIF_DIAG_NOTIF <= 10 ~ "Notificação Tardia (2 a 10 anos)",
      DIF_DIAG_NOTIF > 10 ~ "Notificação Histórica (> 10 anos)",
      TRUE ~ "Ignorado/Em branco"
    ),
    
    DIF_DIAG_NOTIF_CAT = factor(DIF_DIAG_NOTIF_CAT, levels = c(
      "Notificação Oportuna (Mesmo ano)", "Notificação Recente (1 ano)", 
      "Notificação Tardia (2 a 10 anos)", "Notificação Histórica (> 10 anos)", "Ignorado/Em branco"
    )),
    
    QTD_EXAMES_CAT = case_when(
      is.na(QTD_EXAMES_ALTERADOS) ~ "Ignorado/Em branco",
      QTD_EXAMES_ALTERADOS == 0 ~ "Nenhum exame alterado",
      QTD_EXAMES_ALTERADOS == 1 ~ "Um alterado",
      QTD_EXAMES_ALTERADOS == 2 ~ "Dois alterados",
      QTD_EXAMES_ALTERADOS == 3 ~ "Três alterados",
      QTD_EXAMES_ALTERADOS >= 4 ~ "Quatro ou mais alterados",
    ),
    
    QTD_EXAMES_CAT = factor(QTD_EXAMES_CAT, levels = c(
      "Nenhum exame alterado", "Um alterado", "Dois alterados", 
      "Três alterados", "Quatro ou mais alterados", "Ignorado/Em branco"
    )),
    
    QTD_SORO_CAT = case_when(
      is.na(QTD_SOROLOGIAS_REAGENTES) ~ "Ignorado/Em branco",
      QTD_SOROLOGIAS_REAGENTES == 0 ~ "Nenhuma sorologia alterada",
      QTD_SOROLOGIAS_REAGENTES == 1 ~ "Uma alterada",
      QTD_SOROLOGIAS_REAGENTES == 2 ~ "Duas alteradas",
      QTD_SOROLOGIAS_REAGENTES == 3 ~ "Três alteradas",
      QTD_SOROLOGIAS_REAGENTES >= 4 ~ "Quatro ou mais alteradas",
    ),
    
    QTD_SORO_CAT = factor(QTD_SORO_CAT, levels = c(
      "Nenhuma sorologia alterada", "Uma alterada", "Duas alteradas", 
      "Três alteradas", "Quatro ou mais alteradas", "Ignorado/Em branco"))
  )

table(CHAGAS_CRONICA_2_FINAL$ANO_DIAG_CAT, useNA = "ifany")

table(CHAGAS_CRONICA_2_FINAL$DIF_DIAG_NOTIF_CAT, useNA = "ifany")

table(CHAGAS_CRONICA_2_FINAL$QTD_EXAMES_CAT, useNA = "ifany")

table(CHAGAS_CRONICA_2_FINAL$QTD_SORO_CAT, useNA = "ifany")

print(table(CHAGAS_CRONICA_2_TABULADO$FAIXA_ETARIA))

caminho_dados <- paste0(file.path(DIR_DADOS, "IBGE/IBGE_POP_2001_2024"), "/")

pop_padrao_br2022 <- readRDS(paste0(caminho_dados, "pop_padrao_br2022.rds"))

pop_ibge <- readRDS(paste0(caminho_dados, "POP_IBGE_MUNICIPIO_TRATADA.rds")) %>% 
  rename(NU_ANO = ANO_OBITO) %>%
  filter(NU_ANO == 2023) %>%
  mutate(COD_MUN = as.character(substr(COD_MUN, 1, 6)))

pop_municipio_2023 <- pop_ibge %>%
  group_by(COD_MUN, FAIXA_QUINQUENAL) %>%
  summarise(populacao_residente = sum(populacao_residente, na.rm = TRUE), .groups = "drop")

casos_municipio <- CHAGAS_CRONICA_2_FINAL %>%
  filter(!is.na(IDADE_FINAL), ANO_NOTIFICACAO == 2023, !is.na(CD_MN_RESI)) %>% 
  mutate(
    COD_MUN = as.character(substr(CD_MN_RESI, 1, 6)),
    FAIXA_QUINQUENAL = cut(IDADE_FINAL, 
                           breaks = c(seq(0, 80, by = 5), Inf),
                           labels = pop_padrao_br2022$FAIXA_QUINQUENAL, 
                           right = FALSE)
  ) %>%
  group_by(COD_MUN, FAIXA_QUINQUENAL) %>%
  summarise(total_casos = n(), .groups = "drop")

dados_combinados <- pop_municipio_2023 %>%
  left_join(casos_municipio, by = c("COD_MUN", "FAIXA_QUINQUENAL")) %>%
  mutate(total_casos = replace_na(total_casos, 0)) %>%
  left_join(pop_padrao_br2022, by = "FAIXA_QUINQUENAL")

taxas_municipais <- dados_combinados %>%
  mutate(
    taxa_especifica = if_else(populacao_residente == 0, 0, total_casos / populacao_residente),
    casos_esperados_padrao = taxa_especifica * peso_pop_padrao 
  ) %>%
  group_by(COD_MUN) %>%
  summarise(
    total_casos_mun = sum(total_casos),     
    pop_mun         = sum(populacao_residente), 
    taxa_bruta       = (total_casos_mun / pop_mun) * 100000,
    taxa_padronizada = sum(casos_esperados_padrao, na.rm = TRUE),
    .groups = "drop"
  )

dir_shapefile <- file.path(DIR_DADOS, "Análises/Parte 4 - Análise Espacial/Shapes") 
setwd(dir_shapefile)

malha_brasil <- st_read("BR_Municipios_2024.shp", quiet = TRUE) %>%
  mutate(COD_MUN = as.character(substr(CD_MUN, 1, 6)))

mapa_taxas <- malha_brasil %>%
  left_join(taxas_municipais, by = "COD_MUN") %>%
  mutate(
    total_casos_mun  = replace_na(total_casos_mun, 0),
    pop_mun          = replace_na(pop_mun, 0),
    taxa_bruta       = replace_na(taxa_bruta, 0),
    taxa_padronizada = replace_na(taxa_padronizada, 0)
  )

mapa_valido <- mapa_taxas %>% 
  filter(!st_is_empty(st_geometry(mapa_taxas)) & pop_mun > 0)

mapa_valido <- mapa_valido %>%
  mutate(
    casos_padronizados_esperados = (taxa_padronizada * pop_mun) / 100000
  )

res_bayes_global <- EBest(
  n = mapa_valido$casos_padronizados_esperados,  
  x = mapa_valido$pop_mun 
)

mapa_valido$taxa_padronizada_suavizada_global <- res_bayes_global$est * 100000

taxas_finais <- mapa_valido %>%
  st_drop_geometry() %>%
  select(codigo_municipio = COD_MUN, taxa_padronizada_suavizada_global) %>%
  mutate(codigo_municipio = as.numeric(codigo_municipio))

dir_etapa6 <- file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo")
setwd(dir_etapa6)

df_etapa6 <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Cronica.csv", show_col_types = FALSE)

df_etapa6_atualizado <- df_etapa6 %>%
  mutate(chave_6_digitos = as.numeric(substr(as.character(codigo_municipio), 1, 6))) %>%
  left_join(taxas_finais %>% rename(chave_6_digitos = codigo_municipio), by = "chave_6_digitos") %>%
  select(-chave_6_digitos) %>%
  select(-any_of(c("cronica_taxa_padronizada_suavizada", "taxa_padronizada_suavizada"))) %>%
  rename(cronica_taxa_padronizada_suavizada = taxa_padronizada_suavizada_global)

write_csv(df_etapa6_atualizado, "Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Cronica_global.csv")