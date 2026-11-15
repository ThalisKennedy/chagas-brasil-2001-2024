# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SINAN/CHAGBR 2001-2024/CHAGBR 2001-2024 - Parte 1.2.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SINAN/CHAGBR 2001-2024"))

library(tidyverse)
library(writexl)
library(tidyr)
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
library(rlang)
library(dplyr)


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
          
        FAIXA_ETARIA_GRUPO = case_when(
        IDADE_FINAL >= 0 & IDADE_FINAL <= 19  ~ "0 a 19 anos",
        IDADE_FINAL >= 20 & IDADE_FINAL <= 59  ~ "20 a 59 anos",
        IDADE_FINAL >= 60 ~ "60 anos ou mais",
        TRUE ~ NA_character_
      ))

print(table(CHAGAS_AGUDA_FILTRADO_2001_2024$FAIXA_ETARIA_GRUPO, useNA = "ifany"))

table(CHAGAS_AGUDA_FILTRADO_2001_2024$USOU_MEDICAMENTO, useNA = "always")

class(CHAGAS_AGUDA_FILTRADO_2001_2024$USOU_MEDICAMENTO)

casos_sem_confirmacao <- CHAGAS_AGUDA_2_2001_2024 %>%
  filter(IS_CONFIRMADO != "1") %>%
  nrow()
print(paste("Casos Sem Confirmação:", casos_sem_confirmacao))

idade_nao_preenchida <- CHAGAS_AGUDA_FILTRADO_2001_2024 %>%
  filter(is.na(IDADE_FINAL) | IDADE_FINAL == "" | IDADE_FINAL == "Ignorado/Em branco") %>%
  nrow()
print(paste("Idade Não Preenchida:", idade_nao_preenchida))

total_casos_final <- nrow(CHAGAS_AGUDA_FILTRADO_2001_2024) - idade_nao_preenchida
print(paste("Total de Casos Final:", total_casos_final))

vars_qualidade <- c("NU_ANO", "SEXO_FINAL", "RACACOR_FINAL", "FAIXA_ETARIA", 
                    "ESCOLARIDADE_FINAL", "PERFIL_OCUPACIONAL", "OPORTUNIDADE_NOTIFICACAO",
                    "REGIAO_RESIDENCIA", "CD_MN_RESI", "FLUXO_ASSISTENCIA", "LOCAL_INFECCAO_FINAL",
                    "PRESENCA_FINAL", "TPAUTOCTO_FINAL_2007", "MODO_INFECCAO_FINAL", "ASSINTOMA_FINAL",
                    "USOU_MEDICAMENTO", "DROGA_FINAL", "CRITERIO_FINAL", "EVOLUCAO_FINAL",
                    "QTD_SINTOMAS_CAT", "QTD_SORO_CAT", "QTD_PARASITO_CAT", "QTD_MEDIDAS_CAT")

vars_analise <- setdiff(vars_qualidade, "NU_ANO")

avaliar_completude <- function(df, variaveis) {
  
  df %>%
    
    select(all_of(variaveis)) %>%
    
    summarise(across(everything(), ~ {
      
      total <- length(.x) 
      
      total_incompletos <- sum(as.character(.x) %in% c("Ignorado/Em branco", "Ignorado", "") | is.na(.x), na.rm = TRUE)
      
      pct_incompleto <- (total_incompletos / total) * 100
      
      pct_completo <- 100 - pct_incompleto
      
      classificacao <- case_when(
        pct_completo >= 95 ~ "Excelente",
        pct_completo >= 90 & pct_completo < 95 ~ "Bom",
        pct_completo >= 70 & pct_completo < 90 ~ "Regular",
        pct_completo >= 50 & pct_completo < 70 ~ "Ruim",
        TRUE ~ "Muito Ruim"
      )
      
      paste0(round(pct_completo, 1), "% (", classificacao, ")")
      
    })) %>%
    
    pivot_longer(everything(), names_to = "Variável", values_to = "Completitude (%) e Classificação")
}

TABELA_COMPLETUDE <- avaliar_completude(CHAGAS_AGUDA_FILTRADO_2001_2024, vars_qualidade)
# --- A classificação de autoctonia só existe na ficha de 2007-2024: completude e
# tendência calculadas sobre os casos desse período (seção 4.5.1; Tabela 2).
TABELA_COMPLETUDE[TABELA_COMPLETUDE$Variável == "TPAUTOCTO_FINAL_2007", ] <-
  avaliar_completude(filter(CHAGAS_AGUDA_FILTRADO_2001_2024, as.numeric(NU_ANO) >= 2007), "TPAUTOCTO_FINAL_2007")

print(TABELA_COMPLETUDE)

df_completudade_anual <- CHAGAS_AGUDA_FILTRADO_2001_2024 %>%
  
  group_by(NU_ANO) %>%
  
  summarise(across(all_of(vars_analise), ~ {
    
    total <- length(.x)
    
    incompletos <- sum(as.character(.x) %in% c("Ignorado/Em branco", "Ignorado", "") | is.na(.x), na.rm = TRUE)
    
    pct_completo <- ((total - incompletos) / total) * 100
    
    return(pct_completo)
    
  }), .groups = 'drop')

calcular_prais <- function(var_nome) {
  
  df_reg <- df_completudade_anual %>%
    select(ano = NU_ANO, y = !!sym(var_nome)) %>%
    mutate(ano = as.numeric(ano),
           y = ifelse(y == 0, 0.01, y),
           log_y = log(y))
  
  if (var_nome == "TPAUTOCTO_FINAL_2007") df_reg <- filter(df_reg, ano >= 2007)  # --- ver nota acima

  var_y <- var(df_reg$log_y, na.rm = TRUE)
  if (is.na(var_y) || var_y == 0) {
    return(tibble(Variavel = var_nome, 
                  APC = 0, 
                  IC95 = "(0 a 0)", 
                  P_valor = NA_real_, 
                  Tendencia = "Constante (Dados Insuficientes)"))
  }
  
  modelo <- tryCatch({
    
    prais_winsten(log_y ~ ano, data = as.data.frame(df_reg), index = "ano")
    
  }, error = function(e) return(NULL))
  
  if(is.null(modelo)) {
    
    return(tibble(Variavel = var_nome, APC = NA, IC95 = NA, P_valor = NA, Tendencia = "Erro/Indeterminada"))
  
    }
  
  sumario <- summary(modelo)
  beta <- sumario$coefficients["ano", "Estimate"]
  se <- sumario$coefficients["ano", "Std. Error"]
  p_valor <- sumario$coefficients["ano", "Pr(>|t|)"]
  
  apc <- (exp(beta) - 1) * 100
  ic_inf <- (exp(beta - 1.96 * se) - 1) * 100
  ic_sup <- (exp(beta + 1.96 * se) - 1) * 100
  
  tendencia <- case_when(
    p_valor >= 0.05 ~ "Estacionária",
    ic_inf > 0 & ic_sup > 0 ~ "Crescente",
    ic_inf < 0 & ic_sup < 0 ~ "Decrescente",
    TRUE ~ "Estacionária"
  )
  
  tibble(
    Variavel = var_nome,
    APC = round(apc, 2),
    IC95 = paste0("(", round(ic_inf, 2), " a ", round(ic_sup, 2), ")"),
    P_valor = round(p_valor, 3),
    Tendencia = tendencia
  )
}

TABELA_TENDENCIA <- lapply(vars_analise, calcular_prais) %>% bind_rows()

print(as.data.frame(TABELA_TENDENCIA))

# ==============================================================================
# APÊNDICE B: MAPA DE CALOR DA COMPLETUDE AO LONGO DO TEMPO (SINAN)
# ==============================================================================

df_completudade_anual <- df_completudade_anual %>%
  rowwise() %>%
  mutate(MEDIA_GERAL_BANCO = mean(c_across(all_of(vars_analise)), na.rm = TRUE)) %>%
  ungroup()

# Tendência da média geral de completude ("Média do Banco" das Tabelas 2 e 4).
# Linhas do script "Parte 1" de fevereiro, que gerou as tabelas; o "Parte 1.2"
# só acrescentou o mapa de calor (Figura C1).
resultado_media_banco <- calcular_prais("MEDIA_GERAL_BANCO")
print(resultado_media_banco)

library(tidyverse)
library(ggplot2)

df_heatmap_sinan <- df_completudade_anual %>%
  select(-MEDIA_GERAL_BANCO, -any_of(c("CD_MN_RESI", "CODMUNRES"))) %>% 
  mutate(NU_ANO = as.numeric(NU_ANO)) %>% 
  pivot_longer(
    cols = -NU_ANO, 
    names_to = "Variavel",
    values_to = "Completude"
  ) %>%
  mutate(
    Variavel_Formatada = case_when(
      Variavel == "SEXO_FINAL" ~ "Sexo",
      Variavel == "RACACOR_FINAL" ~ "Raça/Cor",
      Variavel == "FAIXA_ETARIA" ~ "Faixa Etária",
      Variavel == "ESCOLARIDADE_FINAL" ~ "Escolaridade",
      Variavel == "PERFIL_OCUPACIONAL" ~ "Perfil Ocupacional",
      Variavel == "OPORTUNIDADE_NOTIFICACAO" ~ "Tempo de Notificação",
      Variavel == "REGIAO_RESIDENCIA" ~ "Região de Residência",
      Variavel == "FLUXO_ASSISTENCIA" ~ "Fluxo Assistencial",
      Variavel == "LOCAL_INFECCAO_FINAL" ~ "Local Provável de Infecção",
      Variavel == "PRESENCA_FINAL" ~ "Vestígios de Triatomíneos Intradomicílio",
      Variavel == "TPAUTOCTO_FINAL_2007" ~ "Classificação de Autoctonia",
      Variavel == "MODO_INFECCAO_FINAL" ~ "Modo Provável de Infecção",
      Variavel == "ASSINTOMA_FINAL" ~ "Paciente Assintomático",
      Variavel == "USOU_MEDICAMENTO" ~ "Uso de Medicamento",
      Variavel == "DROGA_FINAL" ~ "Medicamento Utilizado",
      Variavel == "CRITERIO_FINAL" ~ "Critério de Confirmação",
      Variavel == "EVOLUCAO_FINAL" ~ "Evolução do Caso",
      Variavel == "QTD_SINTOMAS_CAT" ~ "Quantidade de Sintomas",
      Variavel == "QTD_SORO_CAT" ~ "Quantidade de Sorologias Reagentes",
      Variavel == "QTD_PARASITO_CAT" ~ "Quantidade de Parasitológicos Positivos",
      Variavel == "QTD_MEDIDAS_CAT" ~ "Quantidade de Medidas Tomadas",
      TRUE ~ str_to_title(str_replace_all(Variavel, "_", " "))
    )
  )

grafico_calor_sinan <- ggplot(df_heatmap_sinan, aes(x = NU_ANO, y = fct_rev(Variavel_Formatada), fill = Completude)) +
  geom_tile(color = "white", linewidth = 0.5) + 
  
  scale_fill_gradient(
    low = "#d73027",      # Vermelho
    high = "#1a9850",     # Verde
    limits = c(0, 100),  
    name = "Completude (%)",
    breaks = seq(0, 100, by = 20)
  ) +
  
  scale_x_continuous(breaks = seq(2001, 2024, by = 2)) +
  
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(), 
    legend.position = "right",
    legend.title = element_text(face = "bold")
  ) +
  labs(
    title = "Evolução da Completude das Variáveis de Notificação (SINAN, 2001-2024)",
    x = "Ano de Notificação",
    y = NULL
  )

print(grafico_calor_sinan)

ggsave(
  filename = "Mapa_Calor_Completude_SINAN.png", 
  plot = grafico_calor_sinan, 
  width = 12, 
  height = 8, 
  dpi = 300, 
  bg = "white"
)

# --- Exportação feita no console na execução original (comandos registrados
# no .Rhistory do projeto) ---
writexl::write_xlsx(TABELA_COMPLETUDE, "TABELA_COMPLETUDE_AGUDA.xlsx")
writexl::write_xlsx(TABELA_TENDENCIA, "TABELA_TENDENCIA_AGUDA.xlsx")
