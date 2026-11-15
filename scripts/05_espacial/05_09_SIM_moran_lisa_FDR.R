# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SIM/DOBR - CHAGAS/DOBR - CHAGAS - PARTE 4.2 - MORAN E LISA - GLOBAL FDR.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
# Usa objetos criados por 05_06_SIM_taxas_suavizacao_global.R: rode-o antes, na mesma sessão do R.
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SIM/DOBR - CHAGAS"))

# Versão com correção para comparações múltiplas (FDR, Benjamini-Hochberg) - out/2026

library(spdep)
library(dplyr)
library(sf)

mapa_valido <- mapa_valido %>%
  filter(!is.na(taxa_padronizada_suavizada_global))

vizinhos <- poly2nb(mapa_valido, queen = TRUE)

pesos_espaciais <- nb2listw(vizinhos, style = "W", zero.policy = TRUE)

set.seed(2025)

moran_global <- moran.mc(
  x = mapa_valido$taxa_padronizada_suavizada_global, 
  listw = pesos_espaciais, 
  nsim = 999, 
  zero.policy = TRUE
)

print(moran_global)

moran_local <- localmoran(
  x = mapa_valido$taxa_padronizada_suavizada_global, 
  listw = pesos_espaciais, 
  zero.policy = TRUE
)

variavel_z <- as.numeric(scale(mapa_valido$taxa_padronizada_suavizada_global))
lag_espacial_z <- as.numeric(scale(lag.listw(pesos_espaciais, mapa_valido$taxa_padronizada_suavizada_global, zero.policy = TRUE)))

p_valor_lisa <- moran_local[, 5]

# Correção para comparações múltiplas (FDR, Benjamini-Hochberg)
p_valor_lisa_fdr <- p.adjust(p_valor_lisa, method = "BH")

cat("Municípios sem vizinhos:", sum(card(vizinhos) == 0), "\n")
cat("LISA significativos (p < 0,05) sem correção:", sum(p_valor_lisa < 0.05, na.rm = TRUE), "\n")
cat("LISA significativos (p < 0,05) com FDR:", sum(p_valor_lisa_fdr < 0.05, na.rm = TRUE), "\n")

mapa_valido <- mapa_valido %>%
  mutate(
    lisa_indice = moran_local[, 1],
    lisa_pvalor_bruto = p_valor_lisa,
    lisa_pvalor = p_valor_lisa_fdr,
    
    LISA_CLUSTER = case_when(
      is.na(lisa_pvalor) ~ "Não Significativo",
      lisa_pvalor >= 0.05 ~ "Não Significativo",
      variavel_z > 0 & lag_espacial_z > 0 ~ "Alto-Alto",
      variavel_z < 0 & lag_espacial_z < 0 ~ "Baixo-Baixo",
      variavel_z > 0 & lag_espacial_z < 0 ~ "Alto-Baixo",
      variavel_z < 0 & lag_espacial_z > 0 ~ "Baixo-Alto",
      TRUE ~ "Não Significativo"
    ),
    
    LISA_CLUSTER = factor(LISA_CLUSTER, 
                          levels = c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", "Baixo-Alto", "Não Significativo"))
  )

table(mapa_valido$LISA_CLUSTER)

mapa_valido <- mapa_valido %>%
  mutate(
    LISA_SIGNIFICANCIA = case_when(
      lisa_pvalor <= 0.001 ~ "p < 0,001 (Significância de 99,9%)",
      lisa_pvalor <= 0.01  ~ "p < 0,01 (Significância de 99%)",
      lisa_pvalor <= 0.05  ~ "p < 0,05 (Significância de 95%)",
      TRUE ~ "Não Significativo"
    ),
    
    LISA_SIGNIFICANCIA = factor(LISA_SIGNIFICANCIA,
                                levels = c("p < 0,001 (Significância de 99,9%)", 
                                           "p < 0,01 (Significância de 99%)", 
                                           "p < 0,05 (Significância de 95%)", 
                                           "Não Significativo"))
  )

st_write(
  obj = mapa_valido, 
  dsn = "mapa_chagas_analise_espacial_3_GLOBAL.gpkg", 
  layer = "morbimortalidade_chagas",
  append = FALSE
)