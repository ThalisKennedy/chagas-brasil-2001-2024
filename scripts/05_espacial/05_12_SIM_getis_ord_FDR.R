# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: SIM/DOBR - CHAGAS/DOBR - CHAGAS - PARTE 4.3 - GETIS ORD GI E SATSCAN - GLOBAL FDR.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
# Usa objetos criados por 05_09_SIM_moran_lisa_FDR.R: rode-o antes, na mesma sessão do R.
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "SIM/DOBR - CHAGAS"))

# Versão com correção para comparações múltiplas (FDR, Benjamini-Hochberg) - out/2026

library(spdep)
library(dplyr)
library(sf)

vizinhos_gi <- include.self(vizinhos)

pesos_gi <- nb2listw(vizinhos_gi, style = "W", zero.policy = TRUE)

estatistica_g <- localG(
  x = mapa_valido$taxa_padronizada_suavizada_global,
  listw = pesos_gi, 
  zero.policy = TRUE
)

z_scores_gi <- as.numeric(estatistica_g)

mapa_valido <- mapa_valido %>%
  mutate(
    GETIS_Z_SCORE = z_scores_gi,
    
    GETIS_P_BRUTO = 2 * pnorm(-abs(GETIS_Z_SCORE)),
    
    # Correção para comparações múltiplas (FDR, Benjamini-Hochberg)
    GETIS_P_VALOR = p.adjust(GETIS_P_BRUTO, method = "BH"),
    
    GETIS_HOTSPOT = case_when(
      GETIS_Z_SCORE > 0 & GETIS_P_VALOR <= 0.001 ~ "Hotspot com 99,9% de Confiança",
      GETIS_Z_SCORE > 0 & GETIS_P_VALOR <= 0.01  ~ "Hotspot com 99% de Confiança",
      GETIS_Z_SCORE > 0 & GETIS_P_VALOR <= 0.05  ~ "Hotspot com 95% de Confiança",
      GETIS_Z_SCORE < 0 & GETIS_P_VALOR <= 0.001 ~ "Coldspot com 99,9% de Confiança",
      GETIS_Z_SCORE < 0 & GETIS_P_VALOR <= 0.01  ~ "Coldspot com 99% de Confiança",
      GETIS_Z_SCORE < 0 & GETIS_P_VALOR <= 0.05  ~ "Coldspot com 95% de Confiança",
      TRUE ~ "Não Significativo"
    ),
    GETIS_HOTSPOT = factor(GETIS_HOTSPOT, 
                           levels = c("Hotspot com 99,9% de Confiança", 
                                      "Hotspot com 99% de Confiança", 
                                      "Hotspot com 95% de Confiança", 
                                      "Não Significativo", 
                                      "Coldspot com 95% de Confiança", 
                                      "Coldspot com 99% de Confiança",
                                      "Coldspot com 99,9% de Confiança")),
    
    GETIS_SIGNIFICANCIA = case_when(
      GETIS_P_VALOR <= 0.001 ~ "Confiança de 99,9%(p<0,001)",
      GETIS_P_VALOR <= 0.01  ~ "Confiança de 99,0%(p<0,01)",
      GETIS_P_VALOR <= 0.05  ~ "Confiança de 95,0%(p<0,05)",
      TRUE ~ "Não Significante"
    ),
    GETIS_SIGNIFICANCIA = factor(GETIS_SIGNIFICANCIA,
                                 levels = c("Confiança de 99,9%(p<0,001)",
                                            "Confiança de 99,0%(p<0,01)",
                                            "Confiança de 95,0%(p<0,05)",
                                            "Não Significante"))
  )

print(table(mapa_valido$GETIS_HOTSPOT))
print(table(mapa_valido$GETIS_SIGNIFICANCIA))

cat("Gi* significativos (p <= 0,05) sem correção:", sum(mapa_valido$GETIS_P_BRUTO <= 0.05, na.rm = TRUE), "\n")
cat("Gi* significativos (p <= 0,05) com FDR:", sum(mapa_valido$GETIS_P_VALOR <= 0.05, na.rm = TRUE), "\n")

caminho_saida <- file.path(DIR_DADOS, "SIM/DOBR - CHAGAS")
setwd(caminho_saida)

st_write(
  obj = mapa_valido, 
  dsn = "mapa_chagas_analise_espacial_4.3_GLOBAL.gpkg", 
  layer = "morbimortalidade_chagas",
  append = FALSE 
)

mapa_wgs84 <- st_transform(mapa_valido, 4326)
centroides <- st_centroid(mapa_wgs84)
coords <- st_coordinates(centroides)

base_satscan <- mapa_valido %>%
  st_drop_geometry() %>%
  mutate(
    LAT = coords[, 2],
    LON = coords[, 1],
    ANO = 2024 
  )

satscan_casos <- base_satscan %>%
  select(COD_MUN, total_obitos_mun, ANO) %>%
  mutate(total_obitos_mun = replace_na(total_obitos_mun, 0))

write.table(satscan_casos, "chagas_obitos.cas", 
            row.names = FALSE, col.names = FALSE, sep = " ", dec = ".", quote = FALSE)

satscan_pop <- base_satscan %>%
  select(COD_MUN, ANO, obitos_padronizados_esperados) %>%
  mutate(obitos_padronizados_esperados = round(obitos_padronizados_esperados, 4))

write.table(satscan_pop, "chagas_esperados.pop", 
            row.names = FALSE, col.names = FALSE, sep = " ", dec = ".", quote = FALSE)

satscan_geo <- base_satscan %>%
  select(COD_MUN, LAT, LON)

write.table(satscan_geo, "chagas_coordenadas.geo", 
            row.names = FALSE, col.names = FALSE, sep = " ", dec = ".", quote = FALSE)