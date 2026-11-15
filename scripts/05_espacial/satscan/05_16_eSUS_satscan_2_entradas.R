# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: E-SUS/DCCRBR23/DCCRBR23 - PARTE 4.2 - GETIS ORD GIS E SATSCAN.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
# Usa objetos criados por 05_15_eSUS_satscan_1_moran_lisa.R: rode-o antes, na mesma sessão do R.
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "E-SUS/DCCRBR23"))

library(spdep)
library(dplyr)
library(sf)

vizinhos_gi <- include.self(vizinhos)

pesos_gi <- nb2listw(vizinhos_gi, style = "W", zero.policy = TRUE)

estatistica_g <- localG(
  x = mapa_valido$taxa_padronizada_suavizada, 
  listw = pesos_gi, 
  zero.policy = TRUE
)

z_scores_gi <- as.numeric(estatistica_g)

mapa_valido <- mapa_valido %>%
  mutate(
    GETIS_Z_SCORE = z_scores_gi,
    
    GETIS_P_VALOR = 2 * pnorm(-abs(GETIS_Z_SCORE)),
    
    GETIS_HOTSPOT = case_when(
      GETIS_Z_SCORE >= 3.29  ~ "Hotspot com 99,9% de Confiança",
      GETIS_Z_SCORE >= 2.58  & GETIS_Z_SCORE < 3.29 ~ "Hotspot com 99% de Confiança",
      GETIS_Z_SCORE >= 1.96  & GETIS_Z_SCORE < 2.58 ~ "Hotspot com 95% de Confiança",
      GETIS_Z_SCORE <= -3.29 ~ "Coldspot com 99,9% de Confiança",
      GETIS_Z_SCORE <= -2.58 & GETIS_Z_SCORE > -3.29 ~ "Coldspot com 99% de Confiança",
      GETIS_Z_SCORE <= -1.96 & GETIS_Z_SCORE > -2.58 ~ "Coldspot com 95% de Confiança",
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

st_write(
  obj = mapa_valido, 
  dsn = "mapa_chagas_cronica_espacial_completo.gpkg", 
  layer = "morbi_chagas_cronica",
  delete_dsn = TRUE
)

# SAT SCAN

mapa_wgs84 <- st_transform(mapa_valido, 4326)
centroides <- st_centroid(mapa_wgs84)
coords <- st_coordinates(centroides)

base_satscan <- mapa_valido %>%
  st_drop_geometry() %>%
  mutate(
    LAT = coords[, 2],
    LON = coords[, 1],
    ANO = 2023
  )

satscan_casos <- base_satscan %>%
  select(COD_MUN, total_casos_mun, ANO) %>%
  mutate(total_casos_mun = replace_na(total_casos_mun, 0))

write.table(satscan_casos, "chagas_cronica_casos.cas", 
            row.names = FALSE, col.names = FALSE, sep = " ", dec = ".", quote = FALSE)

satscan_pop <- base_satscan %>%
  select(COD_MUN, ANO, pop_mun) %>% 
  mutate(pop_mun = round(pop_mun, 0))

write.table(satscan_pop, "chagas_cronica_populacao.pop", 
            row.names = FALSE, col.names = FALSE, sep = " ", dec = ".", quote = FALSE)

satscan_geo <- base_satscan %>%
  select(COD_MUN, LAT, LON)

write.table(satscan_geo, "chagas_cronica_coordenadas.geo", 
            row.names = FALSE, col.names = FALSE, sep = " ", dec = ".", quote = FALSE)