# =============================================================================
# Ordem de execução dos scripts
# -----------------------------------------------------------------------------
# Os scripts foram escritos para serem rodados um a um no RStudio, conferindo as
# saídas de cada etapa. Este arquivo registra a ordem real de execução; rode-o
# bloco a bloco. Etapas feitas fora do R estão marcadas como [MANUAL].
# As funções copiar() refazem as cópias (e as renomeações) de arquivos que, na
# execução original, foram feitas à mão.
# =============================================================================

source(here::here("00_configuracao.R"))
rodar <- function(arq) {
  message("\n>>> ", arq)
  source(here::here("scripts", arq), encoding = "UTF-8", echo = FALSE)
  setwd(DIR_RAIZ)
}
D <- function(...) file.path(DIR_DADOS, ...)
copiar <- function(de, para) {
  dir.create(dirname(para), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(de, para, overwrite = TRUE)) stop("Não foi possível copiar ", de)
}
PASTA_SATSCAN <- D("Análises/Parte 4 - Análise Espacial/SIM/Satscan_Chagas")
P4 <- D("Análises/Parte 4 - Análise Espacial")

# 1. Pré-processamento ---------------------------------------------------------
# [MANUAL] Stata: scripts/01_preprocessamento/01_1_SIM_selecao_obitos_chagas.do
# [MANUAL] Stata: conversão dos .dbf do SINAN e do e-SUS Notifica para .dta
for (a in c("01_2_SIM_tratamento.R", "01_3_SINAN_tratamento_2001_2006.R",
            "01_4_SINAN_tratamento_2007_2024.R"))
  rodar(file.path("01_preprocessamento", a))
# 01_5, 02_2 e 04_1 leem as duas bases do SINAN na pasta da série completa:
copiar(D("SINAN/CHAGBR 2001-2006/CHAGAS_AGUDA_FINAL_4_2001_2006_TRATADO.rds"),
       D("SINAN/CHAGBR 2001-2024/CHAGAS_AGUDA_FINAL_4_2001_2006_TRATADO.rds"))
copiar(D("SINAN/CHAGBR 2007-2024/CHAGAS_AGUDA_FINAL_4_2007_2024_TRATADO.rds"),
       D("SINAN/CHAGBR 2001-2024/CHAGAS_AGUDA_FINAL_4_2007_2024_TRATADO.rds"))
for (a in c("01_5_SINAN_juncao_filtro_2001_2024.R", "01_6_eSUS_tratamento.R",
            "01_7_IBGE_populacao_brasil.R", "01_8_IBGE_populacao_regiao.R",
            "01_9_IBGE_populacao_municipio.R"))
  rodar(file.path("01_preprocessamento", a))
bases <- c(D("SINAN/CHAGBR 2001-2024/CHAGAS_AGUDA_FILTRADO_2_2001_2024.rds"),
           D("E-SUS/DCCRBR23/CHAGAS_CRONICA_FINAL_TRATADO.rds"),
           D("SIM/DOBR - CHAGAS/MORT_CHAGAS_FINAL_2_TRATADO_COMPLETO.rds"))
for (b in bases)
  copiar(b, D("Análises/Parte 2 - Análises Descritivas/Apêndice D", basename(b)))
for (b in bases[c(1, 3)])   # a STI usa só o SINAN e o SIM
  copiar(b, D("Análises/Parte 6 - Série Temporal Interrompida", basename(b)))
copiar(D("IBGE/IBGE_POP_2001_2024/POP_IBGE_MUNICIPIO_TRATADA.csv"),
       D("Análises/Parte 5 - Fatores Associados/Desfechos/POP_IBGE_MUNICIPIO_TRATADA.csv"))

# 2. Completude (Tabelas 2 a 4 e Figura C1) ------------------------------------
for (a in c("02_1_SIM_completude.R", "02_2_SINAN_completude.R", "02_3_eSUS_completude.R"))
  rodar(file.path("02_completude", a))

# 3. Análise descritiva (Tabelas 5 a 15 e Apêndices A e D) ----------------------
for (a in c("03_1_tabelas_5_a_15.R", "03_2_apendice_D.R", "03_3_apendice_A_causas_sentinelas.R"))
  rodar(file.path("03_descritiva", a))

# 4. Taxas e tendência temporal (Figuras 4 a 9 e D1) ------------------------------
for (a in c("04_1_SINAN_taxas.R", "04_2_eSUS_taxas.R", "04_3_SIM_taxas.R"))
  rodar(file.path("04_taxas_tendencia", a))
# [MANUAL] Joinpoint Regression Program 6.0.1, com as planilhas gravadas em
#          dados/Análises/Parte 3 - Taxas e Joinpoint/Joinpoint (Tabelas 16 a 18,
#          Tabela E1 e Figuras E1 a E4).

# 5a. Taxas municipais com suavização local e entradas do SaTScan ---------------
#     Os scripts satscan/05_13 a 05_19 usam objetos criados pelo script anterior
#     na mesma sessão; por isso cada bloco roda em sequência.
rodar("05_espacial/05_01_SINAN_taxas_suavizacao_local.R")
rodar("05_espacial/satscan/05_13_SINAN_satscan_1_moran_lisa.R")
rodar("05_espacial/satscan/05_14_SINAN_satscan_2_entradas.R")
rodar("05_espacial/05_02_eSUS_taxas_suavizacao_local.R")
rodar("05_espacial/satscan/05_15_eSUS_satscan_1_moran_lisa.R")
rodar("05_espacial/satscan/05_16_eSUS_satscan_2_entradas.R")
rodar("05_espacial/05_03_SIM_taxas_suavizacao_local.R")
rodar("05_espacial/satscan/05_17_SIM_satscan_0_taxas.R")
rodar("05_espacial/satscan/05_18_SIM_satscan_1_moran_lisa.R")
rodar("05_espacial/satscan/05_19_SIM_satscan_2_entradas.R")
for (x in list(list("SINAN/CHAGBR 2001-2024", "Aguda", c("chagas_aguda_casos.cas", "chagas_aguda_populacao.pop", "chagas_aguda_coordenadas.geo")),
               list("E-SUS/DCCRBR23", "Cronica", c("chagas_cronica_casos.cas", "chagas_cronica_populacao.pop", "chagas_cronica_coordenadas.geo")),
               list("SIM/DOBR - CHAGAS", "", c("chagas_obitos.cas", "chagas_populacao.pop", "chagas_coordenadas.geo"))))
  for (f in x[[3]]) copiar(D(x[[1]], f), file.path(PASTA_SATSCAN, x[[2]], f))
# [MANUAL] SaTScan 10.3.3: abrir scripts/05_espacial/satscan/*.prm, trocar
#          <PASTA_SATSCAN> pelo caminho completo de PASTA_SATSCAN e rodar.
for (a in c("05_20_SINAN_pos_satscan.R", "05_21_eSUS_pos_satscan.R", "05_22_SIM_pos_satscan.R"))
  rodar(file.path("05_espacial/satscan", a))
# Camadas dos mapas das taxas, para o QGIS e para o banco dos fatores associados:
for (x in list(c("SINAN/CHAGBR 2001-2024", "SINAN", "TAXAS_CHAGAS_AGUDA_SUAVIZADAS.gpkg"),
               c("E-SUS/DCCRBR23", "E-SUS", "MAPA_CHAGAS_CRONICA_SUAVIZADO_2023.gpkg"),
               c("SIM/DOBR - CHAGAS", "SIM", "TAXAS_MORTALIDADE_CHAGAS_PRONTAS.gpkg"))) {
  copiar(D(x[1], x[3]), file.path(P4, x[2], x[3]))
  copiar(D(x[1], x[3]), D("Análises/Parte 5 - Fatores Associados/Desfechos", x[3]))
}

# 6a. Fatores associados: montagem do banco municipal ----------------------------
#     Vem antes da etapa 5b porque os scripts 05_04 a 05_06 leem o banco
#     'Dataset_..._Etapa6_Com_Residuos_*.csv', gravado em 06_18a a 06_18c.
# [MANUAL] Python: scripts/06_fatores_associados/06_09_download_PDD_CIDACS.py
P5 <- D("Análises/Parte 5 - Fatores Associados")
for (a in c("06_01_desfechos_municipais.R", "06_02_DAG.R",
            "06_03a_censo2022_base_municipal.R", "06_03b_covariaveis_censo2022.R",
            "06_04_covariaveis_SIDRA.R", "06_05_covariaveis_IDE_MS.R",
            "06_06_covariaveis_MapBiomas.R", "06_07_covariaveis_Portal_SUS.R"))
  rodar(file.path("06_fatores_associados", a))
copiar(file.path(P5, "Portal_Dados_Abertos_SUS_CHAGAS_FINAL.csv"),
       file.path(P5, "Portal de Dados Abertos do SUS", "Portal_Dados_Abertos_SUS_CHAGAS_FINAL.csv"))
for (a in c("06_08_diagnostico_Portal_SUS.R", "06_10_covariaveis_CIDACS.R",
            "06_11_diagnostico_CIDACS.R", "06_12_juncao_bancos.R"))
  rodar(file.path("06_fatores_associados", a))
# Na execução original, a pasta 'Análises' foi renomeada para '1 - Completo':
copiar(file.path(P5, "Análises", "Dataset_Fatores_Associado_Chagas_Completo.csv"),
       file.path(P5, "1 - Completo", "Dataset_Fatores_Associado_Chagas_Completo_2.csv"))
for (a in c("06_13_exclusao_correlacao.R", "06_14_exclusao_etapa2.R",
            "06_15_recodificacao_etapa3.R", "06_16_exclusao_etapa4.R",
            "06_17_exclusao_etapa5.R", "06_18_correlacao_final.R",
            "06_18a_SINAN_banco_etapa6_residuos.R", "06_18b_eSUS_banco_etapa6_residuos.R",
            "06_18c_SIM_banco_etapa6_residuos.R"))
  rodar(file.path("06_fatores_associados", a))
copiar(file.path(P5, "1 - Completo", "Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos.csv"),
       file.path(P5, "1 - Completo", "Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_Mortalidade.csv"))

# 5b. Suavização global, Moran Global e Local (FDR) e Getis-Ord Gi* -------------
#     05_04 a 05_06 acrescentam ao banco a taxa do período com suavização global
#     ('..._global.csv'). Os arquivos do SaTScan gravados por 05_10 a 05_12
#     (com óbitos esperados) não foram usados: o SaTScan usou os de 05_14, 05_16
#     e 05_19.
#     Cada desfecho roda em sequência (taxas globais -> Moran Local -> Gi*), porque
#     os scripts de Moran e de Gi* usam objetos criados pelo anterior.
for (a in c("05_04_SINAN_taxas_suavizacao_global.R", "05_07_SINAN_moran_lisa_FDR.R",
            "05_10_SINAN_getis_ord_FDR.R",
            "05_05_eSUS_taxas_suavizacao_global.R", "05_08_eSUS_moran_lisa_FDR.R",
            "05_11_eSUS_getis_ord_FDR.R",
            "05_06_SIM_taxas_suavizacao_global.R", "05_09_SIM_moran_lisa_FDR.R",
            "05_12_SIM_getis_ord_FDR.R"))
  rodar(file.path("05_espacial", a))
# Camadas do Moran Local e do Gi* para o QGIS (na execução original, salvas com o
# sufixo _22):
copiar(D("SINAN/CHAGBR 2001-2024/mapa_chagas_aguda_espacial_completo_GLOBAL.gpkg"),
       file.path(P4, "SINAN/mapa_chagas_aguda_espacial_completo_GLOBAL_22.gpkg"))
copiar(D("E-SUS/DCCRBR23/mapa_chagas_cronica_espacial_completo_GLOBAL.gpkg"),
       file.path(P4, "E-SUS/mapa_chagas_cronica_espacial_completo_GLOBAL_22.gpkg"))
copiar(D("SIM/DOBR - CHAGAS/mapa_chagas_analise_espacial_4.3_GLOBAL.gpkg"),
       file.path(P4, "SIM/mapa_chagas_analise_espacial_4.3_GLOBAL_22.gpkg"))

# 6b. OLS hierárquico, modelos espaciais e GWR ------------------------------------
for (a in sort(list.files(here::here("scripts/06_fatores_associados/ols_hierarquico"))))
  rodar(file.path("06_fatores_associados/ols_hierarquico", a))
rodar("06_fatores_associados/06_34_regressoes_espaciais_GWR_tres_desfechos.R")
rodar("06_fatores_associados/06_35_SLX_robusto_HC1.R")
rodar("06_fatores_associados/06_36_GWR_stepwise_e_sem_acai.R")
rodar("06_fatores_associados/06_37_categorizar_GWR_para_QGIS.R")

# 5c. Mapas ----------------------------------------------------------------------
# [MANUAL] QGIS 3.44.11: scripts/05_espacial/qgis/05_23_mapas_figuras_qgis.py
#          (Figuras 10 a 24 e G3 a G5). O GeoDa 1.22.0.21 foi usado para o RR e
#          para a conferência das taxas (Apêndice F).

# 7. Série temporal interrompida (Tabelas 31, 32, H1 e H2) --------------------------
rodar("07_serie_temporal_interrompida/07_1_STI_binomial_negativa_e_SARIMA.R")

writeLines(capture.output(sessionInfo()),
           file.path(DIR_RAIZ, "sessionInfo", "sessionInfo_rodada_completa.txt"))
