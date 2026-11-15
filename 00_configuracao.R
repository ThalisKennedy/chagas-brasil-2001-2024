# =============================================================================
# Configuração dos caminhos do repositório
# -----------------------------------------------------------------------------
# Todos os scripts leem e gravam dentro da pasta 'dados/', na raiz do
# repositório. Ela não vai para o GitHub (ver .gitignore e dados/LEIA-ME.md):
# quem for reproduzir coloca ali as bases baixadas das fontes oficiais, na
# estrutura de pastas descrita no README.
#
# Para guardar os dados em outra pasta, defina a variável de ambiente DIR_DADOS
# (por exemplo, no arquivo .Renviron) com o caminho completo dessa pasta.
#
# Abra o projeto pelo arquivo repositorio-chagas.Rproj: assim o pacote 'here'
# encontra a raiz do repositório, qualquer que seja a pasta de trabalho.
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

DIR_RAIZ  <- here::here()
DIR_DADOS <- Sys.getenv("DIR_DADOS")
if (!nzchar(DIR_DADOS)) DIR_DADOS <- here::here("dados")

if (!dir.exists(DIR_DADOS)) {
  stop("Pasta de dados não encontrada: ", DIR_DADOS,
       ". Veja o README (seção 'Estrutura da pasta dados/').")
}

# Subpastas usadas pelos scripts, criadas (vazias) se ainda não existirem.
# Algumas recebem as bases baixadas; as demais, as saídas.
PASTAS_DADOS <- c(
  "Análises/Parte 2 - Análises Descritivas/Apêndice D",
  "Análises/Parte 3 - Taxas e Joinpoint/Joinpoint",
  "Análises/Parte 4 - Análise Espacial/E-SUS",
  "Análises/Parte 4 - Análise Espacial/SIM/Satscan_Chagas/Aguda",
  "Análises/Parte 4 - Análise Espacial/SIM/Satscan_Chagas/Cronica",
  "Análises/Parte 4 - Análise Espacial/SINAN",
  "Análises/Parte 4 - Análise Espacial/Shapes",
  "Análises/Parte 5 - Fatores Associados/1 - Completo",
  "Análises/Parte 5 - Fatores Associados/Bancos/IDE-MS/Dados/Biomas",
  "Análises/Parte 5 - Fatores Associados/Bancos/Portal de Dados Abertos do SUS/Dados",
  "Análises/Parte 5 - Fatores Associados/Bancos/SIDRA/Dados",
  "Análises/Parte 5 - Fatores Associados/CIDACS",
  "Análises/Parte 5 - Fatores Associados/Censo 2022",
  "Análises/Parte 5 - Fatores Associados/DAG - Microvariaveis",
  "Análises/Parte 5 - Fatores Associados/Desfechos",
  "Análises/Parte 5 - Fatores Associados/IDE-MS",
  "Análises/Parte 5 - Fatores Associados/MapBiomas",
  "Análises/Parte 5 - Fatores Associados/Portal de Dados Abertos do SUS",
  "Análises/Parte 5 - Fatores Associados/Regressão Espacial - Final",
  "Análises/Parte 5 - Fatores Associados/SIDRA",
  "Análises/Parte 6 - Série Temporal Interrompida/STI_saidas",
  "E-SUS/DCCRBR23",
  "IBGE/Dados",
  "IBGE/IBGE_POP_2001_2024/BR_Municipios_2022",
  "SIM/DOBR - CHAGAS",
  "SINAN/CHAGBR 2001-2006",
  "SINAN/CHAGBR 2001-2024",
  "SINAN/CHAGBR 2007-2024"
)
invisible(lapply(file.path(DIR_DADOS, PASTAS_DADOS), dir.create,
                 recursive = TRUE, showWarnings = FALSE))
