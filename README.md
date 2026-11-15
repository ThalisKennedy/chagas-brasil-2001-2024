# Epidemiologia da doença de Chagas no Brasil: dinâmica espaço-temporal e determinantes sociais da saúde

Scripts de processamento e análise da dissertação de mestrado:

> ARAUJO, T. K. A. de. *Epidemiologia da doença de Chagas no Brasil: dinâmica espaço-temporal e determinantes sociais da saúde*. 2026. Dissertação (Mestrado em Ciências) – Faculdade de Medicina, Universidade de São Paulo, São Paulo, 2026.

- **Autor:** Thalis Kennedy Azevedo de Araujo
- **Orientador:** Prof. Dr. Expedito José de Albuquerque Luna
- **Instituição:** Faculdade de Medicina da Universidade de São Paulo (FMUSP), Programa de Pós-Graduação em Saúde Coletiva
- **Período analisado:** 2001 a 2024 (doença de Chagas aguda e óbitos); 2023 (doença de Chagas crônica, primeiro ano de notificação no e-SUS Notifica)
- **Unidade espacial:** municípios brasileiros

O repositório contém **apenas código**. Nenhum dado individual é distribuído aqui. Todas as bases são públicas ou de acesso controlado e devem ser obtidas nas fontes listadas abaixo.

## Ordem dos scripts

A numeração segue a seção 4.5 (Análise dos Dados) da dissertação. O arquivo `00_rodar_tudo.R` registra a **ordem real de execução**, que difere da numeração em dois pontos (ver abaixo), marca como **[MANUAL]** as etapas feitas fora do R e refaz as cópias e as renomeações de arquivos que, na execução original, foram feitas à mão.

| Pasta | Etapa (seção da dissertação) | Conteúdo |
|---|---|---|
| `scripts/01_preprocessamento` | 4.3–4.5.1 | Seleção dos óbitos com menção à DC (Stata), tratamento do SIM, do SINAN (2001–2006 e 2007–2024, depois unidos e filtrados) e do e-SUS Notifica; populações por Brasil, região e município |
| `scripts/02_completude` | 4.5.1 | Completude e tendência da completude (Prais-Winsten): Tabelas 2 a 4 e Figura C1 |
| `scripts/03_descritiva` | 4.5.2 | Tabelas 5 a 15, Apêndice D e Apêndice A (causas sentinelas) |
| `scripts/04_taxas_tendencia` | 4.5.3 | Taxas brutas e padronizadas por idade (Figuras 4 a 9 e D1) e planilhas de entrada do Joinpoint |
| `scripts/05_espacial` | 4.5.4 | `05_01`–`05_03`: taxas municipais com suavização bayesiana empírica local (mapas das taxas); `05_04`–`05_06`: taxas com suavização global; `05_07`–`05_12`: Moran Global e Local e Getis-Ord Gi*, com correção FDR; `satscan/`: entradas do SaTScan, parâmetros e leitura dos resultados; `qgis/`: montagem das figuras |
| `scripts/06_fatores_associados` | 4.5.5 | Desfechos e covariáveis municipais (Censo 2022, SIDRA, IDE-MS, MapBiomas, Portal de Dados Abertos do SUS, PDD/CIDACS), DAG, junção e exclusões, OLS hierárquico (blocos distal, intermediário e proximal), testes de Moran e LM, SAR, SEM, SDM, SLX e GWR |
| `scripts/07_serie_temporal_interrompida` | 4.5.6 | Série temporal interrompida: regressão binomial negativa e SARIMA, com contrafactuais |

**Dependências que mudam a ordem de execução:**

- Os scripts `05_04` a `05_06` leem o banco municipal `Dataset_Fatores_Associados_Chagas_Final_Etapa6_Com_Residuos_*.csv`, gravado por `06_18a` a `06_18c`. Por isso, a montagem do banco dos fatores associados roda antes deles.
- Vários scripts usam objetos criados pelo script anterior na mesma sessão do R (indicado no cabeçalho de cada um). Em `00_rodar_tudo.R`, esses blocos rodam em sequência.

Cada script começa com um cabeçalho que indica o arquivo original de onde veio. As alterações em relação à execução original são:

- caminhos absolutos trocados por caminhos relativos à pasta `dados/`;
- importação das bases `.dta`, que na execução original foi feita pelo menu do RStudio;
- blocos de exportação marcados com `# --- Exportação feita no console na execução original`, como `saveRDS()`, `ggsave()`, `write_xlsx()` e `st_write()`. Quando o comando está registrado no `.Rhistory` do projeto, o bloco diz isso; nos demais, os nomes dos arquivos e dos objetos foram conferidos com as saídas usadas na dissertação;
- nos scripts de completude, as duas linhas da tendência da "Média do Banco" (Tabelas 2 e 4), que vêm da versão anterior do mesmo script;
- no `02_1`, uma checagem para série quase constante: com o pacote prais 1.2.0, a de `CODMUNRES` leva rho a 1 e interrompia o script (a variável fica sem APC, como as de completude de 100%);
- no `03_1` e no `03_2`, a leitura do rótulo "Outros Estabelecimento de Saúde", usado pela versão final do `01_2`;
- no `02_2`, a completude e a tendência da classificação de autoctonia calculadas só sobre 2007-2024, período em que o campo existe (como na Tabela 2);
- no `04_1`, a troca das taxas zero na planilha do Joinpoint (anos sem casos numa região) por metade da menor taxa positiva da série, como na planilha usada na execução original.

As bases usadas nas tabelas descritivas e na série temporal interrompida foram gravadas em fevereiro de 2026 por versões anteriores dos scripts `01_2`, `01_5` e `01_6`. As versões finais, que estão aqui, diferem delas só em rótulos de categorias, no tipo de uma coluna e em colunas não usadas nas análises, e reproduzem as mesmas tabelas.

## Como reproduzir

1. Instale o R 4.5.1 e o RStudio e abra o projeto `repositorio-chagas.Rproj`.
2. Coloque as bases baixadas das fontes oficiais na pasta `dados/`, na estrutura abaixo. As subpastas usadas pelos scripts são criadas, vazias, na primeira execução (a lista está em `00_configuracao.R`). Para guardar os dados em outra pasta, defina a variável de ambiente `DIR_DADOS` com o caminho completo dela.
3. Rode `00_rodar_tudo.R` bloco a bloco, conferindo as saídas de cada etapa.

Os pacotes são carregados no início de cada script. As versões usadas estão em `sessionInfo/`.

**Conferência (14/11/2026):** os scripts das etapas 1 a 4 e 7 (`01_2` a `01_9`, `02`, `03_1`, `03_2`, `04` e `07_1`) foram rodados do zero, cada um num R novo, e as saídas foram comparadas com as da execução original. As tabelas e figuras usadas na dissertação saíram iguais; nas figuras das taxas anuais, só muda a posição de alguns rótulos, que o pacote ggrepel sorteia a cada rodada. As versões dos pacotes dessa rodada estão em `sessionInfo/versoes_pacotes_conferencia_2026-11-14.txt`. As etapas 5 e 6 dependem de SaTScan, GeoDa, QGIS e das bases da PDD/CIDACS e não entraram nessa conferência. O script `05_03` usa o pacote geobr para obter a malha municipal e, por isso, precisa de acesso à internet.

### Estrutura da pasta `dados/`

```
dados/
├── SIM/                      SIM-DO 2001–2024 (Dados/DOBR*.dbc) e bases .dta
├── SINAN/                    SINAN-DC aguda (CHAGBR 2001-2006, CHAGBR 2007-2024)
├── E-SUS/                    e-SUS Notifica, DC crônica 2023 (DCCRBR23)
├── IBGE/                     Populações (POPS/DATASUS), Censo 2022 e malha de 2022
└── Análises/
    ├── Parte 2 - Análises Descritivas/
    ├── Parte 3 - Taxas e Joinpoint/
    ├── Parte 4 - Análise Espacial/    (Shapes/: malha de 2024; SIM/Satscan_Chagas/)
    ├── Parte 5 - Fatores Associados/  (Bancos/: covariáveis por fonte)
    └── Parte 6 - Série Temporal Interrompida/
```

### Etapas fora do R

- **Stata:** a seleção dos óbitos com menção à DC no SIM (`01_1_SIM_selecao_obitos_chagas.do`, reconstruído do log da execução) e a conversão dos arquivos `.dbf` do SINAN e do e-SUS Notifica para `.dta`.
- **Joinpoint Regression Program 6.0.1:** tendência temporal, a partir das planilhas com as taxas padronizadas e os erros-padrão gravadas pela etapa 4.
- **SaTScan 10.3.3:** estatística Scan, com os parâmetros de `scripts/05_espacial/satscan/*.prm`. As entradas (`.cas`, `.pop` e `.geo`) são geradas pelos scripts `05_14`, `05_16` e `05_19` e copiadas por `00_rodar_tudo.R` para `dados/Análises/Parte 4 - Análise Espacial/SIM/Satscan_Chagas`. Nos arquivos `.prm`, troque `<PASTA_SATSCAN>` pelo caminho completo dessa pasta.
- **GeoDa 1.22.0.21:** cálculo do risco relativo e conferência numérica das taxas (Apêndice F).
- **QGIS 3.44.11:** o script `05_23_mapas_figuras_qgis.py` monta as Figuras 10 a 24 e G3 a G5 no console Python do QGIS. Antes, defina a variável de ambiente `DIR_DADOS`.
- **Python (PDD/CIDACS):** `06_09_download_PDD_CIDACS.py` baixa as bases da Plataforma de Dados Desidentificados. Exige cadastro na PDD, a biblioteca `pdd_lib` fornecida pelo CIDACS e as variáveis de ambiente `PDD_EMAIL` e `PDD_TOKEN`. Nenhuma credencial fica no código.

As figuras das séries temporais interrompidas (Figuras 25, 26 e H1) foram montadas a partir das planilhas `Curvas_*.xlsx` gravadas pelo script `07_1`.

## Fontes dos dados

| Base | Uso | Onde obter |
|---|---|---|
| SIM – declarações de óbito, 2001–2024 | Óbitos por DC | DATASUS: https://datasus.saude.gov.br/transferencia-de-arquivos/ (ou pacote `microdatasus`) |
| SINAN – doença de Chagas aguda, 2001–2024 | Casos agudos | DATASUS, transferência de arquivos |
| e-SUS Notifica – doença de Chagas crônica, 2023 | Casos crônicos | DATASUS |
| Estimativas populacionais (POPS), 2001–2024 | Denominadores e padronização | DATASUS |
| IBGE – Censo Demográfico 2022 (tabelas SIDRA 9578 e 9718, população por idade e sexo, microdados da amostra, entorno dos domicílios) | População padrão e covariáveis | https://sidra.ibge.gov.br e https://www.ibge.gov.br |
| IBGE – PIB dos Municípios, PAM e IPCA (SIDRA 5938, 5457, 1737) | Covariáveis econômicas e agrícolas | https://sidra.ibge.gov.br |
| IBGE – malhas municipais de 2022 e de 2024 | Malha de 2022: taxas com suavização local e entradas do SaTScan (`05_01`–`05_03` e `satscan/`). Malha de 2024 (`BR_Municipios_2024`): suavização global, Moran, Gi*, modelos e mapas (`05_04`–`05_12`, `06_` e `qgis/`) | https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html |
| IDE-MS | Amazônia Legal, assentamentos, cobertura | Infraestrutura de Dados Espaciais do Ministério da Saúde |
| Portal de Dados Abertos do SUS (RIPSA e outros) | Covariáveis de serviços de saúde | https://dadosabertos.saude.gov.br |
| MapBiomas (cobertura, desmatamento, vegetação secundária, degradação/fogo) | Covariáveis ambientais | https://data.mapbiomas.org |
| PDD/CIDACS (clima, SPEI, CNES, Bolsa Família, Cisternas, entorno dos domicílios) | Covariáveis | Plataforma de Dados Desidentificados, CIDACS/Fiocruz Bahia (acesso mediante cadastro; as bases não podem ser redistribuídas) |
| Índice de Vulnerabilidade Municipal (Instituto Votorantim) | Covariável | CIDACS |

As referências completas estão na dissertação.

## Software

- R 4.5.1 (RStudio), com os pacotes listados em `sessionInfo/`. Os principais são tidyverse, gtsummary, prais, spdep, spatialreg, GWmodel, glmnet, sandwich, sf, geobr, tsibble, fable, feasts e MASS.
- Stata (pré-processamento do SIM e conversão de formatos)
- Joinpoint Regression Program 6.0.1
- GeoDa 1.22.0.21
- SaTScan 10.3.3
- QGIS 3.44.11
- Python 3 (download da PDD/CIDACS e montagem dos mapas no QGIS)

## Licença

Código sob a licença MIT (ver `LICENSE`).

## Como citar

ARAUJO, T. K. A. de. *Scripts de processamento e análise: epidemiologia da doença de Chagas no Brasil, 2001-2024*. Versão 1.0. 2026.
