* =============================================================================
* SIM – seleção dos óbitos com menção à doença de Chagas (Stata)
* -----------------------------------------------------------------------------
* Reconstruído a partir do log da execução original
* (SIM/MORT_CHAGAS_PRE_PROCESSAMENTO.smcl). O log começa depois da leitura
* da base; a linha "use" abaixo indica o arquivo de entrada do SIM-DO de
* 2001 a 2024 empilhado (DOBR2001 ... DOBR2024).
* Saída: SIM/MORT_CHAGAS_PRE_PROCESSAMENTO_2.dta, lida por 01_2_SIM_tratamento.R
* =============================================================================

use "dados/SIM/Dados/DOBR - Completa - STATA.dta", clear

gen chagas_mencao = 0
label variable chagas_mencao "Óbito com qualquer menção de Doença de Chagas"

local lista_cids "B57 B570 B571 B572 B573 B574 B575 I412 I981 K231 K931 *B57 *B570 *B571 *B572 *B573 *B574 *B575 *I412 *I981 *K231 *K931"
local colunas_busca "LINHAA LINHAB LINHAC LINHAD LINHAII CAUSABAS CB_PRE CAUSABAS_O ATESTADO"

quietly {
    foreach variavel of local colunas_busca {
        capture confirm variable `variavel'
        if _rc == 0 {
            foreach cid of local lista_cids {
                replace chagas_mencao = 1 if strpos(`variavel', "`cid'") > 0
            }
        }
    }
}

tab chagas_mencao
keep if chagas_mencao == 1
count

gen tipo_mencao = .
replace tipo_mencao = 1 if chagas_mencao == 1 & strpos(CAUSABAS, "B57") > 0
replace tipo_mencao = 2 if chagas_mencao == 1 & tipo_mencao != 1
label define lab_tipo 1 "Causa Básica (Matou)" 2 "Causa Associada (Comorbidade)"
label values tipo_mencao lab_tipo
tab tipo_mencao

save "dados/SIM/MORT_CHAGAS_PRE_PROCESSAMENTO_2.dta", replace
