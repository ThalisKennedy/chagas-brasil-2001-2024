# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/DAG - Microvariaveis/DAG - Microvariaveis_5_Final.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/DAG - Microvariaveis"))

library(dagitty)
library(ggdag)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(ggforce)

dag_completo <- dagitty('dag {
  
  Renda_PerCapita [pos="0,8"]
  Gini [pos="0,7"]
  PIB_Municipal [pos="0,6"]
  Alfabetizacao [pos="0,5"]
  Ocupacao [pos="0,4"]
  Cor_Raca [pos="0,3"]
  Bolsa_Familia [pos="0,2"]
  IVM [pos="0,1"]
  Pop_Indigena [pos="1,8"]
  Pop_Quilombola [pos="1,7"]
  Amazonia_Legal [pos="1,6"]
  Assentamento_Reforma [pos="1,5"]
  Adapta_Cidades [pos="1,4"]

  Mat_Paredes [pos="3,8"]
  Adensamento_Comodo [pos="3,7"]
  Esgoto [pos="3,6"]
  Agua_Canalizada [pos="3,5"]
  Lixo [pos="3,4"]
  Urbano_Rural [pos="3,3"]
  SISAGUA [pos="3,2"]
  Urbanizacao_Entorno [pos="3,1"]
  Cisternas [pos="3,0"]
  Favelas [pos="2.5,2"]

  Biomas [pos="3,-1"]
  Precipitacao_Temp_SPEI [pos="3,-2"]
  Desmatamento_MapBiomas [pos="3,-3"]
  Cobertura_MapBiomas [pos="3,-4"]
  Veg_Secundaria [pos="3,-5"]
  Cicatriz_Fogo [pos="3,-6"]

  SIOPS_Gasto [pos="6,8"]
  Previne_Brasil [pos="6,7"]
  Mais_Medicos [pos="6,6"]
  RIPSA_Recursos [pos="6,5"]
  eMulti [pos="6,4"]
  RIPSA_Consultas [pos="6,3"]
  CNES_Infra [pos="6,2"]
  Cobertura_APS_ACS [pos="6,1"]
  Leitos_UTI_SAMU [pos="6,0"]
  Atendimentos_CVD [pos="6,-1"]
  Brasil_Saudavel [pos="6,-2"]

  Naturalidade_Migracao [pos="5,3"]
  Trabalho_Subsistencia [pos="5,-3"]
  Lavoura_Acai_Cana [pos="5,-4"]
  Degradacao_Borda_Fogo [pos="5,-5"]

  Infeccao_Chagas [pos="7,-3"]
  Evolucao_Clinica [pos="8,-1.5"] 

  Morbimortalidade_Registrada [outcome, pos="9,0"]

  Cobertura_SIM_SINASC [pos="9,2"]
  Registro_Obitos_Civil [pos="9,1"]

  Cor_Raca -> Renda_PerCapita; Cor_Raca -> Alfabetizacao; Cor_Raca -> Ocupacao
  Alfabetizacao -> Ocupacao; Ocupacao -> Renda_PerCapita
  Bolsa_Familia -> Cisternas
  
  IVM -> SIOPS_Gasto; IVM -> Mais_Medicos
  
  Renda_PerCapita -> Mat_Paredes; Renda_PerCapita -> Agua_Canalizada; Renda_PerCapita -> Esgoto
  Renda_PerCapita -> Adensamento_Comodo; Renda_PerCapita -> Lixo
  Renda_PerCapita -> SISAGUA
  Gini -> Favelas; PIB_Municipal -> Favelas
  Amazonia_Legal -> Urbano_Rural; Amazonia_Legal -> Mat_Paredes
  Urbanizacao_Entorno -> Adapta_Cidades

  PIB_Municipal -> Desmatamento_MapBiomas; Assentamento_Reforma -> Desmatamento_MapBiomas
  PIB_Municipal -> Cicatriz_Fogo

  Renda_PerCapita -> Trabalho_Subsistencia; Amazonia_Legal -> Trabalho_Subsistencia
  Gini -> Naturalidade_Migracao
  Pop_Indigena -> Trabalho_Subsistencia; Pop_Indigena -> Lavoura_Acai_Cana
  Pop_Quilombola -> Trabalho_Subsistencia

  Mat_Paredes -> Infeccao_Chagas; Favelas -> Infeccao_Chagas
  Urbano_Rural -> Trabalho_Subsistencia; Urbano_Rural -> SISAGUA
  Adensamento_Comodo -> Infeccao_Chagas
  Lixo -> Infeccao_Chagas 
  Esgoto -> Infeccao_Chagas; Agua_Canalizada -> Infeccao_Chagas
  Cisternas -> Infeccao_Chagas 
  SISAGUA -> Infeccao_Chagas 
  Urbanizacao_Entorno -> Infeccao_Chagas 

  Biomas -> Lavoura_Acai_Cana; Precipitacao_Temp_SPEI -> Lavoura_Acai_Cana
  Desmatamento_MapBiomas -> Infeccao_Chagas
  Veg_Secundaria -> Infeccao_Chagas
  Cicatriz_Fogo -> Infeccao_Chagas 
  Cobertura_MapBiomas -> Lavoura_Acai_Cana; Cobertura_MapBiomas -> Infeccao_Chagas

  Trabalho_Subsistencia -> Infeccao_Chagas
  Lavoura_Acai_Cana -> Infeccao_Chagas
  Degradacao_Borda_Fogo -> Infeccao_Chagas
  Naturalidade_Migracao -> Infeccao_Chagas

  SIOPS_Gasto -> CNES_Infra; SIOPS_Gasto -> RIPSA_Recursos
  RIPSA_Recursos -> Cobertura_APS_ACS; RIPSA_Recursos -> Leitos_UTI_SAMU
  Previne_Brasil -> RIPSA_Consultas; eMulti -> Cobertura_APS_ACS
  CNES_Infra -> Leitos_UTI_SAMU
  Mais_Medicos -> Cobertura_APS_ACS
  RIPSA_Consultas -> Atendimentos_CVD
  Brasil_Saudavel -> RIPSA_Consultas
  
  Infeccao_Chagas -> Brasil_Saudavel

  Infeccao_Chagas -> Evolucao_Clinica
  Evolucao_Clinica -> Morbimortalidade_Registrada

  Cobertura_APS_ACS -> Evolucao_Clinica
  Mais_Medicos -> Evolucao_Clinica
  Leitos_UTI_SAMU -> Evolucao_Clinica
  Atendimentos_CVD -> Evolucao_Clinica
  Brasil_Saudavel -> Evolucao_Clinica
  RIPSA_Consultas -> Evolucao_Clinica

  Cobertura_APS_ACS -> Morbimortalidade_Registrada
  Mais_Medicos -> Morbimortalidade_Registrada
  Brasil_Saudavel -> Morbimortalidade_Registrada
  RIPSA_Consultas -> Morbimortalidade_Registrada

  Cobertura_SIM_SINASC -> Morbimortalidade_Registrada
  Registro_Obitos_Civil -> Morbimortalidade_Registrada
}')

tidy_dag <- tidy_dagitty(dag_completo) %>%
  mutate(
    nivel = case_when(
      name %in% c("Renda_PerCapita", "Gini", "PIB_Municipal", "Alfabetizacao", "Ocupacao", "Cor_Raca", "Bolsa_Familia", "IVM", "Pop_Indigena", "Pop_Quilombola", "Amazonia_Legal", "Assentamento_Reforma", "Adapta_Cidades") ~ "1. Nível Distal",
      name %in% c("Mat_Paredes", "Adensamento_Comodo", "Esgoto", "Agua_Canalizada", "Lixo", "Urbano_Rural", "SISAGUA", "Urbanizacao_Entorno", "Cisternas", "Favelas") ~ "2A. Intermediário (Saneamento/Urbanismo)",
      name %in% c("Biomas", "Precipitacao_Temp_SPEI", "Desmatamento_MapBiomas", "Cobertura_MapBiomas", "Veg_Secundaria", "Cicatriz_Fogo") ~ "2B. Intermediário (Ambiente/Vetor)",
      name %in% c("SIOPS_Gasto", "Previne_Brasil", "Mais_Medicos", "RIPSA_Recursos", "eMulti", "RIPSA_Consultas", "CNES_Infra", "Cobertura_APS_ACS", "Leitos_UTI_SAMU", "Atendimentos_CVD", "Brasil_Saudavel") ~ "2C. Intermediário (Capacidade de Saúde)",
      name %in% c("Naturalidade_Migracao", "Trabalho_Subsistencia", "Lavoura_Acai_Cana", "Degradacao_Borda_Fogo") ~ "3. Nível Proximal",
      name %in% c("Infeccao_Chagas", "Evolucao_Clinica") ~ "Mecanismos Latentes (Não Medidos)",
      name %in% c("Cobertura_SIM_SINASC", "Registro_Obitos_Civil") ~ "Controle Metodológico (Viés de Observação)",
      name %in% c("Morbimortalidade_Registrada") ~ "Desfecho Registrado (SIM/SINAN/e-SUS)",
      TRUE ~ "Outros"
    ),
    nivel = factor(nivel, levels = c(
      "1. Nível Distal", 
      "2A. Intermediário (Saneamento/Urbanismo)", 
      "2B. Intermediário (Ambiente/Vetor)", 
      "2C. Intermediário (Capacidade de Saúde)", 
      "3. Nível Proximal", 
      "Mecanismos Latentes (Não Medidos)", 
      "Controle Metodológico (Viés de Observação)", 
      "Desfecho Registrado (SIM/SINAN/e-SUS)", 
      "Outros"
    ))
  )

nos_unicos <- tidy_dag$data %>% 
  distinct(name, x, y, nivel) %>% 
  filter(nivel != "Outros")

grafico_formatado <- ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_mark_rect(
    data = nos_unicos,
    aes(x = x, y = y, fill = nivel),
    expand = unit(6, "mm"),    
    radius = unit(4, "mm"),    
    alpha = 0.08,              
    color = "gray80",          
    linetype = "dashed",       
    inherit.aes = FALSE        
  ) +
  geom_dag_edges(
    edge_color = "gray75", 
    edge_width = 0.5, 
    edge_alpha = 0.6,
    arrow_directed = grid::arrow(length = grid::unit(4, "pt"), type = "closed")
  ) +
  geom_dag_point(aes(color = nivel), size = 3, alpha = 0.9) +
  geom_text_repel(
    aes(label = name, color = nivel),
    size = 3.5,
    fontface = "bold",
    bg.color = "white", 
    bg.r = 0.15,        
    segment.color = NA, 
    force = 0,          
    vjust = -1,
    show.legend = FALSE 
  ) +
  scale_color_viridis_d(option = "turbo", direction = -1) + 
  scale_fill_viridis_d(option = "turbo", direction = -1) + 
  theme_dag_blank() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40"),
    plot.background = element_rect(fill = "white", color = NA) 
  ) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(size = 4)),
    fill = "none" 
  ) +
  labs(
    title = "DAG Completo - Fatores Associados à Doença de Chagas",
    subtitle = "Mapeamento estruturado com mediação clínica e de vigilância"
  )

ggsave(
  filename = "DAG_Chagas_AltaResolucao_Arredondado_5_Final.png", 
  plot = grafico_formatado, 
  width = 18, 
  height = 12, 
  dpi = 300, 
  bg = "white"
)