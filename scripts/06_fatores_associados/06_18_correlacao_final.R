# =============================================================================
# Repositório: Doença de Chagas no Brasil, 2001-2024 (dissertação FMUSP)
# Arquivo original: Análises/Parte 5 - Fatores Associados/1 - Completo/Correlação v2.R
# Ajuste para o repositório: caminhos absolutos trocados por caminhos
# relativos à pasta 'dados/' (ver 00_configuracao.R). Blocos acrescentados
# estão marcados com "# ---" e explicados no comentário que os abre.
# =============================================================================
source(here::here("00_configuracao.R"))
# Pasta de trabalho usada na execução original (projeto do RStudio):
setwd(file.path(DIR_DADOS, "Análises/Parte 5 - Fatores Associados/1 - Completo"))

library(tidyverse)
library(corrplot)

df <- read_csv("Dataset_Fatores_Associados_Chagas_Final_Etapa5.csv", show_col_types = FALSE)

y_finais <- c(
  "mort_taxa_padronizada_suavizada", 
  "aguda_taxa_padronizada_suavizada",
  "cronica_taxa_padronizada_suavizada"
)

candidatas_finais <- setdiff(names(df), c("codigo_municipio", y_finais))

num_vars <- df %>% 
  select(all_of(candidatas_finais)) %>% 
  select(where(is.numeric))

M_final <- cor(num_vars, method = "spearman", use = "pairwise.complete.obs")

# ==============================================================================
# 1. Gerar Heatmap
# ==============================================================================

paleta_coolwarm <- colorRampPalette(c("#3B4CC0", "white", "#B40426"))(200)

png("heatmap_correlacao_v2.png", width = 2000, height = 2000, res = 150)

corrplot(M_final, 
         method = "color",          # Preenche com quadrados de cor (substitui os círculos)
         type = "full",             # Renderiza a matriz inteira quadrada
         col = paleta_coolwarm,     # Aplica a paleta divergente criada acima
         addgrid.col = "white",     # Adiciona as linhas de grade brancas separando as células
         tl.col = "black",          # Cor da fonte dos eixos
         tl.cex = 0.7,              # Tamanho da fonte dos eixos
         tl.srt = 90,               # Rotaciona os rótulos do eixo X na vertical (90 graus)
         order = "original",        # Mantém a ordem exata das variáveis do banco de dados
         title = "Heatmap de Correlação - Candidatas Finais (Spearman)",
         mar = c(0, 0, 2, 0))       # Margem superior para o título não sobrepor o gráfico

dev.off()

cat("Heatmap gerado com sucesso e salvo em 'heatmap_correlacao_v2.png'.\n")

# ==============================================================================
# 2. Geração da Tabela de Correlação
# ==============================================================================

tabela_correlacao <- as.data.frame(as.table(M_final)) %>%
  rename(Variavel_1 = Var1, Variavel_2 = Var2, rho = Freq) %>%
  filter(as.character(Variavel_1) < as.character(Variavel_2)) %>%
  arrange(desc(abs(rho))) %>%
  mutate(rho = round(rho, 3))

cat("\nTop 10 maiores correlações identificadas:\n")
print(head(tabela_correlacao, 10))

write_csv(tabela_correlacao, "Tabela_Correlacao_V2.csv")
cat("\nTabela tabular completa salva com sucesso em: 'Tabela_Correlacao_V2.csv'\n")