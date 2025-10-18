################################################################################
#                       MANUAL DE ANÁLISE DE DADOS                             #
#                  Luiz Paulo Fávero e Patrícia Belfiore                       #
#                              Capítulo 03                                     #
################################################################################

################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
# Pacotes utilizados
pacotes <- c("tidyverse", #carregar outros pacotes do R
             "knitr", "kableExtra", #formatação de tabelas
             "sjPlot", #tabelas de contingência
             "DescTools", #diferentes medidas de associação
             "vcd", #diferentes medidas de associação
             "lsr", #coeficiente V de Cramer
             "rcompanion", #coeficiente V de Cramer
             "correlation", #gráfico da correlação de Pearson
             "PerformanceAnalytics", #gráfico da correlação de Pearson com histogramas
             "plotly") #plataforma gráfica

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################################################
#              DESCRIÇÃO E EXPLORAÇÃO DO DATASET 'PlanoSaude'                  #
################################################################################

# Carregamento da base de dados
load(file = "PlanoSaude.RData")

# Visualização da base de dados
PlanoSaude %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

# Tabela de contingência para as variáveis 'operadora' e 'satisfacao'
table(PlanoSaude$operadora, PlanoSaude$satisfacao)

# Exemplo de uma tabela de contingências mais elegante
sjt.xtab(var.row = PlanoSaude$operadora,
         var.col = PlanoSaude$satisfacao)

# Exemplo de uma tabela de contingências mais elegante, com a distribuição
#conjunta de frequências relativas em relação ao total de cada linha,
#ao total de cada coluna e ao total geral
sjt.xtab(var.row = PlanoSaude$operadora,
         var.col = PlanoSaude$satisfacao,
         show.cell.prc = TRUE, #frequências relativas em relação ao total geral
         show.row.prc = TRUE, #frequências relativas em relação ao total de cada linha
         show.col.prc = TRUE) #frequências relativas em relação ao total de cada coluna

# Medida de associação - estatística qui-quadrado e teste
qui2 <- chisq.test(PlanoSaude$operadora, PlanoSaude$satisfacao)
qui2

qui2$statistic
qui2$parameter
qui2$p.value
qui2$method

# Exemplo de uma tabela de contingências mais elegante, com valores observados
#e esperados
sjt.xtab(var.row = PlanoSaude$operadora,
         var.col = PlanoSaude$satisfacao,
         show.exp = TRUE) #valores esperados

# Valores observados, esperados
qui2$observed #valores observados
qui2$expected #valores esperados
qui2$residuals #Resíduos padronizados
qui2$stdres #Resíduos padronizados ajustados


################################################################################
#           DESCRIÇÃO E EXPLORAÇÃO DO DATASET 'Segmentação_Mercado'            #
################################################################################

# Carregamento da base de dados
load(file = "Segmentação_Mercado.RData")

# Visualização da base de dados
Segmentação_Mercado %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

# Coeficiente de contingência
ContCoef(Segmentação_Mercado$roupa, Segmentação_Mercado$regiao, correct = FALSE)

# Coeficiente V de Cramer
cramerV(Segmentação_Mercado$roupa, Segmentação_Mercado$regiao, bias.correct = FALSE)

cramersV(Segmentação_Mercado$roupa, Segmentação_Mercado$regiao)

# Diferentes medidas de associação
Assocs(table(Segmentação_Mercado$roupa, Segmentação_Mercado$regiao))

assocstats(xtabs(~Segmentação_Mercado$roupa + Segmentação_Mercado$regiao))


################################################################################
#                 DESCRIÇÃO E EXPLORAÇÃO DO DATASET 'Notas'                    #
################################################################################

# Carregamento da base de dados
load(file = "Notas.RData")

# Visualização da base de dados
Notas %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

# Coeficiente de correlação de Spearman
cor.test(Notas$simulação, Notas$finanças, method = "spearman",
         alternative = "two.sided")


################################################################################
#              DESCRIÇÃO E EXPLORAÇÃO DO DATASET 'Renda_Estudo'                #
################################################################################

# Carregamento da base de dados
load(file = "Renda_Estudo.RData")

# Visualização da base de dados
Renda_Estudo %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

# Diagrama de dispersão
ggplotly(
  ggplot(Renda_Estudo, aes(x = anosdeestudo, y = rendafamiliar)) +
    geom_point(color = "darkblue", size = 2.5) +
    xlab("Anos de Estudo") +
    ylab("Renda Familiar") +
    theme_light()
)

# Medidas de correlação
# Covariância
cov(Renda_Estudo$rendafamiliar, Renda_Estudo$anosdeestudo)

# Coeficiente de correlação de Pearson
cor(Renda_Estudo$rendafamiliar, Renda_Estudo$anosdeestudo)

# Gráfico da correlação de Pearson
Renda_Estudo %>% 
  correlation(method = "pearson") %>%
  plot()

# Gráfico da correlação de Pearson com histogramas das variáveis
chart.Correlation(Renda_Estudo, histogram = TRUE)

################################################################################