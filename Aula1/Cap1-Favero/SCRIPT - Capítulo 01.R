################################################################################
#                       MANUAL DE ANÁLISE DE DADOS                             #
#                  Luiz Paulo Fávero e Patrícia Belfiore                       #
#                              Capítulo 01                                     #
################################################################################

################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
# gerir conflitos
install.packages("conflicted")
library(conflicted)

# Pacotes utilizados
pacotes <- c("tidyverse", #carregar outros pacotes do R
             "knitr", "kableExtra") #formatação de tabelas

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
#                 DESCRIÇÃO E EXPLORAÇÃO DO DATASET 'VarQuanti'                #
################################################################################

# Carregamento da base de dados
load(file = "VarQuanti.RData")

# Visualização da base de dados
VarQuanti %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

# Escalas de mensuração das variáveis
sapply(VarQuanti, FUN = class)
glimpse(VarQuanti)

################################################################################