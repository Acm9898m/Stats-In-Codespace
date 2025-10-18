################################################################################
#                       MANUAL DE ANÁLISE DE DADOS                             #
#                  Luiz Paulo Fávero e Patrícia Belfiore                       #
#                              Capítulo 02                                     #
################################################################################

################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
# Pacotes utilizados
pacotes <- c("tidyverse", #carregar outros pacotes do R
             "knitr", "kableExtra", #formatação de tabelas
             "questionr", #tabela de frequências - função freq
             "e1071", #medidas de assimetria e curtose
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
#                DESCRIÇÃO E EXPLORAÇÃO DO DATASET 'Cotações'                  #
################################################################################

# Carregamento da base de dados 'Cotações'
load(file = "Cotações.RData")

# Visualização da base de dados 'Cotações'
Cotações %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

# Tabela de frequências da variável 'preço'
table(Cotações$preço)
freq(Cotações$preço)

# Estatísticas descritivas univariadas da variável 'preço'
summary(Cotações$preço)
mean(Cotações$preço)
median(Cotações$preço)
min(Cotações$preço)
max(Cotações$preço)
quantile(Cotações$preço, .25)
quantile(Cotações$preço, .75)
sd(Cotações$preço)
var(Cotações$preço)

# Medidas de assimetria e curtose para a variável 'preço'
skewness(Cotações$preço, type = 1) # igual ao Stata
skewness(Cotações$preço, type = 2) # igual ao SPSS
kurtosis(Cotações$preço, type = 2) # igual ao SPSS

# Gráficos: histograma, ramo-e-folhas e boxplot para a variável 'preço'
# Histograma simples
hist(Cotações$preço)

# Histograma por meio da função 'ggplot'
Cotações %>%
  ggplot(aes(x = preço)) +
  geom_histogram(aes(y = ..density..),
                 color = "grey50",
                 fill = "darkorchid",
                 bins = 7,
                 alpha = 0.6) +
  labs(x = "Preço",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

# Histograma com curva normal por meio da função 'ggplot'
Cotações %>%
  ggplot(aes(x = preço)) +
  geom_histogram(aes(y = ..density..),
                 color = "grey50",
                 fill = "darkorchid",
                 bins = 7,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(Cotações$preço),
                            sd = sd(Cotações$preço)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "darkorchid") +
  labs(x = "Preço",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        plot.title = element_text(size=15)
  ) +
  ggtitle("Histograma da variável 'preço' com curva normal por meio da função 'ggplot'")

# Gráfico de ramo-e-folhas para a variável 'preço'
stem(Cotações$preço, scale = 2)
# O argumento 'scale = 2' faz com que o gráfico seja aproximadamente
#duas vezes maior que o padrão.

# Boxplot da variável 'preço'
# Boxplot simples
boxplot(Cotações$preço)

boxplot(Cotações$preço, 
        ylab = "Preço",
        main = "Preço",
        notch = FALSE,
        varwidth = FALSE,
        col = c("lightblue")
)

# Boxplot por meio da função 'ggplot'
Cotações %>%
  ggplot(aes(y = preço, x = "")) +
  geom_boxplot(fill = "lightblue",      # cor da caixa
               alpha = 0.7,             # transparência
               color = "black",         # cor da borda
               outlier.colour = "red",  # cor dos outliers
               outlier.shape = 15,      # formato dos marcadores dos outliers
               outlier.size = 2.5) +    # tamanho dos marcadores dos outliers
  geom_jitter() +
  labs(y = "Preço") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position="none",
        plot.title = element_text(size=15)
        ) +
  ggtitle("Boxplot da variável 'preço' por meio da função 'ggplot'") +
  xlab("")

################################################################################