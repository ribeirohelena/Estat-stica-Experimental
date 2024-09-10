#-------------------------------------------------------------------------------
#  ATIVIDADES EXTRAS - CURSO R 2024
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#  Exercício 1 - DIC
#-------------------------------------------------------------------------------
#   1- Os dados da tabela referem-se a produtividade de milho (Kg/100m2) de
#   quatro variedades diferentes, em um experimento instalado segundo o
#   delineamento inteiramente casualizado (DIC). Considerando que as
#   pressuposicoes para a analise de variância estao satisfeitas, verifique se
#   existe diferenca significativa entre as variedades (Use α = 5%).

#Banco de dados
Trat<-rep(c("A","B","C","D"),each=5)
Prod<-c(25, 26, 20, 23, 21,
        31, 25, 28, 27, 24,
        22, 26, 28, 25, 29,
        33, 29, 31, 34, 28)

dados<-data.frame(Trat,Prod)
dados

#Analise de Variancia
modelo<-aov(Prod~Trat,data=dados)
anova(modelo)

#Teste de Tukey
require(agricolae)
(tukey=with(dados, HSD.test(Prod,Trat,DFerror=df.residual(modelo),
                            MSerror=deviance(modelo)/df.residual(modelo))))

require(ExpDes.pt)
dic(Trat, Prod, mcomp = "tukey", sigF = 0.05)

#--------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 2 - Regressao Polinomial
#-------------------------------------------------------------------------------
#   2- Num experimento estudou-se o efeito das diferentes concentracoes de
#   agentes gelificantes nas características sensoriais de doce de banana sem
#   acucar. O ensaio, organizado em blocos completos casualizados, abrangeu
#   duas fases distintas e foi constituído de 5 tratamentos e 5 repetições, com
#   8 doces de banana por unidade experimental. Os tratamentos na 1a fase foram
#   formados por doces de banana que continham 0, 15, 30, 45, 60% de concentracao
#   de agentes gelificantes. Encontre o modelo de regressao adequado para o
#   conjunto de dados (use α = 5%).

# Banco de dados
(Bloco<- rep(1:5, 5))
(Gelificante<- rep(c(0, 15, 30, 45, 60), each=5))
(Nota<- c(6.5, 6.4, 6.2, 5.8, 7.3,
          7.1, 7.4, 6.9, 7.3, 7.0,
          7.5, 8.1, 6.7, 7.4, 7.7,
          7.2, 7.0, 6.9, 6.7, 6.5,
          6.4, 6.5, 6.0, 6.3, 6.2))

dados <- data.frame(Bloco, Gelificante, Nota)
dados

# Analise Exploratoria

# Estatísticas descritivas por grupo
library(tidyverse)
(Resumo_Gelificante <- dados %>%
    group_by(Gelificante) %>%
    summarise(n = length(Nota),
              Media = mean(Nota),
              Var = var(Nota),
              Desvio = sd(Nota),
              CV = 100*sd(Nota)/mean(Nota)))

# Análise de variância
modelo <- aov(Nota ~ as.factor(Bloco)+ as.factor(Gelificante), data=dados)
anova(modelo)

# Gráfico de dispersão

require(lattice)
plot(Nota ~ Gelificante, xlab="Concentração de Agentes Gelificantes (%)",
     ylab="Notas sensoriais", pch=20, ylim=c(5, 8.5))

(media <- tapply(dados$Nota, dados$Gelificante, mean))
points(media ~ unique(Gelificante), col="violet", pch=19)

# Regressao Polinomial
require(ExpDes.pt)
dbc(dados$Gelificante, dados$Bloco, dados$Nota, quali=FALSE)

# Regressão Polinomial
modeloaj <- lm(Nota ~ Gelificante + I(Gelificante^2), dados)
summary(modeloaj)

coef(modeloaj)


# Gráfico da curva ajustada

###### USANDO R BÁSICO
plot(media ~ unique(Gelificante), xlab="Concentração de Agentes Gelificantes (%)",
     ylab="Notas Sensoriais", xlim=c(0, 60), ylim=c(5, 8.5), pch=19)

curve(coef(modeloaj)[1] + coef(modeloaj)[2]*x + coef(modeloaj)[3]*x^2,
      add=T, col="dodgerblue")

##### USANDO GGPLOT2
library(ggplot2)
library(dplyr)

(media_gelificante <- dados %>%
    group_by(Gelificante) %>%
    summarise(mean_Nota = mean(Nota)))

# Gráfico da curva ajustada
ggplot(media_gelificante, aes(x = Gelificante, y = mean_Nota)) +
  geom_point() +
  geom_smooth(data = dados, aes(x = Gelificante, y = Nota), method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "dodgerblue") +
  labs(x = "Concentração de Agentes Gelificantes (%)", y = "Notas Sensoriais", title = "Regressão Polinomial - Efeito da Concentração de Gelificantes") +
  xlim(0, 60) + ylim(5, 8.5) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = "15"))

#--------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 3 - Fatorial Qualitativo e Quantitativo
#-------------------------------------------------------------------------------
#   3- Um pesquisador, interessado na comparacao da producao (em kg/ha) de dois
#    capins (Panicum maximum e Brachiaria brisantha) em determinado tipo de solo,
#    bem como na sua interação com tres niveis de adubacao (0, 100 e 200 kg ha-1
#    de N por hectare), planejou um experimento num delineamento inteiramente
#     casualizado com 6 repeticoes e com os tratamentos num esquema fatorial.

#  a)  Construa um quadro de análise de variancia (ANOVA) e realize os testes
#      adequados. Calcular uma estimativa da media geral, da variancia dos dados
#       e o coeficiente de variacao.

#  b)	Faca o desdobramento da interacao para estudar o comportamento das doses
#     de nitrogenio para cada um dos capins.

#  c)  Como o fator dose e quantitativo, faça testes de tendencia linear e
#      quadratica de resposta media e ajuste o polinomio correspondente.
#      Comente sobre o comportamento da producao media de cada capim com o
#      aumento das doses de nitrogenio.

#  d) 	Faca tambem o desdobramento da interacao para estudar o comportamento
#        dos capins em cada uma das doses de nitrogenio.

# Banco de dados
(Especie<-as.factor(rep(c("P","B"),each=18)))
(Dose<-rep(rep(c(0,100,200),each=6),2))
(Trat<- c(250, 290, 280, 250, 260, 200,
          200, 190, 200, 210, 230, 250,
          120, 130, 125, 210, 180, 210,
          280, 270, 290, 285, 335, 310,
          300, 310, 320, 330, 370, 390,
          200, 210, 205, 290, 260, 290))

(dados = data.frame(Especie, Dose,Trat))
str(dados)

#Analise de variancia
mod = with(dados, aov(Trat~Especie*as.factor(Dose)))
anova(mod)

# Análise de Variancia
require(ExpDes.pt)
with(dados, fat2.dic(Especie, Dose, Trat, quali=c(TRUE,FALSE),
                     mcomp="tukey",fac.names=c("Capins", "Doses")))

# Grafico das curvas ajustadas

###### USANDO R BÁSICO

###  Estatisticas descritivas das medias por Especie

(media=tapply(dados$Trat,list(dados$Especie,dados$Dose),mean))

# Grafico da curva ajustada para Brachiaria
media[1,]
plot(media[1,] ~ unique(Dose), xlab="Doses",
     ylab="Produção (Kg/ha)", xlim=c(0,200), ylim=c(100,550), pch=19,
     main="Capim Brachiaria")

dados1<-subset(dados, dados$Especie=="B")
modeloaj= lm(Trat ~ Dose + I(Dose^2), dados1)
summary(modeloaj)

coef(modeloaj)
curve(coef(modeloaj)[1] + coef(modeloaj)[2]*x + coef(modeloaj)[3]*x^2,
      add=T, col="darkmagenta")


#Grafico da curva ajustada para Capim Panicum
media[2,]
plot(media[2,] ~ unique(Dose), xlab="Doses",
     ylab="Produção (Kg/ha)",xlim=c(0,200), ylim=c(100,550), pch=19,
     main="Capim Panicum")
dados2<-subset(dados, dados$Especie=="P")
modeloaj1= lm(Trat ~ Dose, dados2)
summary(modeloaj1)

coef(modeloaj1)
curve(coef(modeloaj1)[1] + coef(modeloaj1)[2]*x,
      add=T, col="chartreuse")


####### USANDO GGPLOT2
library(ggplot2)
library(dplyr)

(media_prod <- dados %>%
    group_by(Especie, Dose) %>%
    summarise(mean_Trat = mean(Trat)))

# Gráfico para Brachiaria com as médias
ggplot(media_prod %>% filter(Especie == "B"), aes(x = Dose, y = mean_Trat)) +
  geom_point() +
  geom_smooth(data = dados1, aes(x = Dose, y = Trat), method = "lm",
              formula = y ~ x + I(x^2), se = FALSE, color = "darkmagenta") +
  labs(x = "Doses", y = "Produção (Kg/ha)", title = "Capim Brachiaria") +
  xlim(0, 200) + ylim(100, 550) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = "15"))

# Gráfico para Brachiaria com todos os pontos
ggplot(dados1 %>% filter(Especie == "B"),
       aes(x = Dose, y = Trat)) +
  geom_point(color= "black", pch=19) +
  geom_smooth(data = dados1, aes(x = Dose, y = Trat),
              method = "lm",formula = y ~ x + I(x^2),
              se = FALSE, color = "darkmagenta") +
  labs(x = "Doses", y = "Produção (Kg/ha)",
       title = "Capim Brachiaria") +
  xlim(0, 200) + ylim(100, 550) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = "15"))


# Gráfico para Panicum com as médias
ggplot(media_prod %>% filter(Especie == "P"),
       aes(x = Dose, y = mean_Trat)) +
  geom_point() +
  geom_smooth(data = dados2, aes(x = Dose, y = Trat),
              method = "lm", formula = y ~ x,
              se = FALSE, color = "chartreuse") +
  labs(x = "Doses", y = "Produção (Kg/ha)",
       title = "Capim Panicum") +
  xlim(0, 200) + ylim(100, 550) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = "15"))


# Gráfico para Brachiaria com todos os pontos
ggplot(dados2 %>% filter(Especie == "P"),
       aes(x = Dose, y = Trat)) +
  geom_point(color= "black", pch=19) +
  geom_smooth(data = dados2, aes(x = Dose, y = Trat),
              method = "lm",formula = y ~ x,
              se = FALSE, color = "chartreuse") +
  labs(x = "Doses", y = "Produção (Kg/ha)",
       title = "Capim Panicum") +
  xlim(0, 200) + ylim(100, 550) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = "15"))

