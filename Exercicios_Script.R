#-------------------------------------------------------------------------------
#  CURSO R 2024
#-------------------------------------------------------------------------------

#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 1
#-------------------------------------------------------------------------------
#     1 - Um experimento de competicao de seis variedades de cana de acucar
#     foi instalado em um delineamento inteiramente casualizado, com 5 repeticoes.
#     Fazer a análise de variancia e utilizar o teste de Tukey
#     para comparar as medias de producao das diversas variedades.

#Banco de dados
(Variedade <- rep(c("CB5034", "CB6245", "IAC6258",
                   "IAC6529", "IAC6814", "IAC6538"),
                    each = 5))
(Prod <- c(112.3, 121.0, 114.3, 112.3, 121.0,
           125.3, 119.7, 120.8, 115.2, 123.2,
           118.4, 120.5, 119.7, 118.4, 120.5,
           127.9, 128.3, 129.5, 125.3, 119.7,
           130.1, 122.4, 126.7, 127.9, 128.3,
           115.2, 123.2, 117.8, 130.1, 122.4))
dados <- data.frame(Variedade,Prod)
dados

#Analise de variancia
modelo<-aov(Prod~Variedade,data=dados)
anova(modelo)

#Teste de Tukey
library(agricolae)
(tukey=with(dados, HSD.test(Prod,Variedade,DFerror=df.residual(modelo),
                            MSerror=deviance(modelo)/df.residual(modelo))))

require(ExpDes.pt)
dic(Variedade, Prod, mcomp = "tukey", sigF = 0.05)


#--------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 2
#-------------------------------------------------------------------------------
#   2 - Com o objetivo de estudar o efeito da idade da castracao no desenvolvimento
#   e producao de suinos, utilizou-se um delineamento em blocos casualizados com
#   4 tratamentos e 4 repeticoes. Os blocos foram utilizados para controlar a
#   variabilidade natural existente entre as leitegadas. Os tratamentos
#   consistiram de - A: suinos castrados aos 56 dias; B: suinos inteiros;
#   C: suinos castrados aos 7 dias e D: suinos castrados aos 21 dias.
#   Com base nos dados de ganhos de peso, em kg, ao final do experimento (252 dias),
#   construa o quadro de analise de variancia. A seguir, use os testes de Duncan
#   e também o de Tukey para comparar as médias de tratamento.

#Banco de dados
Bloco <- rep(c("1", "2", "3", "4"), each=4)
Trat <- rep(c("A", "B", "C", "D"), times=4)

Ganho <- c(93.0, 108.6, 108.9, 102.0,
           77.9, 115.4, 100.2, 96.5,
           94.9, 96.0, 102.1, 116.9,
           97.6, 118.7, 114.1, 117.6)


dados <- data.frame(Bloco, Trat, Ganho)
dados


# Análise de variância
modelo <- aov(Ganho ~ Bloco + Trat, data=dados)
anova(modelo)


#Teste de Duncan
require(ExpDes.pt)
dbc(dados$Trat, dados$Bloco,dados$Ganho, mcomp = "duncan",
    sigF = 0.05)


#Teste de Tukey
require(agricolae)
(tukey=with(dados, HSD.test(Ganho,Trat,DFerror=df.residual(modelo),
                           MSerror=deviance(modelo)/df.residual(modelo))))

#-------------------------------------------------------------------------------
#  Exercicio 3
#-------------------------------------------------------------------------------
#    3- Podemos analisar os dados do Exemplo 2
#    comparando os quatro tratamentos por meio de
#    contrastes ortogonais.
#    Lembrando que A: suinos castrados aos 56 dias;
#    B: suinos inteiros; C: suinos castrados aos 7 dias
#    e D: suínos castrados aos 21 dias.

#Contrastes
require(gmodels)
C<-rbind(c(-1,3,-1,-1),
         c(2,0,-1,-1),
         c(0,0,1,-1))
C

#Ajuste do modelo
modelo<-aov(Ganho~Bloco+Trat, data=dados,
        contrast=list(Trat=make.contrasts(C)))


#Teste F
summary(modelo,split=list(Trat = 1:3))

#Teste t
fit.contrast(modelo,"Trat", C)

#-------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 4
#-------------------------------------------------------------------------------
#   4- Um teste foi realizado para determinar quanto de proteina de soja poderia
#   ser adicionada ao hamburguer sem que os avaliadores percebessem a diferenca
#   de sabor, os  ́dados estao apresentados abaixo. Os hamburgueres testados
#   continham: 0%,  ́5%, 10%, 15% e 20% de proteína de soja. Cada grupo de prova
#   continha um controle (sem soja) e cinco amostras codificadas. Dezesseis
#   avaliadores foram convidados para avaliar essas amostras. Os valores
#   iam de 1 a 9, sendo 1 = extremamente melhor que o controle; 9= extremamente
#   inferior ao controle. Teste se os avaliadores perceberam as diferenças
#   significativas entre as amostras e se as amostras diferiram do grupo controle
#   (sem soja) (Use α = 5%).

#--------------------------------------------------------
# Banco de dados
Bloco<-as.factor(rep(c("Av1", "Av2", "Av3", "Av4", "Av5", "Av6", "Av7", "Av8", "Av9", "Av10",
               "Av11", "Av12", "Av13", "Av14", "Av15", "Av16"), each=5))
Trat<-as.factor(rep(c("sem soja", "5% soja", "10% soja", "15% soja", "20% soja"), times=16))

Nota <- c(1,3,5,1,9,
          3,3,1,7,5,
          7,3,4,4,7,
          1,3,5,4,9,
          6,5,3,2,5,
          4,3,2,7,9,
          1,1,3,3,8,
          2,2,1,1,2,
          2,2,3,2,5,
          5,5,3,5,6,
          3,3,5,5,7,
          3,3,1,5,1,
          3,1,5,3,3,
          7,2,1,3,9,
          5,5,3,5,6,
          5,7,7,3,9)


dados <- data.frame(Bloco, Trat, Nota)
dados
 
#Analise de Variancia
modelo<-aov(Nota~Bloco+Trat, data=dados)
anova(modelo)

#Teste de Dunnett
library(AgroR)
with(dados,dunnett(trat = Trat,
                  resp = Nota,
                  control = "sem soja",
                  block=Bloco,model = "DBC"))
#-------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 5
#-------------------------------------------------------------------------------
#   5- Um pesquisador instalou um experimento para comparar 5 tipos de bacilos
#   (A, B, C, D e E) usados para producao de iogurte. No momento da instalacao
#   do experimento, o pesquisador verificou que o material experimental
#   disponivel (25 unidades de 1 litro de leite) nao era completamente homogeneo
#   entre si, pois apresentavam variacao quanto ao teor de gordura e grau de
#   acidez. Para controlar estas duas fontes de variacao, o pesquisador
#   distribuiu os bacilos ao acaso as amostras de leite de tal forma que cada
#   bacilo pudesse ser testado em todas as condicoes de teor de gordura e grau
#   de acidez. Supondo validas as pressuposicoes para analise de variancia,
#   compare os bacilos usados para a producao de iogurte e de as conclusoes sobre
#   o estudo (utilize o teste de Duncan, se necessario) (use α = 5%)

#Banco de Dados
Teor<-rep(c("1", "2", "3", "4", "5"), each=5)
Grau<-rep(c("1", "2", "3", "4", "5"), times=5)
Bacilo<-c("A", "C", "D", "E", "B",
          "E", "B", "C", "D", "A",
          "C", "E", "A", "B", "D",
          "D", "A", "B", "C", "E",
          "B", "D", "E", "A", "C")
Prod <- c(450, 750, 750, 650, 750,
          620, 990, 910, 890, 720,
          680, 750, 690, 835, 850,
          620, 660, 990, 850, 770,
          780, 830, 760, 875, 890)

dados <- data.frame(Grau, Teor, Bacilo, Prod)
dados

#Grafico
require(ggplot2)
g1<-ggplot(data=dados,
           mapping = aes(x=Teor,
                         y=Grau,
                         fill=Bacilo))+
  geom_tile() +
  geom_text(mapping = aes(label = sprintf("%s\n%g", Bacilo, Prod))) +
  coord_equal()

g2<-ggplot(data=dados,
           mapping = aes(x=Teor,
                         y=Grau,
                         fill=Prod))+
  geom_tile() +
  geom_text(mapping = aes(label = sprintf("%s\n%g", Bacilo, Prod))) +
  scale_fill_distiller(palette="BuPu", direction = 1) + coord_equal();
gridExtra::grid.arrange(g1, g2, nrow = 1)


#Analise de Variancia
modelo<-aov(Prod~Grau+Teor+Bacilo, data=dados)
anova(modelo)

#Teste de Duncan
require(ExpDes.pt)
dql(dados$Bacilo, dados$Grau, dados$Teor, dados$Prod,
         mcomp="duncan", sigF = 0.05)

#-------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 6
#-------------------------------------------------------------------------------
#   6- Num experimento de competicao de variedades de cana forrageira foram usadas
#   5 variedades: A=CO290; B=CO294; C=CO297; D=CO299 e E=CO295, dispostas em um
#   quadrado latino 5x5. O controle feito atraves de blocos horizontais e
#   verticais teve por objetivo eliminar influencias devidas a diferenças de
#   fertilidade em duas direções. Considerando α = 5%, pede-se:a) A analise de
#   Variancia e b) Teste se a variedade C (CO297) difere-se das demais variedades
#   de canas forrageiras.

#Banco de Dados
Coluna <- rep(c("1", "2", "3", "4", "5"), each=5)
Linha <- rep(c("1", "2", "3", "4", "5"), times=5)
Trat <- as.factor(c("D","C","E","B","A",
                    "A","E","B","D","C",
                    "B","A","C","E","D",
                    "C","B","D","A","E",
                    "E","D","A","C","B"))
Prod <- c(432, 724, 489, 494, 515,
          518, 478, 384, 500, 660,
          458, 524, 556, 313, 438,
          583, 550, 297, 486, 394,
          331, 400, 420, 501, 318)

dados <- data.frame(Linha, Coluna, Trat, Prod)
dados

#Grafico
require(ggplot2)
g1<-ggplot(data=dados,
           mapping = aes(x=Coluna,
                         y=Linha,
                         fill=Trat))+
  geom_tile() +
  geom_text(mapping = aes(label = sprintf("%s\n%g", Trat, Prod))) +
  coord_equal()

g2<-ggplot(data=dados,
           mapping = aes(x=Coluna,
                         y=Linha,
                         fill=Prod))+
  geom_tile() +
  geom_text(mapping = aes(label = sprintf("%s\n%g", Trat, Prod))) +
  scale_fill_distiller(palette="YlOrRd", direction = 1) + coord_equal()


gridExtra::grid.arrange(g1, g2, nrow = 1)

#Analise de Variancia
modelo<-aov(Prod~Linha+Coluna+Trat, data=dados)
anova(modelo)

#Teste de Dunnett
require(multcomp)
summary(glht(modelo, linfct = mcp(Trat = c("A-C=0",
                                           "B-C=0",
                                           "D-C=0",
                                           "E-C=0"))))

#-------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 7
#-------------------------------------------------------------------------------
#   7- Em um experimento de fenacao num delineamento inteiramente casualizado
#   foram medidas as producoes de materia seca de cinco variedades de aveia.
#   Construa um quadro de analise de variancia e conclua sobre a diferenca entre
#   as producoes medias das variedades de aveia (Use α = 5%).


#Banco de dados
Trat<-rep(c("A","B","C","D","E"),c(6,4,5,3,6))
Prod<-c(10.34,10.63,9.64,12.63,11.27,9.30,
     7.30,7.69,7.01,8.14,4.94,6.56,5.13,
     6.97,6.27,8.77,9.67,8.36,7.38,8.42,
     7.40,6.81,8.62,8.52)

dados<-data.frame(Trat,Prod)
dados

#Analise de Variancia
modelo<-lm(Prod~Trat,data=dados)
anova(modelo)

##Teste de Tukey
require(agricolae)
(tukey=with(dados, HSD.test(Prod, Trat,DFerror = df.residual(modelo),
                            MSerror = deviance(modelo)/df.residual(modelo))))

#Grafico

require(tidyverse)

(tukey_resumo <- tukey$groups %>%
  rownames_to_column(var = "Trat") %>%
  rename(Prod = Prod, Letras=groups)) 

(resumo_std<-tukey$means %>%
  rownames_to_column(var = "Trat") %>%
  select(Prod, std))

(erros<- merge(tukey_resumo, resumo_std, by ="Prod"))

ggplot(erros, aes(x = Trat, y = Prod, fill = Trat)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Prod - std, ymax = Prod + std), width = 0.2) +
  geom_text(aes(label = Letras), vjust = -4.5, fontface = "bold") +
  labs(title = "",
       x = "Variedades de aveia", y = "Produção Média (ton/ha) de matéria seca") +
  scale_fill_manual(values = c("A" = "darkgoldenrod1", "B" = "aquamarine3", 
                               "C" = "brown1", "D" = "darkolivegreen2",
                               "E" = "maroon")) +
  theme_minimal() +
  theme(legend.position = "none")+
  theme(plot.title=element_text(face="bold",size="15"))+
  ggtitle("Produções de matéria seca segundo variedades",
          subtitle = "Médias seguidas de letras distintas diferem entre si, 
          pelo teste de Tukey (p<0.05)")

#--------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 8
#-------------------------------------------------------------------------------
#   8- O objetivo do experimento foi estudar o desenvolvimento de mudas avaliando
#   a altura, em cm, aos 80 dias de idade, utilizando 3 recipientes (R1: saco
#   plastico pequeno, R2: saco plastico grande e R3: saco laminado) e 2 especies
#   de eucaliptos (E1: Eucalyptus citriodora e E2: Eucalyptus grandis).
#   A partir dos dados apresentados a seguir, realize a analise de variancia.

#Banco de dados
(Recip=rep(c("R1","R2","R3"),each=8))
(Especie=rep(c("E1","E2"),each=4,3))
(Altura=c(26.2,26,25,25.4,
         24.8,24.6,26.7,25.2,
         25.7,26.3,25.1,26.4,
         19.6,21.1,19,18.6,
         22.8,19.4,18.8,19.2,
         19.8,21.4,22.8,21.3))

dados=data.frame(Recip,Especie,Altura)
dados

#Graficos exploratorios

###### R BÁSICO
with(dados, interaction.plot(Recip, Especie, Altura, las=1, col=1:6, bty='l',
                             xlab='Recipientes', ylab='Alturas (cm)', trace.label="Espécies"))


with(dados, interaction.plot(Especie, Recip, Altura, las=1, col=1:6, bty='l',
                             xlab='Espécies', ylab='Alturas (cm)', trace.label="Recipientes"))

##### USANDO GGPLOT2
library(ggplot2)

# Recipientes x Especies
ggplot(dados, aes(x = Recip, y = Altura, color = Especie, group = Especie)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  scale_color_manual(values = c("E1" = "lightslateblue", "E2" = "yellow2")) +
  labs(title = "Interação entre Recipientes e Espécies",
       x = "Recipientes", y = "Altura (cm)") +
  theme_minimal()+
  theme(plot.title=element_text(face="bold",size="15"))

# Especies x Recipientes
ggplot(dados, aes(x = Especie, y = Altura, color = Recip, group = Recip)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  scale_color_manual(values = c("R1" = "maroon1", "R2" = "cyan", "R3" = "chocolate1")) +
  labs(title = "Interação entre Espécies e Recipientes",
       x = "Espécies", y = "Altura (cm)") +
  theme_minimal()+
  theme(plot.title=element_text(face="bold",size="15"))


#Analise de variancia
modelo = with(dados, aov(Altura~Recip*Especie))
anova(modelo)

#Desdobramento da interacao
library(ExpDes.pt)
fat2.dic(dados$Recip, dados$Especie, dados$Altura,
         quali=c(TRUE,TRUE), mcomp="tukey",
         fac.names=c("Recipientes","Espécies"), sigF=0.05)


#--------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 9
#-------------------------------------------------------------------------------
#   9- Com o objetivo de avaliar a utilizacao do farelo bruto na alimentacao
#   de frangos, foi realizado um experimento com uma duracao de 28 dias,
#   envolvendo 4 tratamentos (0, 10, 20 e 30% de substituição) e 5 repeticoes
#   por tratamento, em um delineamento inteiramente casualizado. Cada repeticao
#   foi constituida de 50 pintos de um dia de idade da linhagem Ross, sendo 25
#   machos e 25 femeas. Os ganhos de peso medio (em kg) de cada parcela estao
#   apresentados a seguir:

## Banco de dados
(Farelo<-rep(c(0,10,20,30),each=5))
(Ganho<-c(0.60,0.62,0.61,0.64,0.63,
      0.82,0.85,0.78,0.79,0.80,
      0.79,0.83,0.82,0.81,0.82,
      0.72,0.71,0.69,0.70,0.69))

dados<-data.frame(Farelo,Ganho)
dados

## Analise Exploratoria

# Estatisticas descritivas por grupo
library(tidyverse)
(Resumo_Farelo <- dados %>%
    group_by(Farelo) %>%
    summarise(n = length(Ganho),
              Media = mean(Ganho),
              Var = var(Ganho),
              Desvio = sd(Ganho),
              CV = 100*sd(Ganho)/mean(Ganho)))

# Analise de variancia
modelo<-aov(Ganho~as.factor(Farelo),data=dados)
anova(modelo)

# Grafico de dispersao
require(lattice)
plot(dados$Farelo,dados$Ganho, xlab="Farelo (%)", ylab="Ganhos de peso (kg)",
     pch=20, ylim=c(0.5,1))

(media=tapply(dados$Ganho,dados$Farelo,mean))
points(media ~ unique(Farelo),col="deeppink",pch=19)

# Regressao Polinomial
require(ExpDes.pt)
dic(Farelo, Ganho, quali=FALSE)

# Grafico da curva ajustada
plot(media ~ unique(Farelo), xlab="Farelo (%)",
     ylab="Ganhos de peso (kg)", xlim=c(0,30), ylim=c(0.5,1), pch=19)

modeloaj= lm(Ganho ~ Farelo + I(Farelo^2), dados)

coef(modeloaj)
summary(modeloaj)

curve(coef(modeloaj)[1] + coef(modeloaj)[2]*x + coef(modeloaj)[3]*x^2,
      add=T, col="gold")


##### USANDO GGPLOT2
library(ggplot2)
library(dplyr)

(media_ganho <- dados %>%
  group_by(Farelo) %>%
  summarise(mean_Ganho = mean(Ganho)))


# Gráfico de dispersão 
ggplot(dados, aes(x = Farelo, y = Ganho)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", size = 3, color = "deeppink") +
  labs(x = "Farelo (%)", y = "Ganhos de peso (kg)",
    title = "Relação entre Farelo na Alimentação e Ganhos de Peso
    em Frangos") +
  theme_minimal()+
  theme(plot.title=element_text(face="bold",size="15"))


# Gráfico da curva ajustada 
ggplot(media_ganho, aes(x = Farelo, y = mean_Ganho)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "gold") +
  labs(x = "Farelo (%)",y = "Ganhos de peso (Kg)",
    title = "Curva Ajustada da Regressão Polinomial" ) +
  xlim(0, 30) + ylim(0.5, 1) +
  theme_minimal()+
  theme(plot.title=element_text(face="bold",size="15"))

#--------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

#-------------------------------------------------------------------------------
#  Exercicio 10
#-------------------------------------------------------------------------------
#   10-  Em um experimento de substituicao do farelo de soja pelo farelo de
#   girassol na ração de suinos, montou-se um experimento fatorial 2x5, com
#   os fatores Sexo (S=1: macho e S=2: femea) e Racao com substituicao de farelo
#   de soja por farelo de girassol (G = 0, 25, 50, 75 e 100%), utilizando-se
#   30 suinos (15 machos e 15 femeas) castrados da raça Duroc-Jersey, num
#   delineamento em blocos casualizados com 3 repetições, formados de acordo
#   com os grupos de pesos iniciais.


#Banco de dados
(girassol=rep(c(0,25,50,75,100),e=3,2))
(sexo=as.factor(rep(c("macho","femea"),e=15)))
(bloco=as.factor(rep(c(1,2,3),10)))
GP=c(85.0,86.0,84.0,
     94.5,96.0,95.8,
     99.5,98.0,104.0,
     93.0,96.0,90.5,
     83.0,80.0,78.5,
     77.9,83.2,83.5,
     71.5,73.5,70.5,
     67.5,63.5,65.0,
     71.5,70.8,72.5,
     89.5,91.8,92.9)

dados<-data.frame(girassol,sexo,bloco,GP)
dados
str(dados)

#Analise de variancia
mod = with(dados, aov(GP~bloco+sexo*as.factor(girassol)))
anova(mod)

#Desdobramento do esquema fatorial
library(ExpDes.pt)
fat2.dbc(sexo,girassol,bloco,GP,
         quali=c(TRUE,FALSE), mcomp="tukey",
         fac.names=c("Sexo","Girassol"), sigF=0.05)

# Grafico das curvas ajustadas

###### USANDO R BASICO
###  Estatisticas descritivas das medias por sexo

(media=tapply(dados$GP,list(dados$sexo,dados$girassol),mean))

# Grafico da curva ajustada para as femeas
media[1,]
plot(media[1,] ~ unique(girassol), xlab="Farelo de girassol (%)",
     ylab="Ganhos de peso (kg)", xlim=c(0,100), ylim=c(60,100),
     pch=19, main="Fêmeas")

dados1<-subset(dados, dados$sexo=="femea")
modeloaj= lm(GP ~ girassol + I(girassol^2)+ I(girassol^3), dados1)
summary(modeloaj)

coef(modeloaj)
curve(coef(modeloaj)[1] + coef(modeloaj)[2]*x + coef(modeloaj)[3]*x^2+coef(modeloaj)[4]*x^3,
      add=T, col="firebrick1")

#Grafico da curva ajustada para os machos
media[2,]
plot(media[2,] ~ unique(girassol), xlab="Farelo de girassol (%)",
     ylab="Ganhos de peso (kg)", xlim=c(0,100), ylim=c(78,105),
     pch=19, main="Machos")

dados2<-subset(dados, dados$sexo=="macho")
modeloaj1= lm(GP ~ girassol + I(girassol^2), dados2)
summary(modeloaj1)

coef(modeloaj1)
curve(coef(modeloaj1)[1] + coef(modeloaj1)[2]*x + coef(modeloaj1)[3]*x^2,
      add=T, col="turquoise4")


####### USANDO GGPLOT2
library(ggplot2)

# Gráfico para fêmeas com as médias
(media_femea <- dados1 %>%
  group_by(girassol) %>%
  summarise(mean_GP = mean(GP)))


ggplot(media_femea, aes(x = girassol, y = mean_GP)) +
  geom_point() +
  geom_smooth(data = dados1, aes(x = girassol, y = GP),
              method = "lm", formula = y ~ x + I(x^2) + I(x^3),
              se = FALSE, color = "firebrick1") +
  labs(x = "Substituição de Farelo de Girassol (%)",
       y = "Ganhos de peso (kg)",
       title = "Curva Ajustada - Fêmeas") +
  xlim(0, 100) + ylim(60, 100) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size =15))

# Gráfico para fêmeas com todos os pontos
ggplot(dados1, aes(x = girassol, y = GP)) +
  geom_point(color= "black", pch=19) +
  geom_smooth(data = dados1, aes(x = girassol, y = GP),
              method = "lm", formula = y ~ x + I(x^2) + I(x^3),
              se = FALSE, color = "firebrick1") +
  labs(x = "Substituição de Farelo de Girassol (%)",
       y = "Ganhos de peso (kg)",
       title = "Curva Ajustada - Fêmeas") +
  xlim(0, 100) + ylim(60, 100) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size =15))

# Gráfico para machos com as médias
(media_macho <- dados2 %>%
    group_by(girassol) %>%
    summarise(mean_GP = mean(GP)))

ggplot(media_macho, aes(x = girassol, y = mean_GP)) +
  geom_point() +
  geom_smooth(data = dados2, aes(x = girassol, y = GP),
              method = "lm", formula = y ~ x + I(x^2),
              se = FALSE, color = "turquoise4") +
  labs(x = "Substituição de Farelo de Girassol (%)",
       y = "Ganhos de peso (Kg)",
       title = "Curva Ajustada - Machos") +
  xlim(0, 100) + ylim(78, 105) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = "15"))

# Gráfico para machos com todos os pontos
ggplot(dados2, aes(x = girassol, y = GP)) +
  geom_point() +
  geom_smooth(data = dados2, aes(x = girassol, y = GP),
              method = "lm", formula = y ~ x + I(x^2),
              se = FALSE, color = "turquoise4") +
  labs(x = "Substituição de Farelo de Girassol (%)",
       y = "Ganhos de peso (Kg)",
       title = "Curva Ajustada - Machos") +
  xlim(0, 100) + ylim(78, 105) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = "15"))
#--------------------------------------------------------------------------------
#Removendo os objetos
rm(list=ls(all=TRUE))

