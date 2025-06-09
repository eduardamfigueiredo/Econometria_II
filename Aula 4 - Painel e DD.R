## Econometria II (FEAUSP) - Monitoria em R ##
## Aula 5  - 10.11.2023 - Dados em Painel e Diff-in-Diff ##
## Eduarda Figueiredo (eduardafigueiredo@usp.br), Lucas Novaes (lucasnovaes91@usp.br)

# install.packages("tidyverse")
# install.packages("xtable")
# install.packages("dplyr")
# install.packages("stargazer")
# install.packages("readr")
# install.packages("plm")

library("tidyverse")
library("xtable")
library("dplyr")
library("stargazer")
library("readr")
library("plm")

# ---- EXERCÍCIO 1 -----

# Investigar os efeitos dos subsídios concedidos pelo governo para o treinamento
# de trabalhadores sobre a taxa de perda da produção das firmas.

jtrain <- read_csv("Doutorado/PAE/Econometria II/jtrain.csv")

# Dados de 1987, 1988 e 1989.
freq_anos <- table(jtrain$year)
print(freq_anos)

# Os subsídios foram concedidos a partir de 1988 e cada firma pode ter recebido
# o mesmo em apenas um ano (1988 ou 1989).

# lscrap -> log da taxa de perda
#           pode ser explicado pelo status da firma em relação à sindicalização (union),
#           efeitos do tempo (d88 e d99), o possível recebimento de subsídio (dummy grant) e
#           o recebimento de subsídio no período passado (dummy grant_1), uma vez que devemos
#           permitir que o efeito do subsídio possa persistir por um período.

# Verificar se o painel está balanceado.
painel_balanceado <- is.pbalanced(jtrain)
print(painel_balanceado)

names(jtrain)

# Selecionar: observarções que ocorreram em 1987 mas não estão relacionadas à sindicalização e
# "pegar" apenas as variáveis de interesse para esse problema.
jtrain_subamostra <- subset(jtrain,
                            subset = (year == 1987 & union == 0),
                            select = c(year, union, lscrap, hrsemp, lsales, lemploy))
# 126 observações e 6 variáveis.

# Testar para missings.
sum(is.na(jtrain_subamostra)) # tem NA -> tem que remover.
jtrain_limpo <- na.omit(jtrain_subamostra)
sum(is.na(jtrain_limpo))# ok.

# Regressão: lscrap = \alpha + \beta_1 hrsemp + \beta_2 lsales + \beta_3 lemploy
reg1 <- lm(lscrap ~ hrsemp + lsales + lemploy,
           data = jtrain_limpo)
summary(reg1)