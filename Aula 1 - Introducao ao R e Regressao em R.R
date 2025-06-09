## Econometria II (FEAUSP) - Monitoria em R ##
## Aula 1  - 18.08.2023 - Introdução ao R e Regressão em R ##
## Eduarda Figueiredo (eduardafigueiredo@usp.br), Dimitri Maturano (dimitricecanm@usp.br)

# install.packages("dbplyr")
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("readr")
# install.packages("stargazer")
# install.packages("tibble")

# Carregando os pacotes 
library(corrplot)  # para fazer matriz de correlação 
library(tidyverse)
library(dbplyr)
library(readr) # pacote para ler base de dados
library(stargazer) # para significância

#------------------------------------------#
# Exercício 1 - base direto de pacote do R #
#------------------------------------------#

base_mtcars <- mtcars # carregando a base 

#-- Análise dos dados
names(base_mtcars)

mean(base_mtcars$mpg) # Média
sd(base_mtcars$mpg) # Desvio-padrão
median(base_mtcars$mpg) # Mediana

# ou podemos puxar tudo de uma vez só:
summary(base_mtcars)

# também pode criar um vetor com as informações encontradas anteriormente:
vetor_infor <- c(mean(base_mtcars$mpg),
                 sd(base_mtcars$mpg),
                 median(base_mtcars$mpg))

cov(base_mtcars$mpg, base_mtcars$wt) # -5,12: mais peso, menos milhas ele faz.
cor(base_mtcars$mpg, base_mtcars$wt) # -0,87
cor(base_mtcars) # para todas as variáveis.

# visualização em gráfico:
corr_mtcars <- base_mtcars %>% 
               cor() %>%
               corrplot(method = "circle")

#--- Regressão

reg1 <- lm(mpg ~ hp + drat + wt,
           data = base_mtcars)

summary(reg1) 

# resíduos -> mostra a distribuição dos resíduos do modelo da reg1, ou seja, diferença entre o valor observado e o valor predito do reg1.
#             então temos mínimo, média e máximo.

# a partir do reg1 criamos a equação de resultado: mpg = 29,39 - 0,03hp + 1,62drat - 3,23wt
# coeficientes e significância

# 28 graus de liberdade (n-k-1 = 32-3-1 = 28)

# R² = 0,84 (+ perto do 1, mais tá explicando)
# R² ajustado = 0,82 (ajustado para o número de k's do modelo)

# F-statistic = 47,88 (melhor "fit" para o modelo)


#----------------------------#
# Exercício 2 - base externa #
#----------------------------#
rm(list = ls()) # para limpar tudo antes de iniciar um novo exercício

car_sales <- read_csv("C:/Users/Dimitri/Documents/pessoal/profissional/econometria 2/Car_sales.csv") # carregando base
# mostrar como puxar na mão também


#-- Análise dos dados

names(car_sales)
str(car_sales) # para verificar se tem alguma variável string

summary(car_sales) # para ter a tabela descritiva de toda a base de dados

corr_mtcars <- car_sales %>% 
               select(Sales_in_thousands, `__year_resale_value`,
                      Price_in_thousands, Engine_size, Horsepower,
                      Wheelbase, Width, Length, Curb_weight, 
                      Fuel_capacity, Fuel_efficiency, Power_perf_factor) %>% 
               cor(use="complete.obs") %>% # caso de NA na base
               corrplot(method = "circle")

#--- Regressão
reg1 <- lm(Sales_in_thousands ~ Price_in_thousands, 
              data = car_sales)
summary(reg1)


reg2 <- lm(Sales_in_thousands ~ Price_in_thousands + Engine_size, 
                 data = car_sales)
summary(reg2) # Aumentou significância e o R².


reg3 <- lm(Sales_in_thousands ~ Price_in_thousands + Engine_size + Engine_size + Horsepower + 
                 Curb_weight + I(Price_in_thousands^2), 
                data = car_sales)
summary(reg3) # Também aumenta


reg4 <- lm(Sales_in_thousands ~ Price_in_thousands + Engine_size + Engine_size + Horsepower + 
                 Curb_weight + I(Price_in_thousands^2) + as.factor(Vehicle_type) + as.factor(Manufacturer), 
                 data = car_sales)
summary(reg4) # Caiu de novo e perdeu a significância -> "Contrário à intuição de que “mais informação é melhor”, 
#                                                         algumas vezes incluir variáveis extras pode enviesar a análise, 
#                                                         impedindo uma interpretação causal dos resultados"
