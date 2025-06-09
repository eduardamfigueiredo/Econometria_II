## Econometria II (FEAUSP) - Monitoria em R ##
## Aula 4  - 19.10.2023 - Variáveis Instrumentais em R ##
## Eduarda Figueiredo (eduardafigueiredo@usp.br), Lucas Novaes (lucasnovaes91@usp.br)

# install.packages("AER")
# install.packages("haven")
# install.packages("tidyverse")
# install.packages("xtable")
# install.packages("knitr")
# install.packages("dplyr")
# install.packages("broom")
# install.packages("stargazer")
# install.packages("corrplot")
# install.packages("dplyr")

library("AER")
library("haven")
library("tidyverse")
library("xtable")
library("knitr")
library("dplyr")
library("broom")
library("stargazer")
library("corrplot")
library("dplyr")

# ---- EXERCÍCIO 1 -----

card <- read_dta("C:/Users/Eduarda/OneDrive/Documentos/Doutorado/PAE/Econometria II/card.dta")
# NLS Young Men Cohort of the National Longitudinal Survey (NLS)
# Ano inicial = 1966
# 5.525 homens entre os 14 e 24 anos.
# Acompanhou até o ano de 1981.

# O objetivo é estimar a seguinte equação: Y = /alpha + /delta S_i + /gamma X_i + /varepsilon_i
# Y = log dos rendimentos
# S = anos de escola
# X = matriz de controles (exogenous covariates)
# /varepsilon = termo de erro (habilidades não observadas, p.e.) -> então a escolaridade é tendenciosa.

# Uma das "teorias" é que o termo de erro contém habilidades, então habilidade está correlacionada com escolaridade. 
# -> então a escolaridade é tendenciosa.
# Card (1995): estratégia de variáveis instrumentais.
#              instrumenta-se a escolaridade com uma IV que, nesse caso, será uma variável dummy "faculdade no estado".

y1 <- card$lwage                                                          # Variável Dependente
y2 <- card$educ                                                           # Variável Endógena
x1 <- cbind(card$exper, card$black, card$south, card$married, card$smsa)  # Variáveis Exógenas
x2 <- card$nearc4                                                         # Instrumento


#---------------------------#
# 1º estágio: rodar um OLS  #
#---------------------------#

ols_reg <- lm(y1 ~ y2 + x1,
              data = card) 
summary(ols_reg)
# y2 = 0,071*** (0,003) --> Para cada ano adicional de escolaridade, o rendimento dos entrevistados aumenta 7,1%.

# Quais são os pressupostos para que "faculdade no estado" seja um bom instrumento?
# 1º nearc4 não pode estar correlacionado com o erro do OLS rodado antes.
# 2º nearc4 deve estar correlacionada com educ


#--------------#
# Correlações: #
#--------------#
corr1 <- lm(educ ~ nearc4,
            data = card)
summary(corr1) 

# Salva os valores previstos para educ
card$educ_pred <- corr1$fitted.values

# Faz o y1 como função da educ predita
IV <- lm(y1 ~ educ_pred,
         data = card)
summary(IV)


#-------------------------#
# 2º estágio: MQ2E (2SLS) #
#-------------------------#

iv_reg <- ivreg(y1 ~ y2 + x1 | x1 + x2,
                data = card) 
summary(iv_reg)
# y2 = 0,124* (0,050) -->  Encontramos um retorno de escolaridade muito maior que no OLS

stargazer(ols_reg, iv_reg, IV,
          type = "text",
          column.labels = c("OLS", "IV Reg", "IV")
          )
# educ_pred = 0,188*** (0,021) 
# F-statistic alto.


# ---- EXERCÍCIO 2 -----
options(scipen = 999) #Desligamos a notação cientifica
PNAD2014 <- read_csv("C:/Users/Eduarda/OneDrive/Documentos/Doutorado/PAE/Econometria II/[Base de Dados] PNAD2014-adaptada.csv")
# Vamos estimar a seguinte equação:
# log(salario) = /beta_0 + /beta_1 educ + u
# Que é o retorno sobre o salário da educação das mulheres

#-----------#
# Rodar OLS #
#-----------#

OLS <- lm(log(salario) ~ educ,
          data = PNAD2014)
summary(OLS)

# Podemos ter algum viés? Sim.

#----------#
#   IV     #
#----------#

# Testar correlação:
corr2 <- lm(educ ~ educpai,
            data = PNAD2014)
summary(corr2)
PNAD2014$educ_pred <- corr2$fitted.values

# Salário como função da educação predita:
IV <- lm(log(salario) ~ educ_pred,
         data = PNAD2014)

stargazer(OLS, IV, 
          type="text",
          column.labels = c("OLS","IV"))

# ---- EXERCÍCIO 3 -----

# Se quiser estimar com duas variáveis instrumentais, pode?
# IV1: educmae
# IV2: educpai

# Forma manual:

estagio1 <- lm(educ ~ educpai + educmae,
               data = PNAD2014)


PNAD2014$educ_predMQ2E <- estagio1$fitted.values

estagio2 <- lm(log(salario) ~ educ_predMQ2E,
                data = PNAD2014)

summary(estagio2)

# Forma direta:

SLS2_pacote <- ivreg(formula = log(salario)~ educ| educmae + educpai,
                     data = PNAD2014)
stargazer(OLS, IV, estagio2, SLS2_pacote, 
          type="text",
          column.labels = c("OLS", "IV", "estagio2", "2SLS"))

# E se incluir raça como variável independente também?

SLS2_raca <- ivreg(formula = log(salario)~ educ + raca | educmae + educpai,
                     data = PNAD2014)

stargazer(OLS, IV, estagio2, SLS2_pacote, SLS2_raca,
          type="text",
          column.labels = c("OLS", "IV", "estagio2", "2SLS", "2SLS - 2º modelo"))
