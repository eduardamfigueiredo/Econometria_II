## Econometria II (FEAUSP) - Monitoria em R ##
## Aula 6  - 17.11.2023 - IV aplicado a dados em painel ##
## Eduarda Figueiredo (eduardafigueiredo@usp.br), Lucas Novaes (lucasnovaes91@usp.br)

library("tidyverse")
library("xtable")
library("dplyr")
library("stargazer")
library("readr")
library("wooldridge")
library("AER")
library("plm")


# ---- EXERCÍCIO 1 -----
data("fertil1") # "The Effect of Women’s Schooling on Fertility" (Sander, 1992)

#------------------------------------------------------------
# year: 72 a 84
# educ: anos de escolaridade
# meduc: educação da mãe
# feduc: educação do pai
# age: em anos
# kids: crianças já nascidas
# black: = 1 se negro
# east: = 1 se morava no leste aos 16 anos
# northcen: = 1 se morasse em nc aos 16
# west: = 1 se morasse no oeste aos 16 anos
# farm: = 1 se estiver na fazenda aos 16
# othrural: = 1 se outro rural aos 16
# town: = 1 se morasse na cidade aos 16 anos
# smcity: = 1 se estiver em uma cidade pequena aos 16 anos
# y74: = 1 se ano = 74 ; y76; y78; y80; y82; y84; 
# agesq: age^2
# y74educ; y76educ; y78educ; y80educ; y82educ; y84educ
#------------------------------------------------------------

# Nesse paper é estimado um modelo que explique o número total de nascimentos por mulheres (kids).
# Os fatores controlados pelos autores são: educação, idade, raça, região onde as mulheres residiam
# quando tinham 16 anos e ambiente em que viviam quando tinham essa mesma idade.
summary(fertil1)
freq_year <- table(fertil1$year)
print(freq_year)

# Calcule a equação de OLS agrupando seções transversais independentes ao longo do tempo:
reg1 <- lm(kids ~ educ + age + agesq + black + east + northcen + west + farm + 
                  othrural + town + smcity + y74 + y76 + y78 + y80 + y82 + y84,
           data = fertil1)
summary(reg1)
stargazer(reg1,
          type = "text")
# O ano base é 72.
# y82 = -0,522*** -> mantendo fixos os outros fatores, uma mulher teve, em média, 0,52 menos filhos em 
# 82 do que 72. Isso é uma queda bastante grande: mantendo fixos os outros fatores, prevê-se que 100
# mulheres em 1982 teriam 52 crianças a menos se comparadas com 100 mulheres em 1972.
# Mulheres com mais anos de escoalridade têm menor número de filhos. Com todos os outros fatores
# permanecendo iguais, 100 mulheres com curso superior terão, em média, 51 filhos a menos do que 100 
# mulheres com apenas ensino médio: 0,128(4) = 0,512.
# Portanto, a idade tem efeito redutor sobre a fertilidade. 

#-----
# Em Sander (1992) considera-se a possibilidade de que educ seja endógeno na educação. 
# Como variáveis instrumentais de educ, usam os níveis de educação da mãe e do pai (meduc, feduc).

reg2_IV1 <- ivreg(kids ~ educ + age + agesq + black + east + northcen + west + farm + 
                         othrural + town + smcity + y74 + y76 + y78 + y80 + y82 + y84 | meduc +
                         feduc + age + agesq + black + east + northcen + west + farm + 
                         othrural + town + smcity + y74 + y76 + y78 + y80 + y82 + y84,
                  data = fertil1)

stargazer(reg1, reg2_IV1,
          type = "text")

# A estimativa de MQ2E (2SLS) de \beta_educ é -0,153 (sd = 0,039)comparada com a estimativa do 
# MQO (OLS) de -0,128 (sd = 0,018).
# A estimativa por MQ2E mostra um efeito de certa forma maior da educação sobre a fertilidade, mas
# o erro padrão do MQ2E é mais de 2x maior que o do MQO. 
# Portanto, as estimativas de MQO e MQ2E de \beta_educ não são estatisticamente diferentes. 

reg3_res <- lm(kids ~ educ + age + agesq + black + east + northcen + west + farm + 
                      othrural + town + smcity + y74 + y76 + y78 + y80 + y82 + y84 + reg1$resid,
           data = fertil1)

stargazer(reg1, reg2_IV1, reg3_res,
          type = "text")

# ---- EXERCÍCIO 2 -----

data("jtrain")
# Estimar o efeito de uma hora adicional de treinamento sobre a produtividadde dos trabalhadores.

freq_year_jtrain <- table(jtrain$year)
print(freq_year_jtrain)
summary(jtrain)

# Para os anos de 1987 e 1988, considere o modelo simples de dados em painel:
#     log(scrap_{it}) = \beta_0 + \delta_0 d88_{t} + \beta_1 hrsemp_{it} + \alpha_i + u_{it}
#     t = 1,2
# scrap = taxa de refugo dos produtos da firma i no ano t
# hrsemp_{it} = horas de treinamento por empregado
# \alpha_i = efeito constante não observado da firma

# Podemos estar preocupados com a possibilidade de que hrsemp_{it} seja correlacionado com \alpha_{i},
# que contém a aptidão não medida do trabalhador.
# Fazemos, então, a diferenciação para remover \alpha_i:

d.lscrap <- with(jtrain, 
                 lscrap[year == 1988] - lscrap[year == 1987])
d.hrsemp <- with(jtrain, 
                 hrsemp[year == 1988] - hrsemp[year == 1987])
d.grant <- with(jtrain, 
                grant[year == 1988] - grant[year == 1987])

# Geralmente, poderíamos estimar essa equação por MQO. Mas e se \Delta u_i for correlacionado
# com \Delta hrsemp_i?
# Por exemplo, uma empresa pode empregar trabalhadores mais habilidosos e, simultaneamente,
# reduzir o nível de treinamento. Nesse caso, necessitamos de IV de \Delta hrsemp_i.
# \Delta grant será uma IV válida, desde que \Delta hrsemp_i e \Delta grant sejam correlacionados.
# Então:

reg_1estagio <- lm(d.hrsemp ~ d.grant,
                   data = jtrain)
stargazer(reg_1estagio,
          type = "text")
# Isso confirma que a alteração nas horas de treinamento por trabalhador é forte e positivamente
# relacionada com o recebimento de um subsídio de treinamento de pessoal em 1988.
# Na verdade, o recebimento de um subsídio de treinamento de pessoal aumenta o treinamento por empregado
# em quase 28 horas, e a destinação do subsídio foi responsável por quase 40% da variação em \Delta hrsemp_i.

# Estimação de MQ2E:
reg_2estagio <- ivreg(d.lscrap ~ d.hrsemp | d.grant,
                   data = jtrain)
stargazer(reg_1estagio, reg_2estagio,
          type = "text")

# Significa que 10 horas a mais de treinamento por trabalhador reduziria a taxa de refugo em cerca de
# 14%. 

# ---- EXERCÍCIO 3 -----
data("Crime") # Um painel de 90 unidades (condados) de observações de crimes da Carolina do Norte de 1981 até 1987.
MQ2EFD_reg <- plm(log(crmrte) ~ log(prbarr) + factor(year) | . - log(prbarr) +
                  log(taxpc) + log(mix),
                  data = Crime,
                  model = "fd")

stargazer(MQ2EFD_reg,
          type = "text")
