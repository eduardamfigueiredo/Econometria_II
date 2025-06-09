## Econometria II (FEAUSP) - Monitoria em R ##
## Aula 3  - 15.09.2023 - Matching em R ##
## Eduarda Figueiredo (eduardafigueiredo@usp.br), Dimitri Maturano (dimitricecanm@usp.br)

#install.packages("haven")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packags("stargazer")
#install.packages("magrittr") # para %$% pipes
#install.packages("ggplot2")
#install.packages("sjmisc")
#install.packages("readr")
#install.packages("tableone")

library("haven")
library("tidyverse")
library("readxl")
library("stargazer")
library("magrittr")# para %$% pipes
# %>% é usado principalmente para encadear operações em objetos de dados
# %$% é usado para acessar variáveis dentro de objetos de dados de forma mais conveniente
library("ggplot2")
library("sjmisc")
library("readr")
library("tableone")

#---- EXERCÍCIO 1 ----

titanic <- read_excel("Doutorado/PAE/Econometria II/titanic.xls")

titanic <- titanic %>% mutate(d = case_when(class == "1st class" ~ 1,
                                            TRUE ~ 0),
                              sobrevivente = case_when(survived == "yes" ~ 1,
                                                       TRUE ~ 0),
                              sexo = case_when(sex == "man" ~ 1,
                                               TRUE ~ 0),
                              faixaetaria = case_when(age == "adults" ~ 1,
                                                      TRUE ~ 0))


ey1 <- titanic %>% filter(d == 1) %>% 
                   pull(sobrevivente) %>%
                   mean

ey0 <- titanic %>% filter(d == 0) %>% 
                   pull(sobrevivente) %>%
                   mean

# Calculando uma diferença simples na média dos resultados (SDO)
sdo <- ey1 - ey0
print(sdo) 
# 0,354 = estar sentado na 1ª classe aumenta a probabilidade de sobrevivência em 35,4% 
# No entanto, como isso não é ajustado para fatores de confusão observáveis, como idade e gênero, 
# é uma estimativa enviesada do Efeito Médio Causal (ATE). 

# Para arrumar isso, usaremos a ponderação por subclassificação para controlar esses fatores de confusão. 

# 1º) Estratificar os dados em quatro grupos:

titanic <- titanic %>% mutate (s = case_when(sexo == 0 & faixaetaria == 1 ~ 1, # mulher adulta
                                             sexo == 0 & faixaetaria == 0 ~ 2, # mulher criança
                                             sexo == 1 & faixaetaria == 1 ~ 3, # homem adulto
                                             sexo == 1 & faixaetaria == 0 ~ 4, # homem criança
                                             TRUE ~ 0))

tabelafrequencia_s <- table(titanic$s)
print(tabelafrequencia_s)

# 2º) Calcular a diferença nas probabilidades de sobrevivência para cada grupo.

ey11 <- titanic %>% filter(s == 1 & d == 1) %$% # mulher adulta na 1ª classe
                    mean(sobrevivente)

ey10 <- titanic %>% filter(s == 1 & d == 0) %$% # mulher adulta fora da 1ª classe
                    mean(sobrevivente)

ey21 <- titanic %>% filter(s == 2 & d == 1) %$% # mulher criança na 1ª classe
                    mean(sobrevivente)

ey20 <- titanic %>% filter(s == 2 & d == 0) %$% # mulher criança fora da 1ª classe
                    mean(sobrevivente)

ey31 <- titanic %>% filter(s == 3 & d == 1) %$% # homem adulto na 1ª classe
                    mean(sobrevivente)

ey30 <- titanic %>% filter(s == 3 & d == 0) %$% # homem adulto fora da 1ª classe
                    mean(sobrevivente)

ey41 <- titanic %>% filter(s == 4 & d == 1) %$% # homem criança na 1ª classe
                    mean(sobrevivente)

ey40 <- titanic %>% filter(s == 4 & d == 0) %$% # homem criança fora da 1ª classe
                    mean(sobrevivente)

diff1 = ey11 - ey10
diff2 = ey21 - ey20
diff3 = ey31 - ey30
diff4 = ey41 - ey40

# 3º) Calcular o número de pessoas nos grupos que não estavam na 1ª classe e dividir pelo número total da população que estava fora da 1ª classe. 
# Serão pesos específicos para cada estrato.

obs = nrow(titanic %>% filter(d == 0))

wt1 <- titanic %>% filter(s == 1 & d == 0) %$%
                   nrow(.)/obs

wt2 <- titanic %>% filter(s == 2 & d == 0) %$%
                   nrow(.)/obs

wt3 <- titanic %>% filter(s == 3 & d == 0) %$%
                   nrow(.)/obs

wt4 <- titanic %>% filter(s == 4 & d == 0) %$%
                   nrow(.)/obs

# 4º) Calcular a taxa média ponderada de sobrevivência usando os pesos dos estratos.

wate = diff1*wt1 + diff2*wt2 + diff3*wt3 + diff4*wt4

stargazer(wate, 
          sdo, 
          type = "text")

# Uma vez que condicionamos os fatores de confusão ao gênero e faixa etária, os assentos na 1ª classe 
# têm uma probabilidade de sobrevivência muito menor associado a ele (mas ainda grande!!). 
# O ATE ponderado é 18,9%, contra o SDO, que é de 35,4%.

#---- EXERCÍCIO 2 ----

# E se preenchermos o resultado potencial que falta para cada tratamento unidade usando uma unidade de 
# grupo de controle que estava “mais próxima” do tratamento unidade de grupo para algum confounder X? 
# Isso nos daria estimativas de todos os contrafactuais dos quais poderíamos simplesmente tirar a média das diferenças. 
# Isso também alcançará o equilíbrio das covariáveis. --> MATCHING

training_example <- read_excel("Doutorado/PAE/Econometria II/training_example.xls") %>% 
                    slice(1:20)

# Olhar a base: 
#           2 amostras: uma lista de participantes de um programa de treinamento profissional e uma lista de não participantes.

training_slice <- training_example %>% select(unit_treat, age_treat, earnings_treat)
training_slice <- na.omit(training_slice)

media_treinantes <- mean(training_slice$age_treat) # 24 anos
media_naotreinantes <- mean(training_example$age_control) # 31 anos(32)
# Assim, as pessoas do grupo de controle são mais velhas.

# Uma vez que os salários normalmente aumentam com a idade, podemos suspeitar que parte da razão pela 
# qual os ganhos médios são mais altos é porque o o grupo de controle é mais antigo.

training_slice_earnings_treat <- as.data.frame(t(training_slice$earnings_treat))

media_salariotreinantes <- mean(as.numeric(training_slice_earnings_treat)) # 11.075,00
media_salarionaotreinantes <- mean(training_example$earnings_control) # 11.101,25

# Porém, a covariável não tá equilibrada.

# Olhando para a variável de idade:

ggplot(training_example, 
       aes(x=age_treat)) + 
  stat_bin(bins = 10, 
           na.rm = TRUE) +
  labs(title = "Idade dos tratados",
       x = "Idade",
       y = "Contagem")

ggplot(training_example, 
       aes(x=age_control)) +
  geom_histogram(bins = 10, 
                 na.rm = TRUE) +
  labs(title = "Idade dos controles",
       x = "Idade",
       y = "Contagem")

# Toda a distribuição de idade entre as amostras são diferentes.
# Então, vamos usar nosso algoritmo de correspondência e criar os contrafactuais faltantes para cada unidade do grupo de tratamento.

training_slice_matching <- training_example %>% select(unit_matched, age_matched, earnings_matched)
training_slice_matching <- na.omit(training_slice_matching)

media_matching <- mean(training_slice_matching$age_matched) # 24 anos - é a mesma

ggplot(training_example, 
       aes(x=age_matched)) +
  geom_histogram(bins = 10, 
                 na.rm = TRUE) +
  labs(title = "Idade no matching",
       x = "Idade",
       y = "Contagem")

media_salariomatching <- mean(training_slice_matching$earnings_matched) # 9.380


#---- EXERCÍCIO 3 ----

nsw_dw <- read_csv("nsw_dw.csv")

summary(nsw_dw)

nsw_dw %>% filter(treat == 1) %>% 
           summary(re78)

mean1 <- nsw_dw %>% filter(treat == 1) %>% 
                    pull(re78) %>% 
                     mean()

nsw_dw$y1 <- mean1

nsw_dw %>% filter(treat == 0) %>% 
            summary(re78)

mean0 <- nsw_dw %>% 
          filter(treat == 0) %>% 
          pull(re78) %>% 
          mean()

nsw_dw$y0 <- mean0

ate <- unique(nsw_dw$y1 - nsw_dw$y0) 
# Programa de treinamento profissional fez com que os rendimentos reais em 1978 aumentassem em 1.794,34.

nsw_dw <- nsw_dw %>% filter(treat == 1) %>% 
                     select(-y1, -y0)

print(nsw_dw)

#------

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

nsw_dw_cpscontrol <- read_data("cps_mixtape.dta") %>% 
  bind_rows(nsw_dw) %>% 
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         interaction1 = educ*re74,
         re74sq = re74^2,
         re75sq = re75^2,
         interaction2 = u74*hisp)

# estimação logit
logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                  u75 + interaction1, family = binomial(link = "logit"), 
               data = nsw_dw_cpscontrol)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
 mutate(pscore = logit_nsw$fitted.values)

# média dos pesos(pscore) 
pscore_control <- nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  pull(pscore) %>% 
  mean()

pscore_treated <- nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  pull(pscore) %>% 
  mean()

# histogramas
nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

# O valor médio do escore de propensão para o grupo de tratamento é 0,43, e a média para o grupo controle CPS é 0,007.