## Econometria II (FEAUSP) - Monitoria em R ##
## Aula 5  - 10.11.2023 - Dados em Painel e Diff-in-Diff ##
## Eduarda Figueiredo (eduardafigueiredo@usp.br), Lucas Novaes (lucasnovaes91@usp.br)

library("tidyverse")
library("xtable")
library("dplyr")
library("stargazer")
library("readr")
library("plm")
library("ggplot2")
library("sandwich")
library("lmtest")
library("rstatix")
library("haven")
library("estimatr")
library("ggplot2")
library("sjlabelled")
library("ggrepel")
library("scales")
library("ggpubr")

# ---- EXERCÍCIO 1 -----

data("EmplUK")    # Painel desbalanceado --> emprego e salários no Reino Unido

form1 <- log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + 
                    lag(log(wage), 2) + lag(log(wage), 3) + 
                    diff(log(capital), 2) + diff(log(capital), 3)

reg1 <- plm(formula = form1, 
            data = EmplUK, 
            model = "within")
summary(reg1)

reg2 <- plm(formula = form1,
            data = EmplUK,
            model = "pooling")
summary(reg2)

stargazer(reg1, reg2,
          type = "text",
          labels = c("reg1", "reg2"))

# adicionando lag de 3ª ordem para o nível de emprego e um lag de 1ª ordem para os salários:
form2 <- log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + lag(log(emp), 3) +
                    lag(log(wage), 1) + lag(log(wage), 2) + lag(log(wage), 3) + 
                    diff(log(capital), 2) + diff(log(capital), 3)
reg3 <- plm(formula = form2,
            data = EmplUK,
            model = "pooling")
stargazer(reg1, reg2, reg3,
          type = "text",
          labels = c("reg1", "reg2", "reg3"))

# Efeitos Fixos
reg4_within1 <- plm(formula = form1,
                    data = EmplUK,
                    model = "within")
summary(reg4_within1)

fixef(reg4_within1) # para extrair os efeitos fixos de cada unidade
# agora vamos incluir os efeitos dos períodos:
reg5_within1 <- plm(log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + 
                               lag(log(wage), 2) + lag(log(wage), 3) + 
                               diff(log(capital), 2) + diff(log(capital), 3) +
                               as.factor(year),
                    data = EmplUK,
                    model = "within")
summary(reg5_within1)

stargazer(reg1, reg4_within1, reg5_within1,
          type = "text")

# primeiras diferenças
reg6_pd1 <- plm(formula = form1,
                data = EmplUK,
                model = "fd")
summary(reg6_pd1)

# between
reg7_between <- plm(formula = form1,
                    data = EmplUK,
                    model = "between")
summary(reg7_between)

# Efeitos Aleatórios
reg8_aleatorios <- plm(formula = form1,
                    data = EmplUK,
                    model = "random")
summary(reg8_aleatorios)

phtest(reg5_within1, reg8_aleatorios) # caso rejeitamos H0: modelos de efeitos fixos --> c.c. EA
pFtest(reg5_within1, reg8_aleatorios) # caso rejeitamos H0: EF ou EA --> c.c. POLS

stargazer(reg4_within1, reg8_aleatorios, reg2,
          type = "text")

rm(list = ls(all.names = TRUE))

# ---- EXERCÍCIO 2 -----

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
#           efeitos do tempo (d88 e d89), o possível recebimento de subsídio (dummy grant) e
#           o recebimento de subsídio no período passado (dummy grant_1), uma vez que devemos
#           permitir que o efeito do subsídio possa persistir por um período.

# fcode: número da firma
 
# Verificar se o painel está balanceado.
painel_balanceado <- is.pbalanced(jtrain)
print(painel_balanceado) # caso não esteja balanceado, tem a função pra isso: make.pbalanced

# Algumas estatísticas descritivas
jtrain_somados <- jtrain %>% 
                  group_by(year) %>% 
                  summarize(total_scrap = sum(scrap,
                                              na.rm = TRUE))

print(jtrain_somados)

jtrain_subamostra <- jtrain %>% select(lscrap, union, d88, d89, grant, grant_1)
summary(jtrain_subamostra)

graph1 <- ggplot(jtrain[jtrain$fcode > 419459, ],
                 aes(x = year,
                     y = sales,
                     group = fcode,
                     color = factor(fcode))) +
          geom_line() +
          labs(title = "Gráfico de Linha para Vendas (Sales) por Código de Unidade (fcode > 419459)",
               x = "Ano",
               y = "Vendas")+
          theme_minimal()
  
print(graph1)

# POLS
reg1 <- lm(lscrap ~ grant + grant_1 + union + d88 + d89, 
             data = jtrain)
reg2 <- plm(lscrap ~ grant + grant_1 + union + d88 + d89, 
           data = jtrain)
stargazer(reg1, reg2, 
          type = "text",
          column.labels = c("OLS", "POLS"))
# Sindicalização é significativo para explicar a taxa de perda. 



# Efeitos Aleatórios
reg3_aleatorios <- plm(lscrap ~ grant + grant_1 + union + d88 + d89, 
                       data = jtrain,
                       model = "random", 
                       random.method = "amemiya")
summary(reg3_aleatorios)


reg4_fixos <- plm(lscrap ~ grant + grant_1 + union + d88 + d89, 
                       data = jtrain,
                       model = "within")
summary(reg4_fixos)

phtest(reg3_aleatorios, reg4_fixos) # EA

# primeiras diferenças
reg5_pd1 <- plm(lscrap ~ grant + grant_1 + union + d88 + d89, 
                data = jtrain,
                model = "fd")
summary(reg5_pd1)

stargazer(reg1, reg2, reg3_aleatorios, reg4_fixos, reg5_pd1,
          type = "text")

rm(list = ls(all.names = TRUE))


# ---- EXERCÍCIO 3 ----DIFF-IN-DIFF----

# Houve um aumento no salário mínimo em NJ de 4,25 para 5,05 por hora.
# Os autores avaliam a lei através de uma entrevista em restaurantes fast-food em NJ e PA.
# Analisam se o aumento do salário mínimo reduziu o emprego. 

card_krueger <- read_csv("Doutorado/PAE/Econometria II/card_krueger.csv")

card_krueger <- card_krueger %>% mutate(emptot = emppt + empft)

# Estatísticas Descritivas
card_krueger %>% select(chain, state) %>%
                 table() %>%
                 prop.table(margin = 2)  %>%
                 apply(MARGIN = 2,
                       FUN = scales::percent_format(accuracy = 0.1)) %>%
                 noquote

# 1ª onda (média pré tratamento)
card_krueger %>% filter(observation == "February 1992") %>%
                 group_by(state) %>%
                 summarise(emptot = mean(emptot, na.rm = TRUE),
                           wage_st = mean(wage_st, na.rm = TRUE),
                           hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
                  pivot_longer(cols=-state, 
                               names_to = "variable") %>%
                  pivot_wider(names_from = state, 
                              values_from = value)
# 2ª onda (média pós tratamento)
card_krueger %>% filter(observation == "November 1992") %>%
                 group_by(state) %>%
                 summarise(emptot = mean(emptot, na.rm = TRUE),
                           wage_st = mean(wage_st, na.rm = TRUE),
                           hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
                 pivot_longer(cols=-state, 
                              names_to = "variable") %>%
                 pivot_wider(names_from = state, 
                             values_from = value)

# 1ª diferença

dif_1 <- card_krueger %>% group_by(observation, state) %>%
                          summarise(emptot = mean(emptot, na.rm = TRUE))

NJ_antes <- dif_1[1,3] # Grupo tratado (NJ) antes do tratamento
PA_antes <- dif_1[2,3] # Grupo de controle (PA) antes do tratamento
NJ_depois <- dif_1[3,3] # Grupo tratado (NJ) depois do tratamento
PA_depois <- dif_1[4,3] # Grupo de controle (PA) depois do tratamento

# ATT
# Calcular a diferença entre a diferença de novembro e fevereiro dentro dos grupos
ATT1 <- (NJ_depois - NJ_antes) - (PA_depois - PA_antes) 
print(ATT1)
# Calcular a diferença entre a diferença dos grupos dentro de novembro e fevereiro
ATT2 <- (NJ_depois - PA_depois) - (NJ_antes - PA_antes)
print(ATT2)

# DiD

# Criar 2 variáveis fictícias:
# Inicio_tratam: = 0 antes do tratamento; = 1 depois do tratamento
# Grupos: = 0 restaurantes fast food em PA; = 1 rstaurantes fast food em NJ

card_krueger <- mutate(card_krueger,
                       time = ifelse(observation == "November 1992", 1, 0),
                       treated = ifelse(state == "New Jersey", 1, 0)
                      )

did_model <- lm(emptot ~ time + treated + time:treated, 
                data = card_krueger)
summary(did_model)

# Efeitos Fixos
painel <- pdata.frame(card_krueger, 
                      "sheet")
did_within <- plm(emptot ~ time + treated + time:treated, 
                  data = painel, 
                  model = "within")
summary(did_within)

# Cluster --> erro padrão 
coeftest(did_within, 
         vcov = function(x) vcovHC(x, 
                                   cluster = "group", 
                                   type = "HC1")
         )
