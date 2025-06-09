## Econometria II (FEAUSP) - Monitoria em R ##
## Monitoria R - RDD  - 01.12.2023 ##
## Eduarda Figueiredo (eduardafigueiredo@usp.br), Lucas Novaes (lucasnovaes91@usp.br)

library("readr")
library("readxl")
library("ggplot2")
library("tidyverse")
library("broom")
library("magrittr")
library("stargazer")
library("rdrobust")
library("gridExtra")
library("estimatr")
library("haven")

#----EXERCÍCIO 1----

# Criando dados simulados
dat <- tibble(x = rnorm(1000, 50, 25)) %>% 
       mutate(x = if_else(x < 0, 0, x)) %>% 
       filter(x < 100)

# Cutoff x=50
dat <- dat %>% mutate(D = if_else(x > 50, 1, 0),
                      y1 = 25 + 0 * D + 1.5 * x + rnorm(n(), 0, 20))

ggplot(aes(x,
           y1,
           colour = factor(D)),
       data = dat) +
       geom_point(alpha = 0.5) +
       geom_vline(xintercept = 50, 
                  colour = "grey",
                  linetype = 2) +
       stat_smooth(method = "lm",
                   se = F) +
       labs(x = "Test Score (x)",
            y = "Potential Outcome (y1)")
# y1 não saltou para 50 na variável x.

dat <- dat %>% mutate(y2 = 25 + 40 * D + 1.5 * x + rnorm(n(), 0, 20))
ggplot(aes(x,
           y2,
           colour = factor(D)),
       data = dat) +
       geom_point(alpha = 0.5) +
       geom_vline(xintercept = 50, 
                  colour = "grey",
                  linetype = 2) +
       stat_smooth(method = "lm",
                   se = F) +
       labs(x = "Test Score (x)",
            y = "Potential Outcome (y)")
# y salta para 50 na variável x.
# observe o salto na discontinuidade (LATE)



#----EXERCÍCIO 2----

mlda_df <- readr::read_csv("https://raw.githubusercontent.com/seramirezruiz/stats-ii-lab/master/Session%206/data/mlda.csv")

mlda_df <- mlda_df |> 
           select(c(outcome, agecell, treatment, forcing))

# Vamos estimar um modelo RDD usando dados de 2009 do paper do Carpenter e Dobkin (2009)
# sobre o efeito do aumento da idade permitida para beber nas taxas de mortalidade.
# Vamos apenas olhar as mortes no trânsito como função da idade.

ggplot(mlda_df, aes(x = agecell, 
                    y = treatment,
                    color = factor(treatment))) +
  geom_point(size = 1.5, 
             alpha = 0.5,
             position = position_jitter(width = 0, 
                                        height = 0.25, 
                                        seed = 1234)) + 
  labs(x = "Idade", 
       y = "Probabilidade de ser tratado") +
  scale_color_discrete(name = " ", 
                       labels = c("Não possui idade legal para beber", 
                                  "Possui idade legal para beber")) +
  geom_vline(xintercept = 21, 
             linetype = "dotted") +
  theme_bw()+
  theme(legend.position = "bottom")

# Parece haver uma descontinuidade aos 21 anos. Assim, o uso do RDD sharp é o indicado. A partir
# disso, analisaremos então o comportamento dos dados a fim de entender qual modelo melhor
# capturará a variação dos dados.

ggplot(mlda_df, 
       aes(x = agecell, 
           y = outcome)) + 
  geom_point() +
  geom_vline(xintercept = 21, 
             linetype = "dotted") +
  labs(title = "Análise exploratória",
       x = "Variável forçada (Idade)",
       y = "Taxa de mortalidade por acidentes\n de transito (por 100,000)") +
  theme_minimal()

# Estimar o modelo RDD:
# Y = \alpha + D_\T + \beta X + \varepsilon

# Primeiro, rodamos um modelo linear com retas comum.
summary(mlda_df$forcing) # Forcing é centralizada em 0 (idade 21) e representa a 
                         # distância em anos a partir dos 21 anos, enquanto o
                         # tratamento é apenas binário, acima/abaixo de 21 anos.
modelo_linear <- lm(outcome ~ treatment + forcing,
                    data = mlda_df)
summary(modelo_linear)
stargazer(modelo_linear,
          type = "text")
# Então, podemos formalizar esse modelo da seguinte forma:
# E(Y_i|X_i,D_i) = \tilde{\beta}_0 + \beta_1 treatment_i + \beta_2 \tilde{forcing}_i + \varepsilon
# Logo, a cada 100.00 pessoas que podem legalmente consumir bebidas alcoólicas, podemos esperar
# um aumento de 4,53 mortes por acidentes de trânsito. A cada ano de aumento na idade, o número
# de mortes diminui em 3,15.

# Extrair os valores preditos para criar o fit e visualizar no gráfico:
mlda_df$var_predita <- predict(modelo_linear)
mlda_df %>% ggplot(aes(x = forcing,
                       y = var_predita,
                       col = factor(treatment))) +
            geom_point(aes(x = forcing,
                           y = outcome,
                           col = factor(treatment)))+
            geom_vline(xintercept = 0, 
                       linetype = "dotted") +
            labs(title = "Modelo linear com reta comum",
                 x = "Variável forçada (Idade)",
                 y = "Taxa de mortalidade por acidentes\n de transito (por 100,000)") +
            geom_line(data = mlda_df[mlda_df$forcing >= 0,], 
                      color = "pink",
                      size = 1) +
            geom_line(data = mlda_df[mlda_df$forcing < 0,], 
                      color = "grey", 
                      size = 1) +
            scale_color_manual(name = "",
                               values = c("grey", "pink"),
                               labels = c("Controle", "Tratamento")) +
            theme_bw()

# Modelo que permite a variação entre as retas da regessão:
# E(Y_i|X_i,D_i) = \tilde{\beta}_0 + \beta_1 D_1 + \beta_2 X_i + \tilde{\beta}_3 \tilde{X}_i + \varepsilon
modelo_linar_variacao <- lm(outcome ~ treatment*forcing,
                            data = mlda_df)
summary(modelo_linar_variacao)
stargazer(modelo_linear, modelo_linar_variacao,
          type = "text")
# Para indivíduos com menos de 21 anos, um aumento de 1 ano de idade resultaria, em média, em 
# 2,57 mortes a menos por acidentes de veículos. Para os que tem idade para beber (21),
# é esperado 3,73 mortes a menos por acidentes de trânsito para cada aumento de 1 ano na idade.
# \beta_2 X_i + \beta_3 \tilde{X}_i = -2,57 + (-1,16) = -3,37.
# R² mais alto: modelo mais adequado.
mlda_df$var_predita <- predict(modelo_linear)
mlda_df %>% ggplot(aes(x = forcing,
                       y = outcome,
                       col = factor(treatment))) +
            geom_point()+
            geom_vline(xintercept = 0, 
                       linetype = "dotted") +
            geom_smooth(method = "lm",
                        se = F) +
            labs(title = "Modelo linear com retas variando",
                 x = "Variável forçada (Idade)",
                 y = "Taxa de mortalidade por acidentes\n de transito (por 100,000)") +
            scale_color_manual(name = "",
                               values = c("grey", "pink"),
                               labels = c("Controle", "Tratamento")) +
            theme_bw()

# Calculando o Local Average Treatment Effect para aqueles com mais de 21 anos por meio
# de uma regressão polinomial local com correção de viés.
late <- rdrobust(mlda_df$outcome,
                 mlda_df$forcing,
                 c = 0,
                 kernel = "tri",
                 bwselect = "mserd")
summary(late)
# Efeito não significante do tratamento para aqueles com mais de 21 anos.
rdplot(mlda_df$outcome, 
       mlda_df$forcing,  
       c = 0,
       kernel = "tri",
       title = "Morte em acidentes no transito",
       x.label = "Idade a partir de 21",
       y.label =  "Morte em acidentes no transito (per 100,000)"
       )



#----EXERCÍCIO 3----

lmb_data <- read_dta("Doutorado/PAE/Econometria II/lmb-data.dta") # Lee, Moretti and Butler (2004)

# Dados do Americans for Democratic Action (ADA) vinculados com os resultados das
# eleições para a Câmara de 1946-1995.
# Os autores desse paper usaram a pontuação ADA para todos os representantes da
# Câmara dos EUA de 1946-1995 como índice de registro de votação.
# Foi criado um índice variando de 0 a 100 para cada representante.
# Pontuções mais altas = histórico de votação mais "liberal".
# Running variable =  parcela de votos (voting record), que é a parcela de todos os
# votos que foram para um democrata.

# Para uma avaliação boa, precisamos da aleatorização de D_t. Para isso, os
# autores usam uma variação possivelmente exógena nas vitórias democratas para
# verificar se a convergência ou divergência estão corretas.
# Se a convergência for verdadeira:
#                                  então os republicanos e os democratas que venceram
#                                  por pouco deveriam votar de forma quase idêntica.
# Se a divergência for verdadeira:
#                                  deveriam votar de forma diferente à margem de uma
#                                  disputa acirrada.

lmb_subset <- lmb_data %>% 
              filter(lagdemvoteshare > .48 & lagdemvoteshare < .52)

lm1 <- lm_robust(score ~ lagdemocrat,
                 data = lmb_subset,
                 clusters = id)
lm2 <- lm_robust(score ~ democrat,
                 data = lmb_subset,
                 clusters = id)
lm3 <- lm_robust(democrat ~ lagdemocrat,
                 data = lmb_subset,
                 clusters = id)
summary(lm1) # 21,28*** (1,95) (ADA_t+1)
summary(lm2) # 47,71*** (1,36) (ADA_t)
summary(lm3) # 0,48*** (0,03)  (DEM_t+1)

# O choque exógeno vem da descontinuidade na running variable.
# Com uma parcela de votos pouco acima de 0,5 o candidato democrata vence. Os autores
# argumentam que perto desse limite, uma chance aleatória determinou a vitória democrata.
# O efeito de uma vitória democrata aumenta o voto liberal em 21 pontos no próximo período,
# 48 pontos no período atual e a probabilidade de reeleição em 48%.
# Os autores encontraram evidências tanto para divergência quanto para vantagem de incumbência
# usando este design.

# Plot
categories <- lmb_data$lagdemvoteshare

demmeans <- split(lmb_data$score,
                  cut(lmb_data$lagdemvoteshare, 100)) %>% 
            lapply(mean) %>% 
            unlist()

agg_lmb_data <- data.frame(score = demmeans,
                           lagdemvoteshare = seq(0.01, 
                                                  1, 
                                                  by = 0.01))

lmb_data <- lmb_data %>% 
            mutate(gg_group = case_when(lagdemvoteshare > 0.5 ~ 1,
                                        TRUE ~ 0))

ggplot(lmb_data,
       aes(lagdemvoteshare,
           score)) +
  geom_point(aes(x = lagdemvoteshare,
                 y = score),
             data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, 
                  score, 
                  group = gg_group),
              method = "lm",
              formula = y ~ x + I(x^2)) +
  xlim(0, 1) + ylim(0, 100) +
  geom_vline(xintercept = 0.5) +
  labs(y = "Pontuação ADA, t+1",
       x = "Votos democratas, t")
# Mostra a relação entre a vitória democrata e os candidatos, pontuação ADA do segundo período.
