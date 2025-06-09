## Econometria II (FEAUSP) - Monitoria em R ##
## Aula 2  - 24.08.2023 - Aleatorização em R ##
## Eduarda Figueiredo (eduardafigueiredo@usp.br), Dimitri Maturano (dimitricecanm@usp.br)

#install.packages(tidyverse)
#install.packages("haven")
#install.packages("stats)

library(tidyverse)
library(haven)
library(stats)

read_data <- function(df)
{
 full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, sep = "")
 df <- read_dta(full_path)
 return(df)
}

# Cada linha neste conjunto de dados é um local específico na Inglaterra.
# Então, como existem 32 linhas, isso significa que os dados conjunto contém 32 locais.
# Cada uma das variáveis é expressa como uma taxa de crescimento anual.

yule <- read_data("yule.dta")

# Indico vocês a testarem em casa fazer aquela análise primária dos dados (aula passada)

yulereg <- read_data("yule.dta") %>% 
        lm(paup ~ outrelief + old + pop, .)
summary(yulereg)

# Uma mudança de 10pp em outrelief esta associada a um aumento de 7,5pp na taxa de miséria.
# Yule utilizou a sua regressão para estabelecer a correlação entre a ajuda humanitária e a miséria,
# da qual concluiu que a assistência pública aumentou as taxas de crescimento da pobreza.

# Raciocínio errado? 
# Poderia haver algo despercebido determinantes da pobreza e da assistência pública? R: Não tem controles econômicos.

gap <- function() 
{
  sdo <-  tibble(
    y1 = c(7,5,5,7,4,10,1,5,3,9),
    y0 = c(1,6,1,8,2,1,10,6,7,8),
    random = rnorm(10)
  ) %>% 
    arrange(random) %>% 
    mutate(
      d = c(rep(1,5), rep(0,5)),
      y = d * y1 + (1 - d) * y0
    ) %>%
    pull(y)
  
  sdo <- mean(sdo[1:5]-sdo[6:10])
  
  return(sdo)
}

sim <- replicate(10000, gap()) # Roda 10.000x, cada vez calculando o SDO médio sob independência -
                               # que é assegurada pela classificação de números aleatórios que ocorre.
mean(sim) # 0,59~0,60

#-----

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, 
                     sep = "")
  df <- read_dta(full_path)
  return(df)
}

ri <- read_data("ri.dta") %>% mutate(id = c(1:8)) 

ri <- ri %>% mutate_at(vars(y0, y1), as.numeric)
str(ri)
ri <- ri %>% mutate(y0 = if_else(is.na(y0), 0, y0))
ri <- ri %>% mutate(y1 = if_else(is.na(y1), 0, y1))

# Para cada unidade, há apenas um resultado potencial.
# Mas sob o nulo, se pode inferir o outro contrafactual ausente.
# Só temos informações nos resultados observados com base na equação de comutação.
# Então, se uma unidade é tratada, conhecemos seu Y1, mas não seu Y0.

# A ausência do tratamento para alguns não gera automaticamente o contrafactual de não tratamento para outros.

# Teste de estatística (t(D,Y)): é simplesmente uma estatística conhecida, quantidade escalar 
#                                calculada a partir das atribuições de tratamento e dos resultados observados.
# É a simples diferenças nos resultados médios.
# O uso da comparação direta entre os dois grupos não necessariamente mede o 
# efeito causal do programa. Afinal, as diferenças nas características não
# observáveis entre os grupos poderiam estar misturadas ao efeito do programa.


mediay1 <- mean(ri$y1)
mediay0 <- mean(ri$y0)
simples_tstatistic <- mediay1-mediay0

treated <- c(1:4) # Atribuição de tratamento

# Embora historicamente não saibamos o contrafactual de cada unidade, sob o nulo  
# conhecemos cada contrafactual da unidade. Como isso é possível? Porque se nenhum das
# unidades tem efeitos de tratamento diferentes de zero, então deve ser que cada contrafactual é 
# igual ao seu resultado observado. Isso significa que podemos preencher os contrafactuais 
# faltantes com os valores observados.

# puxa base de novo

ri <- ri %>% mutate(y0 = if_else(is.na(y0), y, y0))
ri <- ri %>% mutate(y1 = if_else(is.na(y1), y, y1))

ri <- ri %>% mutate_at(vars(y0, y1), as.numeric)

mediay1 <- mean(ri$y1)
mediay0 <- mean(ri$y0)
simples_tstatistic2 <- mediay1-mediay0

# Com esses contrafactuais ausentes substituídos pelos correspondentes resultados observados, 
# não há efeito de tratamento no nível da unidade e portanto, um ATE zero.

# Então, por que encontramos anteriormente uma simples diferença em resultados médios de 0,5 se 
# de fato não houve tratamento médio efeito? Simples – era apenas barulho, puro e simples.

# Calcular o pvalor

combo <- ri %$% as_tibble(t(combn(id, 4))) %>%
                transmute(treated1 = V1, 
                          treated2 = V2,
                          treated3 = V3, 
                          treated4 = V4) %>%
                mutate(permutation = 1:70) %>%
                crossing(., ri) %>%
                arrange(permutation, name) %>% 
                mutate(d = case_when(id == treated1 | id == treated2 |
                                     id == treated3 | id == treated4 ~ 1,
                                     TRUE ~ 0)
                       )

te1 <- combo %>%
       group_by(permutation) %>%
       filter(d == 1) %>% 
       summarize(te1 = mean(y, na.rm = TRUE))

te0 <- combo %>%
       group_by(permutation) %>%
       filter(d == 0) %>% 
       summarize(te0 = mean(y, na.rm = TRUE))

n <- nrow(inner_join(te1, 
                     te0, 
                     by = "permutation"))

p_value <- inner_join(te1, 
                      te0, 
                      by = "permutation") %>%
           mutate(ate = te1 - te0) %>% # calculo do valor absoluto da diferença mais simples na classificação média
           select(permutation, 
                  ate) %>% 
           arrange(desc(ate)) %>% 
           mutate(rank = 1:nrow(.)) %>% 
           filter(permutation == 1) %>%
           pull(rank)/n

# Todas essas estatísticas de teste foram diferenças nos resultados por status de tratamento.

# Podemos estar interessados em uma estatística de teste que possa detectar diferenças nas
# distribuições de tratamento e controle.
# Estatística de Teste Kolmogorov-Smirnov

tb <- tibble(
      d = c(rep(0, 20), rep(1, 20)),
      y = c(0.22, -0.87, -2.39, -1.79, 0.37, -1.54, 
            1.28, -0.31, -0.74, 1.72, 
            0.38, -0.17, -0.62, -1.10, 0.30, 
            0.15, 2.30, 0.19, -0.50, -0.9,
            -5.13, -2.19, 2.43, -3.83, 0.5, 
            -3.25, 4.32, 1.63, 5.18, -0.43, 
            7.11, 4.87, -3.10, -5.81, 3.76, 
            6.31, 2.58, 0.07, 5.76, 3.50)
)

kdensity_d1 <- tb %>% filter(d == 1) %>% 
                      pull(y)
kdensity_d1 <- density(kdensity_d1)

kdensity_d0 <- tb %>% filter(d == 0) %>% 
                      pull(y)
kdensity_d0 <- density(kdensity_d0)


kdensity_d0 <- tibble(x = kdensity_d0$x, 
                      y = kdensity_d0$y, 
                      d = 0)

kdensity_d1 <- tibble(x = kdensity_d1$x, 
                      y = kdensity_d1$y, 
                      d = 1)

kdensity <- full_join(kdensity_d1, 
                      kdensity_d0)
kdensity$d <- as_factor(kdensity$d)

ggplot(kdensity)+
       geom_point(size = 0.3, 
                  aes(x, y, 
                      color = d))+
       xlim(-7, 8)+
       labs(title = "Teste Kolmogorov-Smirnov")+
       scale_color_discrete(labels = c("Controle", 
                                       "Tratmento"))

#---- THORTON

read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", 
                     df, 
                     sep = "")
  df <- read_dta(full_path)
  return(df)
}

hiv <- read_data("thornton_hiv.dta")


# Criando as permutações

tb <- NULL

permuteHIV <- function(df, 
                       random = TRUE){
                                      tb <- df
                                      # Nº de tratados na base
                                      n_treated <- 2222
                                      n_control <- nrow(tb) - n_treated
  
  if(random == TRUE){
                     tb <- tb %>%
                     sample_frac(1) %>%
                     mutate(any = c(rep(1, n_treated), 
                                    rep(0, n_control)))
  }
  
  te1 <- tb %>% filter(any == 1) %>%
                pull(got) %>%
                mean(na.rm = TRUE)
  
  te0 <- tb %>% filter(any == 0) %>%
                pull(got) %>% 
                mean(na.rm = TRUE)
  
  ate <-  te1 - te0
  
  return(ate)
}

permuteHIV(hiv, 
           random = FALSE)

iterations <- 1000

permutation <- tibble(
                      iteration = c(seq(iterations)), 
                      ate = as.numeric(
                      c(permuteHIV(hiv, 
                                   random = FALSE), 
                        map(seq(iterations-1), 
                            ~permuteHIV(hiv, 
                                        random = TRUE)))
  )
)

# Calculando o pvalor

permutation <- permutation %>% 
               arrange(-ate) %>% 
               mutate(rank = seq(iterations))

p_value <- permutation %>% 
           filter(iteration == 1) %>% 
           pull(rank)/iterations

# Abrir a base de permutação: mostra o experimento de Thornton sob nulo
# Produz valores p altamente significativos

# A simples diferença nos resultados médios foi igual à soma da média efeito do tratamento, 
# ou o viés de seleção, e viés do efeito heterogêneo ponderado do tratamento. 

# Assim, a simples diferença na média do estimador de resultados é tendencioso.
# A não ser que o segundo e terceiro termos sejam zerados.

# Uma situação em que eles zeram é sob independência do tratamento, que é quando o tratamento 
# foi atribuído independentemente dos resultados potenciais. Quando é que independência ocorre? 
# A situação mais comumente enfrentada é sob randomização física do tratamento para as unidades.


#install.packages("randomizr")
library(randomizr)

# Defina o tamanho da amostra
n <- 100

# Crie um dataframe simulado com duas variáveis: 'grupo' e 'resultado'
set.seed(123)
dados <- data.frame(grupo = factor(c(rep("Controle", n/2), 
                                     rep("Tratamento", n/2))),
                    resultado = c(rnorm(n/2, mean = 10, sd = 2), 
                                  rnorm(n/2, mean = 12, sd = 2.5))
)


# Use a função simple_ra() do pacote 'randomizr' para fazer a aleatorização
dados$simple <- simple_ra(N = 100,
                          num_arms = 2)

# Calcule o ATE
ate <- with(dados, 
            mean(resultado[grupo == "Tratamento"]) - mean(resultado[grupo == "Controle"]))

cat("ATE estimado:", ate, "\n")
