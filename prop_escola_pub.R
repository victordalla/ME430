library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)


# leitura dos dados
questionario <- read_csv2('./dados/2017_quest.txt')
questionario$APROVA1 <- ifelse(is.na(questionario$APROVA1), 0, 1)
colnames(questionario)[1] <- 'EMPCT'

conv_matr <- read_csv2('./dados/ConvocadosMatriculados.csv')
conv_matr$CONVOCADO <- ifelse(is.na(conv_matr$CONVOCADO), 0, 1)
conv_matr$MATRICULADO <- ifelse(is.na(conv_matr$MATRICULADO), 0, 1)

fase1 <- read_csv2('./dados/Fase1TipoQ.csv', 
                   col_types = cols_only('EMPCT' = col_integer(), 
                                         'TOTAL' = col_integer()))

opcoes <- read_xls('./dados/Opcoes.xls')

# Proporcao de alunos de escola pública

pub <- questionario[questionario$Q3 == 1, ]$EMPCT
priv <- questionario[questionario$Q3 != 1, ]$EMPCT

N_h <- c(length(pub), length(priv))
N <- dim(questionario)[1]
W_h <- N_h / N
H <- 2 # quantidade de extratos

# Amostra piloto 

n_piloto <- c(50, 50)
# n_piloto <- c(250, 250)

amostra_pil_1 <- sample_n(questionario %>% filter(EMPCT %in% pub), n_piloto[1])
amostra_pil_2 <- sample_n(questionario %>% filter(EMPCT %in% priv), n_piloto[2])

p_piloto <- c(sum(amostra_pil_1$Q4 == 1) / dim(amostra_pil_1)[1],
              sum(amostra_pil_2$Q4 == 1) / dim(amostra_pil_2)[1])

s2hat_pil <- n_piloto / (n_piloto - 1) * p_piloto * (1 - p_piloto)

varhat_phat_pil <- (1 - n_piloto/N) * s2hat_pil / n_piloto


# Tamanhos amostrais

# Slide 30 
# http://www.ime.unicamp.br/~cnaber/aula_AE%20P2%20Amost%202S%202018.pdf
delta = 0.01 # erro de estimação DUVIDA!!!
z_t = qnorm(0.975) # para 0.95 de IC
n <- 1 / (delta^2/(z_t^2 * sum(W_h * s2hat_pil)) + 1/N)

n_h <- n * N_h * sqrt(s2hat_pil) / (sum(N_h * sqrt(s2hat_pil)))

# Amostragem final

amostra_1 <- sample_n(questionario %>% filter(EMPCT %in% pub), n_h[1])
amostra_2 <- sample_n(questionario %>% filter(EMPCT %in% priv), n_h[2])

phat_h <- c(sum(amostra_1$Q4 == 1) / dim(amostra_1)[1],
            sum(amostra_2$Q4 == 1) / dim(amostra_2)[1])

phat <- sum(W_h * phat_h)

