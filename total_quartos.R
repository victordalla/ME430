library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(fastkendall)


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

opcoes <- read_csv2('./dados/Opcoes.csv')

# Total de quartos na residência

smp <- sample_n(questionario, 500)

smp %>% ggplot(aes(Q32D, Q32C, alpha = 0.2)) + 
    geom_jitter() + xlab("# Banheiros") + ylab("#  Quartos")

# Deve ser maior que 0.6
smp %$% fastkendall(.$Q32D, .$Q32C)

# Deve ser entre 0.5 e 1.3
(sd(smp$Q32D) / mean(smp$Q32D)) / (sd(smp$Q32C)/mean(smp$Q32C))

# Deve ser menor que rho(x, y)
(sd(smp$Q32D) / mean(smp$Q32D)) / (2 * sd(smp$Q32C)/mean(smp$Q32C))

