setwd('~/Documents/UNICAMP/Disciplinas/Caio/Trabalho/')

library(readr)
library(readxl)
library(dplyr)
library(magrittr)
library(ggplot2)

# leitura dos dados
questionario <- read_csv2('./dados/2017_quest.txt')
questionario$APROVA1 <- ifelse(is.na(questionario$APROVA1), 0, 1)

conv_matr <- read_csv2('./dados/ConvocadosMatriculados.csv')
conv_matr$CONVOCADO <- ifelse(is.na(conv_matr$CONVOCADO), 0, 1)
conv_matr$MATRICULADO <- ifelse(is.na(conv_matr$MATRICULADO), 0, 1)

fase1 <- read_csv2('./dados/Fase1TipoQ.csv', 
                   col_types = cols_only('EMPCT' = col_integer(), 
                                         'TOTAL' = col_integer()))

opcoes <- read_xls('./dados/Opcoes.xls')


# exploração
tmp <- left_join(fase1, questionario, by = c('EMPCT' = 'empct')) %>% 
  left_join(conv_matr)


group_by(tmp, Q4, Q5, Q6, CONVOCADO) %>% 
  summarise(mean = mean(TOTAL), sd = sd(TOTAL), n = n()) %>% 
  (function(df) {
    list(sigma_d = sum(df$n * var(df$mean)) / sum(df$n), 
         sigma_e = sum(df$n * (df$mean-mean(tmp$TOTAL))^2) / sum(df$n))
  })
