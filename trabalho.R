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



# DADOS DUPLICADOS 
duplicated(conv_matr$EMPCT) %>% sum()
duplicated(conv_matr) %>% sum()
conv_matr %<>% subset(!duplicated(conv_matr$EMPCT))

# estimar a média do TOTAL
dados <- fase1 %>% left_join(conv_matr) %>% 
  left_join(opcoes) %>% 
  left_join(questionario)
dados$CONVOCADO %<>% replace_na(0)
dados$MATRICULADO %<>% replace_na(0)
dados$opc1 %<>% replace_na(0)
dados$opc1d %<>% replace_na('INFO INDISPONIVEL')
dados$opc2 %<>% replace_na(0)
dados$opc2d %<>% replace_na('INFO INDISPONIVEL')


VarianciasDosEstratos <- function(dados, ...) {
  media <- mean(dados$TOTAL)
  group_by(dados, ...) %>% 
    summarise(mean = mean(TOTAL), sd = sd(TOTAL), n = n()) %>% 
    (function(df) {
      list(sigma_d = sum(df$n * var(df$mean)) / sum(df$n), 
           sigma_e = sum(df$n * (df$mean-media)^2) / sum(df$n))
    })
}

